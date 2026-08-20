#' Draw a 3D Item Response Theory (IRT) map: characteristic curves + double histogram
#'
#' Fits an Item Response Theory model with [mirt::mirt()] -- dichotomous
#' items (0/1) as 1PL/2PL/3PL, polytomous items (e.g. Likert 1-5 or 1-10) as
#' the Graded Response Model (GRM) -- and draws every item's characteristic
#' curve(s) as a 3D "cordillera": `x` is the shared trait scale (theta),
#' `y` separates items (one depth slot per item, ordered by difficulty),
#' `z` is the probability of endorsing that response level or higher. A
#' polytomous item with k response categories contributes k-1 boundary
#' curves (one per threshold between consecutive categories), each with its
#' own legend entry that can be toggled on/off independently by clicking it
#' (or double-clicked to isolate it) -- there is no separate "azar"
#' (guessing) parameter for these, since it does not exist in the graded
#' model.
#'
#' A flat panel is inserted at the front of the scene, sharing the same
#' `x` (theta) axis as the curves and the same "0" point as `z = 0`
#' (probability 0): a histogram of the sample's estimated ability (theta,
#' from [mirt::fscores()]) grows upward from that shared line, and directly
#' below it a histogram of item/threshold difficulty grows downward from
#' the same line, each item/threshold "hanging" from theta = 0 and stacking
#' with others that fall in the same theta bin. A thin line connects each
#' curve's own P = 0.5 crossing point (`theta_50`) to where it lands in
#' that difficulty histogram -- for a 3PL item with guessing, `theta_50`
#' is NOT the raw difficulty parameter `b` (see Details).
#'
#' For a dichotomous 3-parameter item, `P(theta) = c + (1-c) / (1 +
#' exp(-a*(theta-b)))`, and solving `P(theta) = 0.5` gives
#' `theta_50 = b - (1/a) * log(0.5 / (0.5 - c))`, which equals `b` only when
#' `c = 0` (2PL/Rasch); the vertical drop line and the difficulty histogram
#' both use this real crossing point, not `b` directly. For a polytomous
#' (graded) threshold, the boundary curve `P(X >= k | theta) = 1 / (1 +
#' exp(-a*(theta-b_k)))` has no guessing term, so its crossing point is
#' `b_k` itself.
#'
#' @param data A data frame with one row per subject and one column per
#'   item. Each item column must be numeric/integer-coded: 2 distinct
#'   values for a dichotomous item (fit as 1PL/2PL/3PL), 3 or more for a
#'   polytomous item (fit as a Graded Response Model). Items can be mixed
#'   (some dichotomous, some polytomous) in the same call.
#' @param items Character vector of column names in `data` to include.
#'   Default: all columns.
#' @param guessing Logical. If `TRUE`, dichotomous items are fit as 3PL
#'   (with a guessing/"azar" parameter); if `FALSE` (default), as 2PL.
#'   Polytomous items never get a guessing parameter -- it does not exist
#'   in the Graded Response Model -- so this is silently inapplicable to
#'   them (a message notes this when relevant).
#' @param theta_method Ability-estimation method passed to
#'   [mirt::fscores()]: one of `"EAP"` (default), `"MAP"`, `"WLE"`, `"ML"`.
#' @param opacity Numeric in (0, 1]. Initial opacity of curves and
#'   histogram bars (also a live slider). Default 0.85.
#' @param bin_width Width of the shared theta bins used for both
#'   histograms. Default 0.5.
#' @param col_subj Color for the subject (ability) histogram. Default
#'   `"#eda100"`.
#' @param col_items Optional named (or positional, matching `items`)
#'   character vector of colors, one per item. Default: a fixed 6-color
#'   palette recycled/extended with [grDevices::hcl.colors()] for more
#'   items.
#'
#' @return A `plotly` htmlwidget object. The fitted `mirt` model object,
#'   the extracted IRT-parameterized coefficients, the per-subject theta
#'   estimates, and a tidy data frame with one row per drawn curve
#'   (item, threshold label, a, b, c, theta_50) are attached as the
#'   `"stats"` attribute.
#'
#' @examples
#' set.seed(1)
#' n <- 200
#' theta <- rnorm(n)
#' logistic <- function(x) 1 / (1 + exp(-x))
#' df <- data.frame(
#'   i1 = rbinom(n, 1, logistic(1.2 * (theta - (-0.5)))),
#'   i2 = rbinom(n, 1, logistic(1.4 * (theta - 0.3))),
#'   i3 = rbinom(n, 1, logistic(0.9 * (theta - 0.8)))
#' )
#' if (requireNamespace("mirt", quietly = TRUE) &&
#'     requireNamespace("plotly", quietly = TRUE)) {
#'   irt_plot3d(df, items = c("i1", "i2", "i3"))
#' }
#'
#' @export
irt_plot3d <- function(data, items = names(data), guessing = FALSE,
                        theta_method = c("EAP", "MAP", "WLE", "ML"),
                        opacity = 0.85, bin_width = 0.5,
                        col_subj = "#eda100", col_items = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para irt_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para irt_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  if (!requireNamespace("mirt", quietly = TRUE)) {
    stop("El paquete 'mirt' es necesario para irt_plot3d() (ajusta el modelo TRI). ",
         "Instalalo con install.packages('mirt').")
  }
  theta_method <- match.arg(theta_method)
  stopifnot(opacity > 0, opacity <= 1, bin_width > 0)
  if (!all(items %in% names(data))) stop("'items' contiene columnas que no existen en 'data'.")
  if (length(items) < 3) {
    stop("Se necesitan al menos 3 items para irt_plot3d() (con solo 2 items dicotomicos ",
         "el modelo 2PL/3PL no esta identificado: mas parametros libres que grados de libertad).")
  }

  item_data <- as.data.frame(lapply(data[items], .as_plain_numeric))
  names(item_data) <- items
  ok <- stats::complete.cases(item_data)
  if (any(!ok)) {
    message("Se excluyeron ", sum(!ok), " fila(s) con datos faltantes en los items.")
    item_data <- item_data[ok, , drop = FALSE]
  }
  if (nrow(item_data) < 30) {
    stop("Se necesitan al menos 30 sujetos completos para ajustar el modelo con garantias minimas.")
  }

  ncat <- vapply(item_data, function(x) length(unique(x)), integer(1))
  if (any(ncat < 2)) {
    stop("Algun item no tiene variabilidad (menos de 2 categorias observadas): ",
         paste(items[ncat < 2], collapse = ", "))
  }
  is_poly <- stats::setNames(ncat > 2, items)
  itemtype <- ifelse(is_poly, "graded", if (guessing) "3PL" else "2PL")
  if (guessing && any(is_poly)) {
    message("El parametro de azar no existe en el modelo graduado (items politomicos); ",
            "se ignora para: ", paste(items[is_poly], collapse = ", "))
  }

  fit <- tryCatch(
    mirt::mirt(item_data, model = 1, itemtype = unname(itemtype), verbose = FALSE,
               technical = list(NCYCLES = 500)),
    error = function(e) stop("El modelo TRI no convergio: ", conditionMessage(e))
  )
  co <- mirt::coef(fit, IRTpars = TRUE, simplify = TRUE)$items
  theta_hat <- as.numeric(mirt::fscores(fit, method = theta_method)[, 1])

  curves <- .irt_extract_curves(items, is_poly, co, item_data)

  base_palette <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100", "#008300", "#4a3aa7")
  if (is.null(col_items)) {
    col_items <- if (length(items) <= length(base_palette)) {
      stats::setNames(base_palette[seq_along(items)], items)
    } else {
      stats::setNames(as.character(grDevices::hcl.colors(length(items), palette = "Dark 3")), items)
    }
  } else if (is.null(names(col_items))) {
    names(col_items) <- items[seq_along(col_items)]
  }
  curves$color <- unname(col_items[curves$item])
  curves$alpha <- 0.55
  for (nm in items) {
    idx <- which(curves$item == nm)
    if (length(idx) > 1) curves$alpha[idx] <- seq(0.5, 0.95, length.out = length(idx))
  }

  item_order <- items[order(vapply(items, function(nm) min(curves$theta50[curves$item == nm]), numeric(1)))]
  item_depth <- stats::setNames(seq_along(item_order), item_order)
  curves$depth <- item_depth[curves$item]

  theta_max <- max(4, ceiling(max(abs(c(theta_hat, curves$theta50)), na.rm = TRUE)) + 0.5)
  theta_grid <- seq(-theta_max, theta_max, length.out = 300)
  bin_edges <- seq(-theta_max, theta_max + bin_width, by = bin_width)
  bin_centers <- (utils::head(bin_edges, -1) + utils::tail(bin_edges, -1)) / 2

  subj_counts <- graphics::hist(theta_hat, breaks = bin_edges, plot = FALSE)$counts
  SUBJ_MAX <- 0.40
  subj_h <- if (max(subj_counts) > 0) subj_counts / max(subj_counts) * SUBJ_MAX else subj_counts * 0

  curves$bin <- cut(curves$theta50, breaks = bin_edges, labels = FALSE, include.lowest = TRUE)
  curves <- curves[order(curves$bin), ]
  curves$stack <- stats::ave(curves$bin, curves$bin, FUN = seq_along) - 1L
  max_stack <- max(curves$stack) + 1L
  SEG_H <- SUBJ_MAX / max_stack
  curves$seg_top <- -curves$stack * SEG_H
  curves$seg_bottom <- curves$seg_top - SEG_H

  Y_FRONT <- 0.30
  BIN_W <- bin_width * 0.42

  fig <- plotly::plot_ly(height = 850)
  trace_i <- 0L
  bar_idx <- integer(0)
  bar_colors <- character(0)
  first_subj_shown <- FALSE

  for (i in seq_len(nrow(curves))) {
    cv <- curves[i, ]
    p <- if (isTRUE(cv$poly)) {
      1 / (1 + exp(-cv$a * (theta_grid - cv$b)))
    } else {
      cv$c + (1 - cv$c) / (1 + exp(-cv$a * (theta_grid - cv$b)))
    }
    nm <- if (isTRUE(cv$poly)) sprintf("%s - %s", cv$item, cv$threshold_label) else cv$item
    grp <- paste0("grp_", i)
    hover <- sprintf(
      "%s<br>a = %.2f, b = %.2f%s<br>theta (P=0.5) = %.2f",
      nm, cv$a, cv$b, if (!isTRUE(cv$poly) && cv$c > 0) sprintf(", azar = %.2f", cv$c) else "", cv$theta50
    )
    fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
      x = theta_grid, y = rep(cv$depth, length(theta_grid)), z = p,
      line = list(color = cv$color, width = 3), opacity = cv$alpha,
      hoverinfo = "text", text = hover, name = nm, legendgroup = grp, showlegend = TRUE)
    trace_i <- trace_i + 1L

    fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines+markers",
      x = c(cv$theta50, cv$theta50), y = c(cv$depth, Y_FRONT), z = c(0.5, cv$seg_top),
      line = list(color = "#0b0b0b", width = 1.5), marker = list(size = 2, color = "#0b0b0b"),
      opacity = 0.55, hoverinfo = "skip", name = nm, legendgroup = grp, showlegend = FALSE)
    trace_i <- trace_i + 1L
  }

  for (k in seq_along(bin_centers)) {
    if (subj_h[k] <= 0) next
    x0 <- bin_centers[k] - BIN_W; x1 <- bin_centers[k] + BIN_W
    verts <- expand.grid(vx = c(x0, x1), vy = c(Y_FRONT - 0.06, Y_FRONT + 0.06), vz = c(0, subj_h[k]))
    fig <- plotly::add_trace(fig, type = "mesh3d", x = verts$vx, y = verts$vy, z = verts$vz,
      alphahull = 0, opacity = opacity, flatshading = TRUE, hoverinfo = "text",
      text = sprintf("theta en [%.2f, %.2f]<br>Sujetos = %d", bin_edges[k], bin_edges[k + 1], subj_counts[k]),
      name = "Sujetos", legendgroup = "sujetos", showlegend = !first_subj_shown)
    first_subj_shown <- TRUE
    bar_idx <- c(bar_idx, trace_i); bar_colors <- c(bar_colors, col_subj)
    trace_i <- trace_i + 1L
  }

  for (i in seq_len(nrow(curves))) {
    cv <- curves[i, ]
    xc <- bin_centers[cv$bin]
    x0 <- xc - BIN_W; x1 <- xc + BIN_W
    verts <- expand.grid(vx = c(x0, x1), vy = c(Y_FRONT - 0.06, Y_FRONT + 0.06), vz = c(cv$seg_bottom, cv$seg_top))
    nm <- if (isTRUE(cv$poly)) sprintf("%s - %s", cv$item, cv$threshold_label) else cv$item
    grp <- paste0("grp_", i)
    fig <- plotly::add_trace(fig, type = "mesh3d", x = verts$vx, y = verts$vy, z = verts$vz,
      alphahull = 0, opacity = opacity, flatshading = TRUE, hoverinfo = "text",
      text = sprintf("%s<br>theta_50 = %.2f", nm, cv$theta50),
      name = nm, legendgroup = grp, showlegend = FALSE)
    bar_idx <- c(bar_idx, trace_i); bar_colors <- c(bar_colors, cv$color)
    trace_i <- trace_i + 1L
  }

  fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
    x = c(-theta_max, theta_max), y = c(Y_FRONT, Y_FRONT), z = c(0, 0),
    line = list(color = "#0b0b0b", width = 2), opacity = 0.7,
    hoverinfo = "skip", showlegend = FALSE)
  trace_i <- trace_i + 1L

  all_idx <- as.list(0:(trace_i - 1L))
  opacity_steps <- c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
  opacity_active0 <- which.min(abs(opacity_steps - opacity)) - 1L

  txt <- paste(
    sprintf("Modelo: %s", paste(unique(itemtype), collapse = " / ")),
    sprintf("Metodo de puntuacion (theta): %s", theta_method),
    sprintf("n = %d sujetos  |  %d items  |  %d curvas (umbrales)", nrow(item_data), length(items), nrow(curves)),
    "Clic en la leyenda: mostrar/ocultar una curva. Doble clic: aislarla.",
    sep = "<br>"
  )

  fig <- plotly::layout(fig,
    title = list(text = "Mapa TRI: curvas caracteristicas + histograma doble de theta",
                 x = 1, xanchor = "right", y = 0.98, yanchor = "top"),
    margin = list(t = 120),
    legend = list(x = 1.02, y = 0.5),
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = "Nivel de rasgo compartido (theta)"), range = c(-theta_max, theta_max)),
      yaxis = list(title = list(text = "Item (profundidad)"), showticklabels = FALSE),
      zaxis = list(title = list(text = "P(respuesta) / histograma"), range = c(-SUBJ_MAX - 0.15, 1.05)),
      camera = list(eye = list(x = 1.7, y = -1.7, z = 0.9))
    ),
    annotations = list(list(
      text = txt, xref = "paper", yref = "paper", x = 0.01, y = 0.98, xanchor = "left", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 11, color = "#0b0b0b")
    )),
    sliders = list(list(
      active = opacity_active0,
      currentvalue = list(prefix = "Transparencia: "),
      x = 0, len = 0.32, xanchor = "left", y = -0.02, yanchor = "top",
      pad = list(t = 10),
      steps = lapply(opacity_steps, function(v) list(
        method = "restyle", label = sprintf("%d%%", round(v * 100)),
        args = list(list(opacity = v), all_idx)
      ))
    ))
  )

  fig <- htmlwidgets::onRender(fig, "
    function(el, x, data) {
      Plotly.restyle(el, { color: data.trace_colors }, data.trace_indices);
    }
  ", data = list(trace_colors = as.list(bar_colors), trace_indices = as.list(bar_idx)))

  attr(fig, "stats") <- list(
    fit = fit, coefficients = co,
    theta = data.frame(sujeto = seq_along(theta_hat), theta = theta_hat),
    curves = curves[, c("item", "threshold_label", "a", "b", "c", "theta50", "poly")],
    itemtype = itemtype
  )
  fig
}

#' @noRd
.irt_extract_curves <- function(items, is_poly, co, item_data) {
  rows <- list()
  for (nm in items) {
    a_i <- co[nm, "a"]
    if (isTRUE(is_poly[[nm]])) {
      bcols <- grep("^b[0-9]+$", colnames(co), value = TRUE)
      bs <- co[nm, bcols]
      bs <- bs[!is.na(bs)]
      cats <- sort(unique(item_data[[nm]]))
      for (k in seq_along(bs)) {
        rows[[length(rows) + 1L]] <- data.frame(
          item = nm, threshold_label = paste0(">=", cats[k + 1L]),
          a = a_i, b = as.numeric(bs[k]), c = 0, theta50 = as.numeric(bs[k]), poly = TRUE,
          stringsAsFactors = FALSE
        )
      }
    } else {
      b_i <- co[nm, "b"]; g_i <- co[nm, "g"]
      theta50 <- if (g_i > 0) b_i - (1 / a_i) * log(0.5 / (0.5 - g_i)) else b_i
      rows[[length(rows) + 1L]] <- data.frame(
        item = nm, threshold_label = NA_character_,
        a = a_i, b = b_i, c = g_i, theta50 = theta50, poly = FALSE,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
