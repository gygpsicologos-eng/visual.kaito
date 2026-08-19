#' Draw a chi-square association plot (standardized residuals per cell)
#'
#' Builds a contingency table from `row` and `col` -- each of which can be
#' either a single categorical column name or several column names combined
#' into one bigger grouping (e.g. `col = c("sexo", "compra")` produces a
#' "sexo x compra" nested column: Varon-Si, Varon-No, Mujer-Si, Mujer-No) --
#' and draws ONE 3D bar per cell whose height and color both encode the
#' cell's standardized (Pearson) residual: how many standard deviations the
#' observed count is above (red, excess) or below (blue, deficit) what
#' independence would predict. Two translucent reference planes at
#' +/-1.96 and +/-2.58 mark the conventional p < .05 / p < .01 thresholds,
#' so cells with a real deviation are immediately visible without reading
#' a separate table.
#'
#' Two extra diagnostics are computed automatically and attached to the
#' returned object (and summarized in the on-plot annotation):
#' \itemize{
#'   \item \strong{Low-frequency warning} (Cochran's rule): flags the table
#'     when any cell's expected count is below 1, or when more than 20% of
#'     cells have an expected count below 5 -- in either case the usual
#'     chi-square approximation is unreliable and Fisher's exact test or
#'     collapsing categories is recommended.
#'   \item \strong{Simpson's paradox warning}, only when `row` or `col` was
#'     given as more than one column (i.e. one side is "subdivided"): a
#'     \emph{descriptive} flag comparing the sign of each cell's residual
#'     in the collapsed (marginal) table against its sign within each
#'     level of the subdividing variable, plus a \emph{formal} test (Gail &
#'     Simon, 1985, qualitative interaction test) applied per cell of the
#'     collapsed table, treating each stratum's standardized residual as an
#'     approximately N(0,1) effect estimate and testing whether its sign is
#'     consistent across strata.
#' }
#'
#' @param data A data frame.
#' @param row,col Character vector(s) of column name(s) in `data`. Length 1
#'   for a simple two-way table; length > 1 to combine several categorical
#'   columns into one side of the table (e.g. `col = c("sexo", "compra")`).
#'   At most one of `row`/`col` may have length > 1 (needed for the
#'   Simpson's-paradox diagnostics; see Details).
#' @param opacity Numeric in (0, 1]. Initial bar opacity (also a live
#'   slider). Default 0.85.
#' @param sep Character used to join combined-column level labels.
#'   Default `" - "`.
#'
#' @return A `plotly` htmlwidget object. The full numeric results (the
#'   contingency table, the `chisq.test()` object, expected counts,
#'   standardized residuals, the low-frequency note, and -- when
#'   applicable -- the Simpson's-paradox diagnostics) are attached as the
#'   `"stats"` attribute.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   tipo = sample(c("A", "B", "C"), 300, replace = TRUE, prob = c(0.4, 0.35, 0.25)),
#'   sexo = sample(c("Varon", "Mujer"), 300, replace = TRUE),
#'   compra = sample(c("Si", "No"), 300, replace = TRUE)
#' )
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   chisq_plot3d(df, row = "tipo", col = c("sexo", "compra"))
#' }
#'
#' @export
chisq_plot3d <- function(data, row, col, opacity = 0.85, sep = " - ") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para chisq_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para chisq_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  stopifnot(opacity > 0, opacity <= 1)
  if (!all(row %in% names(data))) stop("'row' contiene columnas que no existen en 'data'.")
  if (!all(col %in% names(data))) stop("'col' contiene columnas que no existen en 'data'.")
  if (length(row) > 1 && length(col) > 1) {
    message("Tanto 'row' como 'col' tienen varias columnas: el diagnostico de la ",
            "paradoja de Simpson solo se calcula para la subdivision en 'col'.")
  }

  row_factor <- .combine_factor(data, row, sep = sep)
  col_factor <- .combine_factor(data, col, sep = sep)
  ok <- stats::complete.cases(data.frame(row_factor, col_factor))
  if (any(!ok)) {
    message("Se excluyeron ", sum(!ok), " fila(s) con datos faltantes en las columnas de agrupacion.")
    row_factor <- droplevels(row_factor[ok]); col_factor <- droplevels(col_factor[ok])
  }

  tab <- table(row = row_factor, col = col_factor)
  chi <- stats::chisq.test(tab, correct = FALSE)
  expected <- chi$expected
  stdres <- chi$stdres

  n_below1 <- sum(expected < 1)
  frac_below5 <- mean(expected < 5)
  cochran_note <- if (n_below1 > 0) {
    sprintf(
      "Aviso (regla de Cochran): %d casilla(s) con frecuencia esperada < 1. El chi-cuadrado no es fiable aqui; considera el test exacto de Fisher o agrupar categorias.",
      n_below1
    )
  } else if (frac_below5 > 0.2) {
    sprintf(
      "Aviso (regla de Cochran): %.0f%% de las casillas tienen frecuencia esperada < 5 (> 20%%). Considera el test exacto de Fisher o agrupar categorias.",
      100 * frac_below5
    )
  } else {
    NULL
  }

  simpson <- .simpson_diagnostics(data, row, col, row_factor, col_factor, ok)

  # --- build the plot -------------------------------------------------
  row_labels <- rownames(tab); col_labels <- colnames(tab)
  nr <- length(row_labels); nc <- length(col_labels)

  fig <- plotly::plot_ly(height = 850)
  hover <- matrix("", nr, nc)
  trace_colors <- character(0)
  trace_is_bar <- logical(0)
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      z <- stdres[i, j]
      hover[i, j] <- sprintf(
        "%s / %s<br>Observado = %d<br>Esperado = %.1f<br>Residuo estandarizado = %.2f",
        row_labels[i], col_labels[j], tab[i, j], expected[i, j], z
      )
      lo <- min(0, z); hi <- max(0, z)
      verts <- expand.grid(
        vx = c(j - 0.35, j + 0.35), vy = c(i - 0.35, i + 0.35), vz = c(lo, hi)
      )
      fig <- plotly::add_trace(fig, type = "mesh3d",
        x = verts$vx, y = verts$vy, z = verts$vz,
        alphahull = 0, opacity = opacity,
        flatshading = TRUE, hoverinfo = "text", text = hover[i, j],
        name = hover[i, j], showlegend = FALSE)
      trace_colors <- c(trace_colors, .diverging_color(z))
      trace_is_bar <- c(trace_is_bar, TRUE)
    }
  }

  plane <- function(thr, alpha) {
    xx <- c(0.5, nc + 0.5, nc + 0.5, 0.5); yy <- c(0.5, 0.5, nr + 0.5, nr + 0.5)
    for (sign in c(1, -1)) {
      fig <<- plotly::add_trace(fig, type = "mesh3d",
        x = xx, y = yy, z = rep(sign * thr, 4),
        i = c(0, 0), j = c(1, 2), k = c(2, 3),
        opacity = alpha, hoverinfo = "skip", showlegend = FALSE)
      trace_colors <<- c(trace_colors, "#7f7f7f")
      trace_is_bar <<- c(trace_is_bar, FALSE)
    }
  }
  plane(1.96, 0.18)
  plane(2.58, 0.12)

  txt <- paste(
    sprintf("Chi-cuadrado: X2(%d) = %.3f, p = %.4g", chi$parameter, chi$statistic, chi$p.value),
    sprintf("n = %d  |  filas: %s  |  columnas: %s", sum(tab), paste(row, collapse = "+"), paste(col, collapse = "+")),
    if (!is.null(cochran_note)) cochran_note else "Frecuencias esperadas: sin problemas (regla de Cochran).",
    if (!is.null(simpson) && simpson$applicable) .simpson_summary_text(simpson) else "Paradoja de Simpson: no aplica (ni 'row' ni 'col' tienen subdivision).",
    sep = "<br>"
  )

  fig <- plotly::layout(fig,
    title = list(text = "Chi-cuadrado: residuos estandarizados por casilla",
                 x = 1, xanchor = "right", y = 0.98, yanchor = "top"),
    margin = list(t = 120),
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = paste(col, collapse = " x ")),
                   tickvals = seq_len(nc), ticktext = col_labels),
      yaxis = list(title = list(text = paste(row, collapse = " x ")),
                   tickvals = seq_len(nr), ticktext = row_labels),
      zaxis = list(title = list(text = "Residuo estandarizado")),
      camera = list(eye = list(x = 1.6, y = -1.6, z = 0.9))
    ),
    annotations = list(list(
      text = txt, xref = "paper", yref = "paper", x = 0.01, y = 0.98, xanchor = "left", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 11, color = "#0b0b0b")
    )),
    sliders = list(list(
      active = which.min(abs(c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95) - opacity)) - 1L,
      currentvalue = list(prefix = "Transparencia: "),
      x = 0, len = 0.32, xanchor = "left", y = -0.02, yanchor = "top",
      pad = list(t = 10),
      steps = lapply(c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95), function(v) list(
        method = "restyle", label = sprintf("%d%%", round(v * 100)),
        args = list(list(opacity = v), as.list(0:(nr * nc - 1)))
      ))
    ))
  )

  # add_trace()'s 'color' argument only supports mapped/discrete palettes in
  # this plotly version, not a literal per-trace hex color for mesh3d; set
  # the diverging bar colors and the reference-plane gray directly via a
  # post-render restyle instead.
  fig <- htmlwidgets::onRender(fig, "
    function(el, x, data) {
      Plotly.restyle(el, { color: data.trace_colors });
    }
  ", data = list(trace_colors = as.list(trace_colors)))

  attr(fig, "stats") <- list(
    table = tab, chisq = chi, expected = expected, stdres = stdres,
    cochran_warning = cochran_note, simpson = simpson
  )
  fig
}

#' @noRd
.combine_factor <- function(data, vars, sep = " - ") {
  if (length(vars) == 1) return(factor(data[[vars]]))
  do.call(interaction, c(lapply(vars, function(v) factor(data[[v]])), list(sep = sep, drop = TRUE, lex.order = TRUE)))
}

#' Map a numeric value to a hex color on a diverging (blue-white-red) scale
#' @noRd
.diverging_color <- function(v, vlim = 3) {
  v <- pmin(pmax(v, -vlim), vlim)
  ramp <- grDevices::colorRamp(c("#2166ac", "#f7f7f7", "#b2182b"), space = "Lab")
  t <- (v + vlim) / (2 * vlim)
  rgb <- ramp(t)
  grDevices::rgb(rgb[, 1], rgb[, 2], rgb[, 3], maxColorValue = 255)
}

#' Gail & Simon (1985) qualitative interaction test.
#'
#' Given K approximately independent, approximately N(0,1) statistics
#' `z` (e.g. per-stratum standardized residuals for the same cell), tests
#' H0: the true effect has the same sign in every stratum, against the
#' alternative that it has different signs in different strata (a
#' "qualitative" interaction). Returns the test statistic Q = min(T+, T-)
#' and its reference chi-bar-square p-value.
#' @noRd
.gail_simon_test <- function(z) {
  z <- z[!is.na(z)]
  k <- length(z)
  if (k < 2) return(list(Q = NA_real_, K = k, p.value = NA_real_))
  t_pos <- sum(z[z > 0]^2)
  t_neg <- sum(z[z < 0]^2)
  Q <- min(t_pos, t_neg)
  j <- 0:k
  weights <- stats::dbinom(j, size = k, prob = 0.5)
  # P(Q >= q) = P(T+ >= q AND T- >= q); given |{i: z_i>0}| = j, T+ ~ chi^2_j and
  # T- ~ chi^2_{k-j} independently, so both tail probabilities multiply.
  p_value <- sum(weights * (1 - stats::pchisq(Q, df = j)) * (1 - stats::pchisq(Q, df = k - j)))
  list(Q = Q, K = k, p.value = p_value)
}

#' @noRd
.simpson_diagnostics <- function(data, row, col, row_factor, col_factor, ok) {
  if (length(col) > 1) {
    sub_side <- "col"; sub_var <- col[length(col)]; base_vars <- col[-length(col)]
    other_factor <- row_factor
  } else if (length(row) > 1) {
    sub_side <- "row"; sub_var <- row[length(row)]; base_vars <- row[-length(row)]
    other_factor <- col_factor
  } else {
    return(list(applicable = FALSE))
  }

  base_factor <- .combine_factor(data, base_vars)
  base_factor <- base_factor[ok]
  sub_full <- factor(data[[sub_var]])[ok]

  collapsed_tab <- table(other = other_factor, base = base_factor)
  collapsed_chi <- stats::chisq.test(collapsed_tab, correct = FALSE)
  collapsed_stdres <- collapsed_chi$stdres

  sub_levels <- levels(droplevels(sub_full))
  stratum_stdres <- stats::setNames(vector("list", length(sub_levels)), sub_levels)
  for (lvl in sub_levels) {
    idx <- sub_full == lvl
    tab_s <- table(other = other_factor[idx], base = base_factor[idx])
    chi_s <- tryCatch(stats::chisq.test(tab_s, correct = FALSE), error = function(e) NULL)
    m <- matrix(NA_real_, nrow(collapsed_tab), ncol(collapsed_tab), dimnames = dimnames(collapsed_tab))
    if (!is.null(chi_s)) {
      m[rownames(tab_s), colnames(tab_s)] <- chi_s$stdres
    }
    stratum_stdres[[lvl]] <- m
  }

  nr <- nrow(collapsed_tab); nc <- ncol(collapsed_tab)
  flagged <- data.frame()
  gs <- data.frame()
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      z_collapsed <- collapsed_stdres[i, j]
      z_strata <- vapply(stratum_stdres, function(m) m[i, j], numeric(1))
      reversed <- any(sign(z_collapsed) != sign(z_strata) & z_collapsed != 0 & z_strata != 0, na.rm = TRUE)
      if (isTRUE(reversed)) {
        flagged <- rbind(flagged, data.frame(
          fila = rownames(collapsed_tab)[i], base = colnames(collapsed_tab)[j],
          residuo_colapsado = z_collapsed,
          t(z_strata)
        ))
      }
      test <- .gail_simon_test(z_strata)
      gs <- rbind(gs, data.frame(
        fila = rownames(collapsed_tab)[i], base = colnames(collapsed_tab)[j],
        Q = test$Q, K = test$K, p.value = test$p.value
      ))
    }
  }

  list(
    applicable = TRUE, sub_var = sub_var, base_vars = base_vars, sub_side = sub_side,
    collapsed_table = collapsed_tab, collapsed_stdres = collapsed_stdres,
    stratum_stdres = stratum_stdres, descriptive_flags = flagged, gail_simon = gs
  )
}

#' @noRd
.simpson_summary_text <- function(simpson) {
  n_flag <- nrow(simpson$descriptive_flags)
  n_sig <- sum(simpson$gail_simon$p.value < 0.05, na.rm = TRUE)
  desc <- if (n_flag > 0) {
    sprintf("Paradoja de Simpson (descriptivo): %d casilla(s) cambian de signo al comparar la tabla colapsada con al menos un nivel de '%s'.", n_flag, simpson$sub_var)
  } else {
    sprintf("Paradoja de Simpson (descriptivo): sin cambios de signo al subdividir por '%s'.", simpson$sub_var)
  }
  formal <- if (n_sig > 0) {
    sprintf("Test formal (Gail-Simon): %d casilla(s) con interaccion cualitativa significativa (p < .05).", n_sig)
  } else {
    "Test formal (Gail-Simon): sin interaccion cualitativa significativa (p < .05) en ninguna casilla."
  }
  paste(desc, formal, sep = "<br>")
}
