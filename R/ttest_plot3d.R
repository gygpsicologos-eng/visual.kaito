#' Draw two groups as bivariate 3D "mountains" with their overlap and joint test
#'
#' A 3D counterpart to [ttest_plot()] for comparing 2 groups on 2 continuous
#' variables at once. Each group is drawn as a single bivariate density
#' surface ("mountain") over the (`x`, `z`) plane -- fitted from that group's
#' own sample mean and covariance, `y` is density -- rather than as two
#' separate 1D curves. The region where the two surfaces overlap is shaded,
#' and both the per-axis Student's t-tests (one for `x`, one for `z`) and the
#' joint two-sample Hotelling's T-squared test are reported, so you can read
#' the result either axis-by-axis or as a single combined test.
#'
#' Hotelling's T-squared is the multivariate generalization of Student's t:
#' it tests whether the two groups' mean vectors (on `x` and `z` together)
#' differ, accounting for the covariance between the two variables, and
#' collapses to a single F-statistic and p-value. The per-axis t-tests can
#' disagree with each other (significant on one axis, not the other) and can
#' also disagree with the joint Hotelling result; that disagreement is
#' informative, not a bug, so all three are reported rather than picking one.
#'
#' The plot has 4 live controls, all pre-computed so they respond instantly
#' without needing R again:
#' \itemize{
#'   \item A "Vista" menu switches between the full 3D scene and 3 flat 2D
#'     views (same convention as [boxplot3d_interactive()]'s axis-pair
#'     panels, built as a true flat second cartesian layer, not a camera
#'     trick): \strong{Eje x} and \strong{Eje z} each show that one
#'     variable's two group curves (density vs. value) with the shared
#'     region shaded, and \strong{Ambos ejes} shows a top-down contour view
#'     of both mountains at once, with the same shaded overlap footprint.
#'   \item A transparency slider adjusts the opacity of the two group
#'     surfaces in the 3D view (the overlap surface always stays solid, so
#'     the shared region remains readable at any transparency level).
#'   \item A "Reportar" dropdown chooses which of the reported statistics
#'     show in the on-plot text box (all of them, only the per-axis
#'     t-tests, only Hotelling, or only the overlap percentage); each
#'     group's mean and SD on both variables are always shown underneath.
#'   \item A button pair switches the 3D surfaces between raw units and
#'     standardized (z-score) units. Standardizing only changes what the
#'     surfaces look like -- the t-test, Hotelling and overlap numbers are
#'     mathematically identical either way, since all three are invariant
#'     to a shared linear rescaling of the axes -- so the text box does not
#'     change when you switch scales. This toggle only affects the 3D view.
#' }
#'
#' @param data A data frame with the two continuous axes and the grouping
#'   variable.
#' @param x,z Character. Column names of the two continuous axes.
#' @param group Character. Column name of the grouping variable. Must have
#'   exactly 2 levels.
#' @param var.equal Logical, passed to the per-axis [stats::t.test()] calls
#'   (default `FALSE`, i.e. Welch).
#' @param conf.level Confidence level for the per-axis t-tests (default 0.95).
#' @param n_grid Integer. Grid resolution per axis for the density surfaces
#'   and the numerical overlap estimate (default 60).
#' @param opacity Initial opacity (0-1) of the two group surfaces (default
#'   0.55); adjustable afterwards with the transparency slider.
#' @param col Length-2 character vector of colors, one per group.
#' @param col_overlap Color used to mark the region where the two surfaces
#'   overlap.
#'
#' @return A `plotly` htmlwidget object (auto-displayed if the result isn't
#'   assigned). The full statistics -- `means` and `covariances` per group,
#'   `overlap` (the estimated overlap coefficient, 0-1), `t_x` and `t_z`
#'   (the [stats::t.test()] results for each axis), and `hotelling` (a list
#'   with `T2`, `F`, `df1`, `df2`, `p.value`) -- are attached as the `"stats"`
#'   attribute, e.g. `attr(ttest_plot3d(df), "stats")$hotelling`.
#'
#' @examples
#' set.seed(1)
#' df <- rbind(
#'   data.frame(x = rnorm(30, 0, 1.3), z = rnorm(30, 0, 1.1), group = "A"),
#'   data.frame(x = rnorm(30, 3, 1.4), z = rnorm(30, 2.6, 1.2), group = "B")
#' )
#' ttest_plot3d(df)
#'
#' @export
ttest_plot3d <- function(data, x = "x", z = "z", group = "group",
                          var.equal = FALSE, conf.level = 0.95,
                          n_grid = 60, opacity = 0.55,
                          col = c("#2a78d6", "#eb6834"),
                          col_overlap = "#4a3aa7") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para ttest_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para ttest_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  stopifnot(conf.level > 0, conf.level < 1, opacity > 0, opacity <= 1)

  xv <- .as_plain_numeric(data[[x]])
  zv <- .as_plain_numeric(data[[z]])
  g <- factor(data[[group]])
  groups <- levels(g)
  if (length(groups) != 2) {
    stop("ttest_plot3d() compara exactamente 2 grupos; '", group, "' tiene ",
         length(groups), " (", paste(groups, collapse = ", "), ").")
  }
  if (length(col) < 2) stop("'col' debe tener 2 colores, uno por grupo.")

  idx <- list(g == groups[1], g == groups[2])
  n <- vapply(idx, sum, integer(1))
  mean_x <- vapply(idx, function(i) mean(xv[i]), numeric(1))
  sd_x <- vapply(idx, function(i) stats::sd(xv[i]), numeric(1))
  mean_z <- vapply(idx, function(i) mean(zv[i]), numeric(1))
  sd_z <- vapply(idx, function(i) stats::sd(zv[i]), numeric(1))
  means <- lapply(idx, function(i) c(mean(xv[i]), mean(zv[i])))
  covs <- lapply(idx, function(i) stats::cov(cbind(xv[i], zv[i])))

  t_x <- stats::t.test(xv[idx[[1]]], xv[idx[[2]]], var.equal = var.equal, conf.level = conf.level)
  t_z <- stats::t.test(zv[idx[[1]]], zv[idx[[2]]], var.equal = var.equal, conf.level = conf.level)
  hotelling <- .hotelling_t2(means[[1]], means[[2]], covs[[1]], covs[[2]], n[1], n[2])

  # Standardized (z-score) copy of the two axes, for the "raw vs standardized" toggle.
  # t-tests, Hotelling and overlap are invariant to this shared linear rescaling, so
  # only the surfaces (and axis titles) differ; the reported numbers do not.
  xv_z <- as.numeric(scale(xv)); zv_z <- as.numeric(scale(zv))
  means_z <- lapply(idx, function(i) c(mean(xv_z[i]), mean(zv_z[i])))
  covs_z <- lapply(idx, function(i) stats::cov(cbind(xv_z[i], zv_z[i])))

  surf_grid <- function(av, bv, m, cv) {
    ra <- range(av); rb <- range(bv)
    pad_a <- diff(ra) * 0.3; pad_b <- diff(rb) * 0.3
    as_ <- seq(ra[1] - pad_a, ra[2] + pad_a, length.out = n_grid)
    bs_ <- seq(rb[1] - pad_b, rb[2] + pad_b, length.out = n_grid)
    D1 <- outer(as_, bs_, function(p, q) .dmvnorm2(p, q, m[[1]], cv[[1]]))
    D2 <- outer(as_, bs_, function(p, q) .dmvnorm2(p, q, m[[2]], cv[[2]]))
    ov <- pmin(D1, D2)
    cell <- (as_[2] - as_[1]) * (bs_[2] - bs_[1])
    ov_masked <- ov
    ov_masked[ov_masked < max(ov) * 0.02] <- NA
    list(as_ = as_, bs_ = bs_, D1 = D1, D2 = D2, ov_masked = ov_masked, ovl_coef = sum(ov) * cell)
  }

  raw <- surf_grid(xv, zv, means, covs)
  zsc <- surf_grid(xv_z, zv_z, means_z, covs_z)
  ovl_coef <- raw$ovl_coef  # identical (up to grid resolution) to zsc$ovl_coef; report the raw-grid estimate

  flat_scale <- function(colr) list(list(0, colr), list(1, colr))
  hex_rgba <- function(colr, alpha) {
    rgb <- grDevices::col2rgb(colr)
    sprintf("rgba(%d,%d,%d,%.2f)", rgb[1], rgb[2], rgb[3], alpha)
  }
  stars <- function(p) if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.1) "." else ""

  ovl_line <- sprintf("Solapamiento (OVL) ~ %.1f%%", ovl_coef * 100)
  px_line <- sprintf("p (eje %s) = %.4g %s", x, t_x$p.value, stars(t_x$p.value))
  pz_line <- sprintf("p (eje %s) = %.4g %s", z, t_z$p.value, stars(t_z$p.value))
  hot_line <- sprintf("Hotelling T2: p = %.4g %s", hotelling$p.value, stars(hotelling$p.value))
  desc_line <- function(i) sprintf(
    "Grupo %s: %s = %.3g +/- %.3g; %s = %.3g +/- %.3g",
    groups[i], x, mean_x[i], sd_x[i], z, mean_z[i], sd_z[i]
  )
  desc_block <- paste(desc_line(1), desc_line(2), sep = "<br>")
  txt_all <- paste(ovl_line, px_line, pz_line, hot_line, desc_block, sep = "<br>")
  txt_axes <- paste(ovl_line, px_line, pz_line, desc_block, sep = "<br>")
  txt_hotelling <- paste(ovl_line, hot_line, desc_block, sep = "<br>")
  txt_overlap <- paste(ovl_line, desc_block, sep = "<br>")

  # --- 3D traces (indices 0,1,2): the two group mountains + their overlap ---
  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$D1), type = "surface",
    colorscale = flat_scale(col[1]), showscale = FALSE, opacity = opacity,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    scene = "scene", name = groups[1], legendgroup = "g1", showlegend = TRUE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$D2), type = "surface",
    colorscale = flat_scale(col[2]), showscale = FALSE, opacity = opacity,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    scene = "scene", name = groups[2], legendgroup = "g2", showlegend = TRUE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$ov_masked), type = "surface",
    colorscale = flat_scale(col_overlap), showscale = FALSE, opacity = 0.95,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    scene = "scene", name = "Solapamiento", legendgroup = "ov", showlegend = TRUE)

  # --- flat 2D curves for "eje x" alone (indices 3,4,5), drawn on xaxis2/yaxis2 ---
  dcurve <- function(v, m, s) stats::dnorm(v, m, s)
  yx1 <- dcurve(raw$as_, mean_x[1], sd_x[1]); yx2 <- dcurve(raw$as_, mean_x[2], sd_x[2])
  ovx <- pmin(yx1, yx2)
  fig <- plotly::add_trace(fig, x = raw$as_, y = yx1, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col[1], width = 2),
    fill = "tozeroy", fillcolor = hex_rgba(col[1], 0.25), visible = FALSE,
    name = groups[1], legendgroup = "g1", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = yx2, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col[2], width = 2),
    fill = "tozeroy", fillcolor = hex_rgba(col[2], 0.25), visible = FALSE,
    name = groups[2], legendgroup = "g2", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = ovx, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col_overlap, width = 1),
    fill = "tozeroy", fillcolor = hex_rgba(col_overlap, 0.75), visible = FALSE,
    name = "Solapamiento", legendgroup = "ov", showlegend = FALSE)

  # --- flat 2D curves for "eje z" alone (indices 6,7,8) ---
  yz1 <- dcurve(raw$bs_, mean_z[1], sd_z[1]); yz2 <- dcurve(raw$bs_, mean_z[2], sd_z[2])
  ovz <- pmin(yz1, yz2)
  fig <- plotly::add_trace(fig, x = raw$bs_, y = yz1, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col[1], width = 2),
    fill = "tozeroy", fillcolor = hex_rgba(col[1], 0.25), visible = FALSE,
    name = groups[1], legendgroup = "g1", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$bs_, y = yz2, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col[2], width = 2),
    fill = "tozeroy", fillcolor = hex_rgba(col[2], 0.25), visible = FALSE,
    name = groups[2], legendgroup = "g2", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$bs_, y = ovz, type = "scatter", mode = "lines",
    xaxis = "x2", yaxis = "y2", line = list(color = col_overlap, width = 1),
    fill = "tozeroy", fillcolor = hex_rgba(col_overlap, 0.75), visible = FALSE,
    name = "Solapamiento", legendgroup = "ov", showlegend = FALSE)

  # --- flat 2D top-down contour view for "ambos ejes" (indices 9,10,11) ---
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$D1), type = "contour",
    xaxis = "x2", yaxis = "y2", colorscale = flat_scale(col[1]), showscale = FALSE,
    contours = list(coloring = "lines", showlabels = FALSE), line = list(width = 2),
    visible = FALSE, name = groups[1], legendgroup = "g1", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$D2), type = "contour",
    xaxis = "x2", yaxis = "y2", colorscale = flat_scale(col[2]), showscale = FALSE,
    contours = list(coloring = "lines", showlabels = FALSE), line = list(width = 2),
    visible = FALSE, name = groups[2], legendgroup = "g2", showlegend = FALSE)
  fig <- plotly::add_trace(fig, x = raw$as_, y = raw$bs_, z = t(raw$ov_masked), type = "contour",
    xaxis = "x2", yaxis = "y2", colorscale = flat_scale(col_overlap), showscale = FALSE,
    contours = list(coloring = "fill", showlines = FALSE), opacity = 0.85,
    visible = FALSE, name = "Solapamiento", legendgroup = "ov", showlegend = FALSE)

  op_vals <- c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
  op_active <- which.min(abs(op_vals - opacity)) - 1L

  all_idx <- as.list(0:11)
  vis_3d <- c(TRUE, TRUE, TRUE, rep(FALSE, 9))
  vis_ejex <- c(rep(FALSE, 3), TRUE, TRUE, TRUE, rep(FALSE, 6))
  vis_ejez <- c(rep(FALSE, 6), TRUE, TRUE, TRUE, rep(FALSE, 3))
  vis_ambos <- c(rep(FALSE, 9), TRUE, TRUE, TRUE)

  scene_full <- list(`scene.domain.x` = list(0, 1), `scene.domain.y` = list(0, 1),
                      `xaxis2.domain` = list(0, 0.001), `yaxis2.domain` = list(0, 0.001),
                      `xaxis2.visible` = FALSE, `yaxis2.visible` = FALSE)
  scene_flat <- function(xt, yt) list(
    `scene.domain.x` = list(0, 0.001), `scene.domain.y` = list(0, 0.001),
    `xaxis2.domain` = list(0, 1), `yaxis2.domain` = list(0, 1),
    `xaxis2.visible` = TRUE, `yaxis2.visible` = TRUE,
    `xaxis2.title.text` = xt, `yaxis2.title.text` = yt
  )

  fig <- plotly::layout(fig,
    title = list(text = paste0("Comparacion bivariada: ", groups[1], " vs ", groups[2])),
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = x)),
      yaxis = list(title = list(text = z)),
      zaxis = list(title = list(text = "Densidad")),
      camera = list(eye = list(x = 1.5, y = -1.5, z = 0.9))
    ),
    xaxis2 = list(domain = c(0, 0.001), visible = FALSE, anchor = "y2"),
    yaxis2 = list(domain = c(0, 0.001), visible = FALSE, anchor = "x2"),
    annotations = list(list(
      text = txt_all, xref = "paper", yref = "paper",
      x = 0.99, y = 0.9, xanchor = "right", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 12, color = "#0b0b0b")
    )),
    updatemenus = list(
      list(
        type = "dropdown", direction = "down", showactive = TRUE,
        x = 0, y = 1.15, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "update", label = "Vista: 3D",
               args = list(list(visible = vis_3d), scene_full, all_idx)),
          list(method = "update", label = paste0("Vista: eje ", x),
               args = list(list(visible = vis_ejex), scene_flat(x, "Densidad"), all_idx)),
          list(method = "update", label = paste0("Vista: eje ", z),
               args = list(list(visible = vis_ejez), scene_flat(z, "Densidad"), all_idx)),
          list(method = "update", label = "Vista: ambos ejes",
               args = list(list(visible = vis_ambos), scene_flat(x, z), all_idx))
        )
      ),
      list(
        type = "dropdown", direction = "down", showactive = TRUE,
        x = 0, y = 1.115, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "relayout", label = "Reportar: todas",
               args = list(list(`annotations[0].text` = txt_all))),
          list(method = "relayout", label = "Reportar: solo ejes",
               args = list(list(`annotations[0].text` = txt_axes))),
          list(method = "relayout", label = "Reportar: solo Hotelling",
               args = list(list(`annotations[0].text` = txt_hotelling))),
          list(method = "relayout", label = "Reportar: solo solapamiento",
               args = list(list(`annotations[0].text` = txt_overlap)))
        )
      ),
      list(
        type = "buttons", direction = "right", showactive = TRUE,
        x = 0, y = 1.08, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "update", label = "Datos brutos", args = list(
                 list(x = list(raw$as_, raw$as_, raw$as_), y = list(raw$bs_, raw$bs_, raw$bs_),
                      z = list(t(raw$D1), t(raw$D2), t(raw$ov_masked))),
                 list(`scene.xaxis.title.text` = x, `scene.yaxis.title.text` = z),
                 list(0, 1, 2))),
          list(method = "update", label = "Datos estandarizados", args = list(
                 list(x = list(zsc$as_, zsc$as_, zsc$as_), y = list(zsc$bs_, zsc$bs_, zsc$bs_),
                      z = list(t(zsc$D1), t(zsc$D2), t(zsc$ov_masked))),
                 list(`scene.xaxis.title.text` = paste0(x, " (z-score)"),
                      `scene.yaxis.title.text` = paste0(z, " (z-score)")),
                 list(0, 1, 2)))
        )
      )
    ),
    sliders = list(list(
      active = op_active,
      currentvalue = list(prefix = "Transparencia: "),
      x = 0, len = 0.32, xanchor = "left", y = -0.02, yanchor = "top",
      pad = list(t = 10),
      steps = lapply(op_vals, function(v) list(
        method = "restyle", label = sprintf("%d%%", round(v * 100)),
        args = list(list(opacity = v), list(0, 1))
      ))
    ))
  )

  attr(fig, "stats") <- list(
    means = stats::setNames(means, groups), covariances = stats::setNames(covs, groups),
    mean_sd = list(x = stats::setNames(mean_x, groups), sd_x = stats::setNames(sd_x, groups),
                   z = stats::setNames(mean_z, groups), sd_z = stats::setNames(sd_z, groups)),
    overlap = ovl_coef, t_x = t_x, t_z = t_z, hotelling = hotelling
  )
  fig
}

#' @noRd
.hotelling_t2 <- function(m1, m2, S1, S2, n1, n2) {
  p <- length(m1)
  Sp <- ((n1 - 1) * S1 + (n2 - 1) * S2) / (n1 + n2 - 2)
  d <- m1 - m2
  T2 <- (n1 * n2 / (n1 + n2)) * as.numeric(t(d) %*% solve(Sp) %*% d)
  df1 <- p
  df2 <- n1 + n2 - p - 1
  Fstat <- (df2 / (df1 * (n1 + n2 - 2))) * T2
  p_value <- stats::pf(Fstat, df1, df2, lower.tail = FALSE)
  list(T2 = T2, F = Fstat, df1 = df1, df2 = df2, p.value = p_value)
}

#' @noRd
.dmvnorm2 <- function(xg, zg, mu, S) {
  Sinv <- solve(S)
  detS <- det(S)
  dx <- xg - mu[1]; dz <- zg - mu[2]
  q <- Sinv[1, 1] * dx^2 + 2 * Sinv[1, 2] * dx * dz + Sinv[2, 2] * dz^2
  (1 / (2 * pi * sqrt(detS))) * exp(-0.5 * q)
}
