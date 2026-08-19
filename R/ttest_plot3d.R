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
                          n_grid = 60, col = c("#2a78d6", "#eb6834"),
                          col_overlap = "#4a3aa7") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para ttest_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para ttest_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  stopifnot(conf.level > 0, conf.level < 1)

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
  means <- lapply(idx, function(i) c(mean(xv[i]), mean(zv[i])))
  covs <- lapply(idx, function(i) stats::cov(cbind(xv[i], zv[i])))

  t_x <- stats::t.test(xv[idx[[1]]], xv[idx[[2]]], var.equal = var.equal, conf.level = conf.level)
  t_z <- stats::t.test(zv[idx[[1]]], zv[idx[[2]]], var.equal = var.equal, conf.level = conf.level)
  hotelling <- .hotelling_t2(means[[1]], means[[2]], covs[[1]], covs[[2]], n[1], n[2])

  rx <- range(xv); rz <- range(zv)
  pad_x <- diff(rx) * 0.3; pad_z <- diff(rz) * 0.3
  xs <- seq(rx[1] - pad_x, rx[2] + pad_x, length.out = n_grid)
  zs <- seq(rz[1] - pad_z, rz[2] + pad_z, length.out = n_grid)
  D1 <- outer(xs, zs, function(a, b) .dmvnorm2(a, b, means[[1]], covs[[1]]))
  D2 <- outer(xs, zs, function(a, b) .dmvnorm2(a, b, means[[2]], covs[[2]]))
  ov <- pmin(D1, D2)
  cell <- (xs[2] - xs[1]) * (zs[2] - zs[1])
  ovl_coef <- sum(ov) * cell

  flat_scale <- function(col) list(list(0, col), list(1, col))

  stars <- function(p) if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.1) "." else ""
  info_txt <- paste0(
    sprintf("Solapamiento (OVL) ~ %.1f%%", ovl_coef * 100), "<br>",
    sprintf("p (eje %s) = %.4g %s", x, t_x$p.value, stars(t_x$p.value)), "<br>",
    sprintf("p (eje %s) = %.4g %s", z, t_z$p.value, stars(t_z$p.value)), "<br>",
    sprintf("Hotelling T2: p = %.4g %s", hotelling$p.value, stars(hotelling$p.value))
  )

  ov_min <- max(ov) * 0.02
  ov_masked <- ov
  ov_masked[ov_masked < ov_min] <- NA

  fig <- plotly::plot_ly()
  fig <- plotly::add_trace(fig, x = xs, y = zs, z = t(D1), type = "surface",
    colorscale = flat_scale(col[1]), showscale = FALSE, opacity = 0.55,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    name = groups[1])
  fig <- plotly::add_trace(fig, x = xs, y = zs, z = t(D2), type = "surface",
    colorscale = flat_scale(col[2]), showscale = FALSE, opacity = 0.55,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    name = groups[2])
  fig <- plotly::add_trace(fig, x = xs, y = zs, z = t(ov_masked), type = "surface",
    colorscale = flat_scale(col_overlap), showscale = FALSE, opacity = 0.95,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    name = "Solapamiento")

  fig <- plotly::layout(fig,
    title = list(text = paste0("Comparacion bivariada: ", groups[1], " vs ", groups[2])),
    scene = list(
      xaxis = list(title = list(text = x)),
      yaxis = list(title = list(text = z)),
      zaxis = list(title = list(text = "Densidad")),
      camera = list(eye = list(x = 1.5, y = -1.5, z = 0.9))
    ),
    annotations = list(list(
      text = info_txt, xref = "paper", yref = "paper",
      x = 0.99, y = 0.9, xanchor = "right", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 12, color = "#0b0b0b")
    ))
  )

  attr(fig, "stats") <- list(
    means = stats::setNames(means, groups), covariances = stats::setNames(covs, groups),
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
