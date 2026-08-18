#' Draw a 3-axis (triaxial) box plot
#'
#' Computes box-and-whisker statistics on three continuous axes at once,
#' for each group in `group`, and draws them as three linked pairwise
#' panels (axis 1 vs 2, 2 vs 3, 1 vs 3) using base graphics.
#'
#' Exactly one of `tukey`, `percentile`, `sd`, `letter_value` must be 1
#' (the rest 0): this selects how the box and whiskers are defined.
#' \itemize{
#'   \item \strong{tukey} (default): box = P25-P75; whiskers reach the most
#'     extreme data point within \code{k} * IQR of the box; points beyond
#'     that fence are drawn individually as outliers.
#'   \item \strong{percentile}: box = P25-P75; whiskers reach fixed
#'     percentiles (\code{probs}), regardless of outliers.
#'   \item \strong{sd}: centered on the mean; box = mean +/- \code{sd_mult[1]}
#'     SD; whiskers = mean +/- \code{sd_mult[2]} SD.
#'   \item \strong{letter_value}: nested boxes at successively finer
#'     percentile depths (Tukey's letter values), for large samples.
#' }
#'
#' This base-graphics version draws three 2D projections rather than a
#' single rotatable 3D scene; an interactive rotatable version (with the
#' same four conventions, a notch option, a same-scale/z-score option,
#' and axis-pair view switching) exists as a prototype and is planned for
#' a future release of this package.
#'
#' @param data A data frame with the three axes and the grouping variable.
#' @param x,y,z Character. Column names of the three continuous axes.
#' @param group Character. Column name of the grouping variable.
#' @param tukey,percentile,sd,letter_value 0/1 flags selecting the
#'   box/whisker convention. Exactly one must be 1.
#' @param k Numeric. Tukey fence multiplier (default 1.5).
#' @param probs Length-2 numeric. Lower/upper whisker percentiles for the
#'   `percentile` convention (default `c(0.05, 0.95)`).
#' @param sd_mult Length-2 numeric. Box and whisker multipliers (in SDs)
#'   for the `sd` convention (default `c(1, 2)`).
#' @param col Optional character vector of colors, one per group.
#'
#' @return Invisibly, a list with one element per group, each a list of
#'   per-axis statistics (`q1`, `q3`, `center`, `whisker_lo`, `whisker_hi`,
#'   plus `outliers`, a logical vector flagging points outside the
#'   whiskers on that axis).
#'
#' @examples
#' set.seed(1)
#' df <- rbind(
#'   data.frame(x = rnorm(30, 0), y = rnorm(30, 0), z = rnorm(30, 0), group = "A"),
#'   data.frame(x = rnorm(30, 1), y = rnorm(30, 1), z = rnorm(30, 1), group = "B")
#' )
#' boxplot3d(df)
#'
#' @export
boxplot3d <- function(data, x = "x", y = "y", z = "z", group = "group",
                       tukey = 1, percentile = 0, sd = 0, letter_value = 0,
                       k = 1.5, probs = c(0.05, 0.95), sd_mult = c(1, 2),
                       col = NULL) {
  flags <- c(tukey = tukey, percentile = percentile, sd = sd, letter_value = letter_value)
  if (sum(flags == 1) != 1) {
    stop("Activa exactamente uno de tukey / percentile / sd / letter_value (=1), el resto a 0.")
  }
  method <- names(flags)[flags == 1]

  g <- factor(data[[group]])
  groups <- levels(g)
  if (is.null(col)) col <- grDevices::palette.colors(max(3, length(groups)), palette = "Okabe-Ito")

  axes <- list(x = data[[x]], y = data[[y]], z = data[[z]])
  stats_by_group <- stats::setNames(lapply(groups, function(gr) {
    idx <- g == gr
    lapply(axes, function(v) .box_axis_stats(v[idx], method, k, probs, sd_mult))
  }), groups)

  op <- graphics::par(mfrow = c(1, 3), mar = c(4, 4, 2, 1))
  on.exit(graphics::par(op))
  panels <- list(c("x", "y"), c("y", "z"), c("x", "z"))
  for (p in panels) {
    .draw_panel(axes[[p[1]]], axes[[p[2]]], g, groups, stats_by_group, p[1], p[2], col)
  }

  invisible(stats_by_group)
}

#' @noRd
.box_axis_stats <- function(v, method, k, probs, sd_mult) {
  s <- sort(v); n <- length(s)
  q1 <- stats::quantile(s, 0.25, names = FALSE); q3 <- stats::quantile(s, 0.75, names = FALSE)
  med <- stats::median(s); mu <- mean(s); iqr <- q3 - q1

  if (method == "tukey") {
    fence <- c(q1 - k * iqr, q3 + k * iqr)
    within <- s[s >= fence[1] & s <= fence[2]]
    lo <- if (length(within)) min(within) else q1; hi <- if (length(within)) max(within) else q3
    list(q1 = q1, q3 = q3, center = med, whisker_lo = lo, whisker_hi = hi,
         outliers = v < lo | v > hi)
  } else if (method == "percentile") {
    lo <- stats::quantile(s, probs[1], names = FALSE); hi <- stats::quantile(s, probs[2], names = FALSE)
    list(q1 = q1, q3 = q3, center = med, whisker_lo = lo, whisker_hi = hi,
         outliers = rep(FALSE, length(v)))
  } else if (method == "sd") {
    sdv <- stats::sd(s)
    list(q1 = mu - sd_mult[1] * sdv, q3 = mu + sd_mult[1] * sdv, center = mu,
         whisker_lo = mu - sd_mult[2] * sdv, whisker_hi = mu + sd_mult[2] * sdv,
         outliers = v < (mu - sd_mult[2] * sdv) | v > (mu + sd_mult[2] * sdv))
  } else {
    depth <- (n + 1) / 2; depths <- numeric(0)
    while (depth > 1 && length(depths) < 4) { depth <- (floor(depth) + 1) / 2; depths <- c(depths, depth) }
    ostat <- function(d) mean(s[c(max(1, floor(d)), min(n, ceiling(d)))])
    lo <- ostat(depths[length(depths)]); hi <- ostat(n + 1 - depths[length(depths)])
    list(q1 = ostat(depths[1]), q3 = ostat(n + 1 - depths[1]), center = med,
         whisker_lo = lo, whisker_hi = hi, outliers = v < lo | v > hi)
  }
}

#' @noRd
.draw_panel <- function(u, v, g, groups, stats_by_group, xn, yn, col) {
  graphics::plot(u, v, type = "n", xlab = xn, ylab = yn,
                 main = paste(xn, "vs", yn))
  for (i in seq_along(groups)) {
    gr <- groups[i]; idx <- g == gr
    graphics::points(u[idx], v[idx], col = grDevices::adjustcolor(col[i], alpha.f = 0.25), pch = 16, cex = 0.6)
    sx <- stats_by_group[[gr]][[xn]]; sy <- stats_by_group[[gr]][[yn]]
    graphics::rect(sx$q1, sy$q1, sx$q3, sy$q3, border = col[i], col = grDevices::adjustcolor(col[i], alpha.f = 0.25), lwd = 2)
    graphics::segments(sx$whisker_lo, sy$center, sx$whisker_hi, sy$center, col = col[i])
    graphics::segments(sx$center, sy$whisker_lo, sx$center, sy$whisker_hi, col = col[i])
    graphics::points(sx$center, sy$center, pch = 3, lwd = 3, cex = 1.4, col = "black")
  }
  graphics::legend("topleft", legend = groups, col = col[seq_along(groups)], pch = 16, bty = "n", cex = 0.8)
}

