#' Draw a t-distribution curve with the observed statistic and critical region
#'
#' A didactic plot: the t-distribution density curve for the relevant
#' degrees of freedom, the critical (rejection) region shaded according to
#' `alternative` and `conf.level`, and the observed t-statistic marked with
#' a vertical line -- so it's visually clear whether the observed value
#' falls inside the rejection region.
#'
#' Two ways to use it:
#' \itemize{
#'   \item \strong{From data}: pass `x` (and optionally `y`) as you would to
#'     [stats::t.test()] -- one-sample, two-sample, or paired -- and the
#'     test is run internally.
#'   \item \strong{Manual}: pass `t` and `df` directly (skipping `x`/`y`
#'     entirely), useful for teaching a specific scenario without needing
#'     raw data.
#' }
#'
#' @param x Numeric vector (first sample), or `NULL` if using the manual
#'   `t`/`df` mode.
#' @param y Optional numeric vector (second sample), for a two-sample or
#'   paired test.
#' @param mu Number. Null-hypothesis mean for a one-sample test (default 0).
#' @param paired Logical. Paired t-test (default `FALSE`).
#' @param alternative One of `"two.sided"` (default), `"less"`, `"greater"`.
#' @param var.equal Logical. Assume equal variances in a two-sample test
#'   (default `FALSE`, i.e. Welch).
#' @param conf.level Confidence level defining the critical region
#'   (default 0.95, i.e. alpha = 0.05).
#' @param t,df Numeric. Manual mode: the observed t-statistic and degrees
#'   of freedom, used instead of computing a test from `x`/`y`. Both must
#'   be supplied together.
#' @param col_curve,col_critical,col_observed Colors for the density curve,
#'   the shaded critical region, and the observed-statistic marker.
#' @param main Optional plot title.
#'
#' @return Invisibly, a list with `t`, `df`, `p.value`, `alternative`,
#'   `alpha`, `critical` (the critical value(s)), and `stars`.
#'
#' @examples
#' set.seed(1)
#' a <- rnorm(20, 5, 1)
#' b <- rnorm(20, 6, 1)
#' ttest_plot(a, b)
#'
#' # manual mode: no data needed
#' ttest_plot(t = 2.4, df = 28, alternative = "greater")
#'
#' @export
ttest_plot <- function(x = NULL, y = NULL, mu = 0, paired = FALSE,
                        alternative = c("two.sided", "less", "greater"),
                        var.equal = FALSE, conf.level = 0.95,
                        t = NULL, df = NULL,
                        col_curve = "#2a78d6", col_critical = "#d03b3b",
                        col_observed = "#0b0b0b", main = NULL) {
  alternative <- match.arg(alternative)
  stopifnot(conf.level > 0, conf.level < 1)
  alpha <- 1 - conf.level

  if (is.null(t) || is.null(df)) {
    if (is.null(x)) {
      stop("Proporciona 'x' (variable numerica; opcionalmente 'y' para 2 muestras), ",
           "o bien 't' y 'df' directamente para un escenario manual.")
    }
    tt <- if (is.null(y)) {
      stats::t.test(x, mu = mu, alternative = alternative, conf.level = conf.level)
    } else if (paired) {
      stats::t.test(x, y, paired = TRUE, alternative = alternative, conf.level = conf.level)
    } else {
      stats::t.test(x, y, var.equal = var.equal, alternative = alternative, conf.level = conf.level)
    }
    t <- unname(tt$statistic)
    df <- unname(tt$parameter)
    p_value <- tt$p.value
  } else {
    stopifnot(is.numeric(t), length(t) == 1, is.numeric(df), length(df) == 1, df > 0)
    p_value <- switch(alternative,
      two.sided = 2 * stats::pt(-abs(t), df),
      less = stats::pt(t, df),
      greater = stats::pt(t, df, lower.tail = FALSE)
    )
  }

  xr <- max(4.5, abs(t) + 1, stats::qt(0.999, df))
  xs <- seq(-xr, xr, length.out = 800)
  ys <- stats::dt(xs, df)

  crit <- switch(alternative,
    two.sided = stats::qt(c(alpha / 2, 1 - alpha / 2), df),
    less = c(-Inf, stats::qt(alpha, df)),
    greater = c(stats::qt(1 - alpha, df), Inf)
  )

  shade <- function(lo, hi) {
    xx <- seq(max(lo, -xr), min(hi, xr), length.out = 200)
    yy <- stats::dt(xx, df)
    graphics::polygon(c(xx, rev(xx)), c(yy, rep(0, length(yy))),
                       col = grDevices::adjustcolor(col_critical, alpha.f = 0.35), border = NA)
  }

  op <- graphics::par(mar = c(4.5, 4.5, 3, 1))
  on.exit(graphics::par(op))
  plot_title <- if (is.null(main)) sprintf("Distribucion t (df = %.1f)", df) else main
  graphics::plot(xs, ys, type = "l", lwd = 2, col = col_curve,
                 xlab = "t", ylab = "Densidad", main = plot_title)

  if (alternative == "two.sided") {
    shade(-xr, crit[1]); shade(crit[2], xr)
  } else if (alternative == "less") {
    shade(-xr, crit[2])
  } else {
    shade(crit[1], xr)
  }

  graphics::abline(v = t, col = col_observed, lwd = 2, lty = 2)
  graphics::points(t, stats::dt(t, df), pch = 19, col = col_observed, cex = 1.2)

  stars <- if (p_value < 0.001) "***" else if (p_value < 0.01) "**" else
           if (p_value < 0.05) "*" else if (p_value < 0.1) "." else ""
  legend_pos <- if (t >= 0) "topleft" else "topright"
  graphics::legend(legend_pos, bty = "n", text.col = "#52514e", cex = 0.85,
    legend = c(sprintf("t observado = %.3f", t),
               sprintf("p = %.4g %s", p_value, stars),
               sprintf("alfa = %.3g (%s)", alpha, alternative)))

  invisible(list(t = t, df = df, p.value = p_value, alternative = alternative,
                 alpha = alpha, critical = crit, stars = stars))
}
