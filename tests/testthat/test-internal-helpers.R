# Unit tests for the internal (@noRd) statistical helpers shared across
# boxplot3d_interactive(), ttest_plot3d() and manova_plot3d(). These are the
# functions that actually compute numbers, so they get exact, hand-checkable
# assertions rather than just "does it run" smoke tests.

test_that(".as_plain_numeric() strips haven_labelled/labelled wrappers", {
  v <- structure(c(1, 2, 3), class = "haven_labelled", labels = c(low = 1))
  out <- visual.kaito:::.as_plain_numeric(v)
  expect_type(out, "double")
  expect_null(attr(out, "class"))
  expect_equal(out, c(1, 2, 3))
})

test_that(".as_plain_numeric() passes plain numeric vectors through unchanged", {
  expect_equal(visual.kaito:::.as_plain_numeric(c(1.5, 2.5, NA)), c(1.5, 2.5, NA))
})

test_that(".as_plain_numeric() errors when a column cannot be coerced to numeric", {
  v <- rep(NA_character_, 5)
  expect_error(visual.kaito:::.as_plain_numeric(v), "no se ha podido convertir|numerica", ignore.case = TRUE)
})

test_that(".box_axis_stats() tukey method: whiskers reach the most extreme non-outlier", {
  set.seed(1)
  v <- c(rnorm(30, 0, 1), 50) # 50 is a deliberate outlier
  st <- visual.kaito:::.box_axis_stats(v, "tukey", k = 1.5, probs = c(0.05, 0.95), sd_mult = c(1, 2))
  expect_true(st$whisker_hi < 50)
  expect_true(any(st$outliers))
  expect_true(st$outliers[length(v)]) # the planted 50 must be flagged
  expect_equal(unname(st$q1), unname(stats::quantile(v, 0.25)))
  expect_equal(unname(st$q3), unname(stats::quantile(v, 0.75)))
})

test_that(".box_axis_stats() percentile method never flags outliers", {
  set.seed(2)
  v <- c(rnorm(30), 100)
  st <- visual.kaito:::.box_axis_stats(v, "percentile", k = 1.5, probs = c(0.05, 0.95), sd_mult = c(1, 2))
  expect_false(any(st$outliers))
  expect_equal(unname(st$whisker_lo), unname(stats::quantile(v, 0.05)))
  expect_equal(unname(st$whisker_hi), unname(stats::quantile(v, 0.95)))
})

test_that(".box_axis_stats() sd method centers on the mean with the requested multipliers", {
  v <- c(10, 12, 14, 16, 18)
  st <- visual.kaito:::.box_axis_stats(v, "sd", k = 1.5, probs = c(0.05, 0.95), sd_mult = c(1, 2))
  mu <- mean(v); sdv <- stats::sd(v)
  expect_equal(st$center, mu)
  expect_equal(st$q1, mu - sdv); expect_equal(st$q3, mu + sdv)
  expect_equal(st$whisker_lo, mu - 2 * sdv); expect_equal(st$whisker_hi, mu + 2 * sdv)
})

test_that(".hotelling_t2() reduces exactly to a classic equal-variance two-sample t-test when p = 1", {
  set.seed(3)
  x1 <- rnorm(20, 0, 1.3); x2 <- rnorm(18, 1.1, 1.3)
  hot <- visual.kaito:::.hotelling_t2(
    m1 = mean(x1), m2 = mean(x2),
    S1 = matrix(stats::var(x1), 1, 1), S2 = matrix(stats::var(x2), 1, 1),
    n1 = length(x1), n2 = length(x2)
  )
  tt <- stats::t.test(x1, x2, var.equal = TRUE)
  # For a single variable, Hotelling's T^2 collapses to the square of the
  # pooled-variance t-statistic, and F(1, df) == t(df)^2 -- so the p-values
  # must match to numerical precision, not just approximately.
  expect_equal(hot$p.value, tt$p.value, tolerance = 1e-8)
  expect_equal(hot$T2, unname(tt$statistic)^2, tolerance = 1e-8)
})

test_that(".hotelling_t2() gives a near-1 p-value when the two groups are identical in distribution", {
  set.seed(4)
  m <- c(0, 0)
  S <- diag(2)
  hot <- visual.kaito:::.hotelling_t2(m1 = m, m2 = m, S1 = S, S2 = S, n1 = 40, n2 = 40)
  expect_equal(hot$T2, 0)
  expect_equal(hot$p.value, 1)
})

test_that(".dmvnorm2() matches the product of independent normals when covariance is diagonal", {
  mu <- c(2, -1); S <- diag(c(1.5^2, 0.8^2))
  xg <- seq(-2, 5, length.out = 15); zg <- seq(-4, 2, length.out = 15)
  expect_equal(
    visual.kaito:::.dmvnorm2(xg, zg, mu, S),
    stats::dnorm(xg, mu[1], 1.5) * stats::dnorm(zg, mu[2], 0.8),
    tolerance = 1e-10
  )
})

test_that(".stars3() applies the standard significance thresholds", {
  expect_equal(visual.kaito:::.stars3(0.0001), "***")
  expect_equal(visual.kaito:::.stars3(0.005), "**")
  expect_equal(visual.kaito:::.stars3(0.03), "*")
  expect_equal(visual.kaito:::.stars3(0.07), ".")
  expect_equal(visual.kaito:::.stars3(0.5), "")
  expect_equal(visual.kaito:::.stars3(NA), "")
})
