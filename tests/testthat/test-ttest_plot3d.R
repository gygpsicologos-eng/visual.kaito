skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")

make_ttest_data <- function(seed = 1) {
  set.seed(seed)
  rbind(
    data.frame(x = rnorm(30, 0, 1.2), z = rnorm(30, 0, 0.9), group = "A"),
    data.frame(x = rnorm(30, 2.5, 1.1), z = rnorm(30, 1.8, 1.0), group = "B")
  )
}

test_that("ttest_plot3d() requires exactly 2 groups", {
  df1 <- make_ttest_data(); df1$group <- "A"
  expect_error(ttest_plot3d(df1), "exactamente 2 grupos")

  df3 <- rbind(make_ttest_data(), data.frame(x = rnorm(10), z = rnorm(10), group = "C"))
  expect_error(ttest_plot3d(df3), "exactamente 2 grupos")
})

test_that("ttest_plot3d() validates conf.level, opacity and col length", {
  df <- make_ttest_data()
  expect_error(ttest_plot3d(df, conf.level = 1.5))
  expect_error(ttest_plot3d(df, opacity = 0))
  expect_error(ttest_plot3d(df, col = "#2a78d6"), "2 colores")
})

test_that("ttest_plot3d() returns a plotly widget with a 'stats' attribute of the documented shape", {
  df <- make_ttest_data()
  fig <- ttest_plot3d(df)
  expect_s3_class(fig, "plotly")

  st <- attr(fig, "stats")
  expect_named(st, c("means", "covariances", "mean_sd", "overlap", "t_x", "t_z", "hotelling"))
  expect_length(st$means, 2)
  expect_length(st$covariances, 2)
  expect_true(st$overlap >= 0 && st$overlap <= 1)
  expect_s3_class(st$t_x, "htest")
  expect_s3_class(st$t_z, "htest")
  expect_true(st$hotelling$p.value >= 0 && st$hotelling$p.value <= 1)
})

test_that("ttest_plot3d()'s per-axis t-tests match stats::t.test() run directly on the same data", {
  df <- make_ttest_data()
  fig <- ttest_plot3d(df, var.equal = FALSE, conf.level = 0.90)
  st <- attr(fig, "stats")

  expected_x <- stats::t.test(df$x[df$group == "A"], df$x[df$group == "B"], var.equal = FALSE, conf.level = 0.90)
  expected_z <- stats::t.test(df$z[df$group == "A"], df$z[df$group == "B"], var.equal = FALSE, conf.level = 0.90)
  expect_equal(st$t_x$p.value, expected_x$p.value)
  expect_equal(st$t_z$p.value, expected_z$p.value)
})

test_that("ttest_plot3d()'s reported group means/SDs match direct computation", {
  df <- make_ttest_data()
  fig <- ttest_plot3d(df)
  st <- attr(fig, "stats")
  expect_equal(unname(st$mean_sd$x["A"]), mean(df$x[df$group == "A"]))
  expect_equal(unname(st$mean_sd$sd_z["B"]), stats::sd(df$z[df$group == "B"]))
})

test_that("ttest_plot3d()'s overlap coefficient is near 1 for two virtually identical groups and near 0 for far-apart groups", {
  set.seed(5)
  same <- data.frame(x = rnorm(200, 0, 1), z = rnorm(200, 0, 1))
  df_same <- rbind(cbind(same[1:100, ], group = "A"), cbind(same[101:200, ], group = "B"))
  fig_same <- ttest_plot3d(df_same, n_grid = 40)
  expect_gt(attr(fig_same, "stats")$overlap, 0.5)

  df_far <- rbind(
    data.frame(x = rnorm(100, 0, 0.3), z = rnorm(100, 0, 0.3), group = "A"),
    data.frame(x = rnorm(100, 20, 0.3), z = rnorm(100, 20, 0.3), group = "B")
  )
  fig_far <- ttest_plot3d(df_far, n_grid = 40)
  expect_lt(attr(fig_far, "stats")$overlap, 0.01)
})

test_that("ttest_plot3d()'s reported statistics are identical whether standardized-view data is later toggled or not", {
  # The raw/standardized button only changes what the 3D surfaces show; the
  # t-test/Hotelling/overlap numbers must not depend on it (documented
  # invariance property). We can't click the button in a unit test, but we
  # can confirm the *scale-invariant* quantities agree with a version of the
  # function fed already-standardized columns directly.
  df <- make_ttest_data()
  fig_raw <- ttest_plot3d(df)
  df_z <- df
  df_z$x <- as.numeric(scale(df$x)); df_z$z <- as.numeric(scale(df$z))
  fig_z <- ttest_plot3d(df_z)

  expect_equal(attr(fig_raw, "stats")$t_x$p.value, attr(fig_z, "stats")$t_x$p.value)
  expect_equal(attr(fig_raw, "stats")$hotelling$p.value, attr(fig_z, "stats")$hotelling$p.value, tolerance = 1e-8)
  expect_equal(attr(fig_raw, "stats")$overlap, attr(fig_z, "stats")$overlap, tolerance = 1e-3)
})
