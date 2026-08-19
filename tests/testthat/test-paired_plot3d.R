skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")

make_paired_data <- function(seed = 1, n = 40) {
  set.seed(seed)
  x_pre <- rnorm(n, 60, 10)
  z_pre <- rnorm(n, 40, 8)
  data.frame(
    x_pre = x_pre,
    x_post = x_pre - rnorm(n, 12, 8),
    z_pre = z_pre,
    z_post = z_pre + rnorm(n, 10, 9)
  )
}

test_that("paired_plot3d() validates conf.level, opacity and col_dir length", {
  df <- make_paired_data()
  expect_error(
    paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", conf.level = 1.5)
  )
  expect_error(
    paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", opacity = 0)
  )
  expect_error(
    paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", col_dir = c("#111111", "#222222")),
    "3 colores"
  )
})

test_that("paired_plot3d() requires at least 3 complete pairs", {
  df <- make_paired_data(n = 2)
  expect_error(
    paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post"),
    "al menos 3 pares"
  )
})

test_that("paired_plot3d() excludes incomplete rows with a message", {
  df <- make_paired_data()
  df$x_post[1] <- NA
  expect_message(
    paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post"),
    "1 fila"
  )
})

test_that("paired_plot3d() returns a plotly widget with a 'stats' attribute of the documented shape", {
  df <- make_paired_data()
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post")
  expect_s3_class(fig, "plotly")

  st <- attr(fig, "stats")
  expect_named(st, c("n", "mean_sd", "t_x", "t_z", "hotelling_paired"))
  expect_equal(st$n, 40)
  expect_s3_class(st$t_x, "htest")
  expect_s3_class(st$t_z, "htest")
  expect_true(st$hotelling_paired$p.value >= 0 && st$hotelling_paired$p.value <= 1)
})

test_that("paired_plot3d()'s per-axis paired t-tests match stats::t.test(paired = TRUE) directly", {
  df <- make_paired_data(seed = 2)
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", conf.level = 0.90)
  st <- attr(fig, "stats")

  expected_x <- stats::t.test(df$x_post, df$x_pre, paired = TRUE, conf.level = 0.90)
  expected_z <- stats::t.test(df$z_post, df$z_pre, paired = TRUE, conf.level = 0.90)
  expect_equal(st$t_x$p.value, expected_x$p.value)
  expect_equal(st$t_z$p.value, expected_z$p.value)
})

test_that("paired_plot3d()'s Hotelling T2 matches a direct one-sample computation on the differences", {
  df <- make_paired_data(seed = 3)
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post")
  st <- attr(fig, "stats")

  D <- cbind(dx = df$x_post - df$x_pre, dz = df$z_post - df$z_pre)
  expected <- visual.kaito:::.hotelling_t2_one(D)
  expect_equal(st$hotelling_paired$p.value, expected$p.value)
  expect_equal(st$hotelling_paired$T2, expected$T2)
})

test_that("paired_plot3d()'s reported mean/SD match direct computation", {
  df <- make_paired_data(seed = 4)
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post")
  st <- attr(fig, "stats")
  expect_equal(unname(st$mean_sd$x_pre["mean"]), mean(df$x_pre))
  expect_equal(unname(st$mean_sd$z_post["sd"]), stats::sd(df$z_post))
})

test_that("paired_plot3d() builds one line trace per subject plus 2 marker traces", {
  df <- make_paired_data(seed = 5, n = 25)
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post")
  b <- plotly::plotly_build(fig)
  expect_equal(length(b$x$data), 27)
  modes <- vapply(b$x$data, function(tr) tr$mode, character(1))
  expect_equal(sum(modes == "lines"), 25)
  expect_equal(sum(modes == "markers"), 2)
})

test_that("paired_plot3d()'s transparency slider defaults to the active step matching 'opacity'", {
  df <- make_paired_data()
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", opacity = 0.45)
  sl <- plotly::plotly_build(fig)$x$layout$sliders[[1]]
  expect_equal(sl$steps[[sl$active + 1]]$label, "45%")

  fig2 <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post", opacity = 0.95)
  sl2 <- plotly::plotly_build(fig2)$x$layout$sliders[[1]]
  expect_equal(sl2$steps[[sl2$active + 1]]$label, "95%")
})

test_that("paired_plot3d()'s 'escala' updatemenu restyle args are shaped for all n + 2 traces", {
  df <- make_paired_data(seed = 6, n = 20)
  fig <- paired_plot3d(df, "x_pre", "x_post", "z_pre", "z_post")
  um <- plotly::plotly_build(fig)$x$layout$updatemenus[[2]]
  raw_args <- um$buttons[[1]]$args
  std_args <- um$buttons[[2]]$args

  expect_length(raw_args[[1]]$x, 22)
  expect_length(raw_args[[1]]$z, 22)
  expect_length(raw_args[[3]], 22)
  expect_length(std_args[[1]]$x, 22)

  # The Pre marker trace (index n, i.e. R element n+1 = 21) must carry the raw
  # Pre values untouched, and the standardized version must equal scale()
  # applied to the pooled Pre+Post values.
  expect_equal(raw_args[[1]]$x[[21]], df$x_pre)
  z_pooled <- as.numeric(scale(c(df$x_pre, df$x_post)))
  expect_equal(std_args[[1]]$x[[21]], z_pooled[seq_len(20)])
})
