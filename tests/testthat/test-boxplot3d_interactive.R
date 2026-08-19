skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")

make_boxplot_data <- function(seed = 1) {
  set.seed(seed)
  rbind(
    data.frame(x = rnorm(25, 0), y = rnorm(25, 0), z = rnorm(25, 0),
               group = "A", turno = sample(c("Manana", "Tarde"), 25, replace = TRUE)),
    data.frame(x = rnorm(25, 1.5), y = rnorm(25, -1), z = rnorm(25, 0.5),
               group = "B", turno = sample(c("Manana", "Tarde"), 25, replace = TRUE))
  )
}

test_that("boxplot3d_interactive() requires exactly one whisker method flag", {
  df <- make_boxplot_data()
  expect_error(boxplot3d_interactive(df, tukey = 1, sd = 1), "exactamente uno")
  expect_error(boxplot3d_interactive(df, tukey = 0, percentile = 0, sd = 0, letter_value = 0), "exactamente uno")
})

test_that("boxplot3d_interactive() validates scale and grid3d as 0/1", {
  df <- make_boxplot_data()
  expect_error(boxplot3d_interactive(df, scale = 2))
  expect_error(boxplot3d_interactive(df, grid3d = 5))
})

test_that("boxplot3d_interactive() returns a plotly htmlwidget", {
  df <- make_boxplot_data()
  fig <- boxplot3d_interactive(df)
  expect_s3_class(fig, "plotly")
  expect_s3_class(fig, "htmlwidget")
})

test_that("boxplot3d_interactive() defaults 'Agrupar por' to group plus other factor/character columns", {
  df <- make_boxplot_data()
  fig <- boxplot3d_interactive(df)
  data <- fig$jsHooks$render[[1]]$data
  expect_true("group" %in% data$color_by)
  expect_true("turno" %in% data$color_by)
})

test_that("boxplot3d_interactive() internal trace-bookkeeping vectors stay in sync with the actual traces", {
  # Regression guard: geom_mode/geom_method/geom_role/geom_scale and
  # pt_mode/pt_colorby/pt_scale are built up trace-by-trace as add_trace()
  # is called; if a future edit adds/removes a trace without updating the
  # matching bookkeeping vector, the JS state machine silently shows the
  # wrong traces. This catches that class of bug without needing a browser.
  df <- make_boxplot_data()
  fig <- boxplot3d_interactive(df)
  built <- plotly::plotly_build(fig)
  n_traces <- length(built$x$data)
  data <- fig$jsHooks$render[[1]]$data
  expect_equal(n_traces, length(data$geom_mode) + length(data$pt_mode))
  expect_equal(length(data$geom_mode), length(data$geom_method))
  expect_equal(length(data$geom_mode), length(data$geom_role))
  expect_equal(length(data$geom_mode), length(data$geom_scale))
  expect_equal(length(data$pt_mode), length(data$pt_colorby))
  expect_equal(length(data$pt_mode), length(data$pt_scale))
})

test_that("boxplot3d_interactive() marks exactly one (mode, method, scale) combination visible at load", {
  df <- make_boxplot_data()
  fig <- boxplot3d_interactive(df, tukey = 1, scale = 0)
  built <- plotly::plotly_build(fig)
  vis <- vapply(built$x$data, function(tr) isTRUE(tr$visible), logical(1))
  data <- fig$jsHooks$render[[1]]$data
  n_groups <- length(unique(df$group))
  # 3D mode draws 8 traces per group (mesh, edges, 3 whiskers, 2 median marks, outliers);
  # plus exactly 1 initially-visible point trace (the first "Agrupar por" option).
  expect_equal(sum(vis), 8L * n_groups + 1L)
})

test_that("boxplot3d_interactive() honors a custom initial method (letter_value) and scale (zscore)", {
  df <- make_boxplot_data()
  fig <- boxplot3d_interactive(df, tukey = 0, letter_value = 1, scale = 1)
  data <- fig$jsHooks$render[[1]]$data
  expect_equal(data$default_method, "letter_value")
  expect_equal(data$default_scale, "zscore")
})
