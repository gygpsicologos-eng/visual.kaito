skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")
skip_if_not_installed("mirt")

make_irt_data <- function(seed = 1, n = 150) {
  set.seed(seed)
  theta <- stats::rnorm(n)
  logistic <- function(x) 1 / (1 + exp(-x))
  sim_dich <- function(a, b, c = 0) stats::rbinom(n, 1, c + (1 - c) * logistic(a * (theta - b)))
  sim_poly <- function(a, bs) {
    k <- length(bs) + 1
    cumP <- sapply(bs, function(b) logistic(a * (theta - b)))
    catP <- cbind(1, cumP) - cbind(cumP, 0)
    apply(catP, 1, function(p) sample.int(k, 1, prob = pmax(p, 0)))
  }
  data.frame(
    i1 = sim_dich(1.2, -0.5),
    i2 = sim_dich(1.4, 0.3),
    L1 = sim_poly(1.3, c(-1.2, 0.0, 1.2))
  )
}

test_that("irt_plot3d() validates inputs", {
  df <- make_irt_data()
  expect_error(irt_plot3d(df, items = "i1"), "al menos 3 items")
  expect_error(irt_plot3d(df, items = c("i1", "i2")), "al menos 3 items")
  expect_error(irt_plot3d(df, items = c("i1", "i2", "no_existe")), "no existen")
  expect_error(irt_plot3d(df, opacity = 0))
  expect_error(irt_plot3d(df, bin_width = 0))
})

test_that("irt_plot3d() requires at least 30 complete subjects", {
  df <- make_irt_data(n = 10)
  expect_error(irt_plot3d(df), "al menos 30 sujetos")
})

test_that("irt_plot3d() excludes incomplete rows with a message", {
  df <- make_irt_data()
  df$i1[1] <- NA
  expect_message(irt_plot3d(df), "Se excluyeron 1")
})

test_that("irt_plot3d() auto-detects dichotomous vs polytomous items and fits accordingly", {
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  st <- attr(fig, "stats")
  expect_named(st, c("fit", "coefficients", "theta", "curves", "itemtype"))
  expect_equal(unname(st$itemtype[c("i1", "i2")]), c("2PL", "2PL"))
  expect_equal(unname(st$itemtype["L1"]), "graded")
  expect_true(all(c("i1", "i2") %in% st$curves$item))
  expect_equal(sum(st$curves$item == "L1"), 3) # 4 categorias -> 3 umbrales
  expect_equal(nrow(st$theta), nrow(df))
})

test_that("irt_plot3d() computes theta_50 via the real P=0.5 crossing when guessing > 0", {
  df <- make_irt_data()
  fig <- irt_plot3d(df, guessing = TRUE)
  st <- attr(fig, "stats")
  cv <- st$curves[!st$curves$poly, ]
  has_guessing <- cv$c > 1e-8
  if (any(has_guessing)) {
    manual <- cv$b[has_guessing] - (1 / cv$a[has_guessing]) * log(0.5 / (0.5 - cv$c[has_guessing]))
    expect_equal(cv$theta50[has_guessing], manual)
  }
  if (any(!has_guessing)) {
    expect_equal(cv$theta50[!has_guessing], cv$b[!has_guessing])
  }
  # los umbrales politomicos nunca llevan azar
  expect_true(all(st$curves$c[st$curves$poly] == 0))
})

test_that("irt_plot3d() warns that guessing does not apply to polytomous items", {
  df <- make_irt_data()
  expect_message(irt_plot3d(df, guessing = TRUE), "no existe en el modelo graduado")
})

test_that("irt_plot3d() groups each curve with its own connector line and histogram bar", {
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  st <- attr(fig, "stats")
  n_curves <- nrow(st$curves)
  pb <- plotly::plotly_build(fig)
  types <- vapply(pb$x$data, function(tr) tr$type, character(1))
  expect_equal(sum(types == "scatter3d"), 2 * n_curves + 1)

  scatter_traces <- pb$x$data[types == "scatter3d"]
  curve_and_connector <- scatter_traces[seq_len(2 * n_curves)]
  groups <- vapply(curve_and_connector, function(tr) tr$legendgroup, character(1))
  showlegend <- vapply(curve_and_connector, function(tr) isTRUE(tr$showlegend), logical(1))
  expect_equal(groups[c(TRUE, FALSE)], groups[c(FALSE, TRUE)]) # curva y su conector: mismo grupo
  expect_true(all(showlegend[c(TRUE, FALSE)]))  # la curva se ve en la leyenda
  expect_true(all(!showlegend[c(FALSE, TRUE)])) # el conector no
})

test_that("irt_plot3d()'s transparency slider defaults to the active step matching 'opacity'", {
  df <- make_irt_data()
  fig <- irt_plot3d(df, opacity = 0.45)
  sl <- plotly::plotly_build(fig)$x$layout$sliders[[1]]
  expect_equal(sl$steps[[sl$active + 1]]$label, "45%")

  fig2 <- irt_plot3d(df, opacity = 0.95)
  sl2 <- plotly::plotly_build(fig2)$x$layout$sliders[[1]]
  expect_equal(sl2$steps[[sl2$active + 1]]$label, "95%")
})

test_that("irt_plot3d()'s opacity slider targets every trace in the figure", {
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  pb <- plotly::plotly_build(fig)
  n_traces <- length(pb$x$data)
  sl <- pb$x$layout$sliders[[1]]
  n_targets <- length(sl$steps[[1]]$args[[2]])
  expect_equal(n_targets, n_traces)
})

test_that("irt_plot3d() accepts a custom item color vector", {
  df <- make_irt_data()
  cols <- c(i1 = "#111111", i2 = "#222222", L1 = "#333333")
  fig <- irt_plot3d(df, col_items = cols)
  pb <- plotly::plotly_build(fig)
  first_i1_line <- Find(function(tr) isTRUE(tr$name == "i1") && tr$type == "scatter3d", pb$x$data)
  expect_equal(first_i1_line$line$color, "#111111")
})
