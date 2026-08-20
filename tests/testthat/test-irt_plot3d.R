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

test_that("irt_plot3d() validates the new view/lang/hist_shift arguments", {
  df <- make_irt_data()
  expect_error(irt_plot3d(df, view = "4d"))
  expect_error(irt_plot3d(df, lang = "fr"))
  expect_error(irt_plot3d(df, hist_shift = -0.1))
  expect_error(irt_plot3d(df, hist_shift = 1.1))
})

test_that("irt_plot3d()'s view argument sets the initial camera projection", {
  df <- make_irt_data()
  fig_3d <- irt_plot3d(df, view = "3d")
  fig_2d <- irt_plot3d(df, view = "2d")
  proj_3d <- plotly::plotly_build(fig_3d)$x$layout$scene$camera$projection$type
  proj_2d <- plotly::plotly_build(fig_2d)$x$layout$scene$camera$projection$type
  expect_equal(proj_3d, "perspective")
  expect_equal(proj_2d, "orthographic")
})

test_that("irt_plot3d()'s 3D/2D buttons restyle the camera via flat leaf-level keys", {
  # un objeto anidado en "scene.camera" deja la camara en un estado degenerado
  # al hacer clic (verificado visualmente) -- los botones deben usar claves
  # punteadas de hoja como "scene.camera.eye.x", nunca un objeto anidado.
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  btns <- plotly::plotly_build(fig)$x$layout$updatemenus[[1]]$buttons
  for (btn in btns) {
    arg_names <- names(btn$args[[1]])
    expect_true(all(grepl("^scene\\.camera\\.", arg_names)))
    expect_false("scene.camera" %in% arg_names)
  }
})

test_that("irt_plot3d()'s hist_shift argument sets the initial histogram-panel position", {
  df <- make_irt_data()
  fig_front <- irt_plot3d(df, hist_shift = 1)
  fig_back <- irt_plot3d(df, hist_shift = 0)
  bar_y_front <- Find(function(tr) tr$type == "mesh3d", plotly::plotly_build(fig_front)$x$data)$y
  bar_y_back <- Find(function(tr) tr$type == "mesh3d", plotly::plotly_build(fig_back)$x$data)$y
  y0_front <- mean(range(bar_y_front))
  y0_back <- mean(range(bar_y_back))
  expect_equal(y0_front, 0.30, tolerance = 1e-6)
  expect_gt(y0_back, y0_front) # el reposo (0%) queda hacia el fondo, lejos de las curvas
})

test_that("irt_plot3d()'s histogram-shift slider targets subject bars, item bars, connectors and the reference line", {
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  st <- attr(fig, "stats")
  pb <- plotly::plotly_build(fig)
  sl <- pb$x$layout$sliders[[2]]
  expect_equal(sl$currentvalue$prefix, "Desplazamiento del histograma: ")
  n_targets <- length(sl$steps[[1]]$args[[2]])
  types <- vapply(pb$x$data, function(tr) tr$type, character(1))
  n_mesh3d <- sum(types == "mesh3d") # sujetos + items
  n_curves <- nrow(st$curves)
  expect_equal(n_targets, n_mesh3d + n_curves + 1L) # + conectores + linea de referencia
  expect_equal(length(sl$steps), 5L)
})

test_that("irt_plot3d() lang = 'en' translates the plot text and messages", {
  df <- make_irt_data()
  fig <- irt_plot3d(df, lang = "en")
  pb <- plotly::plotly_build(fig)
  expect_match(pb$x$layout$title$text, "^IRT map")
  expect_equal(pb$x$layout$scene$xaxis$title$text, "Shared trait level (theta)")
  expect_equal(pb$x$layout$sliders[[1]]$currentvalue$prefix, "Opacity: ")
  expect_error(irt_plot3d(df, items = c("i1", "i2"), lang = "en"), "at least 3 items")
})

test_that("irt_plot3d() groups each curve's checkboxes by item and response option", {
  df <- make_irt_data()
  fig <- irt_plot3d(df)
  st <- attr(fig, "stats")
  # L1 tiene 4 categorias -> 3 umbrales -> 3 columnas (opciones) para esa fila
  expect_equal(sum(st$curves$item == "L1"), 3)
  expect_equal(sum(st$curves$item == "i1"), 1)
})
