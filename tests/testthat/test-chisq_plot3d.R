skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")

make_simple_chi_data <- function(seed = 1, n = 300) {
  set.seed(seed)
  data.frame(
    tipo = sample(c("A", "B", "C"), n, replace = TRUE),
    sexo = sample(c("Varon", "Mujer"), n, replace = TRUE)
  )
}

make_nested_chi_data <- function(seed = 11, n = 400) {
  set.seed(seed)
  sexo <- sample(c("Varon", "Mujer"), n, replace = TRUE)
  compra <- sample(c("Si", "No"), n, replace = TRUE)
  tipo <- character(n)
  for (i in seq_len(n)) {
    if (sexo[i] == "Varon" && compra[i] == "Si") {
      tipo[i] <- sample(c("A", "B", "C"), 1, prob = c(0.7, 0.15, 0.15))
    } else if (sexo[i] == "Varon" && compra[i] == "No") {
      tipo[i] <- sample(c("A", "B", "C"), 1, prob = c(0.15, 0.7, 0.15))
    } else {
      tipo[i] <- sample(c("A", "B", "C"), 1, prob = c(0.33, 0.33, 0.34))
    }
  }
  data.frame(tipo = tipo, sexo = sexo, compra = compra)
}

test_that("chisq_plot3d() validates column names and opacity", {
  df <- make_simple_chi_data()
  expect_error(chisq_plot3d(df, row = "no_existe", col = "sexo"))
  expect_error(chisq_plot3d(df, row = "tipo", col = "no_existe"))
  expect_error(chisq_plot3d(df, row = "tipo", col = "sexo", opacity = 0))
})

test_that("chisq_plot3d()'s chi-square statistic and standardized residuals match stats::chisq.test() directly", {
  df <- make_simple_chi_data(seed = 2)
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo")
  st <- attr(fig, "stats")

  expected_tab <- table(df$tipo, df$sexo)
  expected_chi <- stats::chisq.test(expected_tab, correct = FALSE)
  expect_equal(unname(st$chisq$statistic), unname(expected_chi$statistic))
  expect_equal(st$chisq$p.value, expected_chi$p.value)
  expect_equal(unname(st$stdres), unname(expected_chi$stdres), ignore_attr = TRUE)
})

test_that("chisq_plot3d() combines multiple 'col' columns via interaction()", {
  df <- make_nested_chi_data(seed = 3)
  fig <- chisq_plot3d(df, row = "tipo", col = c("sexo", "compra"))
  st <- attr(fig, "stats")
  expect_equal(dim(st$table), c(3, 4))
  expect_true(all(c("Varon - Si", "Varon - No", "Mujer - Si", "Mujer - No") %in% colnames(st$table)))
})

test_that("chisq_plot3d()'s Simpson diagnostics are not applicable for a simple 2-way table", {
  df <- make_simple_chi_data(seed = 4)
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo")
  st <- attr(fig, "stats")
  expect_false(st$simpson$applicable)
})

test_that("chisq_plot3d() detects an engineered qualitative-interaction (Simpson-style) pattern", {
  df <- make_nested_chi_data(seed = 11)
  fig <- chisq_plot3d(df, row = "tipo", col = c("sexo", "compra"))
  st <- attr(fig, "stats")
  simpson <- st$simpson

  expect_true(simpson$applicable)
  expect_equal(simpson$sub_var, "compra")
  expect_equal(simpson$base_vars, "sexo")
  # Tipo A and Tipo B were engineered to reverse sign between Compra levels;
  # Tipo C was left uniform (no engineered interaction).
  expect_true(any(simpson$descriptive_flags$fila == "A"))
  expect_true(any(simpson$descriptive_flags$fila == "B"))
  expect_false(any(simpson$descriptive_flags$fila == "C"))

  gs_C <- simpson$gail_simon[simpson$gail_simon$fila == "C", ]
  expect_true(all(gs_C$p.value > 0.5))
  gs_A <- simpson$gail_simon[simpson$gail_simon$fila == "A", ]
  expect_true(all(gs_A$p.value < 0.001))
})

test_that(".gail_simon_test() is well-calibrated under the null (p-values approximately uniform)", {
  set.seed(99)
  for (k in c(2, 3, 4)) {
    pvals <- replicate(3000, visual.kaito:::.gail_simon_test(stats::rnorm(k))$p.value)
    expect_true(abs(mean(pvals < 0.05) - 0.05) < 0.02)
  }
})

test_that("chisq_plot3d() flags low expected-frequency tables via Cochran's rule", {
  set.seed(5)
  df_small <- data.frame(
    a = sample(c("X", "Y", "Z"), 25, replace = TRUE),
    b = sample(c("P", "Q"), 25, replace = TRUE)
  )
  # stats::chisq.test() itself emits "Chi-squared approximation may be
  # incorrect" for this deliberately sparse table -- that warning is
  # expected here and is precisely the scenario this test exercises.
  fig <- suppressWarnings(chisq_plot3d(df_small, row = "a", col = "b"))
  st <- attr(fig, "stats")
  expect_true(any(st$expected < 5))
  expect_match(st$cochran_warning, "Cochran")
})

test_that("chisq_plot3d() does not warn when expected frequencies are all comfortably large", {
  df <- make_simple_chi_data(seed = 6, n = 600)
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo")
  st <- attr(fig, "stats")
  expect_true(all(st$expected >= 5))
  expect_null(st$cochran_warning)
})

test_that("chisq_plot3d() builds one mesh3d bar per cell plus 4 reference-plane traces", {
  df <- make_simple_chi_data(seed = 7, n = 300)
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo")
  b <- plotly::plotly_build(fig)
  expect_equal(length(b$x$data), 3 * 2 + 4)
  types <- vapply(b$x$data, function(tr) tr$type, character(1))
  expect_true(all(types == "mesh3d"))
})

test_that("chisq_plot3d()'s transparency slider defaults to the step matching 'opacity'", {
  df <- make_simple_chi_data(seed = 8)
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo", opacity = 0.45)
  sl <- plotly::plotly_build(fig)$x$layout$sliders[[1]]
  expect_equal(sl$steps[[sl$active + 1]]$label, "45%")
})

test_that(".diverging_color() returns the extreme colors at +/- the clamp limit and white near zero", {
  expect_equal(visual.kaito:::.diverging_color(0), "#F7F7F7")
  expect_equal(visual.kaito:::.diverging_color(10), visual.kaito:::.diverging_color(3))
  expect_equal(visual.kaito:::.diverging_color(-10), visual.kaito:::.diverging_color(-3))
})

test_that("chisq_plot3d() validates the view/lang arguments", {
  df <- make_simple_chi_data()
  expect_error(chisq_plot3d(df, row = "tipo", col = "sexo", view = "4d"))
  expect_error(chisq_plot3d(df, row = "tipo", col = "sexo", lang = "fr"))
})

test_that("chisq_plot3d()'s view argument sets the initial camera projection", {
  df <- make_simple_chi_data()
  fig_3d <- chisq_plot3d(df, row = "tipo", col = "sexo", view = "3d")
  fig_2d <- chisq_plot3d(df, row = "tipo", col = "sexo", view = "2d")
  proj_3d <- plotly::plotly_build(fig_3d)$x$layout$scene$camera$projection$type
  proj_2d <- plotly::plotly_build(fig_2d)$x$layout$scene$camera$projection$type
  expect_equal(proj_3d, "perspective")
  expect_equal(proj_2d, "orthographic")
})

test_that("chisq_plot3d()'s 3D/2D buttons restyle the camera via flat leaf-level keys", {
  # un objeto anidado en "scene.camera" deja la camara en un estado
  # degenerado al hacer clic (mismo hallazgo que en irt_plot3d()) -- los
  # botones deben usar claves punteadas de hoja, nunca un objeto anidado.
  df <- make_simple_chi_data()
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo")
  btns <- plotly::plotly_build(fig)$x$layout$updatemenus[[1]]$buttons
  for (btn in btns) {
    arg_names <- names(btn$args[[1]])
    expect_true(all(grepl("^scene\\.camera\\.", arg_names)))
    expect_false("scene.camera" %in% arg_names)
  }
})

test_that("chisq_plot3d() lang = 'en' translates the plot text and messages", {
  df <- make_simple_chi_data()
  fig <- chisq_plot3d(df, row = "tipo", col = "sexo", lang = "en")
  pb <- plotly::plotly_build(fig)
  expect_match(pb$x$layout$title$text, "^Chi-square")
  expect_equal(pb$x$layout$scene$zaxis$title$text, "Standardized residual")
  expect_equal(pb$x$layout$sliders[[1]]$currentvalue$prefix, "Opacity: ")
  expect_error(chisq_plot3d(df, row = "no_existe", col = "sexo", lang = "en"), "do not exist")
})
