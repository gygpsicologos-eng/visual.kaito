skip_if_not_installed("plotly")
skip_if_not_installed("htmlwidgets")

make_manova_data <- function(seed = 1, n = 25) {
  set.seed(seed)
  data.frame(
    group = factor(rep(c("Control", "A", "B"), each = n)),
    x = c(stats::rnorm(n, 0, 1.2), stats::rnorm(n, 2.2, 1.3), stats::rnorm(n, 2.7, 1.0)),
    z = c(stats::rnorm(n, 0, 0.8), stats::rnorm(n, 1.5, 1.0), stats::rnorm(n, 2.7, 1.2))
  )
}

# Pulls the p-value for one axis out of the on-plot annotation text produced
# for a given (pair, method), the same text the plotly widget shows the user.
# Numbers are formatted with sprintf("%.4g", ...) inside the package, so we
# compare against the reference value put through the same formatting.
extract_axis_p <- function(txt, axis_name) {
  pat <- paste0("p \\(eje ", axis_name, "\\) = ([0-9.eE+-]+)")
  m <- regmatches(txt, regexec(pat, txt))[[1]]
  if (length(m) < 2) return(NA_real_)
  as.numeric(m[2])
}
fmt <- function(p) as.numeric(sprintf("%.4g", p))

test_that("manova_plot3d() requires more than 2 groups", {
  df2 <- make_manova_data(); df2 <- df2[df2$group != "B", ]
  expect_error(manova_plot3d(df2, group = "group"), "mas de 2 grupos")
})

test_that("manova_plot3d() validates the 'control' argument against the observed group levels", {
  df <- make_manova_data()
  expect_error(manova_plot3d(df, control = "Nope"), "control")
})

test_that("manova_plot3d() defaults 'control' to the first level and messages about it", {
  df <- make_manova_data()
  expect_message(fig <- manova_plot3d(df), "No se especifico 'control'")
  expect_equal(attr(fig, "stats")$control, levels(df$group)[1])
})

test_that("manova_plot3d() returns a plotly widget with a 'stats' attribute of the documented shape", {
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  expect_s3_class(fig, "plotly")

  st <- attr(fig, "stats")
  expect_named(st, c("manova", "means", "covariances", "control", "has_dunnett", "hotelling_by_pair"))
  expect_true(st$manova$p.value >= 0 && st$manova$p.value <= 1)
  expect_length(st$means, 3)
  expect_length(st$hotelling_by_pair, choose(3, 2))
  expect_equal(st$has_dunnett, requireNamespace("multcomp", quietly = TRUE))
})

test_that("manova_plot3d()'s omnibus Wilks test matches stats::manova() run directly on the same data", {
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  expected <- summary(stats::manova(cbind(x, z) ~ group, data = df), test = "Wilks")$stats
  expect_equal(attr(fig, "stats")$manova$p.value, expected[1, "Pr(>F)"])
  expect_equal(attr(fig, "stats")$manova$F, unname(expected[1, "approx F"]))
})

test_that("manova_plot3d()'s pairwise Hotelling T2 matches the internal helper run directly on the same pair", {
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  st <- attr(fig, "stats")

  idxA <- df$group == "A"; idxB <- df$group == "B"
  mA <- c(mean(df$x[idxA]), mean(df$z[idxA])); mB <- c(mean(df$x[idxB]), mean(df$z[idxB]))
  SA <- stats::cov(cbind(df$x[idxA], df$z[idxA])); SB <- stats::cov(cbind(df$x[idxB], df$z[idxB]))
  expected <- visual.kaito:::.hotelling_t2(mA, mB, SA, SB, sum(idxA), sum(idxB))

  # groups are alphabetical (A, B, Control) -> pair (0,1) = "A vs B"
  expect_equal(st$hotelling_by_pair[["0-1"]], expected$p.value)
})

test_that("manova_plot3d()'s Bonferroni/Tukey/DMS post-hoc p-values match the real stats:: routines for a known pair", {
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  data <- fig$jsHooks$render[[1]]$data

  # pair (0,1) = "A vs B" (alphabetical group order: A, B, Control)
  aov_x <- stats::aov(x ~ group, data = df)
  tuk_x <- stats::TukeyHSD(aov_x)[[1]]
  bonf_x <- stats::pairwise.t.test(df$x, df$group, p.adjust.method = "bonferroni")$p.value
  dms_x <- stats::pairwise.t.test(df$x, df$group, p.adjust.method = "none")$p.value

  txt_tukey <- data$post_text[["0-1|tukey"]]
  txt_bonf <- data$post_text[["0-1|bonferroni"]]
  txt_dms <- data$post_text[["0-1|dms"]]

  expect_equal(extract_axis_p(txt_tukey, "x"), fmt(unname(tuk_x["B-A", "p adj"])))
  expect_equal(extract_axis_p(txt_bonf, "x"), fmt(unname(bonf_x["B", "A"])))
  expect_equal(extract_axis_p(txt_dms, "x"), fmt(unname(dms_x["B", "A"])))
})

test_that(".pw_extract() finds the right cell regardless of which side of the matrix it falls on", {
  mat <- matrix(c(0.01, NA, 0.02, 0.03), nrow = 2,
                 dimnames = list(c("B", "Control"), c("A", "B")))
  # (A, B) lives at mat["B", "A"]
  expect_equal(visual.kaito:::.pw_extract(mat, 1, 2, c("A", "B", "Control")), 0.01)
  # (B, Control) lives at mat["Control", "B"]
  expect_equal(visual.kaito:::.pw_extract(mat, 2, 3, c("A", "B", "Control")), 0.03)
})

test_that("manova_plot3d()'s Dunnett post-hoc matches multcomp::glht() for a pair that includes the control group", {
  skip_if_not_installed("multcomp")
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  data <- fig$jsHooks$render[[1]]$data

  g_rel <- stats::relevel(df$group, ref = "Control")
  dun_x <- summary(multcomp::glht(stats::aov(x ~ g_rel, data = df),
                                   linfct = multcomp::mcp(g_rel = "Dunnett")))
  expected_p <- as.numeric(dun_x$test$pvalues)[names(dun_x$test$coefficients) == "A - Control"]

  # pair (0,2) = "A vs Control"
  txt <- data$post_text[["0-2|dunnett"]]
  expect_equal(extract_axis_p(txt, "x"), fmt(expected_p))
})

test_that("manova_plot3d()'s Dunnett method shows an explanatory note (not a fabricated p-value) for a pair without the control group", {
  skip_if_not_installed("multcomp")
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  data <- fig$jsHooks$render[[1]]$data

  # pair (0,1) = "A vs B" does not include "Control"
  txt <- data$post_text[["0-1|dunnett"]]
  expect_match(txt, "grupo de referencia", ignore.case = TRUE)
  expect_false(grepl("p \\(eje x\\)", txt))
})

test_that("manova_plot3d()'s initial dropdown 'active' indices match the actually-plotted default state (regression test)", {
  # Regression test: a previous version left `active` unset on the
  # 'comparar'/'metodo' updatemenus, so the dropdowns displayed the first
  # button's label (e.g. "Comparar: A vs B") even though the plot itself
  # was already showing a different pair/method by default.
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  um <- plotly::plotly_build(fig)$x$layout$updatemenus

  methods4 <- c("bonferroni", "tukey", "dms", "dunnett")
  expect_equal(um[[2]]$active, match("tukey", methods4) - 1L)
  expect_equal(um[[3]]$active, 0L) # "Datos brutos"

  # default_pair = sort(c(control_idx-1, first non-control)) = (0,2) = "A vs Control"
  pairs0 <- utils::combn(0:2, 2, simplify = FALSE)
  expected_active <- which(vapply(pairs0, function(p) identical(p, c(0L, 2L)), logical(1))) - 1L
  expect_equal(um[[1]]$active, expected_active)
})

test_that("manova_plot3d()'s initial annotation text is complete, not truncated (regression test)", {
  # Regression test: an earlier version built the initial annotation's
  # lookup key from an un-sorted default_pair, which didn't match any key
  # in post_text (always sorted ascending), silently truncating the
  # annotation to just the header block.
  df <- make_manova_data()
  fig <- manova_plot3d(df, control = "Control")
  ann <- plotly::plotly_build(fig)$x$layout$annotations[[1]]$text
  expect_match(ann, "Comparando:")
  expect_match(ann, "Hotelling T2")
  expect_match(ann, "MANOVA omnibus")
})
