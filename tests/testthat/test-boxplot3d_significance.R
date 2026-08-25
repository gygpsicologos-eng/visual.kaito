
test_that("boxplot3d_significance handles NA listwise across x, y, z", {
  set.seed(42)
  df <- rbind(
    data.frame(x = rnorm(30, 0), y = rnorm(30, 0), z = rnorm(30, 0), group = "A"),
    data.frame(x = rnorm(30, 1), y = rnorm(30, 1), z = rnorm(30, 1), group = "B")
  )
  df$x[c(1, 5)] <- NA
  df$z[10] <- NA

  res <- boxplot3d_significance(df, nperm = 199)

  expect_false(anyNA(res$p_parametrico))
  expect_false(anyNA(res$p_no_parametrico))

  cc <- stats::complete.cases(df$x, df$y, df$z)
  expect_equal(sum(cc), nrow(df) - 3)

  res_manual <- boxplot3d_significance(df[cc, ], nperm = 199)
  expect_equal(res$p_parametrico, res_manual$p_parametrico)
})

test_that("boxplot3d_significance errors when NA removal leaves under 2 groups", {
  df <- data.frame(
    x = c(1, 2, NA, 4), y = c(1, 2, 3, 4), z = c(1, 2, 3, 4),
    group = c("A", "A", "B", "A")
  )
  expect_error(boxplot3d_significance(df, nperm = 49), "menos de 2 grupos")
})

