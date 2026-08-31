
test_that("ttest_plot lang argument is validated and translates labels", {
  expect_error(ttest_plot(t = 2.4, df = 28, lang = "fr"))
  res_es <- ttest_plot(t = 2.4, df = 28, alternative = "greater")
  res_en <- ttest_plot(t = 2.4, df = 28, alternative = "greater", lang = "en")
  expect_equal(res_es$t, res_en$t)
  expect_equal(res_es$p.value, res_en$p.value)
})

test_that("ttest_plot en mode does not error without x/t/df", {
  expect_error(ttest_plot(lang = "en"))
})

