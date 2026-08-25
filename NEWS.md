# visual.kaito 0.1.0

First public release.

## New functions

* `boxplot3d()` and `boxplot3d_interactive()`: triaxial (three continuous
  variables) box plots comparing groups, with four whisker conventions
  (Tukey, fixed-percentile, mean +/- SD, letter-value). The `_interactive()`
  version draws a single rotatable 3D `plotly` scene with live controls
  (whisker convention, raw/standardized scale, 2D axis-pair panels,
  color-by any categorical column); `boxplot3d()` draws the same four
  conventions as three linked 2D projections using base graphics only.
* `boxplot3d_significance()`: per-axis and joint (3D) significance testing
  to accompany the box plots above, combining a parametric test (Welch
  t-test/ANOVA per axis, MANOVA for the joint test) and a non-parametric
  test (Wilcoxon/Kruskal-Wallis per axis, a dependency-free permutation
  PERMANOVA for the joint test) in parallel.
* `ttest_plot()`: a didactic 2D plot of the t-distribution density with the
  critical (rejection) region shaded and the observed t-statistic marked,
  from raw data or from a manual `t`/`df` pair.
* `ttest_plot3d()`: two-group bivariate density comparison on two
  continuous variables, drawn as overlapping 3D density surfaces, with
  per-axis t-tests and a joint Hotelling's T-squared test.
* `manova_plot3d()`: the >2-group generalization of `ttest_plot3d()`, with
  a one-way MANOVA omnibus test (Wilks' lambda) and Bonferroni, Tukey,
  Fisher's LSD, and Dunnett post-hoc comparisons.
* `chisq_plot3d()`: association between two categorical variables as a 3D
  bar chart of standardized (Pearson) residuals, with Cochran's-rule and
  Simpson's-paradox (descriptive + formal Gail-Simon) diagnostics.
* `paired_plot3d()`: repeated-measures/pre-post comparison on two
  continuous variables at once, with paired t-tests per variable and a
  joint paired Hotelling's T-squared test.
* `irt_plot3d()`: Item Response Theory item calibration map (via `mirt`)
  drawing every item's characteristic curve(s) in 3D against a shared
  ability (theta) axis, with a linked ability/difficulty histogram panel.

## Shared interactive features

* Every `plotly`-based 3D function ships live, pre-computed controls
  (opacity, view/projection buttons, and function-specific selectors) that
  recompute instantly in the browser without calling R again.
* `chisq_plot3d()` and `irt_plot3d()` additionally offer a "3D / 2D"
  orthographic view toggle and a `lang = c("es", "en")` argument
  translating every on-plot label, hover string, and message.

## Documentation

* An introductory vignette (`vignette("visual-kaito")`) covers all nine
  functions with runnable examples and a comparison to related packages
  (`ggstatsplot`, `ggpubr`, `rstatix`, `effectsize`, `superb`).
* A citation entry (`citation("visual.kaito")`) via `CITATION.cff` and
  `inst/CITATION`, including a Zenodo DOI.
