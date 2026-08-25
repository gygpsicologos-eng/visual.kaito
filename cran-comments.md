## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* local Windows 11 install, R 4.6.0
* GitHub Actions R CMD check workflow (see `.github/workflows/`)

## Downstream dependencies

This is a new submission; there are no downstream dependencies.

## Optional/Suggests packages

`mirt` and `multcomp` are used only inside `irt_plot3d()` and the Dunnett
post-hoc branch of `manova_plot3d()` respectively; both functions check
`requireNamespace()` and fail gracefully with an informative message when
these Suggests are not installed, so the package does not require them.
`plotly`, `htmlwidgets`, `knitr`, and `rmarkdown` are likewise Suggested
(all 3D plotting functions and the vignette need them, but the package can
be loaded without them).
