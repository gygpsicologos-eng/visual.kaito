#' Significance testing for a 3-axis (triaxial) box plot
#'
#' Compares groups on three continuous axes at once, both per-axis and
#' jointly (3D), using a parametric test and a non-parametric test in
#' parallel (not mutually exclusive -- set both `parametric` and
#' `nonparametric` to 1 to get both, as is the default).
#'
#' Parametric: Welch t-test (2 groups) / Welch ANOVA (>2 groups) per axis;
#' MANOVA (Pillai's trace) for the joint 3D comparison.
#'
#' Non-parametric: Wilcoxon rank-sum test (2 groups) / Kruskal-Wallis
#' (>2 groups) per axis; a permutation-based PERMANOVA (one-way, Euclidean
#' sum-of-squares decomposition, computed without depending on 'vegan')
#' for the joint 3D comparison.
#'
#' Rows with a missing value (`NA`) on `x`, `y`, or `z` are excluded
#' listwise before any test is run, so the per-axis tests and both joint
#' (3D) tests are computed on the same, identical sample.
#'
#' @param data A data frame containing the three axes and the grouping
#'   variable.
#' @param x,y,z Character. Column names of the three continuous axes.
#' @param group Character. Column name of the grouping variable.
#' @param parametric,nonparametric 0/1. Which family (or both) of tests to
#'   run. At least one must be 1.
#' @param scale 0/1. If 1, standardize (z-score) `x`, `y`, `z` before
#'   testing, so axes on different units/scales contribute comparably to
#'   the joint (3D) test. Per-axis and MANOVA results are invariant to
#'   this; PERMANOVA is not, since it is based on Euclidean distance.
#' @param nperm Integer. Number of permutations for the PERMANOVA test.
#' @param seed Integer. Random seed for the permutations (reproducibility).
#'
#' @return A data frame with one row per axis plus one row for the joint
#'   3D comparison, with the test names, p-values, and significance stars.
#'
#' @examples
#' set.seed(1)
#' df <- rbind(
#'   data.frame(x = rnorm(30, 0), y = rnorm(30, 0), z = rnorm(30, 0), group = "A"),
#'   data.frame(x = rnorm(30, 1), y = rnorm(30, 1), z = rnorm(30, 1), group = "B")
#' )
#' boxplot3d_significance(df, nperm = 199)
#'
#' @export
boxplot3d_significance <- function(data, x = "x", y = "y", z = "z", group = "group",
                                    parametric = 1, nonparametric = 1, scale = 0,
                                    nperm = 999, seed = 1) {
  stopifnot(parametric %in% c(0, 1), nonparametric %in% c(0, 1), scale %in% c(0, 1))
  if (parametric == 0 && nonparametric == 0) {
    stop("Activa al menos uno de parametric o nonparametric (=1).")
  }

  g <- factor(data[[group]])
  k <- nlevels(g)
  if (k < 2) stop("Se necesitan al menos 2 grupos.")

  axes <- list(X = .as_plain_numeric(data[[x]]), Y = .as_plain_numeric(data[[y]]), Z = .as_plain_numeric(data[[z]]))

  # Listwise-delete rows with NA on any of the three axes, so per-axis tests,
  # MANOVA, and PERMANOVA are all computed on the same, consistent sample
  # (stats::manova() does this automatically via its formula interface; the
  # permutation PERMANOVA below does not handle NA on its own, so without
  # this step it silently returns NA whenever the data contain missingness).
  cc <- stats::complete.cases(axes$X, axes$Y, axes$Z)
  if (!all(cc)) {
    axes <- lapply(axes, function(v) v[cc])
    g <- droplevels(g[cc])
    k <- nlevels(g)
    if (k < 2) stop("Tras excluir casos con NA en x/y/z, quedan menos de 2 grupos.")
  }
  if (scale == 1) axes <- lapply(axes, function(v) as.numeric(scale(v)))

  per_axis <- lapply(names(axes), function(nm) {
    v <- axes[[nm]]
    row <- list(comparacion = paste("Eje", nm))
    if (parametric == 1) {
      if (k == 2) {
        pt <- stats::t.test(v ~ g)
        row$parametrico <- "Welch t-test"
        row$p_parametrico <- unname(pt$p.value)
      } else {
        pt <- stats::oneway.test(v ~ g)
        row$parametrico <- "ANOVA (Welch)"
        row$p_parametrico <- unname(pt$p.value)
      }
    }
    if (nonparametric == 1) {
      if (k == 2) {
        wt <- stats::wilcox.test(v ~ g)
        row$no_parametrico <- "Wilcoxon"
        row$p_no_parametrico <- unname(wt$p.value)
      } else {
        kt <- stats::kruskal.test(v ~ g)
        row$no_parametrico <- "Kruskal-Wallis"
        row$p_no_parametrico <- unname(kt$p.value)
      }
    }
    row
  })

  M <- cbind(axes$X, axes$Y, axes$Z)
  joint <- list(comparacion = "Conjunto (3D)")
  if (parametric == 1) {
    fit <- stats::manova(M ~ g)
    sm <- summary(fit, test = "Pillai")
    joint$parametrico <- "MANOVA (traza de Pillai)"
    joint$p_parametrico <- unname(sm$stats[1, "Pr(>F)"])
  }
  if (nonparametric == 1) {
    joint$no_parametrico <- "PERMANOVA (permutaciones)"
    joint$p_no_parametrico <- permanova_1way(M, g, nperm = nperm, seed = seed)
  }

  out <- do.call(rbind, lapply(c(per_axis, list(joint)), as.data.frame))

  p_to_stars <- function(p) {
    ifelse(p < 0.001, "***",
    ifelse(p < 0.01,  "**",
    ifelse(p < 0.05,  "*",
    ifelse(p < 0.1,   ".", ""))))
  }
  pcol <- if (nonparametric == 1) "p_no_parametrico" else "p_parametrico"
  out$sig <- p_to_stars(out[[pcol]])
  rownames(out) <- NULL
  out
}

#' One-way PERMANOVA via permutation (internal, no 'vegan' dependency)
#'
#' @param M Numeric matrix (n x 3), the three axes.
#' @param g Factor, the grouping variable.
#' @param nperm Integer, number of permutations.
#' @param seed Integer, random seed.
#' @return Numeric p-value.
#' @keywords internal
#' @noRd
permanova_1way <- function(M, g, nperm = 999, seed = 1) {
  set.seed(seed)
  n <- nrow(M); k <- nlevels(g)

  ss_between <- function(gg) {
    grand <- colMeans(M)
    ss_total <- sum(scale(M, center = grand, scale = FALSE)^2)
    ss_within <- sum(vapply(split(seq_len(n), gg), function(idx) {
      sub <- M[idx, , drop = FALSE]
      sum(scale(sub, center = colMeans(sub), scale = FALSE)^2)
    }, numeric(1)))
    list(between = ss_total - ss_within, within = ss_within)
  }

  obs <- ss_between(g)
  f_obs <- (obs$between / (k - 1)) / (obs$within / (n - k))

  f_perm <- replicate(nperm, {
    ssb <- ss_between(sample(g))
    (ssb$between / (k - 1)) / (ssb$within / (n - k))
  })

  (1 + sum(f_perm >= f_obs)) / (1 + nperm)
}

