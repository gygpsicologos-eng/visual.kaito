#' Draw more-than-2 groups as bivariate 3D "mountains" with pairwise post-hoc tests
#'
#' The many-groups counterpart to [ttest_plot3d()]. Each group (there must be
#' more than 2) is drawn as its own bivariate density surface ("mountain")
#' over the (`x`, `z`) plane, all in the same 3D scene. An overall one-way
#' MANOVA (Wilks' lambda) tests whether the groups differ jointly on both
#' variables; a "Comparar" menu picks which pair of groups to highlight (their
#' overlap is shaded, same convention as [ttest_plot3d()]), and a "Metodo"
#' menu reports that pair's post-hoc result under Bonferroni, Tukey (HSD),
#' DMS (Fisher's LSD) or Dunnett.
#'
#' Tukey and Dunnett do not have a standard closed-form multivariate version,
#' so the 4 post-hoc methods are computed axis-by-axis with the real
#' underlying R routines ([stats::TukeyHSD()], [stats::pairwise.t.test()]
#' with Bonferroni or no correction, and Dunnett via the `multcomp` package)
#' -- the same per-axis convention [ttest_plot3d()] already uses for its two
#' t-tests. Each pair's unadjusted, 2-variable Hotelling's T-squared (as in
#' [ttest_plot3d()]) is also reported for every method, as a joint summary
#' of that one pair.
#'
#' Dunnett compares every group only against a fixed reference/`control`
#' group, not all pairs against each other -- selecting a pair that does not
#' include `control` while "Metodo" is set to Dunnett shows a note instead of
#' a p-value for that pair.
#'
#' @param data A data frame with the two continuous axes and the grouping
#'   variable.
#' @param x,z Character. Column names of the two continuous axes.
#' @param group Character. Column name of the grouping variable. Must have
#'   more than 2 levels (with exactly 2, use [ttest_plot3d()] instead).
#' @param control Character. The reference group for Dunnett's test. If
#'   `NULL` (default), the first level of `group` is used and a message
#'   reports which one that is.
#' @param conf.level Confidence level (default 0.95).
#' @param n_grid Integer. Grid resolution per axis for the density surfaces
#'   (default 50).
#' @param opacity Initial opacity (0-1) of the group surfaces (default 0.5);
#'   adjustable afterwards with the transparency slider.
#' @param col Character vector of colors, one per group. Defaults to the
#'   Okabe-Ito palette.
#' @param col_overlap Color used to mark the overlap of the currently
#'   compared pair.
#'
#' @return A `plotly` htmlwidget object (auto-displayed if the result isn't
#'   assigned). Full statistics -- `manova` (Wilks' lambda test), `means`,
#'   `covariances`, and `posthoc` (a data frame with one row per pair per
#'   method: per-axis p-values, the pair's Hotelling T2 p-value, and whether
#'   that method applies to that pair) -- are attached as the `"stats"`
#'   attribute.
#'
#' @examples
#' set.seed(1)
#' df <- rbind(
#'   data.frame(x = rnorm(25, 0, 1.2), z = rnorm(25, 0, 1.1), group = "Control"),
#'   data.frame(x = rnorm(25, 2, 1.3), z = rnorm(25, 1.4, 1.2), group = "A"),
#'   data.frame(x = rnorm(25, 2.5, 1.1), z = rnorm(25, 3.2, 1.3), group = "B")
#' )
#' manova_plot3d(df, control = "Control")
#'
#' @export
manova_plot3d <- function(data, x = "x", z = "z", group = "group",
                           control = NULL, conf.level = 0.95,
                           n_grid = 50, opacity = 0.5,
                           col = NULL, col_overlap = "#4a3aa7") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para manova_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para manova_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  stopifnot(conf.level > 0, conf.level < 1, opacity > 0, opacity <= 1)

  xv <- .as_plain_numeric(data[[x]])
  zv <- .as_plain_numeric(data[[z]])
  g <- factor(data[[group]])
  groups <- levels(g)
  G <- length(groups)
  if (G < 3) {
    stop("manova_plot3d() esta pensado para mas de 2 grupos; con exactamente ",
         "2 grupos usa ttest_plot3d().")
  }
  if (is.null(control)) {
    control <- groups[1]
    message("No se especifico 'control'; se usa '", control,
            "' (primer nivel de '", group, "') como grupo de referencia para Dunnett.")
  } else if (!control %in% groups) {
    stop("'control' debe ser uno de los niveles de '", group, "': ",
         paste(groups, collapse = ", "))
  }
  control_idx <- match(control, groups)

  if (is.null(col)) col <- as.character(grDevices::palette.colors(G, palette = "Okabe-Ito"))
  if (length(col) < G) stop("'col' debe tener al menos ", G, " colores (uno por grupo).")

  idx <- lapply(groups, function(gr) g == gr)
  n <- vapply(idx, sum, integer(1))
  mean_x <- vapply(idx, function(i) mean(xv[i]), numeric(1))
  sd_x <- vapply(idx, function(i) stats::sd(xv[i]), numeric(1))
  mean_z <- vapply(idx, function(i) mean(zv[i]), numeric(1))
  sd_z <- vapply(idx, function(i) stats::sd(zv[i]), numeric(1))
  means <- lapply(idx, function(i) c(mean(xv[i]), mean(zv[i])))
  covs <- lapply(idx, function(i) stats::cov(cbind(xv[i], zv[i])))

  # --- Omnibus one-way MANOVA (Wilks' lambda) ---
  man_fit <- stats::manova(cbind(xv, zv) ~ g)
  man_sm <- summary(man_fit, test = "Wilks")$stats
  man_p <- man_sm[1, "Pr(>F)"]
  omnibus_line <- sprintf(
    "MANOVA omnibus (Wilks): F(%.0f,%.0f) = %.3f, p = %.4g %s",
    man_sm[1, "num Df"], man_sm[1, "den Df"], man_sm[1, "approx F"],
    man_p, .stars3(man_p)
  )

  # --- Per-axis post-hoc via the real R routines ---
  aov_x <- stats::aov(xv ~ g); aov_z <- stats::aov(zv ~ g)
  tuk_x <- stats::TukeyHSD(aov_x)[[1]]; tuk_z <- stats::TukeyHSD(aov_z)[[1]]
  bonf_x <- stats::pairwise.t.test(xv, g, p.adjust.method = "bonferroni")$p.value
  bonf_z <- stats::pairwise.t.test(zv, g, p.adjust.method = "bonferroni")$p.value
  dms_x <- stats::pairwise.t.test(xv, g, p.adjust.method = "none")$p.value
  dms_z <- stats::pairwise.t.test(zv, g, p.adjust.method = "none")$p.value

  has_dunnett <- requireNamespace("multcomp", quietly = TRUE)
  if (has_dunnett) {
    g_rel <- stats::relevel(g, ref = control)
    dun_x <- summary(multcomp::glht(stats::aov(xv ~ g_rel),
                                     linfct = multcomp::mcp(g_rel = "Dunnett")))
    dun_z <- summary(multcomp::glht(stats::aov(zv ~ g_rel),
                                     linfct = multcomp::mcp(g_rel = "Dunnett")))
    dun_px <- stats::setNames(as.numeric(dun_x$test$pvalues), names(dun_x$test$coefficients))
    dun_pz <- stats::setNames(as.numeric(dun_z$test$pvalues), names(dun_z$test$coefficients))
  } else {
    dun_px <- dun_pz <- stats::setNames(numeric(0), character(0))
  }

  pairs0 <- utils::combn(0:(G - 1), 2, simplify = FALSE)  # 0-indexed, for JS
  methods4 <- c("bonferroni", "tukey", "dms", "dunnett")
  method_labels <- c(bonferroni = "Bonferroni", tukey = "Tukey (HSD)",
                      dms = "DMS (Fisher LSD)", dunnett = "Dunnett")

  pair_key <- function(p) paste(p, collapse = "-")
  post_text <- list()
  hot_p_by_pair <- list()

  for (p in pairs0) {
    li <- p[1] + 1; lj <- p[2] + 1  # back to 1-indexed for R lookups
    hp <- .hotelling_t2(means[[li]], means[[lj]], covs[[li]], covs[[lj]], n[li], n[lj])$p.value
    hot_p_by_pair[[pair_key(p)]] <- hp
    hot_line <- sprintf("Hotelling T2 (par, sin ajustar): p = %.4g %s", hp, .stars3(hp))
    header <- sprintf("Comparando: %s vs %s", groups[li], groups[lj])

    for (m in methods4) {
      px <- pz <- NA_real_; applicable <- TRUE; note <- NULL
      if (m == "bonferroni") {
        px <- .pw_extract(bonf_x, li, lj, groups); pz <- .pw_extract(bonf_z, li, lj, groups)
      } else if (m == "tukey") {
        px <- .tukey_extract(tuk_x, li, lj, groups); pz <- .tukey_extract(tuk_z, li, lj, groups)
      } else if (m == "dms") {
        px <- .pw_extract(dms_x, li, lj, groups); pz <- .pw_extract(dms_z, li, lj, groups)
      } else if (m == "dunnett") {
        if (!has_dunnett) {
          applicable <- FALSE
          note <- "Dunnett no disponible: instala el paquete 'multcomp' (install.packages('multcomp'))."
        } else if (groups[li] != control && groups[lj] != control) {
          applicable <- FALSE
          note <- sprintf("Dunnett solo compara cada grupo frente al grupo de referencia ('%s'). Elige un par que lo incluya.", control)
        } else {
          other <- if (groups[li] == control) groups[lj] else groups[li]
          px <- .dunnett_extract(dun_px, other, control)
          pz <- .dunnett_extract(dun_pz, other, control)
        }
      }
      txt <- if (applicable) {
        sprintf("%s    Metodo: %s\np (eje %s) = %.4g %s      p (eje %s) = %.4g %s\n%s",
                header, method_labels[[m]], x, px, .stars3(px), z, pz, .stars3(pz), hot_line)
      } else {
        sprintf("%s    Metodo: %s\n%s\n%s", header, method_labels[[m]], note, hot_line)
      }
      post_text[[paste(pair_key(p), m, sep = "|")]] <- gsub("\n", "<br>", txt)
    }
  }

  desc_block <- paste(vapply(seq_len(G), function(i) sprintf(
    "Grupo %s (n=%d): %s = %.3g +/- %.3g; %s = %.3g +/- %.3g",
    groups[i], n[i], x, mean_x[i], sd_x[i], z, mean_z[i], sd_z[i]
  ), character(1)), collapse = "<br>")
  header_block <- paste(omnibus_line, desc_block, sep = "<br>")

  # --- Density grids (one shared grid so pairwise overlaps line up) ---
  xv_z <- as.numeric(scale(xv)); zv_z <- as.numeric(scale(zv))
  means_z <- lapply(idx, function(i) c(mean(xv_z[i]), mean(zv_z[i])))
  covs_z <- lapply(idx, function(i) stats::cov(cbind(xv_z[i], zv_z[i])))

  build_grid <- function(av, bv) {
    ra <- range(av); rb <- range(bv)
    pad_a <- diff(ra) * 0.3; pad_b <- diff(rb) * 0.3
    list(as_ = seq(ra[1] - pad_a, ra[2] + pad_a, length.out = n_grid),
         bs_ = seq(rb[1] - pad_b, rb[2] + pad_b, length.out = n_grid))
  }
  grid_raw <- build_grid(xv, zv)
  D_raw <- lapply(seq_len(G), function(i) outer(grid_raw$as_, grid_raw$bs_,
    function(p, q) .dmvnorm2(p, q, means[[i]], covs[[i]])))
  grid_zsc <- build_grid(xv_z, zv_z)
  D_zsc <- lapply(seq_len(G), function(i) outer(grid_zsc$as_, grid_zsc$bs_,
    function(p, q) .dmvnorm2(p, q, means_z[[i]], covs_z[[i]])))

  mask_overlap <- function(ov) { ov[ov < max(ov) * 0.02] <- NA; ov }
  default_pair <- sort(c(control_idx - 1L, setdiff(0:(G - 1), control_idx - 1L)[1]))  # 0-indexed, ascending to match pairs0 keys
  ov0 <- mask_overlap(pmin(D_raw[[default_pair[1] + 1]], D_raw[[default_pair[2] + 1]]))
  pair_active0 <- which(vapply(pairs0, function(p) identical(p, default_pair), logical(1))) - 1L
  method_active0 <- match("tukey", methods4) - 1L

  flat_scale <- function(colr) list(list(0, colr), list(1, colr))

  fig <- plotly::plot_ly()
  for (i in seq_len(G)) {
    fig <- plotly::add_trace(fig, x = grid_raw$as_, y = grid_raw$bs_, z = t(D_raw[[i]]),
      type = "surface", colorscale = flat_scale(col[i]), showscale = FALSE, opacity = opacity,
      contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
      name = groups[i], legendgroup = paste0("g", i), showlegend = TRUE)
  }
  fig <- plotly::add_trace(fig, x = grid_raw$as_, y = grid_raw$bs_, z = t(ov0), type = "surface",
    colorscale = flat_scale(col_overlap), showscale = FALSE, opacity = 0.95,
    contours = list(x = list(show = FALSE), y = list(show = FALSE), z = list(show = FALSE)),
    name = "Solapamiento", legendgroup = "ov", showlegend = TRUE)

  op_vals <- c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
  op_active <- which.min(abs(op_vals - opacity)) - 1L
  group_trace_idx <- as.list(0:(G - 1))

  onrender_data <- list(
    group_names = groups, n_groups = G,
    pairs = lapply(pairs0, function(p) as.list(p)),
    raw = list(as_ = grid_raw$as_, bs_ = grid_raw$bs_, D = lapply(D_raw, function(m) t(m))),
    zsc = list(as_ = grid_zsc$as_, bs_ = grid_zsc$bs_, D = lapply(D_zsc, function(m) t(m))),
    post_text = post_text, header_block = header_block,
    x_name = x, z_name = z,
    default_pair = as.list(default_pair), default_method = "tukey", default_scale = "raw",
    overlap_trace = G
  )

  fig <- plotly::layout(fig,
    title = list(text = paste0("MANOVA 3D: ", paste(groups, collapse = " / "))),
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = x)),
      yaxis = list(title = list(text = z)),
      zaxis = list(title = list(text = "Densidad")),
      camera = list(eye = list(x = 1.5, y = -1.5, z = 0.9))
    ),
    annotations = list(list(
      text = paste(header_block, post_text[[paste(paste(default_pair, collapse = "-"), "tukey", sep = "|")]], sep = "<br>"),
      xref = "paper", yref = "paper", x = 0.99, y = 0.9, xanchor = "right", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 11.5, color = "#0b0b0b")
    )),
    updatemenus = list(
      list(
        name = "comparar", type = "dropdown", direction = "down", showactive = TRUE,
        active = pair_active0,
        x = 0, y = 1.15, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = lapply(pairs0, function(p) list(
          method = "skip",
          label = paste0("Comparar: ", groups[p[1] + 1], " vs ", groups[p[2] + 1])
        ))
      ),
      list(
        name = "metodo", type = "dropdown", direction = "down", showactive = TRUE,
        active = method_active0,
        x = 0, y = 1.115, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = lapply(methods4, function(m) list(
          method = "skip", label = paste0("Metodo: ", method_labels[[m]])
        ))
      ),
      list(
        name = "escala", type = "buttons", direction = "right", showactive = TRUE,
        active = 0L,
        x = 0, y = 1.08, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "skip", label = "Datos brutos"),
          list(method = "skip", label = "Datos estandarizados")
        )
      )
    ),
    sliders = list(list(
      active = op_active,
      currentvalue = list(prefix = "Transparencia: "),
      x = 0, len = 0.32, xanchor = "left", y = -0.02, yanchor = "top",
      pad = list(t = 10),
      steps = lapply(op_vals, function(v) list(
        method = "restyle", label = sprintf("%d%%", round(v * 100)),
        args = list(list(opacity = v), group_trace_idx)
      ))
    ))
  )

  js <- "
  function(el, x, data) {
    var gd = el;
    var state = {pair: data.default_pair, method: data.default_method, scale: data.default_scale};

    function pairKey(p) { return p[0] + '-' + p[1]; }
    function findPair(p0, p1) {
      for (var i = 0; i < data.pairs.length; i++) {
        var p = data.pairs[i];
        if ((p[0] === p0 && p[1] === p1) || (p[0] === p1 && p[1] === p0)) return p;
      }
      return data.pairs[0];
    }

    function applyState() {
      var g = data[state.scale];
      var xArrs = [], yArrs = [], zArrs = [];
      for (var i = 0; i < data.n_groups; i++) {
        xArrs.push(g.as_); yArrs.push(g.bs_); zArrs.push(g.D[i]);
      }
      var d0 = g.D[state.pair[0]], d1 = g.D[state.pair[1]];
      var ov = [];
      var maxv = -Infinity;
      for (var r = 0; r < d0.length; r++) {
        var row = [];
        for (var c = 0; c < d0[r].length; c++) {
          var m = Math.min(d0[r][c], d1[r][c]);
          row.push(m);
          if (m > maxv) maxv = m;
        }
        ov.push(row);
      }
      var thr = maxv * 0.02;
      for (var r2 = 0; r2 < ov.length; r2++) {
        for (var c2 = 0; c2 < ov[r2].length; c2++) {
          if (ov[r2][c2] < thr) ov[r2][c2] = null;
        }
      }
      xArrs.push(g.as_); yArrs.push(g.bs_); zArrs.push(ov);

      var xt = data.x_name, zt = data.z_name;
      if (state.scale === 'zsc') { xt += ' (z-score)'; zt += ' (z-score)'; }

      var key = pairKey(state.pair) + '|' + state.method;
      var txt = data.header_block + '<br>' + data.post_text[key];

      Plotly.update(gd, {x: xArrs, y: yArrs, z: zArrs},
        {'scene.xaxis.title.text': xt, 'scene.yaxis.title.text': zt, 'annotations[0].text': txt});
    }

    gd.on('plotly_buttonclicked', function(ev) {
      var name = ev.menu.name;
      if (name === 'comparar') {
        var p = data.pairs[ev.active];
        state.pair = findPair(p[0], p[1]);
      } else if (name === 'metodo') {
        var methods = ['bonferroni', 'tukey', 'dms', 'dunnett'];
        state.method = methods[ev.active];
      } else if (name === 'escala') {
        state.scale = ev.active === 0 ? 'raw' : 'zsc';
      } else {
        return;
      }
      applyState();
    });
  }
  "
  fig <- htmlwidgets::onRender(fig, js, data = onrender_data)

  attr(fig, "stats") <- list(
    manova = list(wilks = man_sm[1, "Wilks"], F = man_sm[1, "approx F"],
                  df1 = man_sm[1, "num Df"], df2 = man_sm[1, "den Df"], p.value = man_p),
    means = stats::setNames(means, groups), covariances = stats::setNames(covs, groups),
    control = control, has_dunnett = has_dunnett,
    hotelling_by_pair = stats::setNames(hot_p_by_pair, names(hot_p_by_pair))
  )
  fig
}

#' @noRd
.stars3 <- function(p) if (is.na(p)) "" else if (p < 0.001) "***" else
  if (p < 0.01) "**" else if (p < 0.05) "*" else if (p < 0.1) "." else ""

#' @noRd
.pw_extract <- function(mat, li, lj, groups) {
  gi <- groups[li]; gj <- groups[lj]
  v <- NA_real_
  if (gi %in% rownames(mat) && gj %in% colnames(mat)) v <- mat[gi, gj]
  if (is.na(v) && gj %in% rownames(mat) && gi %in% colnames(mat)) v <- mat[gj, gi]
  unname(v)
}

#' @noRd
.tukey_extract <- function(tukmat, li, lj, groups) {
  gi <- groups[li]; gj <- groups[lj]
  nm1 <- paste0(gj, "-", gi); nm2 <- paste0(gi, "-", gj)
  if (nm1 %in% rownames(tukmat)) return(unname(tukmat[nm1, "p adj"]))
  if (nm2 %in% rownames(tukmat)) return(unname(tukmat[nm2, "p adj"]))
  NA_real_
}

#' @noRd
.dunnett_extract <- function(pnamed, other, control) {
  if (length(pnamed) == 0) return(NA_real_)
  target <- paste(other, "-", control)
  hit <- which(names(pnamed) == target)
  if (length(hit) == 0) hit <- which(trimws(names(pnamed)) == trimws(target))
  if (length(hit) == 0) return(NA_real_)
  unname(pnamed[hit[1]])
}
