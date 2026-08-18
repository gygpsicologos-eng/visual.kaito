#' Draw an interactive, rotatable 3-axis (triaxial) box plot
#'
#' Draws a single interactive scene (via \pkg{plotly}) that can show either
#' a rotatable 3D view with one translucent cuboid per group spanning the
#' box on all three axes at once, or a true flat 2D panel for any one pair
#' of axes (x-axis at the bottom, y-axis on the left -- an ordinary 2D
#' plot, not a 3D scene viewed from a fixed camera angle). Each 3D box is
#' drawn with crisp wireframe edges (flat-shaded faces plus explicit edge
#' lines) so its shape reads clearly instead of as a smooth blob. Each box
#' also shows whiskers along its axes, a cross-shaped median marker (a
#' black halo plus a smaller marker in the group's own color, so medians
#' stay distinguishable by group even when they land close together),
#' faint raw points, and outliers (when the method produces any) as open
#' diamonds. In 3D mode the scene rotates with click-drag, pans with
#' right-click or shift-drag, and zooms with the mouse wheel.
#'
#' This function precomputes all four whisker conventions (see
#' [boxplot3d()] for what tukey/percentile/sd/letter_value mean), all four
#' views (3D plus the three axis-pair 2D panels), and both the raw and
#' standardized (z-score) versions of the data, and lets you switch
#' between all of them live with the controls above the plot, without
#' re-calling the function:
#' \itemize{
#'   \item \strong{Vista}: 3D, or a flat 2D panel for axes 1-2, 2-3, or 1-3.
#'   \item \strong{Metodo}: which box/whisker convention to draw.
#'   \item \strong{Agrupar por}: which column colors the individual raw
#'     data points (subjects). This only recolors the points -- the boxes,
#'     whiskers and medians always reflect the `group` argument, since
#'     that is what defines the statistical groups being compared. By
#'     default this offers `group` itself plus any other factor/character
#'     column found in `data`.
#'   \item \strong{Escala}: raw values, or all three axes standardized
#'     (z-score) to a common scale.
#'   \item \strong{Cuadricula}: on/off for the background grid.
#' }
#'
#' @param data A data frame with the three axes and the grouping variable.
#' @param x,y,z Character. Column names of the three continuous axes.
#'   These names are used verbatim as the axis titles, so pass the real
#'   column names directly (e.g. `x = "Score_ISI"`) rather than renaming
#'   your data frame's columns to generic `"x"`/`"y"`/`"z"` first.
#' @param group Character. Column name of the grouping variable that
#'   defines the boxes being compared.
#' @param tukey,percentile,sd,letter_value 0/1 flags selecting which
#'   convention is shown initially (switchable live via "Metodo").
#'   Exactly one must be 1.
#' @param k Numeric. Tukey fence multiplier (default 1.5).
#' @param probs Length-2 numeric. Lower/upper whisker percentiles for the
#'   `percentile` convention (default `c(0.05, 0.95)`).
#' @param sd_mult Length-2 numeric. Box and whisker multipliers (in SDs)
#'   for the `sd` convention (default `c(1, 2)`).
#' @param scale 0/1. Which scale is shown initially: 0 = raw values,
#'   1 = standardized (z-score). Switchable live via "Escala".
#' @param grid3d 0/1. Initial state of the background grid (default 1 =
#'   on); also togglable live from the plot itself.
#' @param color_by Optional character vector of column names to offer in
#'   the "Agrupar por" dropdown (for recoloring individual points). If
#'   `NULL` (default), `group` is offered together with any other
#'   factor/character/logical column found in `data`.
#' @param col Optional character vector of colors, one per level of
#'   `group`.
#'
#' @return A `plotly` htmlwidget object.
#'
#' @examples
#' set.seed(1)
#' df <- rbind(
#'   data.frame(x = rnorm(30, 0), y = rnorm(30, 0), z = rnorm(30, 0), group = "A"),
#'   data.frame(x = rnorm(30, 1), y = rnorm(30, 1), z = rnorm(30, 1), group = "B")
#' )
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'   boxplot3d_interactive(df)
#' }
#'
#' @export
boxplot3d_interactive <- function(data, x = "x", y = "y", z = "z", group = "group",
                                   tukey = 1, percentile = 0, sd = 0, letter_value = 0,
                                   k = 1.5, probs = c(0.05, 0.95), sd_mult = c(1, 2),
                                   scale = 0, grid3d = 1, color_by = NULL, col = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para boxplot3d_interactive(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para boxplot3d_interactive(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  flags <- c(tukey = tukey, percentile = percentile, sd = sd, letter_value = letter_value)
  if (sum(flags == 1) != 1) {
    stop("Activa exactamente uno de tukey / percentile / sd / letter_value (=1), el resto a 0.")
  }
  default_method <- names(flags)[flags == 1]
  stopifnot(scale %in% c(0, 1), grid3d %in% c(0, 1))
  default_scale <- if (scale == 1) "zscore" else "raw"

  g <- factor(data[[group]]); groups <- levels(g)
  if (is.null(col)) col <- as.character(grDevices::palette.colors(max(3, length(groups)), palette = "Okabe-Ito"))

  ax_names <- c(x, y, z)
  axes_raw <- list(.as_plain_numeric(data[[x]]), .as_plain_numeric(data[[y]]), .as_plain_numeric(data[[z]]))
  M_by_scale <- list(
    raw = do.call(cbind, axes_raw),
    zscore = do.call(cbind, lapply(axes_raw, function(v) as.numeric(scale(v))))
  )
  ax_titles_by_scale <- list(raw = ax_names, zscore = paste0(ax_names, " (z-score)"))

  reserved <- c(x, y, z, group)
  if (is.null(color_by)) {
    cand <- setdiff(names(data), reserved)
    cand <- cand[vapply(data[cand], function(v) is.factor(v) || is.character(v) || is.logical(v), logical(1))]
    color_by <- c(group, cand)
  } else {
    color_by <- unique(c(group, intersect(color_by, names(data))))
  }

  modes <- c("3D", "12", "23", "13")
  mode_labels <- c(`3D` = "3D", `12` = "Eje 1-2", `23` = "Eje 2-3", `13` = "Eje 1-3")
  pair_axes <- list(`3D` = 1:3, `12` = c(1, 2), `23` = c(2, 3), `13` = c(1, 3))
  methods <- c("tukey", "percentile", "sd", "letter_value")
  method_labels <- c(tukey = "Tukey", percentile = "Percentil", sd = "Media +/- DE",
                      letter_value = "Letter-value")
  scales_ <- c("raw", "zscore")
  scale_labels <- c(raw = "Valores originales", zscore = "Estandarizado (z-score)")

  GRIDCOL <- "#c9c8c0"; BGCOL <- "#f4f3f0"

  # 12 edges of a box, as vertex-code pairs ("0"/"1" per axis: x,y,z)
  edge_pairs <- list(
    c("000", "100"), c("100", "110"), c("110", "010"), c("010", "000"),
    c("001", "101"), c("101", "111"), c("111", "011"), c("011", "001"),
    c("000", "001"), c("100", "101"), c("110", "111"), c("010", "011")
  )
  vertex_xyz <- function(code, lo, hi) {
    c(if (substr(code, 1, 1) == "0") lo[1] else hi[1],
      if (substr(code, 2, 2) == "0") lo[2] else hi[2],
      if (substr(code, 3, 3) == "0") lo[3] else hi[3])
  }
  cuboid_edges <- function(lo, hi) {
    ex <- numeric(0); ey <- numeric(0); ez <- numeric(0)
    for (p in edge_pairs) {
      a <- vertex_xyz(p[1], lo, hi); b <- vertex_xyz(p[2], lo, hi)
      ex <- c(ex, a[1], b[1], NA); ey <- c(ey, a[2], b[2], NA); ez <- c(ez, a[3], b[3], NA)
    }
    list(x = ex, y = ey, z = ez)
  }

  fig <- plotly::plot_ly()
  geom_mode <- character(0); geom_method <- character(0); geom_role <- character(0); geom_scale <- character(0)
  pt_mode <- character(0); pt_colorby <- character(0); pt_scale <- character(0)

  for (sc in scales_) {
    M <- M_by_scale[[sc]]
    for (md in modes) {
      ax_idx <- pair_axes[[md]]
      for (m in methods) {
        vis0 <- (md == "3D") && (m == default_method) && (sc == default_scale)
        for (i in seq_along(groups)) {
          gr <- groups[i]; idx <- which(g == gr)
          sub <- M[idx, , drop = FALSE]
          st <- lapply(1:3, function(j) .box_axis_stats(sub[, j], m, k, probs, sd_mult))
          lo <- vapply(st, `[[`, numeric(1), "q1"); hi <- vapply(st, `[[`, numeric(1), "q3")
          ctr <- vapply(st, `[[`, numeric(1), "center")
          wlo <- vapply(st, `[[`, numeric(1), "whisker_lo"); whi <- vapply(st, `[[`, numeric(1), "whisker_hi")
          out_mask <- Reduce(`|`, lapply(st, `[[`, "outliers"))

          if (md == "3D") {
            verts <- expand.grid(vx = c(lo[1], hi[1]), vy = c(lo[2], hi[2]), vz = c(lo[3], hi[3]))
            fig <- plotly::add_trace(fig, type = "mesh3d", x = verts$vx, y = verts$vy, z = verts$vz,
                                      alphahull = 0, color = I(col[i]), opacity = 0.4, flatshading = TRUE,
                                      lighting = list(ambient = 0.75, diffuse = 0.4, specular = 0.05, roughness = 0.9),
                                      name = gr, legendgroup = gr, showlegend = vis0, visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "mesh"); geom_scale <- c(geom_scale, sc)

            ed <- cuboid_edges(lo, hi)
            fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
                                      x = ed$x, y = ed$y, z = ed$z,
                                      line = list(color = col[i], width = 5),
                                      name = gr, legendgroup = gr, showlegend = FALSE,
                                      hoverinfo = "skip", visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "edges"); geom_scale <- c(geom_scale, sc)

            for (ax in 1:3) {
              p0 <- ctr; p1 <- ctr; p0[ax] <- wlo[ax]; p1[ax] <- whi[ax]
              seg <- rbind(p0, p1)
              fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
                                        x = seg[, 1], y = seg[, 2], z = seg[, 3],
                                        line = list(color = col[i], width = 4),
                                        name = gr, legendgroup = gr, showlegend = FALSE,
                                        hoverinfo = "skip", visible = vis0)
              geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
              geom_role <- c(geom_role, "whisker"); geom_scale <- c(geom_scale, sc)
            }

            fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                      x = ctr[1], y = ctr[2], z = ctr[3],
                                      marker = list(symbol = "cross", size = 9, color = "black", opacity = 0.55),
                                      name = paste(gr, "(mediana)"), legendgroup = gr,
                                      showlegend = FALSE, hoverinfo = "skip", visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "median"); geom_scale <- c(geom_scale, sc)
            fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers+text",
                                      x = ctr[1], y = ctr[2], z = ctr[3],
                                      marker = list(symbol = "cross", size = 6, color = col[i]),
                                      text = gr, textposition = "top center",
                                      textfont = list(color = col[i], size = 11),
                                      name = paste(gr, "(mediana)"), legendgroup = gr,
                                      showlegend = FALSE, visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "median"); geom_scale <- c(geom_scale, sc)

            so <- sub[out_mask, , drop = FALSE]
            fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                      x = if (any(out_mask)) so[, 1] else numeric(0),
                                      y = if (any(out_mask)) so[, 2] else numeric(0),
                                      z = if (any(out_mask)) so[, 3] else numeric(0),
                                      marker = list(symbol = "diamond-open", size = 5, color = col[i]),
                                      name = paste(gr, "(outliers)"), legendgroup = gr, showlegend = FALSE,
                                      visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "outliers"); geom_scale <- c(geom_scale, sc)
          } else {
            a1 <- ax_idx[1]; a2 <- ax_idx[2]
            rx <- c(lo[a1], hi[a1], hi[a1], lo[a1], lo[a1]); ry <- c(lo[a2], lo[a2], hi[a2], hi[a2], lo[a2])
            fig <- plotly::add_trace(fig, type = "scatter", mode = "lines", fill = "toself",
                                      x = rx, y = ry, xaxis = "x2", yaxis = "y2",
                                      line = list(color = col[i], width = 2),
                                      fillcolor = grDevices::adjustcolor(col[i], alpha.f = 0.30),
                                      name = gr, legendgroup = gr, showlegend = vis0, visible = vis0,
                                      hoverinfo = "skip")
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "box2d"); geom_scale <- c(geom_scale, sc)

            wx <- c(wlo[a1], whi[a1], NA, ctr[a1], ctr[a1])
            wy <- c(ctr[a2], ctr[a2], NA, wlo[a2], whi[a2])
            fig <- plotly::add_trace(fig, type = "scatter", mode = "lines",
                                      x = wx, y = wy, xaxis = "x2", yaxis = "y2",
                                      line = list(color = col[i], width = 2),
                                      name = gr, legendgroup = gr, showlegend = FALSE,
                                      hoverinfo = "skip", visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "whisker"); geom_scale <- c(geom_scale, sc)

            fig <- plotly::add_trace(fig, type = "scatter", mode = "markers",
                                      x = ctr[a1], y = ctr[a2], xaxis = "x2", yaxis = "y2",
                                      marker = list(symbol = "cross", size = 12, color = "black", opacity = 0.55),
                                      name = paste(gr, "(mediana)"), legendgroup = gr,
                                      showlegend = FALSE, hoverinfo = "skip", visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "median"); geom_scale <- c(geom_scale, sc)
            fig <- plotly::add_trace(fig, type = "scatter", mode = "markers+text",
                                      x = ctr[a1], y = ctr[a2], xaxis = "x2", yaxis = "y2",
                                      marker = list(symbol = "cross", size = 9, color = col[i]),
                                      text = gr, textposition = "top center",
                                      textfont = list(color = col[i], size = 11),
                                      name = paste(gr, "(mediana)"), legendgroup = gr,
                                      showlegend = FALSE, visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "median"); geom_scale <- c(geom_scale, sc)

            so <- sub[out_mask, , drop = FALSE]
            fig <- plotly::add_trace(fig, type = "scatter", mode = "markers",
                                      x = if (any(out_mask)) so[, a1] else numeric(0),
                                      y = if (any(out_mask)) so[, a2] else numeric(0),
                                      xaxis = "x2", yaxis = "y2",
                                      marker = list(symbol = "diamond-open", size = 7, color = col[i]),
                                      name = paste(gr, "(outliers)"), legendgroup = gr, showlegend = FALSE,
                                      visible = vis0)
            geom_mode <- c(geom_mode, md); geom_method <- c(geom_method, m)
            geom_role <- c(geom_role, "outliers"); geom_scale <- c(geom_scale, sc)
          }
        }
      }
    }
  }

  for (sc in scales_) {
    M <- M_by_scale[[sc]]
    for (md in modes) {
      ax_idx <- pair_axes[[md]]
      for (cb in color_by) {
        cbf <- factor(data[[cb]])
        cbcol <- as.character(grDevices::palette.colors(max(3, nlevels(cbf)), palette = "Okabe-Ito"))
        pt_colors <- cbcol[as.integer(cbf)]
        vis0 <- (md == "3D") && (cb == color_by[1]) && (sc == default_scale)
        if (md == "3D") {
          fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                    x = M[, 1], y = M[, 2], z = M[, 3],
                                    marker = list(size = 2.5, color = pt_colors, opacity = 0.35),
                                    text = as.character(cbf), hoverinfo = "text",
                                    name = "datos", showlegend = FALSE, visible = vis0)
        } else {
          fig <- plotly::add_trace(fig, type = "scatter", mode = "markers",
                                    x = M[, ax_idx[1]], y = M[, ax_idx[2]], xaxis = "x2", yaxis = "y2",
                                    marker = list(size = 5, color = pt_colors, opacity = 0.35),
                                    text = as.character(cbf), hoverinfo = "text",
                                    name = "datos", showlegend = FALSE, visible = vis0)
        }
        pt_mode <- c(pt_mode, md); pt_colorby <- c(pt_colorby, cb); pt_scale <- c(pt_scale, sc)
      }
    }
  }

  fig <- plotly::layout(
    fig,
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = ax_titles_by_scale[[default_scale]][1]),
                    showbackground = grid3d == 1, showgrid = grid3d == 1,
                    backgroundcolor = BGCOL, gridcolor = GRIDCOL),
      yaxis = list(title = list(text = ax_titles_by_scale[[default_scale]][2]),
                    showbackground = grid3d == 1, showgrid = grid3d == 1,
                    backgroundcolor = BGCOL, gridcolor = GRIDCOL),
      zaxis = list(title = list(text = ax_titles_by_scale[[default_scale]][3]),
                    showbackground = grid3d == 1, showgrid = grid3d == 1,
                    backgroundcolor = BGCOL, gridcolor = GRIDCOL),
      camera = list(eye = list(x = 1.6, y = 1.6, z = 1.6)),
      dragmode = "orbit"
    ),
    xaxis2 = list(domain = c(0, 0.001), visible = FALSE, title = list(text = ax_titles_by_scale[[default_scale]][1]),
                  showgrid = grid3d == 1, gridcolor = GRIDCOL, anchor = "y2"),
    yaxis2 = list(domain = c(0, 0.001), visible = FALSE, title = list(text = ax_titles_by_scale[[default_scale]][2]),
                  showgrid = grid3d == 1, gridcolor = GRIDCOL, anchor = "x2"),
    legend = list(x = 1.02, y = 0.5),
    margin = list(t = 150)
  )

  fig <- htmlwidgets::onRender(fig, "
    function(el, x, data) {
      var state = { mode: data.default_mode, method: data.default_method,
                     colorby: data.default_colorby, scale: data.default_scale };
      function applyState() {
        var n = el.data.length;
        var vis = new Array(n).fill(false);
        var showleg = new Array(n).fill(false);
        for (var i = 0; i < data.geom_mode.length; i++) {
          if (data.geom_mode[i] === state.mode && data.geom_method[i] === state.method &&
              data.geom_scale[i] === state.scale) {
            vis[i] = true;
            if (data.geom_role[i] === 'mesh' || data.geom_role[i] === 'box2d') showleg[i] = true;
          }
        }
        var off = data.geom_mode.length;
        for (var j = 0; j < data.pt_mode.length; j++) {
          if (data.pt_mode[j] === state.mode && data.pt_colorby[j] === state.colorby &&
              data.pt_scale[j] === state.scale) vis[off + j] = true;
        }
        Plotly.restyle(el, { visible: vis, showlegend: showleg });

        var titles = data.ax_titles[state.scale];
        var lu = {
          'scene.xaxis.title.text': titles[0],
          'scene.yaxis.title.text': titles[1],
          'scene.zaxis.title.text': titles[2]
        };
        if (state.mode === '3D') {
          lu['scene.domain.x'] = [0, 1]; lu['scene.domain.y'] = [0, 1];
          lu['xaxis2.domain'] = [0, 0.001]; lu['yaxis2.domain'] = [0, 0.001];
          lu['xaxis2.visible'] = false; lu['yaxis2.visible'] = false;
        } else {
          lu['scene.domain.x'] = [0, 0.001]; lu['scene.domain.y'] = [0, 0.001];
          lu['xaxis2.domain'] = [0, 1]; lu['yaxis2.domain'] = [0, 1];
          lu['xaxis2.visible'] = true; lu['yaxis2.visible'] = true;
          var pair = data.pair_axes[state.mode];
          lu['xaxis2.title.text'] = titles[pair[0] - 1];
          lu['yaxis2.title.text'] = titles[pair[1] - 1];
        }
        Plotly.relayout(el, lu);
      }
      el.on('plotly_buttonclicked', function(ev) {
        var name = ev.menu.name, idx = ev.active;
        if (name === 'vista') state.mode = data.modes[idx];
        else if (name === 'metodo') state.method = data.methods[idx];
        else if (name === 'colorby') state.colorby = data.color_by[idx];
        else if (name === 'escala') state.scale = data.scales[idx];
        else return;
        applyState();
      });
      applyState();
    }
  ", data = list(
    geom_mode = geom_mode, geom_method = geom_method, geom_role = geom_role, geom_scale = geom_scale,
    pt_mode = pt_mode, pt_colorby = pt_colorby, pt_scale = pt_scale,
    modes = modes, methods = methods, color_by = color_by, scales = scales_,
    ax_titles = list(raw = as.list(ax_titles_by_scale$raw), zscore = as.list(ax_titles_by_scale$zscore)),
    pair_axes = list(`12` = c(1, 2), `23` = c(2, 3), `13` = c(1, 3)),
    default_mode = "3D", default_method = default_method, default_colorby = color_by[1],
    default_scale = default_scale
  ))

  fig <- plotly::layout(fig, updatemenus = list(
    list(type = "buttons", direction = "right", x = 0.0, y = 1.28, xanchor = "left", yanchor = "top",
         name = "vista", active = 0, pad = list(t = 1, b = 1, l = 2, r = 2),
         buttons = lapply(modes, function(md) list(method = "skip", label = mode_labels[[md]], args = list()))),
    list(type = "dropdown", direction = "down", x = 0.0, y = 1.215, xanchor = "left", yanchor = "top",
         name = "metodo", active = match(default_method, methods) - 1, pad = list(t = 1, b = 1, l = 2, r = 2),
         buttons = lapply(methods, function(m) list(method = "skip", label = method_labels[[m]], args = list()))),
    list(type = "dropdown", direction = "down", x = 0.0, y = 1.15, xanchor = "left", yanchor = "top",
         name = "colorby", active = 0, pad = list(t = 1, b = 1, l = 2, r = 2),
         buttons = lapply(color_by, function(cb) list(method = "skip", label = paste("Color:", cb), args = list()))),
    list(type = "dropdown", direction = "down", x = 0.0, y = 1.085, xanchor = "left", yanchor = "top",
         name = "escala", active = match(default_scale, scales_) - 1, pad = list(t = 1, b = 1, l = 2, r = 2),
         buttons = lapply(scales_, function(sc) list(method = "skip", label = scale_labels[[sc]], args = list()))),
    list(type = "buttons", direction = "right", x = 0.0, y = 1.02, xanchor = "left", yanchor = "top",
         name = "grid", pad = list(t = 1, b = 1, l = 2, r = 2),
         buttons = list(
           list(method = "relayout", label = "Cuadricula: ON",
                args = list(list(`scene.xaxis.showbackground` = TRUE, `scene.xaxis.showgrid` = TRUE,
                                  `scene.yaxis.showbackground` = TRUE, `scene.yaxis.showgrid` = TRUE,
                                  `scene.zaxis.showbackground` = TRUE, `scene.zaxis.showgrid` = TRUE,
                                  `xaxis2.showgrid` = TRUE, `yaxis2.showgrid` = TRUE))),
           list(method = "relayout", label = "Cuadricula: OFF",
                args = list(list(`scene.xaxis.showbackground` = FALSE, `scene.xaxis.showgrid` = FALSE,
                                  `scene.yaxis.showbackground` = FALSE, `scene.yaxis.showgrid` = FALSE,
                                  `scene.zaxis.showbackground` = FALSE, `scene.zaxis.showgrid` = FALSE,
                                  `xaxis2.showgrid` = FALSE, `yaxis2.showgrid` = FALSE)))
         ))
  ))

  fig
}
