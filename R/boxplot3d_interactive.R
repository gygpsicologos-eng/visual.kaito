#' Draw an interactive, rotatable 3-axis (triaxial) box plot
#'
#' Draws a single interactive 3D scene (via \pkg{plotly}) with one
#' translucent cuboid per group spanning the box on all three axes at
#' once, whiskers along each axis, a cross-shaped median marker (a black
#' halo plus a smaller marker in the group's own color, so medians stay
#' distinguishable by group even when they land close together), faint
#' raw points, and outliers (when the method produces any) as open
#' diamonds. The scene rotates with click-drag, pans with right-click or
#' shift-drag, and zooms with the mouse wheel -- all native \pkg{plotly}
#' behavior.
#'
#' Unlike [boxplot3d()], which requires picking exactly one whisker
#' convention up front, this function precomputes all four conventions
#' (tukey/percentile/sd/letter_value -- see [boxplot3d()] for what each
#' one means) and lets you switch between them live with the "Metodo"
#' dropdown above the plot, without re-calling the function. The
#' `tukey`/`percentile`/`sd`/`letter_value` arguments only choose which
#' one is shown first.
#'
#' Three groups of controls sit above the plot:
#' \itemize{
#'   \item View buttons: a general 3D angle, and three axis-pair views
#'     (1-2, 2-3, 1-3) that look straight down the third axis -- the scene
#'     stays fully rotatable afterward.
#'   \item A "Metodo" dropdown to switch box/whisker convention live.
#'   \item A grid on/off toggle for the wall/floor panels.
#' }
#'
#' @param data A data frame with the three axes and the grouping variable.
#' @param x,y,z Character. Column names of the three continuous axes.
#'   These names (or, if you pass friendlier ones, whatever you pass
#'   here) are used verbatim as the axis titles -- so if you renamed your
#'   columns to generic `"x"`/`"y"`/`"z"` before calling this function,
#'   that is what will show on the axes. Passing the original column
#'   names directly (e.g. `x = "Score_ISI"`), without renaming the data
#'   frame first, gives you meaningful axis labels for free.
#' @param group Character. Column name of the grouping variable.
#' @param tukey,percentile,sd,letter_value 0/1 flags selecting which
#'   convention is shown initially (you can switch live afterward via the
#'   "Metodo" dropdown). Exactly one must be 1.
#' @param k Numeric. Tukey fence multiplier (default 1.5).
#' @param probs Length-2 numeric. Lower/upper whisker percentiles for the
#'   `percentile` convention (default `c(0.05, 0.95)`).
#' @param sd_mult Length-2 numeric. Box and whisker multipliers (in SDs)
#'   for the `sd` convention (default `c(1, 2)`).
#' @param scale 0/1. If 1, standardize (z-score) `x`, `y`, `z` before
#'   plotting, so axes on very different units/scales are shown on a
#'   common scale.
#' @param grid3d 0/1. Initial state of the wall/floor grid (default 1 =
#'   on); also togglable live from the plot itself.
#' @param col Optional character vector of colors, one per group.
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
                                   scale = 0, grid3d = 1, col = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para boxplot3d_interactive(). ",
         "Instalalo con install.packages('plotly').")
  }
  flags <- c(tukey = tukey, percentile = percentile, sd = sd, letter_value = letter_value)
  if (sum(flags == 1) != 1) {
    stop("Activa exactamente uno de tukey / percentile / sd / letter_value (=1), el resto a 0.")
  }
  default_method <- names(flags)[flags == 1]
  stopifnot(scale %in% c(0, 1), grid3d %in% c(0, 1))

  g <- factor(data[[group]]); groups <- levels(g)
  if (is.null(col)) col <- grDevices::palette.colors(max(3, length(groups)), palette = "Okabe-Ito")

  ax_names <- c(x, y, z)
  axes <- list(.as_plain_numeric(data[[x]]), .as_plain_numeric(data[[y]]), .as_plain_numeric(data[[z]]))
  if (scale == 1) axes <- lapply(axes, function(v) as.numeric(scale(v)))
  M <- do.call(cbind, axes)

  methods <- c("tukey", "percentile", "sd", "letter_value")
  method_labels <- c(tukey = "Tukey", percentile = "Percentil", sd = "Media +/- DE",
                      letter_value = "Letter-value")

  fig <- plotly::plot_ly()
  trace_method <- character(0); trace_role <- character(0)

  for (m in methods) {
    for (i in seq_along(groups)) {
      gr <- groups[i]; idx <- which(g == gr)
      sub <- M[idx, , drop = FALSE]
      st <- lapply(1:3, function(j) .box_axis_stats(sub[, j], m, k, probs, sd_mult))
      lo <- vapply(st, `[[`, numeric(1), "q1"); hi <- vapply(st, `[[`, numeric(1), "q3")
      ctr <- vapply(st, `[[`, numeric(1), "center")
      wlo <- vapply(st, `[[`, numeric(1), "whisker_lo"); whi <- vapply(st, `[[`, numeric(1), "whisker_hi")
      out_mask <- Reduce(`|`, lapply(st, `[[`, "outliers"))
      vis <- (m == default_method)

      verts <- expand.grid(vx = c(lo[1], hi[1]), vy = c(lo[2], hi[2]), vz = c(lo[3], hi[3]))
      fig <- plotly::add_trace(fig, type = "mesh3d", x = verts$vx, y = verts$vy, z = verts$vz,
                                alphahull = 0, color = I(col[i]), opacity = 0.35,
                                name = gr, legendgroup = gr, showlegend = vis, visible = vis)
      trace_method <- c(trace_method, m); trace_role <- c(trace_role, "mesh")

      for (ax in 1:3) {
        p0 <- ctr; p1 <- ctr; p0[ax] <- wlo[ax]; p1[ax] <- whi[ax]
        seg <- rbind(p0, p1)
        fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
                                  x = seg[, 1], y = seg[, 2], z = seg[, 3],
                                  line = list(color = col[i], width = 4),
                                  name = gr, legendgroup = gr, showlegend = FALSE,
                                  hoverinfo = "skip", visible = vis)
        trace_method <- c(trace_method, m); trace_role <- c(trace_role, "whisker")
      }

      # median: black halo behind + a smaller marker in the group's own color on top,
      # so groups stay visually distinguishable even when medians land close together
      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                x = ctr[1], y = ctr[2], z = ctr[3],
                                marker = list(symbol = "cross", size = 10, color = "black"),
                                name = paste(gr, "(mediana)"), legendgroup = gr,
                                showlegend = FALSE, hoverinfo = "skip", visible = vis)
      trace_method <- c(trace_method, m); trace_role <- c(trace_role, "median")
      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                x = ctr[1], y = ctr[2], z = ctr[3],
                                marker = list(symbol = "cross", size = 6, color = col[i]),
                                name = paste(gr, "(mediana)"), legendgroup = gr,
                                showlegend = FALSE, visible = vis)
      trace_method <- c(trace_method, m); trace_role <- c(trace_role, "median")

      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                x = sub[, 1], y = sub[, 2], z = sub[, 3],
                                marker = list(size = 2.5, color = col[i], opacity = 0.18),
                                name = paste(gr, "(datos)"), legendgroup = gr, showlegend = FALSE,
                                hoverinfo = "skip", visible = vis)
      trace_method <- c(trace_method, m); trace_role <- c(trace_role, "points")

      so_x <- if (any(out_mask)) sub[out_mask, 1] else numeric(0)
      so_y <- if (any(out_mask)) sub[out_mask, 2] else numeric(0)
      so_z <- if (any(out_mask)) sub[out_mask, 3] else numeric(0)
      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                x = so_x, y = so_y, z = so_z,
                                marker = list(symbol = "diamond-open", size = 5, color = col[i]),
                                name = paste(gr, "(outliers)"), legendgroup = gr, showlegend = FALSE,
                                visible = vis)
      trace_method <- c(trace_method, m); trace_role <- c(trace_role, "outliers")
    }
  }

  ax_titles <- paste0(ax_names, if (scale == 1) " (z-score)" else "")
  fig <- plotly::layout(
    fig,
    scene = list(
      xaxis = list(title = list(text = ax_titles[1]), showbackground = grid3d == 1, showgrid = grid3d == 1),
      yaxis = list(title = list(text = ax_titles[2]), showbackground = grid3d == 1, showgrid = grid3d == 1),
      zaxis = list(title = list(text = ax_titles[3]), showbackground = grid3d == 1, showgrid = grid3d == 1),
      camera = list(eye = list(x = 1.6, y = 1.6, z = 1.6)),
      dragmode = "orbit"
    ),
    updatemenus = list(
      list(type = "buttons", direction = "right", x = 0.5, y = 1.15, xanchor = "center",
           buttons = list(
             list(method = "relayout", label = "3D",
                  args = list(list(`scene.camera` = list(eye = list(x = 1.6, y = 1.6, z = 1.6))))),
             list(method = "relayout", label = "Eje 1-2",
                  args = list(list(`scene.camera` = list(
                    eye = list(x = 0, y = 0, z = 2.4), up = list(x = 0, y = 1, z = 0))))),
             list(method = "relayout", label = "Eje 2-3",
                  args = list(list(`scene.camera` = list(
                    eye = list(x = 2.4, y = 0, z = 0), up = list(x = 0, y = 0, z = 1))))),
             list(method = "relayout", label = "Eje 1-3",
                  args = list(list(`scene.camera` = list(
                    eye = list(x = 0, y = 2.4, z = 0), up = list(x = 0, y = 0, z = 1)))))
           )),
      list(type = "dropdown", direction = "down", x = 0.0, y = 1.15, xanchor = "left",
           buttons = lapply(methods, function(m) {
             vism <- trace_method == m
             showlegm <- vism & (trace_role == "mesh")
             list(method = "restyle", label = method_labels[[m]],
                  args = list(list(visible = as.list(vism), showlegend = as.list(showlegm))))
           })),
      list(type = "buttons", direction = "left", x = 1.0, y = 1.15, xanchor = "right",
           buttons = list(
             list(method = "relayout", label = "Cuadricula: ON",
                  args = list(list(`scene.xaxis.showbackground` = TRUE, `scene.xaxis.showgrid` = TRUE,
                                    `scene.yaxis.showbackground` = TRUE, `scene.yaxis.showgrid` = TRUE,
                                    `scene.zaxis.showbackground` = TRUE, `scene.zaxis.showgrid` = TRUE))),
             list(method = "relayout", label = "Cuadricula: OFF",
                  args = list(list(`scene.xaxis.showbackground` = FALSE, `scene.xaxis.showgrid` = FALSE,
                                    `scene.yaxis.showbackground` = FALSE, `scene.yaxis.showgrid` = FALSE,
                                    `scene.zaxis.showbackground` = FALSE, `scene.zaxis.showgrid` = FALSE)))
           ))
    )
  )
  fig
}
