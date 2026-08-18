#' Draw an interactive, rotatable 3-axis (triaxial) box plot
#'
#' Draws a single interactive 3D scene (via \pkg{plotly}) with one
#' translucent cuboid per group spanning the box on all three axes at
#' once, whiskers along each axis, a cross-shaped median marker, faint raw
#' points, and outliers (when the method produces any) as open diamonds.
#' The scene rotates with click-drag, pans with right-click/shift-drag,
#' and zooms with the mouse wheel — all native \pkg{plotly} behavior.
#'
#' Exactly one of `tukey`, `percentile`, `sd`, `letter_value` must be 1
#' (the rest 0): this selects how the box and whiskers are defined, using
#' the same four conventions as [boxplot3d()] (see that function's
#' documentation for details on each).
#'
#' Four buttons above the plot jump the camera to preset views: a general
#' 3D angle, and three axis-pair views (1-2, 2-3, 1-3) that look straight
#' down the third axis, mirroring the pairwise panels of [boxplot3d()]
#' while keeping the scene fully rotatable afterward.
#'
#' @param data A data frame with the three axes and the grouping variable.
#' @param x,y,z Character. Column names of the three continuous axes.
#' @param group Character. Column name of the grouping variable.
#' @param tukey,percentile,sd,letter_value 0/1 flags selecting the
#'   box/whisker convention. Exactly one must be 1.
#' @param k Numeric. Tukey fence multiplier (default 1.5).
#' @param probs Length-2 numeric. Lower/upper whisker percentiles for the
#'   `percentile` convention (default `c(0.05, 0.95)`).
#' @param sd_mult Length-2 numeric. Box and whisker multipliers (in SDs)
#'   for the `sd` convention (default `c(1, 2)`).
#' @param scale 0/1. If 1, standardize (z-score) `x`, `y`, `z` before
#'   plotting, so axes on very different units/scales are shown on a
#'   common scale.
#' @param grid3d 0/1. If 1 (default), draw the shaded wall/floor grid
#'   panels behind the scene; if 0, a plain background.
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
  method <- names(flags)[flags == 1]
  stopifnot(scale %in% c(0, 1), grid3d %in% c(0, 1))

  g <- factor(data[[group]]); groups <- levels(g)
  if (is.null(col)) col <- grDevices::palette.colors(max(3, length(groups)), palette = "Okabe-Ito")

  ax_names <- c(x, y, z)
  axes <- list(.as_plain_numeric(data[[x]]), .as_plain_numeric(data[[y]]), .as_plain_numeric(data[[z]]))
  if (scale == 1) axes <- lapply(axes, function(v) as.numeric(scale(v)))
  M <- do.call(cbind, axes)

  fig <- plotly::plot_ly()
  for (i in seq_along(groups)) {
    gr <- groups[i]; idx <- which(g == gr)
    sub <- M[idx, , drop = FALSE]
    st <- lapply(1:3, function(j) .box_axis_stats(sub[, j], method, k, probs, sd_mult))
    lo <- vapply(st, `[[`, numeric(1), "q1"); hi <- vapply(st, `[[`, numeric(1), "q3")
    ctr <- vapply(st, `[[`, numeric(1), "center")
    wlo <- vapply(st, `[[`, numeric(1), "whisker_lo"); whi <- vapply(st, `[[`, numeric(1), "whisker_hi")
    out_mask <- Reduce(`|`, lapply(st, `[[`, "outliers"))

    verts <- expand.grid(vx = c(lo[1], hi[1]), vy = c(lo[2], hi[2]), vz = c(lo[3], hi[3]))
    fig <- plotly::add_trace(fig, type = "mesh3d", x = verts$vx, y = verts$vy, z = verts$vz,
                              alphahull = 0, color = I(col[i]), opacity = 0.35,
                              name = gr, legendgroup = gr, showlegend = TRUE)

    for (ax in 1:3) {
      p0 <- ctr; p1 <- ctr
      p0[ax] <- wlo[ax]; p1[ax] <- whi[ax]
      seg <- rbind(p0, p1)
      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "lines",
                                x = seg[, 1], y = seg[, 2], z = seg[, 3],
                                line = list(color = col[i], width = 4),
                                name = gr, legendgroup = gr, showlegend = FALSE,
                                hoverinfo = "skip")
    }

    fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                              x = ctr[1], y = ctr[2], z = ctr[3],
                              marker = list(symbol = "cross", size = 6, color = "black",
                                            line = list(color = col[i], width = 2)),
                              name = paste(gr, "(mediana)"), legendgroup = gr, showlegend = FALSE)

    fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                              x = sub[, 1], y = sub[, 2], z = sub[, 3],
                              marker = list(size = 2.5, color = col[i], opacity = 0.18),
                              name = paste(gr, "(datos)"), legendgroup = gr, showlegend = FALSE,
                              hoverinfo = "skip")

    if (any(out_mask)) {
      so <- sub[out_mask, , drop = FALSE]
      fig <- plotly::add_trace(fig, type = "scatter3d", mode = "markers",
                                x = so[, 1], y = so[, 2], z = so[, 3],
                                marker = list(symbol = "diamond-open", size = 5, color = col[i]),
                                name = paste(gr, "(outliers)"), legendgroup = gr, showlegend = FALSE)
    }
  }

  ax_titles <- paste0(ax_names, if (scale == 1) " (z-score)" else "")
  fig <- plotly::layout(
    fig,
    scene = list(
      xaxis = list(title = ax_titles[1], showbackground = grid3d == 1, showgrid = grid3d == 1),
      yaxis = list(title = ax_titles[2], showbackground = grid3d == 1, showgrid = grid3d == 1),
      zaxis = list(title = ax_titles[3], showbackground = grid3d == 1, showgrid = grid3d == 1),
      camera = list(eye = list(x = 1.6, y = 1.6, z = 1.6))
    ),
    updatemenus = list(list(
      type = "buttons", direction = "right", x = 0.5, y = 1.08, xanchor = "center",
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
      )
    ))
  )
  fig
}
