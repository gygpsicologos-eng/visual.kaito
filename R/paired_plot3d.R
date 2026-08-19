#' Draw a paired (pre/post) comparison on two variables as a 3D "spaghetti" plot
#'
#' A repeated-measures counterpart to [ttest_plot3d()]: instead of two
#' independent groups, the same subjects are measured twice (e.g. before and
#' after an intervention) on two continuous variables. Each subject is drawn
#' as a point at their "Pre" measurement and a point at their "Post"
#' measurement, joined by a line, in a 3D scene where `x` and `z` are the two
#' variables and the third axis ("Momento") separates Pre from Post. Both
#' paired Student's t-tests (one per variable) and a joint one-sample
#' (paired) Hotelling's T-squared test on the vector of differences are
#' reported.
#'
#' @param data A data frame in wide format: one row per subject, with
#'   separate columns for the Pre and Post measurement of each variable.
#' @param x_pre,x_post Character. Column names of the first variable's Pre
#'   and Post measurements.
#' @param z_pre,z_post Character. Column names of the second variable's Pre
#'   and Post measurements.
#' @param x_name,z_name Character. Axis titles for the two variables
#'   (default: derived from `x_pre`/`z_pre` by stripping a trailing
#'   `_pre`/`_Pre` suffix if present).
#' @param conf.level Confidence level for the paired t-tests (default 0.95).
#' @param opacity Initial opacity (0-1) of the subject lines (default 0.75);
#'   adjustable afterwards with the transparency slider.
#' @param col_pre,col_post Colors for the Pre and Post markers.
#' @param col_dir Length-3 character vector of colors for the "Direccion de
#'   cambio" coloring scheme: both axes increase, both decrease, mixed
#'   change (in that order).
#'
#' @return A `plotly` htmlwidget object (auto-displayed if the result isn't
#'   assigned). The full statistics -- `n`, per-variable Pre/Post means and
#'   SDs, `t_x` and `t_z` (paired [stats::t.test()] results), and
#'   `hotelling_paired` (a list with `T2`, `F`, `df1`, `df2`, `p.value`) --
#'   are attached as the `"stats"` attribute.
#'
#' @examples
#' set.seed(1)
#' n <- 20
#' df <- data.frame(
#'   x_pre = rnorm(n, 60, 9), z_pre = rnorm(n, 18, 4)
#' )
#' df$x_post <- df$x_pre - rnorm(n, 9, 6)
#' df$z_post <- df$z_pre - rnorm(n, 6, 4)
#' paired_plot3d(df, x_pre = "x_pre", x_post = "x_post",
#'               z_pre = "z_pre", z_post = "z_post")
#'
#' @export
paired_plot3d <- function(data, x_pre, x_post, z_pre, z_post,
                           x_name = NULL, z_name = NULL,
                           conf.level = 0.95, opacity = 0.75,
                           col_pre = "#2a78d6", col_post = "#eb6834",
                           col_dir = c("#1baf7a", "#d03b3b", "#9a9890")) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("El paquete 'plotly' es necesario para paired_plot3d(). ",
         "Instalalo con install.packages('plotly').")
  }
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("El paquete 'htmlwidgets' es necesario para paired_plot3d(). ",
         "Instalalo con install.packages('htmlwidgets').")
  }
  stopifnot(conf.level > 0, conf.level < 1, opacity > 0, opacity <= 1)
  if (length(col_dir) < 3) stop("'col_dir' debe tener 3 colores (sube ambos / baja ambos / mixto).")

  xv0 <- .as_plain_numeric(data[[x_pre]]); xv1 <- .as_plain_numeric(data[[x_post]])
  zv0 <- .as_plain_numeric(data[[z_pre]]); zv1 <- .as_plain_numeric(data[[z_post]])
  ok <- stats::complete.cases(xv0, xv1, zv0, zv1)
  if (any(!ok)) {
    message("Se excluyeron ", sum(!ok), " fila(s) con datos incompletos en alguna de las 4 columnas.")
  }
  xv0 <- xv0[ok]; xv1 <- xv1[ok]; zv0 <- zv0[ok]; zv1 <- zv1[ok]
  n <- length(xv0)
  if (n < 3) stop("Se necesitan al menos 3 pares completos para paired_plot3d().")

  if (is.null(x_name)) x_name <- sub("_?[Pp]re$", "", x_pre)
  if (is.null(z_name)) z_name <- sub("_?[Pp]re$", "", z_pre)

  dx <- xv1 - xv0; dz <- zv1 - zv0
  t_x <- stats::t.test(xv1, xv0, paired = TRUE, conf.level = conf.level)
  t_z <- stats::t.test(zv1, zv0, paired = TRUE, conf.level = conf.level)
  hot <- .hotelling_t2_one(cbind(dx, dz))

  up_both <- dx > 0 & dz > 0
  down_both <- dx < 0 & dz < 0
  dir_idx <- ifelse(up_both, 1L, ifelse(down_both, 2L, 3L))
  dir_labels <- c("Sube en ambos ejes", "Baja en ambos ejes", "Cambio mixto")
  dir_colors <- col_dir[dir_idx]
  subj_colors <- as.character(grDevices::hcl.colors(n, palette = "Dark 3"))

  # Standardized (z-score) copy: Pre and Post pooled together per axis, so the
  # same linear rescaling applies to both timepoints (mirrors ttest_plot3d()).
  xv0z <- as.numeric(scale(c(xv0, xv1)))[seq_len(n)]
  xv1z <- as.numeric(scale(c(xv0, xv1)))[(n + 1):(2 * n)]
  zv0z <- as.numeric(scale(c(zv0, zv1)))[seq_len(n)]
  zv1z <- as.numeric(scale(c(zv0, zv1)))[(n + 1):(2 * n)]

  stars <- function(p) if (p < 0.001) "***" else if (p < 0.01) "**" else
    if (p < 0.05) "*" else if (p < 0.1) "." else ""
  desc_line <- function(nm, v0, v1) sprintf(
    "%s: Pre = %.3g +/- %.3g; Post = %.3g +/- %.3g",
    nm, mean(v0), stats::sd(v0), mean(v1), stats::sd(v1)
  )
  txt <- paste(
    sprintf("Prueba t pareada (eje %s): p = %.4g %s", x_name, t_x$p.value, stars(t_x$p.value)),
    sprintf("Prueba t pareada (eje %s): p = %.4g %s", z_name, t_z$p.value, stars(t_z$p.value)),
    sprintf("Hotelling T2 pareado (conjunto): p = %.4g %s", hot$p.value, stars(hot$p.value)),
    desc_line(x_name, xv0, xv1), desc_line(z_name, zv0, zv1),
    sprintf("n = %d  |  %d suben en ambos, %d bajan en ambos, %d mixto",
            n, sum(up_both), sum(down_both), sum(dir_idx == 3L)),
    sep = "<br>"
  )

  fig <- plotly::plot_ly(height = 850)

  # --- one line-trace per subject (indices 0..n-1): raw scale, colored by direction ---
  for (i in seq_len(n)) {
    fig <- plotly::add_trace(fig,
      x = c(xv0[i], xv1[i]), y = c(0, 1), z = c(zv0[i], zv1[i]),
      type = "scatter3d", mode = "lines",
      line = list(color = dir_colors[i], width = 3),
      opacity = opacity, hoverinfo = "skip",
      name = "Pacientes", legendgroup = "lineas", showlegend = FALSE)
  }
  # --- Pre / Post markers (indices n, n+1) ---
  fig <- plotly::add_trace(fig, x = xv0, y = rep(0, n), z = zv0, type = "scatter3d", mode = "markers",
    marker = list(color = col_pre, size = 5, line = list(color = "black", width = 0.4)),
    name = "Pre", legendgroup = "pre", showlegend = TRUE)
  fig <- plotly::add_trace(fig, x = xv1, y = rep(1, n), z = zv1, type = "scatter3d", mode = "markers",
    marker = list(color = col_post, size = 5, line = list(color = "black", width = 0.4)),
    name = "Post", legendgroup = "post", showlegend = TRUE)

  line_idx <- as.list(0:(n - 1))
  opacity_steps <- c(0.15, 0.25, 0.35, 0.45, 0.55, 0.65, 0.75, 0.85, 0.95)
  opacity_active0 <- which.min(abs(opacity_steps - opacity)) - 1L

  fig <- plotly::layout(fig,
    title = list(text = paste0("Comparacion pareada (Pre/Post): ", x_name, " y ", z_name),
                 x = 1, xanchor = "right", y = 0.98, yanchor = "top"),
    margin = list(t = 120),
    legend = list(x = 1.02, y = 0.5),
    scene = list(
      domain = list(x = c(0, 1), y = c(0, 1)),
      xaxis = list(title = list(text = x_name)),
      yaxis = list(title = list(text = "Momento"), tickvals = list(0, 1), ticktext = list("Pre", "Post")),
      zaxis = list(title = list(text = z_name)),
      camera = list(eye = list(x = 1.6, y = -1.6, z = 0.9))
    ),
    annotations = list(list(
      text = txt, xref = "paper", yref = "paper", x = 0.99, y = 0.9, xanchor = "right", yanchor = "top",
      showarrow = FALSE, align = "left", bordercolor = "#e1e0d9", borderwidth = 1,
      borderpad = 6, bgcolor = "#fcfcfb", font = list(size = 11, color = "#0b0b0b")
    )),
    updatemenus = list(
      list(
        name = "colorear", type = "dropdown", direction = "down", showactive = TRUE, active = 0,
        x = 0, y = 1.15, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "restyle", label = "Colorear por: direccion de cambio",
               args = list(list(`line.color` = dir_colors), line_idx)),
          list(method = "restyle", label = "Colorear por: paciente individual",
               args = list(list(`line.color` = subj_colors), line_idx))
        )
      ),
      list(
        name = "escala", type = "buttons", direction = "right", showactive = TRUE, active = 0,
        x = 0, y = 1.08, xanchor = "left", yanchor = "top",
        pad = list(t = 0, b = 0, l = 1, r = 1),
        buttons = list(
          list(method = "update", label = "Datos brutos", args = list(
                 list(x = c(as.list(Map(c, xv0, xv1)), list(xv0, xv1)),
                      z = c(as.list(Map(c, zv0, zv1)), list(zv0, zv1))),
                 list(), c(as.list(0:(n - 1)), list(n, n + 1)))),
          list(method = "update", label = "Datos estandarizados", args = list(
                 list(x = c(as.list(Map(c, xv0z, xv1z)), list(xv0z, xv1z)),
                      z = c(as.list(Map(c, zv0z, zv1z)), list(zv0z, zv1z))),
                 list(`scene.xaxis.title.text` = paste0(x_name, " (z-score)"),
                      `scene.zaxis.title.text` = paste0(z_name, " (z-score)")),
                 c(as.list(0:(n - 1)), list(n, n + 1))))
        )
      )
    ),
    sliders = list(list(
      active = opacity_active0,
      currentvalue = list(prefix = "Transparencia: "),
      x = 0, len = 0.32, xanchor = "left", y = -0.02, yanchor = "top",
      pad = list(t = 10),
      steps = lapply(opacity_steps, function(v) list(
        method = "restyle", label = sprintf("%d%%", round(v * 100)),
        args = list(list(opacity = v), line_idx)
      ))
    ))
  )

  attr(fig, "stats") <- list(
    n = n,
    mean_sd = list(
      x_pre = c(mean = mean(xv0), sd = stats::sd(xv0)), x_post = c(mean = mean(xv1), sd = stats::sd(xv1)),
      z_pre = c(mean = mean(zv0), sd = stats::sd(zv0)), z_post = c(mean = mean(zv1), sd = stats::sd(zv1))
    ),
    t_x = t_x, t_z = t_z, hotelling_paired = hot
  )
  fig
}

#' @noRd
.hotelling_t2_one <- function(D) {
  n <- nrow(D); p <- ncol(D)
  md <- colMeans(D)
  Sd <- stats::cov(D)
  T2 <- n * as.numeric(t(md) %*% solve(Sd) %*% md)
  df1 <- p
  df2 <- n - p
  Fstat <- ((n - p) / (p * (n - 1))) * T2
  p_value <- stats::pf(Fstat, df1, df2, lower.tail = FALSE)
  list(T2 = T2, F = Fstat, df1 = df1, df2 = df2, p.value = p_value)
}
