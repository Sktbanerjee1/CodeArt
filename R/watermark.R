#' Watermark settings
#'
#' Two stacked lines in a corner: a bold label (default "CodeArt") above a
#' name. Sizes are fractions of the image height, so the watermark scales with
#' any aspect ratio. Defaults for `name` and `label` can be set once with
#' `options(codeart.name = ..., codeart.label = ...)`.
#'
#' @param enabled Draw the watermark.
#' @param name Name line.
#' @param label Bold label line.
#' @param position One of "bottom-right", "bottom-left", "top-right", "top-left".
#' @param size Font size as a fraction of image height.
#' @param margin Distance from the edges as a fraction of image height.
#' @param colour Text colour.
#' @param alpha Text opacity (a soft drop shadow is added automatically).
#' @param family Font family.
#' @return A list, used as the `watermark` element of any `*_params()`.
#' @examples
#' watermark_params(position = "top-left", alpha = 0.6)
#' @export
watermark_params <- function(enabled = TRUE,
                             name = getOption("codeart.name", "Saikat Bandyopadhyay"),
                             label = getOption("codeart.label", "CodeArt"),
                             position = c("bottom-right", "bottom-left", "top-right", "top-left"),
                             size = 0.020, margin = 0.025,
                             colour = "#FFFFFF", alpha = 0.80, family = "sans") {
  position <- match.arg(position)
  list(enabled = enabled, name = name, label = label, position = position,
       size = size, margin = margin, colour = colour, alpha = alpha, family = family)
}

# draw on the open canvas; xlim/ylim are the canvas data limits, H the image height in inches
draw_watermark <- function(wm, xlim, ylim, H) {
  if (!isTRUE(wm$enabled)) return(invisible())
  dy    <- diff(ylim)
  right <- grepl("right", wm$position)
  top   <- grepl("top", wm$position)
  m     <- wm$margin*dy
  gap   <- 1.35*wm$size*dy
  x     <- if (right) xlim[2] - m else xlim[1] + m
  hj    <- if (right) 1 else 0
  vj    <- if (top) 1 else 0
  y_label <- if (top) ylim[2] - m else ylim[1] + m + gap
  y_name  <- if (top) ylim[2] - m - gap else ylim[1] + m
  cex <- wm$size*H*72/graphics::par("ps")       # font size = size * image height
  sh  <- 0.0012*dy
  put <- function(ox, oy, col, a) {
    graphics::text(x + ox, y_label + oy, wm$label, adj = c(hj, vj), font = 2, cex = cex*1.15,
                   col = grDevices::adjustcolor(col, a), family = wm$family)
    graphics::text(x + ox, y_name + oy, wm$name, adj = c(hj, vj), font = 1, cex = cex*0.80,
                   col = grDevices::adjustcolor(col, a), family = wm$family)
  }
  put(sh, -sh, "#000000", wm$alpha*0.6)          # soft drop shadow for legibility
  put(0, 0, wm$colour, wm$alpha)
  invisible()
}
