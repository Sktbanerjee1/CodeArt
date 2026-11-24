#' Watermark settings
#'
#' Two stacked lines in a corner: a bold label (default "CodeArt") above a
#' caption (default the project address, https://github.com/Sktbanerjee1/CodeArt).
#' Sizes are fractions of the image height, so the watermark scales with any
#' aspect ratio.
#'
#' Ways to render without a watermark:
#' * one image: `render_silk(silk_params(), clean = TRUE)` (file name gets `_clean`);
#' * one recipe: `silk_params(watermark = watermark_params(enabled = FALSE))`;
#' * every render in the session: `set_watermark(FALSE)` (see [set_watermark()]).
#'
#' @param enabled Draw the watermark. Default: `getOption("codeart.watermark", TRUE)`.
#' @param caption Second line. Default: `getOption("codeart.caption")`, else the project URL.
#' @param label Bold first line. Default: `getOption("codeart.label", "CodeArt")`.
#' @param position One of "bottom-right", "bottom-left", "top-right", "top-left".
#' @param size Font size as a fraction of image height.
#' @param margin Distance from the edges as a fraction of image height.
#' @param colour Text colour.
#' @param alpha Text opacity (a soft drop shadow is added automatically).
#' @param family Font family.
#' @param name Deprecated alias for `caption`, kept so older scripts still work.
#' @return A list, used as the `watermark` element of any `*_params()`.
#' @examples
#' watermark_params(position = "top-left", alpha = 0.6)
#' watermark_params(caption = "Your Name")
#' watermark_params(enabled = FALSE)
#' @export
watermark_params <- function(enabled = getOption("codeart.watermark", TRUE),
                             caption = getOption("codeart.caption", "https://github.com/Sktbanerjee1/CodeArt"),
                             label = getOption("codeart.label", "CodeArt"),
                             position = c("bottom-right", "bottom-left", "top-right", "top-left"),
                             size = 0.020, margin = 0.025,
                             colour = "#FFFFFF", alpha = 0.80, family = "sans",
                             name = NULL) {
  position <- match.arg(position)
  if (!is.null(name)) caption <- name
  list(enabled = isTRUE(enabled), caption = caption, label = label, position = position,
       size = size, margin = margin, colour = colour, alpha = alpha, family = family)
}

#' Session-wide watermark defaults
#'
#' Sets the watermark used by every `*_params()` call that follows, for the
#' rest of the R session. Call it before building your recipes.
#'
#' @param enabled `FALSE` renders everything without a watermark.
#' @param caption Second line, e.g. your name or website. `NULL` keeps the current value.
#' @param label Bold first line. `NULL` keeps the current value.
#' @return The previous options, invisibly (pass them to `options()` to restore).
#' @examples
#' old <- set_watermark(FALSE)          # no watermark from now on
#' p <- silk_params()
#' p$watermark$enabled                  # FALSE
#' options(old)                         # back to the previous setting
#'
#' old <- set_watermark(TRUE, caption = "Your Name")
#' options(old)
#' @export
set_watermark <- function(enabled = TRUE, caption = NULL, label = NULL) {
  new <- list(codeart.watermark = isTRUE(enabled))
  if (!is.null(caption)) new$codeart.caption <- caption
  if (!is.null(label))   new$codeart.label   <- label
  invisible(options(new))
}

# draw on the open canvas; xlim/ylim are the canvas data limits, H the image height in inches
draw_watermark <- function(wm, xlim, ylim, H) {
  if (!isTRUE(wm$enabled)) return(invisible())
  caption <- if (!is.null(wm$caption)) wm$caption else wm$name    # lists built before 'caption' existed
  dy    <- diff(ylim)
  right <- grepl("right", wm$position)
  top   <- grepl("top", wm$position)
  m     <- wm$margin*dy
  gap   <- 1.35*wm$size*dy
  x     <- if (right) xlim[2] - m else xlim[1] + m
  hj    <- if (right) 1 else 0
  vj    <- if (top) 1 else 0
  y_label <- if (top) ylim[2] - m else ylim[1] + m + gap
  y_cap   <- if (top) ylim[2] - m - gap else ylim[1] + m
  cex <- wm$size*H*72/graphics::par("ps")       # font size = size * image height
  sh  <- 0.0012*dy
  put <- function(ox, oy, col, a) {
    graphics::text(x + ox, y_label + oy, wm$label, adj = c(hj, vj), font = 2, cex = cex*1.15,
                   col = grDevices::adjustcolor(col, a), family = wm$family)
    graphics::text(x + ox, y_cap + oy, caption, adj = c(hj, vj), font = 1, cex = cex*0.80,
                   col = grDevices::adjustcolor(col, a), family = wm$family)
  }
  put(sh, -sh, "#000000", wm$alpha*0.6)          # soft drop shadow for legibility
  put(0, 0, wm$colour, wm$alpha)
  invisible()
}
