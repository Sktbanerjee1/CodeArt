# Output device shared by all styles -----------------------------------------

# open a ragg PNG with a full-bleed plotting region in data coordinates;
# the caller closes it with on.exit(grDevices::dev.off(), add = TRUE)
open_canvas <- function(path, W, H, dpi, bg, xlim, ylim) {
  ragg::agg_png(path, width = W, height = H, units = "in", res = dpi, background = bg)
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = bg)
  graphics::plot.new()
  graphics::plot.window(xlim = xlim, ylim = ylim)
  invisible(path)
}

# one-line render summary
report <- function(path, what, W, H, dpi, extra = "") {
  message(sprintf("saved %s  (%s, %d x %d px%s)", path, what,
                  round(W*dpi), round(H*dpi), if (nzchar(extra)) paste0(", ", extra) else ""))
}
