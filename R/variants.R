#' Render a batch of variants
#'
#' Renders every combination of the values given in `...`, starting from a
#' base parameter list. Settings that are not already part of the automatic
#' file name (anything other than palette, mono, multi, palette_mode and
#' aspect) are added to it as a suffix, so no render overwrites another.
#'
#' @param render_fun One of [render_pinfield()], [render_silk()],
#'   [render_shards()], [render_cubes()], [render_wires()].
#' @param params Base settings from the matching `*_params()` function.
#' @param ... Named vectors of values to vary, e.g. `palette = names(SILK_PALETTES)`.
#' @param clean `TRUE` to render without watermark.
#' @return Character vector of PNG paths, invisibly.
#' @examples
#' \dontrun{
#' render_variants(render_silk, silk_params(), palette = names(SILK_PALETTES))
#' render_variants(render_cubes, cubes_params(), mono = names(MONO_COLOURS), clean = TRUE)
#' render_variants(render_shards, shards_params(), seed = 1:6)
#' }
#' @export
render_variants <- function(render_fun, params, ..., clean = FALSE) {
  vary <- list(...)
  if (!length(vary) || is.null(names(vary)) || any(names(vary) == ""))
    stop("give named vectors to vary, e.g. palette = c(\"nebula\", \"ocean\")", call. = FALSE)
  bad <- setdiff(names(vary), names(params))
  if (length(bad)) stop("unknown parameter(s): ", paste(bad, collapse = ", "), call. = FALSE)

  in_name <- c("palette", "mono", "multi", "palette_mode", "aspect")
  grid  <- expand.grid(vary, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  paths <- character(nrow(grid))
  for (i in seq_len(nrow(grid))) {
    p <- params
    for (key in names(grid)) p[[key]] <- grid[[key]][i]
    extra <- setdiff(names(grid), in_name)
    if (length(extra))
      p$suffix <- paste0(params$suffix, paste0("_", extra, "-", vapply(extra, function(k) as.character(grid[[k]][i]), ""), collapse = ""))
    paths[i] <- render_fun(p, clean = clean)
  }
  invisible(paths)
}
