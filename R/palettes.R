#' Palette presets
#'
#' * `PINFIELD_PALETTES`: ramps for [render_pinfield()], each a list with
#'   `ramp` (cool edge -> warm core) and `fixed` (`bg`, `valley`, `tip`).
#' * `SILK_PALETTES`: ramps for [render_silk()], each a list with `bg` and `ramp`.
#' * `MONO_COLOURS`: base colours for single-colour shades ([tone_palette()],
#'   used by [render_shards()] and [render_cubes()]).
#' * `MULTI_PALETTES`: multi-colour palettes for [render_shards()] and
#'   [render_cubes()]; sorted dark -> light automatically.
#'
#' All ramps step through neighbouring hues only (no muddy complements) and
#' are interpolated in CIE Lab.
#' @name palettes
NULL

#' @rdname palettes
#' @export
PINFIELD_PALETTES <- list(
  # full-spectrum blue -> violet -> peach -> red
  chromatic = list(
    ramp  = c(navy = "#1A2766", cobalt = "#2D50D2", sky = "#74A3EE", lavender = "#9C7FD8",
              orchid = "#E28CC0", peach = "#FBD4A2", tangerine = "#F7913E",
              coral = "#F36B5E", vermilion = "#DD2E30"),
    fixed = c(bg = "#17161D", valley = "#3A3742", tip = "#F8F5EF")
  ),
  # cool: deep navy -> teal -> mint -> violet -> magenta
  aurora = list(
    ramp  = c(midnight = "#14215A", petrol = "#155E75", teal = "#1FA39A", mint = "#86E3C3",
              ice = "#B9D8F5", lavender = "#9A86E6", violet = "#6B3FC8",
              magenta = "#B23AAE", fuchsia = "#EE5A9E"),
    fixed = c(bg = "#0D1118", valley = "#2A323C", tip = "#F2FFFB")
  ),
  # warm: plum -> wine -> crimson -> orange -> gold (glowing core)
  ember = list(
    ramp  = c(plum = "#3E1446", wine = "#7B1E4E", crimson = "#B9253F", vermilion = "#E2462E",
              tangerine = "#F57D2A", amber = "#F8AE3B", saffron = "#FAD25C"),
    fixed = c(bg = "#151011", valley = "#352C2A", tip = "#FFF7EA")
  )
)

#' @rdname palettes
#' @export
SILK_PALETTES <- list(
  nebula    = list(bg = "#060828", ramp = c("#3B2BD9", "#7424E0", "#B41FC8", "#E0286E", "#EB5E2F", "#EE9416")),
  aurora    = list(bg = "#03121A", ramp = c("#2A2A9E", "#1F6FC2", "#14B3B0", "#3EDC8F", "#C8F27A")),
  sunset    = list(bg = "#170A22", ramp = c("#3D2A8A", "#B23A8C", "#F2546B", "#FF8C5A", "#FFC46B", "#FFE6A3")),
  ocean     = list(bg = "#020A1C", ramp = c("#0A2A6B", "#0E54A8", "#1F8FD6", "#39C5E8", "#9AF0F5")),
  ember     = list(bg = "#110405", ramp = c("#5A0A1E", "#A3122A", "#E0301E", "#FF6A13", "#FFA51F", "#FFD86B")),
  orchid    = list(bg = "#1E1633", ramp = c("#6B5BD6", "#A47CF0", "#E58BD8", "#FFB3C7", "#FFE1D6")),
  citrus    = list(bg = "#06140F", ramp = c("#0F6B4F", "#2FB36B", "#B6E04A", "#FFE14D", "#FF9F1C")),
  rose_gold = list(bg = "#1A0D12", ramp = c("#5B2C3F", "#A8546A", "#E39A8F", "#F3C6A5", "#FFF0DC")),
  neon      = list(bg = "#05010F", ramp = c("#00E0FF", "#2D7BFF", "#8A2BFF", "#FF2BD6", "#FF5E7E")),
  graphite  = list(bg = "#07080B", ramp = c("#2B2F3A", "#565D6E", "#8F97A8", "#C9CFDA", "#F2F4F7"))
)

#' @rdname palettes
#' @export
MONO_COLOURS <- c(
  steel_teal = "#3E8A99",
  teal       = "#2ED3B0",
  emerald    = "#2FCB6B",
  cobalt     = "#3B7BFF",
  violet     = "#9B6BFF",
  crimson    = "#F0435A",
  amber      = "#FFB020",
  rose       = "#FF7EB6",
  slate      = "#9FB4C8"
)

#' @rdname palettes
#' @export
MULTI_PALETTES <- list(
  jade_night   = c("#0E1B2A", "#14394A", "#1B6170", "#21A39A", "#7FE8C8"),
  tropical     = c("#13293D", "#006D77", "#2BB3A3", "#F4D35E", "#EE964B"),
  sunset       = c("#2B1B3D", "#5C2A5E", "#B23A6E", "#F2725B", "#FFC07A"),
  ocean        = c("#071A2E", "#0B3B66", "#1565A8", "#2E9BD6", "#9ADCF2"),
  forest       = c("#0F1F17", "#1E3B2A", "#2F6B45", "#5FA35E", "#B8D98A"),
  ember        = c("#1A0A0A", "#4A1212", "#8C1F1A", "#D9481C", "#FFA43A"),
  berry        = c("#1C0F24", "#3E1A4D", "#7A2E7E", "#C2509A", "#F29BC4"),
  desert       = c("#2A1E1A", "#5A3A2B", "#9C6B48", "#D4A373", "#F1DDBF"),
  neon         = c("#0B0620", "#24107A", "#7A2BE2", "#FF2E97", "#2EF2E0"),
  slate_accent = c("#111315", "#24282C", "#3A4046", "#5C646C", "#2ED3B0")
)

#' Flat tone palette: shades of one colour or a multi-colour palette
#'
#' Mono mode spreads `n` shades of one base colour along constant hue in CIE
#' LCh: lightness runs over `L_range`, chroma is lowest in the darks and the
#' darkest shade is turned by `hue_shift` degrees (cooler shadows). Multi mode
#' sorts a palette dark -> light and interpolates it in Lab to `n` colours.
#'
#' @param mode "mono" or "multi".
#' @param mono A name from [MONO_COLOURS] or any hex colour.
#' @param multi A name from [MULTI_PALETTES].
#' @param n Number of tones.
#' @param L_range Lightness range (CIE L*) of mono shades, dark -> light.
#' @param hue_shift Hue shift of the darkest mono shade, in degrees.
#' @param chroma_floor Chroma of the darkest mono shade relative to the base colour.
#' @return Character vector of `n` hex colours, dark -> light.
#' @examples
#' tone_palette("mono", "cobalt", n = 6)
#' tone_palette("multi", multi = "tropical", n = 5)
#' @export
tone_palette <- function(mode = c("mono", "multi"), mono = "steel_teal", multi = "sunset", n = 5,
                         L_range = c(18, 58), hue_shift = -8, chroma_floor = 0.35) {
  mode <- match.arg(mode)
  if (mode == "mono") {
    base <- if (mono %in% names(MONO_COLOURS)) MONO_COLOURS[[mono]] else mono
    lch  <- decode_colour(base, to = "lch")[1, ]
    f    <- seq(0, 1, length.out = n)
    L    <- L_range[1] + f*diff(L_range)
    C    <- lch[2]*(chroma_floor + (1 - chroma_floor)*f^0.8)
    h    <- (lch[3] + hue_shift*(1 - f)) %% 360
    return(encode_colour(cbind(L, C, h), from = "lch"))
  }
  if (!multi %in% names(MULTI_PALETTES))
    stop("multi must be one of: ", paste(names(MULTI_PALETTES), collapse = ", "), call. = FALSE)
  cols <- MULTI_PALETTES[[multi]]
  cols <- cols[order(decode_colour(cols, to = "lab")[, 1])]
  grDevices::colorRampPalette(cols, space = "Lab")(n)
}

# palette label used in file names
tone_id <- function(P) {
  if (P$palette_mode == "mono") paste0("mono-", sub("#", "", P$mono, fixed = TRUE)) else P$multi
}

#' List palette names by style
#' @return A named list of character vectors.
#' @examples
#' list_palettes()
#' @export
list_palettes <- function() {
  list(pinfield = names(PINFIELD_PALETTES),
       silk     = names(SILK_PALETTES),
       mono     = names(MONO_COLOURS),
       multi    = names(MULTI_PALETTES))
}

#' Show colours as labelled swatches
#'
#' Draws on the current graphics device.
#' @param colours Character vector of colours; names are used as labels.
#' @param labels Labels under each swatch.
#' @return `colours`, invisibly.
#' @examples
#' preview_colours(tone_palette("mono", "violet", n = 6))
#' preview_colours(SILK_PALETTES$nebula$ramp)
#' @export
preview_colours <- function(colours, labels = if (is.null(names(colours))) colours else names(colours)) {
  n  <- length(colours)
  op <- graphics::par(mar = c(3, 0.5, 0.5, 0.5))
  on.exit(graphics::par(op), add = TRUE)
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, n), ylim = c(0, 1))
  graphics::rect(seq_len(n) - 1, 0, seq_len(n), 1, col = colours, border = NA)
  graphics::axis(1, at = seq_len(n) - 0.5, labels = labels, tick = FALSE, las = 2, cex.axis = 0.7)
  invisible(colours)
}
