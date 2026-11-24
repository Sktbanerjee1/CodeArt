#' Silk-flow parameters
#'
#' Default settings for [render_silk()]: layered colour ribbons with crisp
#' folds, long soft fades, cast shadows, rim light, bloom and film grain.
#' Pass any setting to override it; unknown names are an error.
#'
#' Main settings:
#' * `palette`: a name from [SILK_PALETTES].
#' * `n_sheets`, `flow_angle`, `angle_jitter`, `amp`, `wave_freq`: number,
#'   direction and waviness of the ribbons; `warp`, `warp_freq`: organic bending.
#' * `width`, `edge`, `opacity`: fade length, edge softness (each ribbon gets
#'   1-30x `edge`) and opacity.
#' * `hue_span`, `hue_noise`, `bg_mix`: colour spread along a ribbon and background tint.
#' * `depth_dark`, `rim`, `shadow`, `shadow_w`, `bloom`, `bloom_r`, `vignette`, `grain`: light.
#' * `compute_px`: long side of the internal raster (output is upsampled smoothly).
#' * Shared: `seed`, `aspect`, `W`, `dpi`, `out`, `out_dir`, `suffix`, `watermark`.
#'
#' @param ... Settings to override.
#' @return A named list of settings.
#' @examples
#' silk_params(palette = "ocean", n_sheets = 5)
#' @export
silk_params <- function(...) {
  defaults <- c(
    list(
      palette = "nebula",
      compute_px = 2000,
      n_sheets = 4, flow_angle = -0.35, angle_jitter = 0.35,
      amp = 0.16, wave_freq = c(0.20, 0.55),
      warp = 0.06, warp_freq = 0.6,
      width = c(0.18, 0.50), edge = 0.003, opacity = 0.95,
      hue_span = 0.45, hue_noise = 0.08, bg_mix = 0.22,
      depth_dark = 0.40, rim = 6, shadow = 0.50, shadow_w = 0.08,
      bloom = 0.35, bloom_r = 0.025, vignette = 0.30, grain = 0.010
    ),
    common_defaults(seed = 2025, aspect = "16:10", W = 16)
  )
  merge_params(defaults, list(...), "silk_params")
}

#' Render silk ribbons
#'
#' @param params Settings from [silk_params()].
#' @inheritParams render_pinfield
#' @return The PNG path, invisibly.
#' @examples
#' \dontrun{
#' render_silk(silk_params(palette = "sunset"))
#' render_silk(silk_params(aspect = "9:16", W = 9), clean = TRUE)
#' }
#' @export
render_silk <- function(params = silk_params(), clean = FALSE) {
  P <- apply_clean(params, clean)
  if (!P$palette %in% names(SILK_PALETTES))
    stop("palette must be one of: ", paste(names(SILK_PALETTES), collapse = ", "), call. = FALSE)
  pal  <- SILK_PALETTES[[P$palette]]
  A    <- parse_aspect(P$aspect); H <- P$W / A
  path <- output_path(P, "silk_flow", P$palette, clean)
  set.seed(P$seed)
  S <- silk_sheets(P)

  # ---- pixel grid (image-height units, origin at centre, y up) ----
  d <- raster_dims(A, P$compute_px); w <- d[["w"]]; h <- d[["h"]]
  r  <- rep(seq_len(h), times = w); cc <- rep(seq_len(w), each = h)   # row fastest = raster order
  X  <- (cc - 0.5)/h - A/2
  Y  <- 0.5 - (r - 0.5)/h

  # shared warp + colour noise
  wX <- fracture(gen_simplex, fbm, octaves = 3, x = X*P$warp_freq,       y = Y*P$warp_freq,       seed = P$seed + 1)
  wY <- fracture(gen_simplex, fbm, octaves = 3, x = X*P$warp_freq + 5.2, y = Y*P$warp_freq - 1.7, seed = P$seed + 2)
  Xw <- X + P$warp*wX; Yw <- Y + P$warp*wY
  hueN <- fracture(gen_perlin, fbm, octaves = 3, x = X*1.3, y = Y*1.3, seed = P$seed + 3)
  bgN  <- normalise(fracture(gen_simplex, fbm, octaves = 2, x = X*0.8, y = Y*0.8, seed = P$seed + 4))

  lab_tab <- decode_colour(grDevices::colorRampPalette(pal$ramp, space = "Lab")(1024), to = "lab")

  # ---- background: deep tone with a faint tint of the first ramp colour ----
  bg   <- decode_colour(pal$bg, to = "lab")[1, ]
  tint <- bg*(1 - P$bg_mix) + lab_tab[1, ]*P$bg_mix
  cL <- bg[1] + (tint[1] - bg[1])*bgN
  ca <- bg[2] + (tint[2] - bg[2])*bgN
  cb <- bg[3] + (tint[3] - bg[3])*bgN

  # ---- ribbons, bottom -> top ----
  for (k in seq_len(nrow(S))) {
    sk <- S[k, ]
    xr <-  Xw*cos(sk$theta) + Yw*sin(sk$theta)
    yr <- -Xw*sin(sk$theta) + Yw*cos(sk$theta)
    curve <- sk$c0 + sk$a1*sin(2*pi*sk$f1*xr + sk$p1) + sk$a2*sin(2*pi*sk$f2*xr + sk$p2)
    dd <- sk$side*(yr - curve)                         # signed distance, > 0 inside the ribbon

    # cast shadow on everything below, just outside the edge
    sh <- P$shadow*exp(-pmax(-dd, 0)/P$shadow_w)*(dd < 0)
    cL <- cL*(1 - sh); ca <- ca*(1 - 0.5*sh); cb <- cb*(1 - 0.5*sh)

    # coverage: sharp edge, long Gaussian fade into the body
    a <- P$opacity*smooth01(-sk$edge, sk$edge, dd)*exp(-(pmax(dd, 0)/sk$width)^2)

    # colour runs along the ribbon, shifts slightly with depth
    xn <- (xr - min(xr))/diff(range(xr))
    if (sk$dir < 0) xn <- 1 - xn
    tt <- pmin(1, pmax(0, sk$t0 + P$hue_span*xn + P$hue_noise*hueN + 0.12*pmin(pmax(dd, 0)/sk$width, 1)))
    col <- lab_tab[1 + round(tt*1023), , drop = FALSE]

    # brightest at the fold, fading toward the background tone (hue-shifted, so
    # orange darkens to plum instead of brown), plus a thin rim light
    lit <- (1 - P$depth_dark) + P$depth_dark*exp(-pmax(dd, 0)/(0.5*sk$width))
    cL <- cL*(1 - a) + (bg[1]*(1 - lit) + col[, 1]*lit + P$rim*exp(-(dd/(3*sk$edge + 0.004))^2))*a
    ca <- ca*(1 - a) + (bg[2]*(1 - lit) + col[, 2]*lit)*a
    cb <- cb*(1 - a) + (bg[3]*(1 - lit) + col[, 3]*lit)*a
  }

  # ---- bloom, vignette ----
  if (P$bloom > 0) {
    rb <- max(1, round(P$bloom_r*h))
    B  <- box_blur(box_blur(matrix(pmax(cL - 55, 0), h, w), rb), rb)
    cL <- cL + P$bloom*as.vector(B)
  }
  rr <- ((X/(A/2))^2 + (Y/0.5)^2)/2
  cL <- cL*(1 - P$vignette*rr)

  # ---- to sRGB, grain, raster ----
  rgbm <- convert_colour(cbind(pmin(100, pmax(0, cL)), ca, cb), from = "lab", to = "rgb")
  rgbm <- rgbm + stats::runif(length(rgbm), -1, 1)*P$grain*255
  img  <- array(pmin(255, pmax(0, rgbm))/255, dim = c(h, w, 3))

  open_canvas(path, P$W, H, P$dpi, pal$bg, c(0, A), c(0, 1))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::rasterImage(img, 0, 0, A, 1, interpolate = TRUE)
  draw_watermark(P$watermark, c(0, A), c(0, 1), H)

  report(path, P$palette, P$W, H, P$dpi, sprintf("compute %d x %d", w, h))
  invisible(path)
}

# random ribbon geometry, one row per ribbon (drawn bottom -> top)
silk_sheets <- function(P) {
  K <- P$n_sheets
  slot <- (sample(K) - 0.5)/K                          # stratified positions across the frame
  data.frame(
    theta = P$flow_angle + stats::runif(K, -P$angle_jitter, P$angle_jitter),
    c0    = (slot - 0.5)*0.9 + stats::runif(K, -0.05, 0.05),
    a1 = P$amp*stats::runif(K, 0.6, 1.0),  f1 = stats::runif(K, P$wave_freq[1], P$wave_freq[2]),   p1 = stats::runif(K, 0, 2*pi),
    a2 = P$amp*stats::runif(K, 0.15, 0.4), f2 = stats::runif(K, P$wave_freq[2], 2*P$wave_freq[2]), p2 = stats::runif(K, 0, 2*pi),
    side  = sample(c(-1, 1), K, replace = TRUE),       # which side of the edge the ribbon body lies on
    width = stats::runif(K, P$width[1], P$width[2]),
    edge  = P$edge*10^stats::runif(K, 0, 1.5),         # mix of crisp and blurred folds
    t0    = sort(stats::runif(K, 0, 1 - P$hue_span)),  # upper ribbons take warmer parts of the ramp
    dir   = sample(c(-1, 1), K, replace = TRUE)
  )
}
