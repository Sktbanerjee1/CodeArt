#' Pin-field parameters
#'
#' Default settings for [render_pinfield()]: a 3D terrain of thin vertical
#' pins seen from an oblique camera. Pass any setting to override it; unknown
#' names are an error.
#'
#' Main settings (see the source for the full list):
#' * `palette`: a name from [PINFIELD_PALETTES].
#' * `nx`, `ny`: pins across / in depth (more = finer, slower).
#' * `sea`: share of dark valley floor; `gamma`: peak sharpness;
#'   `spike`: needle jitter; `ridge_mix`: crystalline crests; `warp`: organic bending.
#' * `hue_freq`, `core_bias`: size of colour zones and strength of the warm centre.
#' * `h_scale`, `row_gap`, `persp`: height, camera tilt, perspective.
#' * `tip_strength`, `fog`, `speck`: white tips, depth fog, coloured specks in valleys.
#' * Shared: `seed`, `aspect`, `W`, `dpi`, `out`, `out_dir`, `suffix`, `watermark`.
#'
#' @param ... Settings to override.
#' @return A named list of settings.
#' @examples
#' pinfield_params(palette = "aurora", seed = 7)
#' @export
pinfield_params <- function(...) {
  defaults <- c(
    list(
      palette = "chromatic",
      nx = 300, ny = 300, n_seg = 4,
      freq = 2.4, warp = 0.45, hue_freq = 1.1, core_bias = 0.9,
      sea = 0.40, gamma = 1.4, ridge_mix = 0.55, spike = 0.9,
      h_scale = 30, row_gap = 0.62, persp = 0.55,
      light = c(-0.55, 0.45, 0.70), tip_strength = 0.85, fog = 0.22,
      speck = 0.05
    ),
    common_defaults(seed = 1024, aspect = "16:9", W = 16)
  )
  merge_params(defaults, list(...), "pinfield_params")
}

#' Render a pin-field terrain
#'
#' @param params Settings from [pinfield_params()].
#' @param clean `TRUE` to render without watermark (file name gets `_clean`).
#' @return The PNG path, invisibly.
#' @examples
#' \dontrun{
#' render_pinfield(pinfield_params(palette = "ember"))
#' render_pinfield(pinfield_params(aspect = "4:5", W = 10), clean = TRUE)
#' }
#' @export
render_pinfield <- function(params = pinfield_params(), clean = FALSE) {
  P <- apply_clean(params, clean)
  if (!P$palette %in% names(PINFIELD_PALETTES))
    stop("palette must be one of: ", paste(names(PINFIELD_PALETTES), collapse = ", "), call. = FALSE)
  pal_ramp  <- PINFIELD_PALETTES[[P$palette]]$ramp
  pal_fixed <- PINFIELD_PALETTES[[P$palette]]$fixed
  H    <- P$W / parse_aspect(P$aspect)
  ny   <- pinfield_rows(P, H)
  path <- output_path(P, "chromatic_pinfield", P$palette, clean)
  set.seed(P$seed)

  # ---- grid + noise fields (scaled by nx: same terrain scale at every aspect) ----
  g <- expand.grid(ix = seq_len(P$nx), iy = seq_len(ny))   # ix varies fastest
  n <- nrow(g); u <- g$ix / P$nx; v <- g$iy / P$nx

  wx <- fracture(gen_simplex, fbm, octaves = 3, x = u*2,       y = v*2,       seed = P$seed + 1)
  wy <- fracture(gen_simplex, fbm, octaves = 3, x = u*2 + 7.3, y = v*2 - 3.1, seed = P$seed + 2)
  uw <- u + P$warp*wx; vw <- v + P$warp*wy

  base  <- fracture(gen_simplex, fbm,    octaves = 6, x = uw*P$freq,     y = vw*P$freq,     seed = P$seed)
  ridge <- fracture(gen_simplex, ridged, octaves = 5, x = uw*P$freq*3,   y = vw*P$freq*3,   seed = P$seed + 3)
  hueN  <- fracture(gen_perlin,  fbm,    octaves = 3, x = uw*P$hue_freq, y = vw*P$hue_freq, seed = P$seed + 4)

  # ---- height field ----
  hb <- pmax(0, normalise(base) - P$sea) / (1 - P$sea)
  h_smooth <- hb^P$gamma
  h <- h_smooth * ((1 - P$ridge_mix) + P$ridge_mix*normalise(ridge))
  h <- h * (1 + P$spike * stats::runif(n)^3)                # sparse tall needles
  h <- h / max(h)

  # ---- Lambert lighting from the smooth field ----
  Hm <- matrix(h_smooth / max(h_smooth), P$nx, ny)
  gx <- as.vector((Hm[c(2:P$nx, P$nx), ] - Hm[c(1, 1:(P$nx - 1)), ]) / 2)
  gy <- as.vector((Hm[, c(2:ny, ny)] - Hm[, c(1, 1:(ny - 1))]) / 2)
  k  <- P$h_scale * 0.35
  Ld <- unit_vec(P$light)
  lambert <- pmax(0, (-k*gx*Ld[1] - k*gy*Ld[2] + Ld[3]) / sqrt((k*gx)^2 + (k*gy)^2 + 1))

  # ---- colour zones (warm core, rank-equalised) ----
  rad   <- sqrt((u - mean(range(u)))^2 + (v - mean(range(v)))^2)
  hue_t <- (rank(normalise(hueN) - P$core_bias*normalise(rad)) - 0.5) / n
  hue_t <- pmin(1, pmax(0, hue_t + stats::rnorm(n, 0, 0.012)))
  ramp  <- grDevices::colorRampPalette(pal_ramp, space = "Lab")(512)
  lab0  <- decode_colour(ramp[1 + round(hue_t*511)], to = "lab")

  # ---- projection ----
  cx <- (P$nx + 1)/2
  s  <- 1 / (1 + P$persp*(g$iy - 1)/ny)
  sx <- cx + (g$ix - cx)*s
  sy <- g$iy * P$row_gap * s
  hp <- pmax(h, 0.012) * P$h_scale * s                      # stubs keep the valley grid visible

  far_s  <- 1 / (1 + P$persp*(ny - 1)/ny)
  half_w <- (P$nx - 1)/2 * far_s * 0.98
  xlim <- c(cx - half_w, cx + half_w)
  ylim <- c(2*P$row_gap, 2*P$row_gap + diff(xlim)*H/P$W)

  # ---- segments (far -> near, base -> tip) ----
  keep <- sx >= xlim[1] - 1 & sx <= xlim[2] + 1 & sy <= ylim[2] & (sy + hp) >= ylim[1]
  idx  <- which(keep)[order(-g$iy[keep], g$ix[keep])]
  S <- P$n_seg; pin <- rep(idx, each = S); kk <- rep(seq_len(S), times = length(idx))
  tm <- (kk - 0.5)/S

  # ---- shading in Lab ----
  jit   <- stats::runif(n)
  lab   <- lab0[pin, , drop = FALSE]
  depth <- (g$iy[pin] - 1)/(ny - 1)
  lab[, 1] <- lab[, 1] * (0.50 + 0.65*lambert[pin]) * (0.38 + 0.62*tm) * (1 - P$fog*depth)

  tip <- matrix(decode_colour(pal_fixed["tip"], to = "lab"), nrow(lab), 3, byrow = TRUE)
  mix <- P$tip_strength * tm^3 * h[pin]^0.6
  lab <- lab*(1 - mix) + tip*mix

  val <- matrix(decode_colour(pal_fixed["valley"], to = "lab"), nrow(lab), 3, byrow = TRUE)
  val[, 1] <- val[, 1] * (0.75 + 0.5*jit[pin]) * (0.6 + 0.6*lambert[pin])
  val <- val*0.85 + lab0[pin, ]*0.15                        # tinted charcoal
  vwt <- (1 - smooth01(0, 0.05, h[pin])) * (jit[pin] > P$speck)   # keep coloured specks
  lab <- lab*(1 - vwt) + val*vwt
  lab[, 1] <- pmin(100, pmax(0, lab[, 1]))
  col <- encode_colour(lab, from = "lab")

  # ---- draw (pin width ~78% of pitch) ----
  open_canvas(path, P$W, H, P$dpi, pal_fixed[["bg"]], xlim, ylim)
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::segments(sx[pin], sy[pin] + hp[pin]*(kk - 1)/S,
                     sx[pin], sy[pin] + hp[pin]*kk/S + 0.02,   # overlap hides seams
                     col = col, lwd = 0.78*s[pin]*(P$W/diff(xlim))*96, lend = "butt")
  draw_watermark(P$watermark, xlim, ylim, H)

  report(path, P$palette, P$W, H, P$dpi, sprintf("%d rows", ny))
  invisible(path)
}

# rows needed so the far edge of the terrain still fills the frame at this aspect
pinfield_rows <- function(P, H) {
  ny <- P$ny
  repeat {
    far_s <- 1 / (1 + P$persp*(ny - 1)/ny)
    top   <- 2*P$row_gap + (P$nx - 1)*far_s*0.98*H/P$W
    if (ny*P$row_gap*far_s >= top*1.02) return(ny)
    ny <- ceiling(ny*1.1)
  }
}
