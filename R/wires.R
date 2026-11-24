#' Wire-flow parameters
#'
#' Default settings for [render_wires()]: hundreds of thin tube-shaded wires
#' that follow a swirling flow field and drape over a smooth mound, seen from
#' an oblique camera, with a dark sky above the horizon. Pass any setting to
#' override it; unknown names are an error.
#'
#' Main settings:
#' * `palette_mode`: "mono" (shades of `mono`) or "multi" (palette `multi`);
#'   `n_tones`, `mono_L`, `mono_hue_shift`: see [tone_palette()]. The darkest
#'   tone colours the surface under the wires; `bg` is the sky colour
#'   (`NULL` = derived from the palette).
#' * Terrain: `freq` (size of the mound: lower = broader), `contrast`, `gamma`,
#'   `warp`, `h_scale` (height), `nx`, `ny` (surface grid).
#' * Camera: `row_gap` (tilt), `persp` (perspective), `zoom` (close-up factor),
#'   `pan` (sideways shift, fraction of the field).
#' * Wires: `n_wires`, `steps` (length), `step`, `wire_width`, `width_jitter`,
#'   `lift` (gap above the surface), `end_len`, `end_lift` (tapered wire ends
#'   that curl up off the surface), `band_rows` (how finely wires are depth-sorted
#'   against each other).
#' * Flow: `flow_angle` (main direction, radians), `swirl` (how much they bend),
#'   `flow_freq` (size of the swirls), `aniso` (depth component of the flow).
#' * Light and colour: `light`, `surface_level`, `highlight`, `outline`, `fog`,
#'   `region_freq`, `tone_random`.
#' * Shared: `seed`, `aspect`, `W`, `dpi`, `out`, `out_dir`, `suffix`, `watermark`.
#'
#' @param ... Settings to override.
#' @return A named list of settings.
#' @examples
#' wires_params(n_wires = 1500, swirl = 2.4)
#' wires_params(palette_mode = "multi", multi = "ocean")
#' @export
wires_params <- function(...) {
  defaults <- c(
    list(
      palette_mode = "mono", mono = "silver", multi = "ocean",
      n_tones = 5, mono_L = c(30, 86), mono_hue_shift = -6, bg = NULL,
      nx = 160, ny = 160, freq = 0.25, contrast = 0.6, gamma = 1.0, warp = 0.3, h_scale = 70,
      row_gap = 0.62, persp = 0.5, zoom = 1.7, pan = 0,
      n_wires = 1200, steps = 260, step = 1.0, wire_width = 0.25, width_jitter = 0.5, lift = 0.012,
      end_len = 0.12, end_lift = 0, band_rows = 12,
      flow_angle = 0.25, swirl = 1.9, flow_freq = 0.5, aniso = 1.0,
      light = c(-0.50, 0.55, 0.67), surface_level = 0.55, highlight = 0.5, outline = 1.25,
      fog = 0.35, region_freq = 1.2, tone_random = 0.6
    ),
    common_defaults(seed = 8, aspect = "3:2", W = 16)
  )
  merge_params(defaults, list(...), "wires_params")
}

#' Render flowing wires
#'
#' Wires are streamlines of a noise-based flow field, lifted slightly above a
#' smooth height field. The surface is drawn far to near; wire segments hidden
#' behind nearer terrain are removed with a floating-horizon test, and the
#' remaining wires are drawn far to near in depth groups. Each wire is drawn as a
#' dark outline, a body shaded by the surface light and the wire's own
#' orientation (tube shading), and a thin highlight on the side facing the light.
#'
#' @param params Settings from [wires_params()].
#' @inheritParams render_pinfield
#' @return The PNG path, invisibly.
#' @examples
#' \dontrun{
#' render_wires()
#' render_wires(wires_params(palette_mode = "multi", multi = "jade_night"))
#' render_wires(wires_params(aspect = "4:5", W = 10), clean = TRUE)
#' }
#' @export
render_wires <- function(params = wires_params(), clean = FALSE) {
  P   <- apply_clean(params, clean)
  if (P$steps < 4 || P$n_wires < 1 || P$nx < 4 || P$ny < 4 || P$band_rows < 1)
    stop("wires_params(): steps, nx, ny need to be at least 4; n_wires and band_rows at least 1", call. = FALSE)
  pal <- tone_palette(P$palette_mode, P$mono, P$multi, P$n_tones,
                      L_range = P$mono_L, hue_shift = P$mono_hue_shift, chroma_floor = 0.35)
  pal_lab <- decode_colour(pal, to = "lab")
  if (is.null(P$bg)) {                                    # dark sky tinted with the palette's mid hue
    mid <- pal_lab[ceiling(nrow(pal_lab)/2), ]; ch <- sqrt(mid[2]^2 + mid[3]^2) + 1e-9; cc <- min(max(ch*0.5, 6), 14)
    bg  <- encode_colour(matrix(c(9, mid[2]/ch*cc, mid[3]/ch*cc), 1), from = "lab")
  } else bg <- P$bg
  A    <- parse_aspect(P$aspect); H <- P$W / A
  path <- output_path(P, "wire_flow", tone_id(P), clean)
  set.seed(P$seed)

  nx <- P$nx
  ny <- ceiling(P$ny*max(1, 1.5/A))                       # taller frames get more depth rows

  # ---- height field on the grid (bilinear lookup for wires) ----
  height <- function(ix, iy) {
    u  <- ix/nx; v <- iy/nx
    wx <- fracture(gen_simplex, fbm, octaves = 3, x = u*1.5,       y = v*1.5,       seed = P$seed + 1)
    wy <- fracture(gen_simplex, fbm, octaves = 3, x = u*1.5 + 4.1, y = v*1.5 - 2.7, seed = P$seed + 2)
    f  <- fracture(gen_simplex, fbm, octaves = 4, x = (u + P$warp*wx)*P$freq, y = (v + P$warp*wy)*P$freq, seed = P$seed)
    pmin(1, pmax(0, 0.5 + P$contrast*f))^P$gamma
  }
  gx <- rep(seq_len(nx), times = ny); gy <- rep(seq_len(ny), each = nx)
  Hm <- matrix(height(gx, gy), nx, ny)                     # Hm[ix, iy]
  dHx <- (Hm[c(2:nx, nx), ] - Hm[c(1, 1:(nx - 1)), ])/2*P$h_scale
  dHy <- (Hm[, c(2:ny, ny)] - Hm[, c(1, 1:(ny - 1))])/2*P$h_scale
  Ld  <- unit_vec(P$light)
  nlen <- sqrt(dHx^2 + (0.8*dHy)^2 + 1)
  Lam  <- (-dHx*Ld[1] - 0.8*dHy*Ld[2] + Ld[3])/nlen         # surface Lambert; stays an nx x ny matrix
  Lam[Lam < 0] <- 0                                        # (pmax(0, M) would drop the matrix dims)

  bilin <- function(M, x, y) {                            # bilinear lookup, x/y clamped to the grid
    stopifnot(is.matrix(M))
    x <- pmin(nrow(M), pmax(1, x)); y <- pmin(ncol(M), pmax(1, y))
    x0 <- pmin(floor(x), nrow(M) - 1); y0 <- pmin(floor(y), ncol(M) - 1)
    fx <- x - x0; fy <- y - y0
    M[cbind(x0, y0)]*(1 - fx)*(1 - fy) + M[cbind(x0 + 1, y0)]*fx*(1 - fy) +
      M[cbind(x0, y0 + 1)]*(1 - fx)*fy + M[cbind(x0 + 1, y0 + 1)]*fx*fy
  }

  # ---- projection (same oblique camera as the pin-field) ----
  cx   <- (nx + 1)/2 + P$pan*nx
  proj <- function(ix, iy, z) {
    s <- 1/(1 + P$persp*(iy - 1)/ny)
    list(x = cx + (ix - cx)*s, y = iy*P$row_gap*s + z*P$h_scale*s, s = s)
  }
  far_s  <- 1/(1 + P$persp*(ny - 1)/ny)
  half_w <- (nx - 1)/2*far_s*0.98/P$zoom
  xlim   <- c(cx - half_w, cx + half_w)
  S0     <- proj(gx, gy, as.vector(Hm))
  near   <- gy <= 3 & S0$x >= xlim[1] & S0$x <= xlim[2]
  ylim   <- max(S0$y[near]) + c(0, diff(xlim)*H/P$W)      # frame starts at the nearest ridge

  # ---- wires: streamlines of the flow field, traced both ways from random seeds ----
  theta <- function(ix, iy)
    P$flow_angle + P$swirl*fracture(gen_simplex, fbm, octaves = 3, x = ix/nx*P$flow_freq,
                                    y = iy/nx*P$flow_freq, seed = P$seed + 7)
  nw   <- P$n_wires; half <- P$steps %/% 2
  WX <- matrix(NA_real_, nw, 2*half + 1); WY <- WX
  WX[, half + 1] <- stats::runif(nw, 1, nx); WY[, half + 1] <- stats::runif(nw, 1, ny)
  for (dir in c(1, -1)) {
    cols <- if (dir > 0) (half + 2):(2*half + 1) else half:1
    prev <- half + 1
    for (k in cols) {
      t <- theta(WX[, prev], WY[, prev])
      WX[, k] <- WX[, prev] + dir*P$step*cos(t)
      WY[, k] <- WY[, prev] + dir*P$step*sin(t)*P$aniso
      prev <- k
    }
  }
  tone_w <- (1 - P$tone_random)*rank(fracture(gen_simplex, fbm, octaves = 2,
                                              x = WX[, half + 1]/nx*P$region_freq,
                                              y = WY[, half + 1]/nx*P$region_freq, seed = P$seed + 9))/nw +
            P$tone_random*stats::runif(nw)
  ti_w  <- pmin(P$n_tones, 1 + floor(tone_w*P$n_tones))
  wid_w <- P$wire_width*(1 - P$width_jitter/2 + P$width_jitter*stats::runif(nw))

  # segments between consecutive points that stay inside the grid
  m   <- ncol(WX) - 1
  a   <- cbind(as.vector(row(WX)[, 1:m]), as.vector(col(WX)[, 1:m]))
  x0i <- WX[a]; y0i <- WY[a]; x1i <- WX[cbind(a[, 1], a[, 2] + 1)]; y1i <- WY[cbind(a[, 1], a[, 2] + 1)]
  pos <- (a[, 2] - 1)/(m - 1); ef <- pmin(1, pmax(0, 1 - pmin(pos, 1 - pos)/P$end_len))^2   # 1 at the wire ends
  ok  <- x0i >= 1 & x0i <= nx & y0i >= 1 & y0i <= ny & x1i >= 1 & x1i <= nx & y1i >= 1 & y1i <= ny
  wid <- a[ok, 1]; ef <- ef[ok]; x0i <- x0i[ok]; y0i <- y0i[ok]; x1i <- x1i[ok]; y1i <- y1i[ok]
  el <- P$end_lift*ef
  z0 <- bilin(Hm, x0i, y0i) + P$lift + el; z1 <- bilin(Hm, x1i, y1i) + P$lift + el
  p0 <- proj(x0i, y0i, z0); p1 <- proj(x1i, y1i, z1)
  keep <- pmax(p0$x, p1$x) >= xlim[1] - 2 & pmin(p0$x, p1$x) <= xlim[2] + 2 &
          pmax(p0$y, p1$y) >= ylim[1] - 2 & pmin(p0$y, p1$y) <= ylim[2] + 2
  wid <- wid[keep]; ef <- ef[keep]; x0i <- x0i[keep]; y0i <- y0i[keep]; x1i <- x1i[keep]; y1i <- y1i[keep]
  z0 <- z0[keep]; z1 <- z1[keep]
  p0 <- lapply(p0, `[`, keep); p1 <- lapply(p1, `[`, keep)

  # ---- floating horizon: drop segments hidden behind nearer terrain ----
  nb   <- 2000
  bins <- seq(xlim[1], xlim[2], length.out = nb)
  prof <- matrix(-Inf, ny, nb)                             # surface screen height per row and x bin
  for (r in seq_len(ny)) {
    k <- (r - 1)*nx + seq_len(nx)
    prof[r, ] <- stats::approx(S0$x[k], S0$y[k], xout = bins, rule = 1)$y
  }
  prof[is.na(prof)] <- -Inf
  hor <- apply(prof, 2, cummax)                            # hor[r, ] = highest of rows 1..r
  horizon_at <- function(px, iy) {                         # highest terrain in front of a point
    bi <- pmin(nb, pmax(1, round((px - xlim[1])/diff(xlim)*(nb - 1)) + 1))
    rr <- floor(iy)                                        # rows nearer than the point
    hv <- rep(-Inf, length(px)); fr <- rr >= 1
    hv[fr] <- hor[cbind(pmin(rr[fr], ny), bi[fr])]
    hv
  }
  vis <- p0$y >= horizon_at(p0$x, y0i) - 0.1 & p1$y >= horizon_at(p1$x, y1i) - 0.1   # both ends in view
  wid <- wid[vis]; ef <- ef[vis]; x0i <- x0i[vis]; y0i <- y0i[vis]; x1i <- x1i[vis]; y1i <- y1i[vis]
  z0 <- z0[vis]; z1 <- z1[vis]
  p0 <- lapply(p0, `[`, vis); p1 <- lapply(p1, `[`, vis)

  # ---- wire shading: surface light + tube term, fog, highlight side ----
  tx <- x1i - x0i; ty <- y1i - y0i; tz <- (z1 - z0)*P$h_scale
  tl <- sqrt(tx^2 + ty^2 + tz^2) + 1e-9
  tube  <- sqrt(pmax(0, 1 - ((tx*Ld[1] + ty*Ld[2] + tz*Ld[3])/tl)^2))
  lit   <- 0.35*bilin(Lam, x0i, y0i) + 0.65*tube
  depth <- pmin(1, pmax(0, (y0i - 1)/ny))
  lab   <- pal_lab[ti_w[wid], , drop = FALSE]
  bgl   <- decode_colour(bg, to = "lab")[1, ]
  shade <- (0.18 + 0.82*lit)*(1 - P$fog*depth)
  body  <- cbind(lab[, 1]*shade, lab[, 2]*sqrt(shade), lab[, 3]*sqrt(shade))
  hl    <- body; hl[, 1] <- pmin(100, body[, 1] + (100 - body[, 1])*P$highlight*lit^3)
  col_body <- encode_colour(body, from = "lab")
  if (length(col_body) != length(wid) || anyNA(col_body)) stop("render_wires(): wire colours could not be computed", call. = FALSE)
  col_hl   <- encode_colour(hl, from = "lab")
  col_out  <- encode_colour(matrix(c(bgl[1]*0.5, bgl[2], bgl[3]), 1), from = "lab")

  sx <- p1$x - p0$x; sy <- p1$y - p0$y; sl <- sqrt(sx^2 + sy^2) + 1e-9
  nxp <- -sy/sl; nyp <- sx/sl
  sgn <- sign(nxp*Ld[1] + nyp*Ld[2]); sgn[sgn == 0] <- 1
  wdat <- wid_w[wid]*p0$s*(1 - 0.5*ef)                    # wire width in data units, tapered ends
  u2lwd <- (P$W/diff(xlim))*96
  band  <- floor(y0i/P$band_rows)                         # wires are depth-sorted in groups of rows

  # ---- surface strips (darkest tone, Lambert-lit) ----
  sl_lab <- pal_lab[1, ]
  sL     <- sl_lab[1]*P$surface_level*(0.25 + 0.75*as.vector(Lam))*(1 - P$fog*pmin(1, (gy - 1)/ny))
  scol   <- encode_colour(cbind(sL, sl_lab[2]*0.6, sl_lab[3]*0.6), from = "lab")
  quad_x <- function(r) as.vector(rbind(S0$x[(r - 1)*nx + 1:(nx - 1)], S0$x[(r - 1)*nx + 2:nx],
                                        S0$x[r*nx + 2:nx], S0$x[r*nx + 1:(nx - 1)], NA))
  quad_y <- function(r) as.vector(rbind(S0$y[(r - 1)*nx + 1:(nx - 1)], S0$y[(r - 1)*nx + 2:nx],
                                        S0$y[r*nx + 2:nx], S0$y[r*nx + 1:(nx - 1)], NA))
  by_band <- split(seq_along(band), band)

  # ---- draw: surface far -> near, then visible wires far -> near ----
  open_canvas(path, P$W, H, P$dpi, bg, xlim, ylim)
  on.exit(grDevices::dev.off(), add = TRUE)
  draw_wires <- function(ii) {
    if (!length(ii)) return(invisible())
    graphics::segments(p0$x[ii], p0$y[ii], p1$x[ii], p1$y[ii], col = col_out,
                       lwd = wdat[ii]*P$outline*u2lwd, lend = "butt")
    graphics::segments(p0$x[ii], p0$y[ii], p1$x[ii], p1$y[ii], col = col_body[ii],
                       lwd = wdat[ii]*u2lwd, lend = "round")
    off <- 0.22*wdat[ii]*sgn[ii]
    graphics::segments(p0$x[ii] + off*nxp[ii], p0$y[ii] + off*nyp[ii],
                       p1$x[ii] + off*nxp[ii], p1$y[ii] + off*nyp[ii], col = col_hl[ii],
                       lwd = wdat[ii]*0.3*u2lwd, lend = "round")
  }
  for (r in (ny - 1):1) {
    fc <- scol[(r - 1)*nx + 1:(nx - 1)]
    if (r == 1) {                                         # skirt below the nearest row
      graphics::polygon(as.vector(rbind(S0$x[1:(nx - 1)], S0$x[2:nx], S0$x[2:nx], S0$x[1:(nx - 1)], NA)),
                        as.vector(rbind(S0$y[1:(nx - 1)], S0$y[2:nx], ylim[1] - 10, ylim[1] - 10, NA)),
                        col = fc, border = fc, lwd = 0.6)
    }
    graphics::polygon(quad_x(r), quad_y(r), col = fc, border = fc, lwd = 0.6)
  }
  for (b in rev(sort(as.integer(names(by_band))))) draw_wires(by_band[[as.character(b)]])
  draw_watermark(P$watermark, xlim, ylim, H)

  report(path, tone_id(P), P$W, H, P$dpi,
         sprintf("%d wires, %d segments", nw, length(band)))
  invisible(path)
}
