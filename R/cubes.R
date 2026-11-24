#' Iso-cube parameters
#'
#' Default settings for [render_cubes()]: a wall of glossy rounded boxes
#' stacked as a 3D staircase and seen at a corner angle (horizontal highlighted
#' ledges, tall faces, dark side faces, deep crevices). Pass any setting to
#' override it; unknown names are an error.
#'
#' Main settings:
#' * `palette_mode`: "mono" (shades of `mono`) or "multi" (palette `multi`);
#'   `n_tones`, `mono_L`, `mono_hue_shift`: see [tone_palette()].
#' * `cube_size`, `elongation`: ledge length (fraction of image height) and
#'   relative length of the other edges (1 = true cubes).
#' * `yaw`, `pitch`, `spacing`, `bevel`, `depth_fall`, `search`: rotation,
#'   gaps, edge rounding, crevice darkness, neighbour search radius.
#' * `region_freq`, `tone_random`: colour regions vs random colour per box.
#' * `key`, `fill`, `*_strength`, `ambient`, `specular`, `shininess`,
#'   `spec_tint`, `spot_*`, `grain`: lighting.
#' * `compute_px`: long side of the internal raster (RAM grows with its square).
#' * Shared: `seed`, `aspect`, `W`, `dpi`, `out`, `out_dir`, `suffix`, `watermark`.
#'
#' @param ... Settings to override.
#' @return A named list of settings.
#' @examples
#' cubes_params(mono = "violet", spacing = 1.06)
#' @export
cubes_params <- function(...) {
  defaults <- c(
    list(
      palette_mode = "mono", mono = "steel_teal", multi = "sunset",
      n_tones = 5, mono_L = c(18, 58), mono_hue_shift = -8,
      compute_px = 2000,
      cube_size = 0.16, elongation = 1.7, yaw = 0, pitch = 0, spacing = 1,
      bevel = 0.20, depth_fall = 1.0, search = 1,
      region_freq = 1.2, tone_random = 0.5,
      key = c(-0.80, 0.35, 0.50), key_strength = 1.00,
      fill = c(0.60, -0.20, 0.77), fill_strength = 0.20,
      ambient = 0.04, specular = 0.40, shininess = 16, spec_tint = 0.25,
      spot_x = 0, spot_y = 0, spot_r = 0.42, spot_floor = 0.20,
      grain = 0.010
    ),
    common_defaults(seed = 1, aspect = "3:2", W = 16)
  )
  merge_params(defaults, list(...), "cubes_params")
}

#' Render a wall of rounded cubes
#'
#' Each pixel casts an orthographic ray into the box lattice (analytic box
#' intersection); rounded edges are shaded with bent normals; lighting is done
#' in linear RGB.
#'
#' @param params Settings from [cubes_params()].
#' @inheritParams render_pinfield
#' @return The PNG path, invisibly.
#' @examples
#' \dontrun{
#' render_cubes(cubes_params(palette_mode = "multi", multi = "ocean"))
#' render_cubes(cubes_params(elongation = 1), clean = TRUE)   # classic tumbling blocks
#' }
#' @export
render_cubes <- function(params = cubes_params(), clean = FALSE) {
  P   <- apply_clean(params, clean)
  pal <- tone_palette(P$palette_mode, P$mono, P$multi, P$n_tones,
                      L_range = P$mono_L, hue_shift = P$mono_hue_shift, chroma_floor = 0.35)
  n_t  <- length(pal)
  A    <- parse_aspect(P$aspect); H <- P$W / A
  path <- output_path(P, "iso_cubes", tone_id(P), clean)
  set.seed(P$seed)

  # ---- pixel grid (image-height units, origin at centre, y up) ----
  d <- raster_dims(A, P$compute_px); w <- d[["w"]]; h <- d[["h"]]
  X <- (rep(seq_len(w), each = h) - 0.5)/h - A/2             # row fastest = raster order
  Y <- 0.5 - (rep(seq_len(h), times = w) - 0.5)/h
  N <- length(X)

  # ---- box orientation: corner view (ledge horizontal) + optional yaw/pitch ----
  r6 <- sqrt(1/6); r2 <- sqrt(1/2); r3 <- sqrt(1/3)
  Riso <- matrix(c(sqrt(2/3), 0, r3,  -r6, r2, r3,  -r6, -r2, r3), 3, 3)   # columns = box axes
  yw <- P$yaw*pi/180; pt <- P$pitch*pi/180
  Ry <- matrix(c(cos(yw), 0, -sin(yw),  0, 1, 0,  sin(yw), 0, cos(yw)), 3, 3)
  Rx <- matrix(c(1, 0, 0,  0, cos(pt), sin(pt),  0, -sin(pt), cos(pt)), 3, 3)
  R  <- Ry %*% Rx %*% Riso
  hv <- P$cube_size/2*c(1, P$elongation, P$elongation)        # box half-sizes along its own axes

  # staircase lattice: neighbours step one box along one axis and back along another
  B1 <- P$spacing*(2*hv[1]*R[, 1] - 2*hv[3]*R[, 3])
  B2 <- P$spacing*(2*hv[2]*R[, 2] - 2*hv[3]*R[, 3])
  Mi <- solve(cbind(B1[1:2], B2[1:2]))
  i0 <- round(Mi[1, 1]*X + Mi[1, 2]*Y)
  j0 <- round(Mi[2, 1]*X + Mi[2, 2]*Y)

  # ---- ray cast: nearest box hit among neighbouring lattice cells ----
  dl <- -R[3, ]; dl[abs(dl) < 1e-9] <- 1e-9                  # view ray (0,0,-1) in box coordinates
  z0 <- 100
  best <- rep(Inf, N); bax <- integer(N); bi <- numeric(N); bj <- numeric(N); btc <- numeric(N)
  bp1 <- numeric(N); bp2 <- numeric(N); bp3 <- numeric(N)
  for (di in -P$search:P$search) for (dj in -P$search:P$search) {
    ci <- i0 + di; cj <- j0 + dj
    ox <- X - (ci*B1[1] + cj*B2[1]); oy <- Y - (ci*B1[2] + cj*B2[2]); oz <- z0 - (ci*B1[3] + cj*B2[3])
    o1 <- R[1, 1]*ox + R[2, 1]*oy + R[3, 1]*oz                # ray origin in box coordinates
    o2 <- R[1, 2]*ox + R[2, 2]*oy + R[3, 2]*oz
    o3 <- R[1, 3]*ox + R[2, 3]*oy + R[3, 3]*oz
    ta <- (-hv[1] - o1)/dl[1]; tb <- (hv[1] - o1)/dl[1]; tn1 <- pmin(ta, tb); tf1 <- pmax(ta, tb)
    ta <- (-hv[2] - o2)/dl[2]; tb <- (hv[2] - o2)/dl[2]; tn2 <- pmin(ta, tb); tf2 <- pmax(ta, tb)
    ta <- (-hv[3] - o3)/dl[3]; tb <- (hv[3] - o3)/dl[3]; tn3 <- pmin(ta, tb); tf3 <- pmax(ta, tb)
    tmin <- pmax(tn1, tn2, tn3); tmax <- pmin(tf1, tf2, tf3)
    hit  <- which(tmin <= tmax & tmin < best)
    if (!length(hit)) next
    best[hit] <- tmin[hit]
    bax[hit]  <- ifelse(tn1[hit] >= tn2[hit] & tn1[hit] >= tn3[hit], 1L, ifelse(tn2[hit] >= tn3[hit], 2L, 3L))
    bi[hit] <- ci[hit]; bj[hit] <- cj[hit]
    btc[hit] <- z0 - (ci[hit]*B1[3] + cj[hit]*B2[3]) - sum(hv*abs(R[3, ]))   # ray depth of the near corner
    bp1[hit] <- o1[hit] + tmin[hit]*dl[1]; bp2[hit] <- o2[hit] + tmin[hit]*dl[2]; bp3[hit] <- o3[hit] + tmin[hit]*dl[3]
  }
  rm(ox, oy, oz, o1, o2, o3, ta, tb, tn1, tn2, tn3, tf1, tf2, tf3, tmin, tmax, i0, j0)
  hitm <- is.finite(best)

  # ---- normals with rounded edges (bent toward the neighbouring face) ----
  rb <- P$bevel*P$cube_size
  bp <- list(bp1, bp2, bp3)
  nx <- numeric(N); ny <- numeric(N); nz <- numeric(N)
  for (k in 1:3) {
    ii <- which(hitm & bax == k)
    if (!length(ii)) next
    v <- list(numeric(length(ii)), numeric(length(ii)), numeric(length(ii)))
    v[[k]] <- rep(-sign(dl[k]), length(ii))
    for (j in setdiff(1:3, k)) {
      pj <- bp[[j]][ii]
      s  <- pmin(1, pmax(0, 1 - (hv[j] - abs(pj))/rb))
      v[[j]] <- v[[j]] + tan(asin(s*sqrt(0.5)))*sign(pj)     # 0 inside the face -> 45 deg at the edge
    }
    len <- sqrt(v[[1]]^2 + v[[2]]^2 + v[[3]]^2)
    nx[ii] <- (R[1, 1]*v[[1]] + R[1, 2]*v[[2]] + R[1, 3]*v[[3]])/len
    ny[ii] <- (R[2, 1]*v[[1]] + R[2, 2]*v[[2]] + R[2, 3]*v[[3]])/len
    nz[ii] <- (R[3, 1]*v[[1]] + R[3, 2]*v[[2]] + R[3, 3]*v[[3]])/len
  }
  rm(bp, bp1, bp2, bp3, bax)
  dk <- ifelse(hitm, exp(-P$depth_fall*(best - btc)/P$cube_size), 0)   # crevice darkening
  rm(best, btc)

  # ---- one colour per box: light/dark regions + per-box randomness ----
  key <- ifelse(hitm, bi*100003 + bj, NA)
  uk  <- unique(key[hitm]); idx <- match(key, uk); first <- match(uk, key)
  ui  <- bi[first]; uj <- bj[first]
  reg <- fracture(gen_simplex, fbm, octaves = 3,
                  x = (ui*B1[1] + uj*B2[1])*P$region_freq, y = (ui*B1[2] + uj*B2[2])*P$region_freq, seed = P$seed)
  tt  <- (1 - P$tone_random)*rank(reg)/length(uk) + P$tone_random*stats::runif(length(uk))
  ti  <- pmin(n_t, 1 + floor(tt*n_t))[idx]
  pal_lin <- to_linear(decode_colour(pal, to = "rgb")/255)
  rm(key, bi, bj)

  # ---- lighting in linear RGB ----
  Lk <- unit_vec(P$key); Lf <- unit_vec(P$fill); Hh <- unit_vec(Lk + c(0, 0, 1))
  dif  <- pmax(0, nx*Lk[1] + ny*Lk[2] + nz*Lk[3])
  fil  <- pmax(0, nx*Lf[1] + ny*Lf[2] + nz*Lf[3])
  spec <- pmax(0, nx*Hh[1] + ny*Hh[2] + nz*Hh[3])^P$shininess
  spot <- P$spot_floor + (1 - P$spot_floor)*exp(-((X - P$spot_x)^2 + (Y - P$spot_y)^2)/(2*P$spot_r^2))
  lightf <- (P$ambient + (P$key_strength*dif + P$fill_strength*fil)*spot)*dk
  sp     <- P$specular*spec*spot*dk
  rm(nx, ny, nz, dif, fil, spec, dk)

  bg_lin <- pal_lin[1, ]*0.08                                # background where no box is hit
  rgbm <- sapply(1:3, function(j) {
    alb <- pal_lin[ti, j]
    v <- alb*lightf + sp*((1 - P$spec_tint) + P$spec_tint*alb)
    v[!hitm] <- bg_lin[j]
    to_srgb(v)
  })
  rgbm <- rgbm + stats::runif(length(rgbm), -1, 1)*P$grain
  img  <- array(pmin(1, pmax(0, rgbm)), dim = c(h, w, 3))

  bg_hex <- encode_colour(matrix(to_srgb(bg_lin)*255, 1), from = "rgb")
  open_canvas(path, P$W, H, P$dpi, bg_hex, c(0, A), c(0, 1))
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::rasterImage(img, 0, 0, A, 1, interpolate = TRUE)
  draw_watermark(P$watermark, c(0, A), c(0, 1), H)

  report(path, tone_id(P), P$W, H, P$dpi,
         sprintf("%d tones, %d boxes, compute %d x %d", n_t, length(uk), w, h))
  invisible(path)
}
