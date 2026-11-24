# ===== Iso Cubes | Data-ART =====
# A wall of glossy rounded boxes stacked as a 3D staircase and seen at a corner
# angle: horizontal highlighted ledges, tall upper/lower faces, dark side faces
# and deep crevices. Each pixel casts an orthographic ray into the box lattice
# (analytic box intersection), then rounded edges are shaded via bent normals.
# Key + fill light, specular sheen, spotlight falloff and film grain.
# install.packages(c("ambient","farver","ragg"))
library(ambient); library(farver); library(ragg)

# ---- settings ----
P <- list(
  seed = 1,

  # palette: "mono" = shades of one colour, "multi" = a multi-colour palette
  palette_mode = "mono",
  mono  = "steel_teal",            # a name from MONO, or any hex such as "#3E8A99"
  multi = "sunset",                # a name from MULTI
  n_tones = 5,                     # number of cube colours
  mono_L = c(18, 58),              # lightness range of mono shades (CIE L*, dark -> light)
  mono_hue_shift = -8,             # hue shift of the darkest shade in degrees

  # output size: aspect ratio as "W:H" ("3:2", "16:9", "21:9", "1:1", "4:5", "9:16");
  # width in inches stays fixed, height follows from the ratio
  aspect = "3:2", W = 16, dpi = 300,
  compute_px = 2000,               # long side of the internal raster (output is smoothly upsampled);
                                   # higher = crisper, but RAM grows with its square
  out = NULL,                      # NULL = auto name: iso_cubes_<palette>_<aspect>.png

  # geometry
  cube_size  = 0.16,               # short box edge (the horizontal ledge) as a fraction of image height
  elongation = 1.7,                # length of the other two edges relative to the ledge (1 = true cube)
  yaw = 0, pitch = 0,              # extra rotation of the boxes in degrees (0, 0 = reference view)
  spacing = 1,                     # 1 = boxes touch; > 1 opens gaps into the dark background
  bevel = 0.20,                    # edge rounding as a fraction of cube_size
  depth_fall = 1.0,                # darkening of surfaces set back in the crevices
  search = 1,                      # neighbour search radius; raise to 2 for extreme yaw/pitch/elongation

  # colour per box
  region_freq = 1.2,               # size of the light/dark colour regions
  tone_random = 0.5,               # 0 = colours follow the regions, 1 = random per box

  # light (x = right, y = up, z = toward the viewer)
  key = c(-0.80, 0.35, 0.50), key_strength = 1.00,
  fill = c(0.60, -0.20, 0.77), fill_strength = 0.20,
  ambient = 0.04,
  specular = 0.40, shininess = 16, spec_tint = 0.25,  # glossy highlight; tint = share of box colour
  spot_x = 0, spot_y = 0,          # spotlight centre (image-height units from the centre)
  spot_r = 0.42, spot_floor = 0.20,# spotlight radius and brightness at the dark edges
  grain = 0.010,                   # +-RGB noise; film grain and no banding

  # watermark: name + "CodeArt" label
  watermark = list(
    enabled  = TRUE,
    name     = "Saikat Bandyopadhyay",
    label    = "CodeArt",
    position = "bottom-right",     # "bottom-right" | "bottom-left" | "top-right" | "top-left"
    size     = 0.020,              # text height as a fraction of image height
    margin   = 0.025,              # distance from the edges, fraction of image height
    colour   = "#FFFFFF",
    alpha    = 0.80,
    family   = "sans"
  )
)

# ---- palette options ----
# Mono: one base colour -> n shades along constant hue in CIE LCh. Chroma is low
# in the darks and rises with lightness, so shadows stay rich, not neon.
MONO <- c(
  steel_teal = "#3E8A99",   # reference image
  teal       = "#2ED3B0",
  emerald    = "#2FCB6B",
  cobalt     = "#3B7BFF",
  violet     = "#9B6BFF",
  crimson    = "#F0435A",
  amber      = "#FFB020",
  rose       = "#FF7EB6",
  slate      = "#9FB4C8"
)

# Multi: colours are sorted dark -> light automatically and interpolated in Lab
# to n_tones, so the light/dark regions keep working with any palette.
MULTI <- list(
  jade_night   = c("#0E1B2A","#14394A","#1B6170","#21A39A","#7FE8C8"),
  tropical     = c("#13293D","#006D77","#2BB3A3","#F4D35E","#EE964B"),
  sunset       = c("#2B1B3D","#5C2A5E","#B23A6E","#F2725B","#FFC07A"),
  ocean        = c("#071A2E","#0B3B66","#1565A8","#2E9BD6","#9ADCF2"),
  forest       = c("#0F1F17","#1E3B2A","#2F6B45","#5FA35E","#B8D98A"),
  ember        = c("#1A0A0A","#4A1212","#8C1F1A","#D9481C","#FFA43A"),
  berry        = c("#1C0F24","#3E1A4D","#7A2E7E","#C2509A","#F29BC4"),
  desert       = c("#2A1E1A","#5A3A2B","#9C6B48","#D4A373","#F1DDBF"),
  neon         = c("#0B0620","#24107A","#7A2BE2","#FF2E97","#2EF2E0"),
  slate_accent = c("#111315","#24282C","#3A4046","#5C646C","#2ED3B0")
)

# ---- helpers ----
parse_aspect <- function(a) {
  r <- as.numeric(strsplit(a, ":", fixed = TRUE)[[1]])
  if (length(r) != 2 || any(is.na(r)) || any(r <= 0)) stop("aspect must look like \"16:9\"")
  r[1] / r[2]
}

build_palette <- function(P) {
  n <- P$n_tones
  if (P$palette_mode == "mono") {
    base <- if (P$mono %in% names(MONO)) MONO[[P$mono]] else P$mono
    lch  <- decode_colour(base, to = "lch")[1, ]
    f    <- seq(0, 1, length.out = n)
    L    <- P$mono_L[1] + f*diff(P$mono_L)
    C    <- lch[2]*(0.35 + 0.65*f^0.8)
    h    <- (lch[3] + P$mono_hue_shift*(1 - f)) %% 360
    return(encode_colour(cbind(L, C, h), from = "lch"))
  }
  if (P$palette_mode == "multi") {
    if (!P$multi %in% names(MULTI)) stop("multi must be one of: ", paste(names(MULTI), collapse = ", "))
    cols <- MULTI[[P$multi]]
    cols <- cols[order(decode_colour(cols, to = "lab")[, 1])]      # dark -> light
    return(colorRampPalette(cols, space = "Lab")(n))
  }
  stop('palette_mode must be "mono" or "multi"')
}

palette_id <- function(P) {
  if (P$palette_mode == "mono") paste0("mono-", sub("#", "", P$mono)) else P$multi
}

to_linear <- function(c) ifelse(c <= 0.04045, c/12.92, ((c + 0.055)/1.055)^2.4)
to_srgb   <- function(c) { c <- pmax(c, 0); ifelse(c <= 0.0031308, 12.92*c, 1.055*c^(1/2.4) - 0.055) }
unit      <- function(v) v / sqrt(sum(v^2))

draw_watermark <- function(wm, A, H) {
  if (!isTRUE(wm$enabled)) return(invisible())
  right <- grepl("right", wm$position); top <- grepl("top", wm$position)
  m   <- wm$margin
  x   <- if (right) A - m else m
  hj  <- if (right) 1 else 0
  vj  <- if (top) 1 else 0
  gap <- 1.35*wm$size
  y_label <- if (top) 1 - m else m + gap
  y_name  <- if (top) 1 - m - gap else m
  cex <- wm$size*H*72/par("ps")                  # font size = size * image height
  put <- function(dx, dy, col, a) {
    text(x + dx, y_label + dy, wm$label, adj = c(hj, vj), font = 2, cex = cex*1.15,
         col = adjustcolor(col, a), family = wm$family)
    text(x + dx, y_name + dy, wm$name, adj = c(hj, vj), font = 1, cex = cex*0.80,
         col = adjustcolor(col, a), family = wm$family)
  }
  put(0.0012, -0.0012, "#000000", wm$alpha*0.6) # soft drop shadow for legibility
  put(0, 0, wm$colour, wm$alpha)
}

# ---- renderer ----
render_cubes <- function(P) {
  pal <- build_palette(P); n_t <- length(pal)
  A   <- parse_aspect(P$aspect); H <- P$W / A
  out <- if (is.null(P$out)) sprintf("iso_cubes_%s_%s.png", palette_id(P), gsub(":", "x", P$aspect)) else P$out
  set.seed(P$seed)

  # ---- pixel grid (image-height units, origin at centre, y up) ----
  if (A >= 1) { w <- P$compute_px; h <- round(P$compute_px/A) } else { h <- P$compute_px; w <- round(P$compute_px*A) }
  X <- (rep(seq_len(w), each = h) - 0.5)/h - A/2       # row fastest = raster order
  Y <- 0.5 - (rep(seq_len(h), times = w) - 0.5)/h
  N <- length(X)

  # ---- box orientation: corner view (ledge horizontal) + optional yaw/pitch ----
  r6 <- sqrt(1/6); r2 <- sqrt(1/2); r3 <- sqrt(1/3)
  Riso <- matrix(c(sqrt(2/3), 0, r3,  -r6, r2, r3,  -r6, -r2, r3), 3, 3)   # columns = box axes
  yw <- P$yaw*pi/180; pt <- P$pitch*pi/180
  Ry <- matrix(c(cos(yw), 0, -sin(yw),  0, 1, 0,  sin(yw), 0, cos(yw)), 3, 3)
  Rx <- matrix(c(1, 0, 0,  0, cos(pt), sin(pt),  0, -sin(pt), cos(pt)), 3, 3)
  R  <- Ry %*% Rx %*% Riso
  hv <- P$cube_size/2*c(1, P$elongation, P$elongation)  # box half-sizes along its own axes

  # staircase lattice: neighbours step one box along one axis and back along another
  B1 <- P$spacing*(2*hv[1]*R[, 1] - 2*hv[3]*R[, 3])
  B2 <- P$spacing*(2*hv[2]*R[, 2] - 2*hv[3]*R[, 3])
  Mi <- solve(cbind(B1[1:2], B2[1:2]))
  i0 <- round(Mi[1, 1]*X + Mi[1, 2]*Y)
  j0 <- round(Mi[2, 1]*X + Mi[2, 2]*Y)

  # ---- ray cast: nearest box hit among neighbouring lattice cells ----
  dl <- -R[3, ]; dl[abs(dl) < 1e-9] <- 1e-9            # view ray (0,0,-1) in box coordinates
  z0 <- 100
  best <- rep(Inf, N); bax <- integer(N); bi <- numeric(N); bj <- numeric(N); btc <- numeric(N)
  bp1 <- numeric(N); bp2 <- numeric(N); bp3 <- numeric(N)
  for (di in -P$search:P$search) for (dj in -P$search:P$search) {
    ci <- i0 + di; cj <- j0 + dj
    ox <- X - (ci*B1[1] + cj*B2[1]); oy <- Y - (ci*B1[2] + cj*B2[2]); oz <- z0 - (ci*B1[3] + cj*B2[3])
    o1 <- R[1, 1]*ox + R[2, 1]*oy + R[3, 1]*oz          # ray origin in box coordinates
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
    btc[hit] <- z0 - (ci[hit]*B1[3] + cj[hit]*B2[3]) - sum(hv*abs(R[3, ]))   # ray depth of the box's near corner
    bp1[hit] <- o1[hit] + tmin[hit]*dl[1]; bp2[hit] <- o2[hit] + tmin[hit]*dl[2]; bp3[hit] <- o3[hit] + tmin[hit]*dl[3]
  }
  rm(ox, oy, oz, o1, o2, o3, ta, tb, tn1, tn2, tn3, tf1, tf2, tf3, tmin, tmax, i0, j0)
  hitm <- is.finite(best)

  # ---- normals with rounded edges (bent toward the neighbouring face) ----
  rb <- P$bevel*P$cube_size
  bp <- list(bp1, bp2, bp3)
  nx <- numeric(N); ny <- numeric(N); nz <- numeric(N)
  for (k in 1:3) {
    ii <- which(hitm & bax == k); if (!length(ii)) next
    v <- list(numeric(length(ii)), numeric(length(ii)), numeric(length(ii)))
    v[[k]] <- rep(-sign(dl[k]), length(ii))
    for (j in setdiff(1:3, k)) {
      pj <- bp[[j]][ii]
      s  <- pmin(1, pmax(0, 1 - (hv[j] - abs(pj))/rb))
      v[[j]] <- v[[j]] + tan(asin(s*sqrt(0.5)))*sign(pj)   # 0 inside the face -> 45 deg at the edge
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
  tt  <- (1 - P$tone_random)*rank(reg)/length(uk) + P$tone_random*runif(length(uk))
  ti  <- pmin(n_t, 1 + floor(tt*n_t))[idx]
  pal_lin <- to_linear(decode_colour(pal, to = "rgb")/255)
  rm(key, bi, bj)

  # ---- lighting in linear RGB ----
  Lk <- unit(P$key); Lf <- unit(P$fill); Hh <- unit(Lk + c(0, 0, 1))
  diff <- pmax(0, nx*Lk[1] + ny*Lk[2] + nz*Lk[3])
  fil  <- pmax(0, nx*Lf[1] + ny*Lf[2] + nz*Lf[3])
  spec <- pmax(0, nx*Hh[1] + ny*Hh[2] + nz*Hh[3])^P$shininess
  spot <- P$spot_floor + (1 - P$spot_floor)*exp(-((X - P$spot_x)^2 + (Y - P$spot_y)^2)/(2*P$spot_r^2))
  lightf <- (P$ambient + (P$key_strength*diff + P$fill_strength*fil)*spot)*dk
  sp     <- P$specular*spec*spot*dk
  rm(nx, ny, nz, diff, fil, spec, dk)

  bg_lin <- pal_lin[1, ]*0.08                          # background where no box is hit
  rgbm <- sapply(1:3, function(j) {
    alb <- pal_lin[ti, j]
    v <- alb*lightf + sp*((1 - P$spec_tint) + P$spec_tint*alb)
    v[!hitm] <- bg_lin[j]
    to_srgb(v)
  })
  rgbm <- rgbm + runif(length(rgbm), -1, 1)*P$grain
  img  <- array(pmin(1, pmax(0, rgbm)), dim = c(h, w, 3))

  # ---- render ----
  bg_hex <- encode_colour(matrix(to_srgb(bg_lin)*255, 1), from = "rgb")
  agg_png(out, width = P$W, height = H, units = "in", res = P$dpi, background = bg_hex)
  on.exit(dev.off(), add = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = bg_hex)
  plot.new(); plot.window(xlim = c(0, A), ylim = c(0, 1))
  rasterImage(img, 0, 0, A, 1, interpolate = TRUE)
  draw_watermark(P$watermark, A, H)

  message(sprintf("saved %s  (%s, %d tones, %d x %d px, %d boxes, compute %d x %d)", out, palette_id(P),
                  n_t, round(P$W*P$dpi), round(H*P$dpi), length(uk), w, h))
  invisible(out)
}

# ---- without watermark ----
# adds a "_clean" suffix to the file name so the watermarked image is not overwritten
render_clean <- function(P) {
  P$watermark$enabled <- FALSE
  P$out <- if (is.null(P$out)) {
    sprintf("iso_cubes_%s_%s_clean.png", palette_id(P), gsub(":", "x", P$aspect))
  } else sub("\\.png$", "_clean.png", P$out)
  render_cubes(P)
}

# ---- run ----
#render_cubes(P)
#render_clean(P)

# shades of one colour: preset name or any hex
#render_cubes(modifyList(P, list(palette_mode = "mono", mono = "violet")))
#render_cubes(modifyList(P, list(palette_mode = "mono", mono = "#C9A227", n_tones = 4)))

# multi-colour palette
#render_cubes(modifyList(P, list(palette_mode = "multi", multi = "ocean")))

# all presets
# for (m in names(MONO))  render_cubes(modifyList(P, list(palette_mode = "mono",  mono  = m)))
# 
# for (m in names(MULTI)) render_cubes(modifyList(P, list(palette_mode = "multi", multi = m)))
#for (m in names(MULTI)) render_clean(modifyList(P, list(palette_mode = "multi", multi = m)))

# variations: true cubes, bigger boxes, gaps, rotation, off-centre spotlight, other aspect ratios
#render_cubes(modifyList(P, list(elongation = 1)))                         # classic tumbling blocks
#render_cubes(modifyList(P, list(cube_size = 0.24, bevel = 0.25)))
#render_cubes(modifyList(P, list(spacing = 1.08)))
#render_cubes(modifyList(P, list(yaw = 8, pitch = -6, search = 2)))
#render_cubes(modifyList(P, list(spot_x = -0.5, spot_y = 0.2, spot_r = 0.35)))
#render_cubes(modifyList(P, list(aspect = "9:16", W = 9)))
