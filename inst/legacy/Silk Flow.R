# ===== Silk Flow | Data-ART =====
# Layered colour ribbons: crisp folds with long soft fades, cast shadows,
# rim light, bloom and film grain, rendered as a smooth raster.
# install.packages(c("ambient","farver","ragg"))
library(ambient); library(farver); library(ragg)

# ---- settings ----
P <- list(
  seed = 2025,
  palette = "nebula",              # one of names(PALETTES), see below

  # output size: aspect ratio as "W:H" ("16:9", "16:10", "21:9", "3:2", "1:1", "4:5", "9:16");
  # width in inches stays fixed, height follows from the ratio
  aspect = "16:10", W = 16, dpi = 300,
  compute_px = 2000,               # long side of the internal raster (output is smoothly upsampled);
                                   # raise to W*dpi for maximum edge sharpness (slower, more RAM)
  out = NULL,                      # NULL = auto name: silk_flow_<palette>_<aspect>.png

  # ribbons
  n_sheets = 4,                    # number of layered ribbons
  flow_angle = -0.35,              # main direction of the folds (radians, 0 = horizontal)
  angle_jitter = 0.35,
  amp = 0.16,                      # wave amplitude (fraction of image height)
  wave_freq = c(0.20, 0.55),       # wave frequency range (cycles per image height)
  warp = 0.06, warp_freq = 0.6,    # organic bending shared by all ribbons
  width = c(0.18, 0.50),           # fade length behind each edge (fraction of height)
  edge = 0.003,                    # sharpest edge softness; each ribbon gets 1-30x this
  opacity = 0.95,

  # colour
  hue_span = 0.45,                 # share of the ramp one ribbon spans along its length
  hue_noise = 0.08,
  bg_mix = 0.22,                   # how much the first ramp colour tints the background

  # light
  depth_dark = 0.40,               # fade toward the background tone away from each edge
  rim = 6,                         # L* lift right at each edge
  shadow = 0.50, shadow_w = 0.08,  # cast-shadow strength and length beyond each edge
  bloom = 0.35, bloom_r = 0.025,   # glow around bright areas
  vignette = 0.30,
  grain = 0.010,                   # +-RGB noise; prevents banding in smooth gradients

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
# Each ramp steps through neighbouring hues only (no muddy complements) and is
# interpolated in CIE Lab. bg is a deep tone of the same family, so the dark
# gaps between ribbons stay coloured instead of flat black.
PALETTES <- list(
  nebula    = list(bg = "#060828", ramp = c("#3B2BD9","#7424E0","#B41FC8","#E0286E","#EB5E2F","#EE9416")),  # reference image
  aurora    = list(bg = "#03121A", ramp = c("#2A2A9E","#1F6FC2","#14B3B0","#3EDC8F","#C8F27A")),
  sunset    = list(bg = "#170A22", ramp = c("#3D2A8A","#B23A8C","#F2546B","#FF8C5A","#FFC46B","#FFE6A3")),
  ocean     = list(bg = "#020A1C", ramp = c("#0A2A6B","#0E54A8","#1F8FD6","#39C5E8","#9AF0F5")),
  ember     = list(bg = "#110405", ramp = c("#5A0A1E","#A3122A","#E0301E","#FF6A13","#FFA51F","#FFD86B")),
  orchid    = list(bg = "#1E1633", ramp = c("#6B5BD6","#A47CF0","#E58BD8","#FFB3C7","#FFE1D6")),
  citrus    = list(bg = "#06140F", ramp = c("#0F6B4F","#2FB36B","#B6E04A","#FFE14D","#FF9F1C")),
  rose_gold = list(bg = "#1A0D12", ramp = c("#5B2C3F","#A8546A","#E39A8F","#F3C6A5","#FFF0DC")),
  neon      = list(bg = "#05010F", ramp = c("#00E0FF","#2D7BFF","#8A2BFF","#FF2BD6","#FF5E7E")),
  graphite  = list(bg = "#07080B", ramp = c("#2B2F3A","#565D6E","#8F97A8","#C9CFDA","#F2F4F7"))
)

# ---- helpers ----
smooth01 <- function(e0, e1, x) { t <- pmin(1, pmax(0, (x - e0)/(e1 - e0))); t*t*(3 - 2*t) }

parse_aspect <- function(a) {
  r <- as.numeric(strsplit(a, ":", fixed = TRUE)[[1]])
  if (length(r) != 2 || any(is.na(r)) || any(r <= 0)) stop("aspect must look like \"16:9\"")
  r[1] / r[2]
}

mov  <- function(z, r) {
  f <- stats::filter(z, rep(1/(2*r + 1), 2*r + 1), sides = 2)
  f[is.na(f)] <- z[is.na(f)]; as.numeric(f)
}
blur <- function(M, r) t(apply(apply(M, 2, mov, r = r), 1, mov, r = r))

# random ribbon geometry, one row per ribbon (drawn bottom -> top)
make_sheets <- function(P) {
  K <- P$n_sheets
  slot <- (sample(K) - 0.5)/K                    # stratified positions across the frame
  data.frame(
    theta = P$flow_angle + runif(K, -P$angle_jitter, P$angle_jitter),
    c0    = (slot - 0.5)*0.9 + runif(K, -0.05, 0.05),
    a1 = P$amp*runif(K, 0.6, 1.0),  f1 = runif(K, P$wave_freq[1], P$wave_freq[2]),   p1 = runif(K, 0, 2*pi),
    a2 = P$amp*runif(K, 0.15, 0.4), f2 = runif(K, P$wave_freq[2], 2*P$wave_freq[2]), p2 = runif(K, 0, 2*pi),
    side  = sample(c(-1, 1), K, replace = TRUE), # which side of the edge the ribbon body lies on
    width = runif(K, P$width[1], P$width[2]),
    edge  = P$edge*10^runif(K, 0, 1.5),          # mix of crisp and blurred folds
    t0    = sort(runif(K, 0, 1 - P$hue_span)),   # upper ribbons take warmer parts of the ramp
    dir   = sample(c(-1, 1), K, replace = TRUE)
  )
}

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
render_silk <- function(P) {
  if (!P$palette %in% names(PALETTES)) stop("palette must be one of: ", paste(names(PALETTES), collapse = ", "))
  pal <- PALETTES[[P$palette]]
  A   <- parse_aspect(P$aspect); H <- P$W / A
  out <- if (is.null(P$out)) sprintf("silk_flow_%s_%s.png", P$palette, gsub(":", "x", P$aspect)) else P$out
  set.seed(P$seed)
  S <- make_sheets(P)

  # ---- pixel grid (coordinates in image-height units, origin at centre, y up) ----
  if (A >= 1) { w <- P$compute_px; h <- round(P$compute_px/A) } else { h <- P$compute_px; w <- round(P$compute_px*A) }
  r  <- rep(seq_len(h), times = w); cc <- rep(seq_len(w), each = h)   # row fastest = raster order
  X  <- (cc - 0.5)/h - A/2
  Y  <- 0.5 - (r - 0.5)/h

  # shared warp + colour noise
  wX <- fracture(gen_simplex, fbm, octaves = 3, x = X*P$warp_freq,       y = Y*P$warp_freq,       seed = P$seed + 1)
  wY <- fracture(gen_simplex, fbm, octaves = 3, x = X*P$warp_freq + 5.2, y = Y*P$warp_freq - 1.7, seed = P$seed + 2)
  Xw <- X + P$warp*wX; Yw <- Y + P$warp*wY
  hueN <- fracture(gen_perlin, fbm, octaves = 3, x = X*1.3, y = Y*1.3, seed = P$seed + 3)
  bgN  <- normalise(fracture(gen_simplex, fbm, octaves = 2, x = X*0.8, y = Y*0.8, seed = P$seed + 4))

  # ramp lookup table in Lab
  lab_tab <- decode_colour(colorRampPalette(pal$ramp, space = "Lab")(1024), to = "lab")

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
    d <- sk$side*(yr - curve)                         # signed distance, > 0 inside the ribbon

    # cast shadow on everything below, just outside the edge
    sh <- P$shadow*exp(-pmax(-d, 0)/P$shadow_w)*(d < 0)
    cL <- cL*(1 - sh); ca <- ca*(1 - 0.5*sh); cb <- cb*(1 - 0.5*sh)

    # coverage: sharp edge, long Gaussian fade into the body
    a <- P$opacity*smooth01(-sk$edge, sk$edge, d)*exp(-(pmax(d, 0)/sk$width)^2)

    # colour runs along the ribbon, shifts slightly with depth
    xn <- (xr - min(xr))/diff(range(xr))
    if (sk$dir < 0) xn <- 1 - xn
    tt <- pmin(1, pmax(0, sk$t0 + P$hue_span*xn + P$hue_noise*hueN + 0.12*pmin(pmax(d, 0)/sk$width, 1)))
    col <- lab_tab[1 + round(tt*1023), , drop = FALSE]

    # shading: brightest at the fold, fading toward the background tone (hue-shifted,
    # so orange darkens to plum instead of brown), plus a thin rim light
    lit <- (1 - P$depth_dark) + P$depth_dark*exp(-pmax(d, 0)/(0.5*sk$width))
    cL <- cL*(1 - a) + (bg[1]*(1 - lit) + col[, 1]*lit + P$rim*exp(-(d/(3*sk$edge + 0.004))^2))*a
    ca <- ca*(1 - a) + (bg[2]*(1 - lit) + col[, 2]*lit)*a
    cb <- cb*(1 - a) + (bg[3]*(1 - lit) + col[, 3]*lit)*a
  }

  # ---- bloom, vignette ----
  if (P$bloom > 0) {
    rb <- max(1, round(P$bloom_r*h))
    B  <- blur(blur(matrix(pmax(cL - 55, 0), h, w), rb), rb)
    cL <- cL + P$bloom*as.vector(B)
  }
  rr <- ((X/(A/2))^2 + (Y/0.5)^2)/2
  cL <- cL*(1 - P$vignette*rr)

  # ---- to sRGB, grain, raster ----
  rgbm <- convert_colour(cbind(pmin(100, pmax(0, cL)), ca, cb), from = "lab", to = "rgb")
  rgbm <- rgbm + runif(length(rgbm), -1, 1)*P$grain*255
  img  <- array(pmin(255, pmax(0, rgbm))/255, dim = c(h, w, 3))

  # ---- render ----
  agg_png(out, width = P$W, height = H, units = "in", res = P$dpi, background = pal$bg)
  on.exit(dev.off(), add = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = pal$bg)
  plot.new(); plot.window(xlim = c(0, A), ylim = c(0, 1))
  rasterImage(img, 0, 0, A, 1, interpolate = TRUE)
  draw_watermark(P$watermark, A, H)

  message(sprintf("saved %s  (%s, %d x %d px, compute %d x %d)", out, P$palette,
                  round(P$W*P$dpi), round(H*P$dpi), w, h))
  invisible(out)
}

# ---- without watermark ----
# adds a "_clean" suffix to the file name so the watermarked image is not overwritten
render_clean <- function(P) {
  P$watermark$enabled <- FALSE
  P$out <- if (is.null(P$out)) {
    sprintf("silk_flow_%s_%s_clean.png", P$palette, gsub(":", "x", P$aspect))
  } else sub("\\.png$", "_clean.png", P$out)
  render_silk(P)
}

# ---- run ----
render_silk(P)
#render_clean(P)

# all ten palettes (same composition, different colours):
for (pl in names(PALETTES)) render_silk(modifyList(P, list(palette = pl)))
#for (pl in names(PALETTES)) render_clean(modifyList(P, list(palette = pl)))

# other aspect ratios, e.g. portrait for social media or ultrawide:
#render_silk(modifyList(P, list(aspect = "4:5", W = 10)))
#render_silk(modifyList(P, list(aspect = "21:9")))

# quick composition search: low-res drafts for several seeds
#for (sd in c(7, 42, 101, 2025, 4242, 9001)) render_clean(modifyList(P, list(seed = sd, compute_px = 800, dpi = 100,
#                                                                    out = sprintf("draft_seed_%d.png", sd))))
