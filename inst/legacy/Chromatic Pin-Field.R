# ===== Chromatic Pin-Field | Data-ART =====
# install.packages(c("ggplot2","ambient","farver","ragg"))
library(ggplot2); library(ambient); library(farver)

# ---- settings ----
P <- list(
  seed = 1024, nx = 300, ny = 300, n_seg = 4,
  freq = 2.4, warp = 0.45, hue_freq = 1.1, core_bias = 0.9,
  sea = 0.40, gamma = 1.4, ridge_mix = 0.55, spike = 0.9,
  h_scale = 30, row_gap = 0.62, persp = 0.55,
  light = c(-0.55, 0.45, 0.70), tip_strength = 0.85, fog = 0.22,
  speck = 0.05,

  # palette option: "chromatic" | "aurora" | "ember"
  palette = "chromatic",

  # output size: aspect ratio as "W:H" (e.g. "16:9", "21:9", "3:2", "1:1", "4:5", "9:16");
  # width in inches stays fixed, height follows from the ratio
  aspect = "16:9", W = 16, dpi = 600,
  out = NULL,                          # NULL = auto name: chromatic_pinfield_<palette>_<aspect>.png

  # watermark: name + "CodeArt" label
  watermark = list(
    enabled  = TRUE,
    name     = "Saikat Bandyopadhyay",
    label    = "CodeArt",
    position = "bottom-right",         # "bottom-right" | "bottom-left" | "top-right" | "top-left"
    size     = 0.020,                  # text height as a fraction of image height
    margin   = 0.025,                  # distance from the edges, fraction of image height
    colour   = "#FFFFFF",
    alpha    = 0.80,
    family   = "sans"
  )
)

# ---- palette options ----
# Ramps run cool -> warm with neighbouring hues only (no muddy complements),
# interpolated in CIE Lab. White highlights come from pin height, not from the ramp.
PALETTES <- list(
  # 1. full-spectrum blue -> violet -> peach -> red (original)
  chromatic = list(
    ramp  = c(navy="#1A2766", cobalt="#2D50D2", sky="#74A3EE", lavender="#9C7FD8",
              orchid="#E28CC0", peach="#FBD4A2", tangerine="#F7913E",
              coral="#F36B5E", vermilion="#DD2E30"),
    fixed = c(bg="#17161D", valley="#3A3742", tip="#F8F5EF")
  ),
  # 2. cool: deep navy -> teal -> mint -> violet -> magenta
  aurora = list(
    ramp  = c(midnight="#14215A", petrol="#155E75", teal="#1FA39A", mint="#86E3C3",
              ice="#B9D8F5", lavender="#9A86E6", violet="#6B3FC8",
              magenta="#B23AAE", fuchsia="#EE5A9E"),
    fixed = c(bg="#0D1118", valley="#2A323C", tip="#F2FFFB")
  ),
  # 3. warm: plum -> wine -> crimson -> orange -> gold (glowing core)
  ember = list(
    ramp  = c(plum="#3E1446", wine="#7B1E4E", crimson="#B9253F", vermilion="#E2462E",
              tangerine="#F57D2A", amber="#F8AE3B", saffron="#FAD25C"),
    fixed = c(bg="#151011", valley="#352C2A", tip="#FFF7EA")
  )
)

# ---- helpers ----
smooth01 <- function(e0, e1, x) { t <- pmin(1, pmax(0, (x - e0)/(e1 - e0))); t*t*(3 - 2*t) }

parse_aspect <- function(a) {
  r <- as.numeric(strsplit(a, ":", fixed = TRUE)[[1]])
  if (length(r) != 2 || any(is.na(r)) || any(r <= 0)) stop("aspect must look like \"16:9\"")
  r[1] / r[2]
}

# rows needed so the far edge of the terrain still fills the frame at this aspect
rows_needed <- function(P, H) {
  ny <- P$ny
  repeat {
    far_s <- 1 / (1 + P$persp*(ny - 1)/ny)
    top   <- 2*P$row_gap + (P$nx - 1)*far_s*0.98*H/P$W
    if (ny*P$row_gap*far_s >= top*1.02) return(ny)
    ny <- ceiling(ny*1.1)
  }
}

watermark_layers <- function(wm, xlim, ylim, H) {
  if (!isTRUE(wm$enabled)) return(NULL)
  dx <- diff(xlim); dy <- diff(ylim)
  m  <- wm$margin*dy
  right <- grepl("right", wm$position); top <- grepl("top", wm$position)
  x  <- if (right) xlim[2] - m else xlim[1] + m
  hj <- if (right) 1 else 0
  size_mm <- wm$size*H*25.4                      # ggplot text size is in mm
  # label line (bold) and name line, stacked; order depends on top/bottom
  y_label <- if (top) ylim[2] - m else ylim[1] + m + 1.35*wm$size*dy
  y_name  <- if (top) ylim[2] - m - 1.35*wm$size*dy else ylim[1] + m
  vj <- if (top) 1 else 0
  sh <- 0.0012*dy                                # drop-shadow offset for legibility
  txt <- function(x, y, lab, face, size, col, a)
    annotate("text", x = x, y = y, label = lab, hjust = hj, vjust = vj,
             fontface = face, size = size, colour = col, alpha = a, family = wm$family)
  list(
    txt(x + sh, y_label - sh, wm$label, "bold",  size_mm*1.15, "#000000", wm$alpha*0.6),
    txt(x + sh, y_name  - sh, wm$name,  "plain", size_mm*0.80, "#000000", wm$alpha*0.6),
    txt(x,      y_label,      wm$label, "bold",  size_mm*1.15, wm$colour, wm$alpha),
    txt(x,      y_name,       wm$name,  "plain", size_mm*0.80, wm$colour, wm$alpha)
  )
}

# ---- renderer ----
render_pinfield <- function(P) {
  if (!P$palette %in% names(PALETTES)) stop("palette must be one of: ", paste(names(PALETTES), collapse = ", "))
  pal_ramp  <- PALETTES[[P$palette]]$ramp
  pal_fixed <- PALETTES[[P$palette]]$fixed
  H  <- P$W / parse_aspect(P$aspect)
  ny <- rows_needed(P, H)
  out <- if (is.null(P$out)) sprintf("chromatic_pinfield_%s_%s.png", P$palette, gsub(":", "x", P$aspect)) else P$out
  set.seed(P$seed)

  # ---- grid + noise fields ----
  g <- expand.grid(ix = seq_len(P$nx), iy = seq_len(ny))   # ix varies fastest
  n <- nrow(g); u <- g$ix / P$nx; v <- g$iy / P$nx         # scaled by nx: same terrain scale at every aspect

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
  h <- h * (1 + P$spike * runif(n)^3)            # sparse tall needles
  h <- h / max(h)

  # ---- Lambert lighting from smooth field ----
  Hm <- matrix(h_smooth / max(h_smooth), P$nx, ny)
  gx <- as.vector((Hm[c(2:P$nx, P$nx), ] - Hm[c(1, 1:(P$nx-1)), ]) / 2)
  gy <- as.vector((Hm[, c(2:ny, ny)] - Hm[, c(1, 1:(ny-1))]) / 2)
  k  <- P$h_scale * 0.35
  Ld <- P$light / sqrt(sum(P$light^2))
  lambert <- pmax(0, (-k*gx*Ld[1] - k*gy*Ld[2] + Ld[3]) / sqrt((k*gx)^2 + (k*gy)^2 + 1))

  # ---- colour zones (warm core, rank-equalised) ----
  rad <- sqrt((u - mean(range(u)))^2 + (v - mean(range(v)))^2)
  hue_t <- (rank(normalise(hueN) - P$core_bias*normalise(rad)) - 0.5) / n
  hue_t <- pmin(1, pmax(0, hue_t + rnorm(n, 0, 0.012)))
  ramp  <- colorRampPalette(pal_ramp, space = "Lab")(512)
  lab0  <- decode_colour(ramp[1 + round(hue_t*511)], to = "lab")

  # ---- projection ----
  cx <- (P$nx + 1)/2
  s  <- 1 / (1 + P$persp*(g$iy - 1)/ny)
  sx <- cx + (g$ix - cx)*s
  sy <- g$iy * P$row_gap * s
  hp <- pmax(h, 0.012) * P$h_scale * s           # stubs keep valley grid visible

  far_s  <- 1 / (1 + P$persp*(ny - 1)/ny)
  half_w <- (P$nx - 1)/2 * far_s * 0.98
  xlim <- c(cx - half_w, cx + half_w)
  ylim <- c(2*P$row_gap, 2*P$row_gap + diff(xlim)*H/P$W)

  # ---- segments (far -> near, base -> tip) ----
  keep <- sx >= xlim[1]-1 & sx <= xlim[2]+1 & sy <= ylim[2] & (sy + hp) >= ylim[1]
  idx  <- which(keep)[order(-g$iy[keep], g$ix[keep])]
  S <- P$n_seg; pin <- rep(idx, each = S); kk <- rep(seq_len(S), times = length(idx))
  tm <- (kk - 0.5)/S

  # ---- shading in Lab ----
  jit   <- runif(n)
  lab   <- lab0[pin, , drop = FALSE]
  depth <- (g$iy[pin] - 1)/(ny - 1)
  lab[, 1] <- lab[, 1] * (0.50 + 0.65*lambert[pin]) * (0.38 + 0.62*tm) * (1 - P$fog*depth)

  tip <- matrix(decode_colour(pal_fixed["tip"], to = "lab"), nrow(lab), 3, byrow = TRUE)
  mix <- P$tip_strength * tm^3 * h[pin]^0.6
  lab <- lab*(1 - mix) + tip*mix

  val <- matrix(decode_colour(pal_fixed["valley"], to = "lab"), nrow(lab), 3, byrow = TRUE)
  val[, 1] <- val[, 1] * (0.75 + 0.5*jit[pin]) * (0.6 + 0.6*lambert[pin])
  val <- val*0.85 + lab0[pin, ]*0.15                       # tinted charcoal
  vwt <- (1 - smooth01(0, 0.05, h[pin])) * (jit[pin] > P$speck)  # keep coloured specks
  lab <- lab*(1 - vwt) + val*vwt
  lab[, 1] <- pmin(100, pmax(0, lab[, 1]))

  seg <- data.frame(
    x = sx[pin],
    y0 = sy[pin] + hp[pin]*(kk - 1)/S,
    y1 = sy[pin] + hp[pin]*kk/S + 0.02,                    # overlap hides seams
    col = encode_colour(lab, from = "lab"),
    lw = 0.78 * s[pin] * (P$W*25.4/diff(xlim)) / 0.753     # ~78% of pin pitch
  )

  # ---- render ----
  p <- ggplot(seg) +
    geom_segment(aes(x = x, xend = x, y = y0, yend = y1, colour = col, linewidth = lw),
                 lineend = "butt") +
    scale_colour_identity() + scale_linewidth_identity() +
    watermark_layers(P$watermark, xlim, ylim, H) +
    coord_fixed(xlim = xlim, ylim = ylim, expand = FALSE, clip = "on") +
    theme_void() +
    theme(plot.background = element_rect(fill = pal_fixed[["bg"]], colour = NA),
          plot.margin = margin(0, 0, 0, 0))

  ggsave(out, p, width = P$W, height = H, dpi = P$dpi,
         device = ragg::agg_png, bg = pal_fixed[["bg"]])
  message(sprintf("saved %s  (%s, %d x %d px, %d rows)", out, P$palette,
                  round(P$W*P$dpi), round(H*P$dpi), ny))
  invisible(out)
}

# ---- run ----
#render_pinfield(P)

# render all three palettes:
#for (pl in names(PALETTES)) render_pinfield(modifyList(P, list(palette = pl)))

# other aspect ratios, e.g. portrait for social media:
#render_pinfield(modifyList(P, list(aspect = "4:5", W = 10)))

# ---- without watermark ----
# adds a "_clean" suffix to the file name so the watermarked image is not overwritten
render_clean <- function(P) {
  P$watermark$enabled <- FALSE
  P$out <- if (is.null(P$out)) {
    sprintf("chromatic_pinfield_%s_%s_clean.png", P$palette, gsub(":", "x", P$aspect))
  } else sub("\\.png$", "_clean.png", P$out)
  render_pinfield(P)
}
#render_clean(P)

# all three palettes without watermark:
for (pl in names(PALETTES)) render_clean(modifyList(P, list(palette = pl)))
