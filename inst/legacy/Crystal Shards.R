# ===== Crystal Shards | Data-ART =====
# Stacked layers of Voronoi shards from coarse to fine, flat posterised tones,
# clustered light/dark regions and translucent overlaps. Drawn as vector
# polygons, so edges stay crisp at any size.
# install.packages(c("ambient","farver","ragg","deldir"))
library(ambient); library(farver); library(ragg); library(deldir)

# ---- settings ----
P <- list(
  seed = 77,

  # palette: "mono" = shades of one colour, "multi" = a multi-colour palette
  palette_mode = "mono",
  mono  = "teal",                  # a name from MONO, or any hex such as "#2ED3B0"
  multi = "tropical",              # a name from MULTI
  n_tones = 6,                     # number of flat tones (posterisation levels)
  mono_L = c(16, 80),              # lightness range of mono shades (CIE L*, dark -> light)
  mono_hue_shift = -10,            # hue shift of the darkest shade in degrees (cooler shadows)

  # output size: aspect ratio as "W:H" ("9:16", "4:5", "1:1", "3:2", "16:9", "21:9");
  # width in inches stays fixed, height follows from the ratio
  aspect = "9:16", W = 9, dpi = 300,
  out = NULL,                      # NULL = auto name: crystal_shards_<palette>_<aspect>.png

  # shards
  n_layers = 6,
  size = c(0.14, 0.012),           # typical shard size, coarsest -> finest layer (fraction of height)
  coverage_min = 0.40,             # share of cells drawn in the finest layer (base layer = 100%)
  cluster = 0.6, cluster_freq = 3, # 0 = small shards scattered at random, 1 = in tight patches
  overlay = 0.12,                  # share of big shards drawn on top of the fine detail
  alpha = 0.88,                    # opacity of all layers above the base
  shuffle_layers = FALSE,          # TRUE = random stacking order (more big shards on top)
  seam = 0.6,                      # outline in fill colour; hides hairline gaps (0 = off)

  # tone field
  region_freq = 1.6, warp = 0.25,  # size and bending of the light/dark regions
  tone_random = 0.35,              # 0 = tones follow the regions, 1 = random per shard
  tone_gamma = 1.25,               # > 1 more dark shards, < 1 more light shards

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
  teal    = "#2ED3B0",   # reference image
  emerald = "#2FCB6B",
  cobalt  = "#3B7BFF",
  violet  = "#9B6BFF",
  crimson = "#F0435A",
  amber   = "#FFB020",
  rose    = "#FF7EB6",
  slate   = "#9FB4C8"
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
    C    <- lch[2]*(0.25 + 0.75*f^0.8)
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
render_shards <- function(P) {
  pal <- build_palette(P); n_t <- length(pal)
  A   <- parse_aspect(P$aspect); H <- P$W / A
  out <- if (is.null(P$out)) sprintf("crystal_shards_%s_%s.png", palette_id(P), gsub(":", "x", P$aspect)) else P$out
  set.seed(P$seed)

  K     <- P$n_layers
  sizes <- exp(seq(log(P$size[1]), log(P$size[2]), length.out = K))
  cover <- seq(1, P$coverage_min, length.out = K)
  pad   <- 0.05                                   # tiles extend past the frame; device clips

  # warped low-frequency light/dark field (coordinates in image-height units)
  region <- function(x, y) {
    wx <- fracture(gen_simplex, fbm, octaves = 3, x = x*P$region_freq,       y = y*P$region_freq,       seed = P$seed + 1)
    wy <- fracture(gen_simplex, fbm, octaves = 3, x = x*P$region_freq + 4.7, y = y*P$region_freq - 2.3, seed = P$seed + 2)
    fracture(gen_simplex, fbm, octaves = 4, x = (x + P$warp*wx)*P$region_freq,
             y = (y + P$warp*wy)*P$region_freq, seed = P$seed)
  }
  # ECDF over the frame -> tones are spread evenly whatever the noise range
  F_reg <- ecdf(region(runif(20000, 0, A), runif(20000, 0, 1)))

  # one Voronoi layer -> NA-separated polygon coordinates + colours
  make_layer <- function(size, cover, a, id) {
    m  <- max(8, round((A + 2*pad)*(1 + 2*pad)/size^2))
    tl <- tile.list(suppressMessages(deldir(runif(m, -pad, A + pad), runif(m, -pad, 1 + pad),
                                            rw = c(-pad, A + pad, -pad, 1 + pad))))
    px <- vapply(tl, function(t) t$pt[1], 0)
    py <- vapply(tl, function(t) t$pt[2], 0)
    nc <- length(tl)

    # which cells are drawn: clustered patches via a per-layer noise field
    if (cover >= 1) draw <- rep(TRUE, nc) else {
      cn    <- fracture(gen_simplex, fbm, octaves = 2, x = px*P$cluster_freq, y = py*P$cluster_freq, seed = P$seed + 10 + id)
      score <- P$cluster*rank(cn)/nc + (1 - P$cluster)*runif(nc)
      draw  <- score > quantile(score, 1 - cover)
    }
    # one flat tone per cell: light/dark region at the seed + per-cell randomness, posterised
    tt <- ((1 - P$tone_random)*F_reg(region(px, py)) + P$tone_random*runif(nc))^P$tone_gamma
    ti <- pmin(n_t, 1 + floor(tt*n_t))
    sel <- tl[draw]
    list(x   = unlist(lapply(sel, function(t) c(t$x, NA))),
         y   = unlist(lapply(sel, function(t) c(t$y, NA))),
         col = adjustcolor(pal[ti[draw]], alpha.f = a))
  }

  layers <- lapply(seq_len(K), function(k) make_layer(sizes[k], cover[k], if (k == 1) 1 else P$alpha, k))
  ord <- if (P$shuffle_layers && K > 2) c(1, 1 + sample(K - 1)) else seq_len(K)
  if (P$overlay > 0) {                            # sparse big shards on top of the fine detail
    layers[[K + 1]] <- make_layer(sizes[min(2, K)], P$overlay, P$alpha, K + 1)
    ord <- c(ord, K + 1)
  }

  # ---- render ----
  agg_png(out, width = P$W, height = H, units = "in", res = P$dpi, background = pal[1])
  on.exit(dev.off(), add = TRUE)
  par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i", bg = pal[1])
  plot.new(); plot.window(xlim = c(0, A), ylim = c(0, 1))
  for (k in ord) {
    L <- layers[[k]]
    polygon(L$x, L$y, col = L$col, border = if (P$seam > 0) L$col else NA, lwd = P$seam)
  }
  draw_watermark(P$watermark, A, H)

  message(sprintf("saved %s  (%s, %d tones, %d x %d px, %d shards)", out, palette_id(P), n_t,
                  round(P$W*P$dpi), round(H*P$dpi),
                  sum(vapply(layers, function(L) length(L$col), 0))))
  invisible(out)
}

# ---- without watermark ----
# adds a "_clean" suffix to the file name so the watermarked image is not overwritten
render_clean <- function(P) {
  P$watermark$enabled <- FALSE
  P$out <- if (is.null(P$out)) {
    sprintf("crystal_shards_%s_%s_clean.png", palette_id(P), gsub(":", "x", P$aspect))
  } else sub("\\.png$", "_clean.png", P$out)
  render_shards(P)
}

# ---- run ----
render_shards(P)
#render_clean(P)

# shades of one colour: preset name or any hex
#render_shards(modifyList(P, list(palette_mode = "mono", mono = "cobalt")))
#render_shards(modifyList(P, list(palette_mode = "mono", mono = "#E0457B", n_tones = 5)))

# multi-colour palette
#render_shards(modifyList(P, list(palette_mode = "multi", multi = "sunset")))

# all presets
# for (m in names(MONO))  render_shards(modifyList(P, list(palette_mode = "mono",  mono  = m, aspect = "16:9", W = 16)))

# for (m in names(MULTI)) render_shards(modifyList(P, list(palette_mode = "multi", multi = m, aspect= "16:9", W = 16)))

# for (m in names(MULTI)) render_clean(modifyList(P, list(palette_mode = "multi", multi = m, aspect = "16:9", W = 16)))

# other aspect ratios, e.g. landscape or square
#render_shards(modifyList(P, list(aspect = "16:9", W = 16)))
#render_shards(modifyList(P, list(aspect = "1:1", W = 10)))
