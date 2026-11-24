#' Crystal-shard parameters
#'
#' Default settings for [render_shards()]: stacked layers of Voronoi shards
#' from coarse to fine, flat posterised tones, clustered light/dark regions and
#' translucent overlaps, drawn as vector polygons. Pass any setting to
#' override it; unknown names are an error.
#'
#' Main settings:
#' * `palette_mode`: "mono" (shades of `mono`) or "multi" (palette `multi`);
#'   `n_tones`, `mono_L`, `mono_hue_shift`: see [tone_palette()].
#' * `n_layers`, `size`: number of layers and shard size, coarsest -> finest.
#' * `coverage_min`, `cluster`, `cluster_freq`: how many fine shards are drawn and how tightly they bunch.
#' * `overlay`, `alpha`, `shuffle_layers`, `seam`: big shards on top, opacity, stacking, hairline fix.
#' * `region_freq`, `warp`, `tone_random`, `tone_gamma`: light/dark regions and tone balance.
#' * Shared: `seed`, `aspect`, `W`, `dpi`, `out`, `out_dir`, `suffix`, `watermark`.
#'
#' @param ... Settings to override.
#' @return A named list of settings.
#' @examples
#' shards_params(palette_mode = "multi", multi = "tropical")
#' shards_params(mono = "#E0457B", n_tones = 5)
#' @export
shards_params <- function(...) {
  defaults <- c(
    list(
      palette_mode = "mono", mono = "teal", multi = "tropical",
      n_tones = 6, mono_L = c(16, 80), mono_hue_shift = -10,
      n_layers = 6, size = c(0.14, 0.012),
      coverage_min = 0.40, cluster = 0.6, cluster_freq = 3,
      overlay = 0.12, alpha = 0.88, shuffle_layers = FALSE, seam = 0.6,
      region_freq = 1.6, warp = 0.25, tone_random = 0.35, tone_gamma = 1.25
    ),
    common_defaults(seed = 77, aspect = "9:16", W = 9)
  )
  merge_params(defaults, list(...), "shards_params")
}

#' Render crystal shards
#'
#' @param params Settings from [shards_params()].
#' @inheritParams render_pinfield
#' @return The PNG path, invisibly.
#' @examples
#' \dontrun{
#' render_shards(shards_params(mono = "cobalt"))
#' render_shards(shards_params(palette_mode = "multi", multi = "sunset", aspect = "16:9", W = 16))
#' }
#' @export
render_shards <- function(params = shards_params(), clean = FALSE) {
  P   <- apply_clean(params, clean)
  pal <- tone_palette(P$palette_mode, P$mono, P$multi, P$n_tones,
                      L_range = P$mono_L, hue_shift = P$mono_hue_shift, chroma_floor = 0.25)
  n_t  <- length(pal)
  A    <- parse_aspect(P$aspect); H <- P$W / A
  path <- output_path(P, "crystal_shards", tone_id(P), clean)
  set.seed(P$seed)

  K     <- P$n_layers
  sizes <- exp(seq(log(P$size[1]), log(P$size[2]), length.out = K))
  cover <- seq(1, P$coverage_min, length.out = K)
  pad   <- 0.05                                          # tiles extend past the frame; device clips

  # warped low-frequency light/dark field (image-height units)
  region <- function(x, y) {
    wx <- fracture(gen_simplex, fbm, octaves = 3, x = x*P$region_freq,       y = y*P$region_freq,       seed = P$seed + 1)
    wy <- fracture(gen_simplex, fbm, octaves = 3, x = x*P$region_freq + 4.7, y = y*P$region_freq - 2.3, seed = P$seed + 2)
    fracture(gen_simplex, fbm, octaves = 4, x = (x + P$warp*wx)*P$region_freq,
             y = (y + P$warp*wy)*P$region_freq, seed = P$seed)
  }
  # ECDF over the frame -> tones are spread evenly whatever the noise range
  F_reg <- stats::ecdf(region(stats::runif(20000, 0, A), stats::runif(20000, 0, 1)))

  # one Voronoi layer -> NA-separated polygon coordinates + colours
  make_layer <- function(size, cover, a, id) {
    m  <- max(8, round((A + 2*pad)*(1 + 2*pad)/size^2))
    tl <- deldir::tile.list(suppressMessages(
      deldir::deldir(stats::runif(m, -pad, A + pad), stats::runif(m, -pad, 1 + pad),
                     rw = c(-pad, A + pad, -pad, 1 + pad))))
    px <- vapply(tl, function(t) t$pt[1], 0)
    py <- vapply(tl, function(t) t$pt[2], 0)
    nc <- length(tl)

    # which cells are drawn: clustered patches via a per-layer noise field
    if (cover >= 1) draw <- rep(TRUE, nc) else {
      cn    <- fracture(gen_simplex, fbm, octaves = 2, x = px*P$cluster_freq, y = py*P$cluster_freq, seed = P$seed + 10 + id)
      score <- P$cluster*rank(cn)/nc + (1 - P$cluster)*stats::runif(nc)
      draw  <- score > stats::quantile(score, 1 - cover)
    }
    # one flat tone per cell: light/dark region at the seed + per-cell randomness, posterised
    tt <- ((1 - P$tone_random)*F_reg(region(px, py)) + P$tone_random*stats::runif(nc))^P$tone_gamma
    ti <- pmin(n_t, 1 + floor(tt*n_t))
    sel <- tl[draw]
    list(x   = unlist(lapply(sel, function(t) c(t$x, NA))),
         y   = unlist(lapply(sel, function(t) c(t$y, NA))),
         col = grDevices::adjustcolor(pal[ti[draw]], alpha.f = a))
  }

  layers <- lapply(seq_len(K), function(k) make_layer(sizes[k], cover[k], if (k == 1) 1 else P$alpha, k))
  ord <- if (P$shuffle_layers && K > 2) c(1, 1 + sample(K - 1)) else seq_len(K)
  if (P$overlay > 0) {                                   # sparse big shards on top of the fine detail
    layers[[K + 1]] <- make_layer(sizes[min(2, K)], P$overlay, P$alpha, K + 1)
    ord <- c(ord, K + 1)
  }

  open_canvas(path, P$W, H, P$dpi, pal[1], c(0, A), c(0, 1))
  on.exit(grDevices::dev.off(), add = TRUE)
  for (k in ord) {
    L <- layers[[k]]
    graphics::polygon(L$x, L$y, col = L$col, border = if (P$seam > 0) L$col else NA, lwd = P$seam)
  }
  draw_watermark(P$watermark, c(0, A), c(0, 1), H)

  report(path, tone_id(P), P$W, H, P$dpi,
         sprintf("%d tones, %d shards", n_t, sum(vapply(layers, function(L) length(L$col), 0))))
  invisible(path)
}
