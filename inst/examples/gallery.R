# ===== codeart gallery script =====
# Usage after install:  source(system.file("examples", "gallery.R", package = "codeart"))
# or open this file and run the blocks you want. Renders go to ./renders.
library(codeart)

out <- "renders"

# ---- single renders (watermarked + clean) ----
render_pinfield(pinfield_params(out_dir = out))
render_silk(silk_params(out_dir = out))
render_shards(shards_params(out_dir = out))
render_cubes(cubes_params(out_dir = out))
#render_cubes(cubes_params(out_dir = out), clean = TRUE)

# ---- every palette of a style ----
#render_variants(render_pinfield, pinfield_params(out_dir = out), palette = names(PINFIELD_PALETTES))
#render_variants(render_silk,     silk_params(out_dir = out),     palette = names(SILK_PALETTES))
#render_variants(render_shards,   shards_params(out_dir = out),   mono = names(MONO_COLOURS))
#render_variants(render_shards,   shards_params(out_dir = out, palette_mode = "multi"), multi = names(MULTI_PALETTES))
#render_variants(render_cubes,    cubes_params(out_dir = out),    mono = names(MONO_COLOURS))
#render_variants(render_cubes,    cubes_params(out_dir = out, palette_mode = "multi"), multi = names(MULTI_PALETTES), clean = TRUE)

# ---- other aspect ratios ----
#render_pinfield(pinfield_params(out_dir = out, aspect = "4:5", W = 10))
#render_silk(silk_params(out_dir = out, aspect = "21:9"))
#render_shards(shards_params(out_dir = out, aspect = "16:9", W = 16))
#render_cubes(cubes_params(out_dir = out, aspect = "9:16", W = 9))

# ---- composition search: low-res drafts for several seeds ----
#render_variants(render_silk, silk_params(out_dir = file.path(out, "drafts"), compute_px = 800, dpi = 100),
#                seed = c(7, 42, 101, 2025, 4242, 9001), clean = TRUE)

# ---- palette swatches ----
#preview_colours(tone_palette("mono", "steel_teal", n = 6))
#preview_colours(SILK_PALETTES$nebula$ramp)
#list_palettes()

# ---- your own watermark defaults for the session ----
#options(codeart.name = "Saikat Bandyopadhyay", codeart.label = "CodeArt")
#render_silk(silk_params(out_dir = out, watermark = watermark_params(position = "top-left", alpha = 0.6)))
