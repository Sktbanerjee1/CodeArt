# ===== README figures =====
# Renders every image the README uses, at 900 x 600 px (3:2) with the current
# watermark, into renders/readme/. Run from the project folder:
#   source("tools/readme_figures.R")
# The thumbnails, palette sheets and the animated GIF in man/figures/ are then
# built from these files. Takes a few minutes; progress is printed per image.

if (requireNamespace("devtools", quietly = TRUE) && file.exists("DESCRIPTION")) {
  devtools::load_all(".", quiet = TRUE)
} else {
  library(codeart)
}

out  <- file.path("renders", "readme")
base <- list(aspect = "3:2", W = 6, dpi = 150, out_dir = out)     # 900 x 600 px

job <- function(render_fun, params_fun, file, ...) {
  p <- do.call(params_fun, c(base, list(out = file), list(...)))
  t0 <- Sys.time()
  render_fun(p)
  message(sprintf("  %-28s %5.1f s", file, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

message("Pin-field")
for (pl in c("chromatic", "aurora", "ember"))
  job(render_pinfield, pinfield_params, sprintf("pinfield_%s.png", pl), palette = pl)

message("Silk flow")
for (pl in names(SILK_PALETTES))
  job(render_silk, silk_params, sprintf("silk_%s.png", pl), palette = pl, compute_px = 1100)

mono_multi <- function(render_fun, params_fun, prefix, mono, multi, ...) {
  for (m in mono)  job(render_fun, params_fun, sprintf("%s_mono-%s.png", prefix, m),
                       palette_mode = "mono", mono = m, ...)
  for (m in multi) job(render_fun, params_fun, sprintf("%s_multi-%s.png", prefix, m),
                       palette_mode = "multi", multi = m, ...)
}

message("Crystal shards")
mono_multi(render_shards, shards_params, "shards",
           mono = c("teal", "cobalt", "crimson"), multi = c("tropical", "sunset", "berry"))

message("Iso cubes")
mono_multi(render_cubes, cubes_params, "cubes",
           mono = c("steel_teal", "violet", "amber"), multi = c("sunset", "ocean", "neon"),
           compute_px = 1100)

message("Wire flow")
mono_multi(render_wires, wires_params, "wires",
           mono = c("silver", "amber", "violet"), multi = c("ocean", "neon", "sunset"))

message("Done: ", length(list.files(out, pattern = "\\.png$")), " images in ", normalizePath(out))
