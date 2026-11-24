test_that("params constructors return defaults and accept overrides", {
  expect_equal(pinfield_params()$palette, "chromatic")
  expect_equal(silk_params(palette = "ocean")$palette, "ocean")
  expect_equal(shards_params(mono = "cobalt")$mono, "cobalt")
  expect_equal(cubes_params(spacing = 1.1)$spacing, 1.1)
  expect_equal(wires_params()$mono, "silver")
  expect_equal(wires_params(n_wires = 50)$n_wires, 50)
})

test_that("unknown or unnamed settings are rejected", {
  expect_error(silk_params(pallete = "ocean"), "unknown parameter")
  expect_error(cubes_params(1), "must be named")
})

test_that("watermark defaults to the project URL and can be switched off", {
  wm <- watermark_params()
  expect_equal(wm$caption, "https://github.com/Sktbanerjee1/CodeArt")
  expect_equal(watermark_params(name = "Old Name")$caption, "Old Name")   # deprecated alias
  old <- set_watermark(FALSE, caption = "Someone")
  on.exit(options(old), add = TRUE)
  expect_false(watermark_params()$enabled)
  expect_false(silk_params()$watermark$enabled)
  expect_equal(cubes_params()$watermark$caption, "Someone")
  options(old)
  expect_true(watermark_params()$enabled)
})

test_that("nested watermark overrides merge", {
  p <- shards_params(watermark = list(enabled = FALSE))
  expect_false(p$watermark$enabled)
  expect_equal(p$watermark$label, "CodeArt")
})

test_that("aspect parsing", {
  expect_equal(codeart:::parse_aspect("16:9"), 16/9)
  expect_error(codeart:::parse_aspect("wide"), "aspect")
})

test_that("tone palettes have n colours, dark to light", {
  for (mode in c("mono", "multi")) {
    cols <- tone_palette(mode, n = 6)
    expect_length(cols, 6)
    L <- farver::decode_colour(cols, to = "lab")[, 1]
    expect_true(all(diff(L) > 0))
  }
  expect_length(tone_palette("mono", "#E0457B", n = 4), 4)
  expect_error(tone_palette("multi", multi = "nope"), "multi must be one of")
})

test_that("output paths follow the naming scheme", {
  P <- cubes_params(out_dir = tempdir())
  expect_equal(basename(codeart:::output_path(P, "iso_cubes", "mono-steel_teal", FALSE)),
               "iso_cubes_mono-steel_teal_3x2.png")
  expect_equal(basename(codeart:::output_path(P, "iso_cubes", "mono-steel_teal", TRUE)),
               "iso_cubes_mono-steel_teal_3x2_clean.png")
})
