# tiny renders: check that every style writes a PNG of the right pixel size
png_dims <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  hdr <- readBin(con, "raw", 24)
  c(w = sum(as.integer(hdr[17:20]) * 256^(3:0)), h = sum(as.integer(hdr[21:24]) * 256^(3:0)))
}

small <- list(W = 2, dpi = 60, out_dir = tempdir())

test_that("pin-field renders", {
  p <- do.call(pinfield_params, c(small, list(nx = 40, ny = 40)))
  f <- suppressMessages(render_pinfield(p))
  expect_true(file.exists(f))
  d <- png_dims(f)
  expect_equal(unname(d[["w"]]), 120)
  expect_lt(abs(d[["h"]] - 120*9/16), 1.01)
})

test_that("silk renders, clean variant gets suffix", {
  p <- do.call(silk_params, c(small, list(compute_px = 120)))
  f <- suppressMessages(render_silk(p, clean = TRUE))
  expect_match(f, "_clean\\.png$")
  expect_true(file.exists(f))
})

test_that("shards render in both palette modes", {
  for (mode in c("mono", "multi")) {
    p <- do.call(shards_params, c(small, list(palette_mode = mode, n_layers = 3, size = c(0.3, 0.1))))
    expect_true(file.exists(suppressMessages(render_shards(p))))
  }
})

test_that("cubes render", {
  p <- do.call(cubes_params, c(small, list(compute_px = 120)))
  expect_true(file.exists(suppressMessages(render_cubes(p))))
})

test_that("render_variants names every file uniquely", {
  p <- do.call(cubes_params, c(small, list(compute_px = 80)))
  f <- suppressMessages(render_variants(render_cubes, p, seed = 1:2, mono = c("teal", "rose")))
  expect_length(unique(f), 4)
  expect_true(all(file.exists(f)))
})
