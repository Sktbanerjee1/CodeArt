# Internal helpers shared by all styles -------------------------------------

# "16:9" -> 1.777...
parse_aspect <- function(a) {
  r <- suppressWarnings(as.numeric(strsplit(a, ":", fixed = TRUE)[[1]]))
  if (length(r) != 2 || any(is.na(r)) || any(r <= 0)) stop("aspect must look like \"16:9\"", call. = FALSE)
  r[1] / r[2]
}

# smooth Hermite step from 0 (x <= e0) to 1 (x >= e1)
smooth01 <- function(e0, e1, x) {
  t <- pmin(1, pmax(0, (x - e0)/(e1 - e0)))
  t*t*(3 - 2*t)
}

# separable box blur of a matrix with radius r (cells)
moving_mean <- function(z, r) {
  f <- stats::filter(z, rep(1/(2*r + 1), 2*r + 1), sides = 2)
  f[is.na(f)] <- z[is.na(f)]
  as.numeric(f)
}
box_blur <- function(M, r) t(apply(apply(M, 2, moving_mean, r = r), 1, moving_mean, r = r))

# sRGB <-> linear light, values in [0, 1]
to_linear <- function(c) ifelse(c <= 0.04045, c/12.92, ((c + 0.055)/1.055)^2.4)
to_srgb   <- function(c) { c <- pmax(c, 0); ifelse(c <= 0.0031308, 12.92*c, 1.055*c^(1/2.4) - 0.055) }
unit_vec  <- function(v) v / sqrt(sum(v^2))

# internal raster size from aspect ratio and long side
raster_dims <- function(A, compute_px) {
  if (A >= 1) c(w = compute_px, h = round(compute_px/A)) else c(w = round(compute_px*A), h = compute_px)
}

# settings every style shares
common_defaults <- function(seed, aspect, W, dpi = 300) {
  list(
    seed      = seed,
    aspect    = aspect,              # "W:H"; width W (inches) is fixed, height follows
    W         = W,
    dpi       = dpi,
    out       = NULL,                # NULL = automatic file name
    out_dir   = ".",                 # folder for automatic and relative file names
    suffix    = "",                  # appended to the file name (used by render_variants)
    watermark = watermark_params()
  )
}

# merge user overrides into defaults; unknown names are an error (catches typos)
merge_params <- function(defaults, overrides, caller) {
  if (length(overrides) && (is.null(names(overrides)) || any(names(overrides) == "")))
    stop(caller, "(): all parameters must be named", call. = FALSE)
  bad <- setdiff(names(overrides), names(defaults))
  if (length(bad))
    stop(caller, "(): unknown parameter(s): ", paste(bad, collapse = ", "),
         "\n  valid: ", paste(names(defaults), collapse = ", "), call. = FALSE)
  utils::modifyList(defaults, overrides)
}

# file path for a render: <out_dir>/<prefix>_<id>_<aspect><suffix>[_clean].png
output_path <- function(P, prefix, id, clean) {
  tail <- paste0(P$suffix, if (clean) "_clean" else "")
  name <- if (is.null(P$out)) {
    sprintf("%s_%s_%s%s.png", prefix, id, gsub(":", "x", P$aspect), tail)
  } else sub("\\.png$", paste0(tail, ".png"), P$out)
  if (!dir.exists(P$out_dir)) dir.create(P$out_dir, recursive = TRUE)
  file.path(P$out_dir, name)
}

# clean renders switch the watermark off
apply_clean <- function(P, clean) {
  if (isTRUE(clean)) P$watermark$enabled <- FALSE
  P
}
