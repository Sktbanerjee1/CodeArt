#' codeart: generative data art in R
#'
#' Five rendering styles, each with a parameter constructor and a renderer:
#'
#' | Style | Parameters | Renderer |
#' |---|---|---|
#' | Extruded pin-field terrain | [pinfield_params()] | [render_pinfield()] |
#' | Layered silk ribbons | [silk_params()] | [render_silk()] |
#' | Voronoi crystal shards | [shards_params()] | [render_shards()] |
#' | Glossy rounded cube wall | [cubes_params()] | [render_cubes()] |
#' | Flowing wires over a mound | [wires_params()] | [render_wires()] |
#'
#' Shared building blocks: [watermark_params()], [set_watermark()], [tone_palette()],
#' [list_palettes()], [preview_colours()] and [render_variants()].
#'
#' Every image carries a small "CodeArt" watermark with the project address
#' (https://github.com/Sktbanerjee1/CodeArt). Render without it with
#' `clean = TRUE`, or switch it off for the whole session with
#' `set_watermark(FALSE)`.
#'
#' @keywords internal
#' @importFrom ambient fracture gen_simplex gen_perlin fbm ridged normalise
#' @importFrom farver decode_colour encode_colour convert_colour
"_PACKAGE"
