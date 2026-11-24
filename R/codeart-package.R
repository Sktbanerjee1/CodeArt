#' codeart: generative data art in R
#'
#' Four rendering styles, each with a parameter constructor and a renderer:
#'
#' | Style | Parameters | Renderer |
#' |---|---|---|
#' | Extruded pin-field terrain | [pinfield_params()] | [render_pinfield()] |
#' | Layered silk ribbons | [silk_params()] | [render_silk()] |
#' | Voronoi crystal shards | [shards_params()] | [render_shards()] |
#' | Glossy rounded cube wall | [cubes_params()] | [render_cubes()] |
#'
#' Shared building blocks: [watermark_params()], [tone_palette()],
#' [list_palettes()], [preview_colours()] and [render_variants()].
#'
#' Default watermark text can be set once per session with
#' `options(codeart.name = "Your Name", codeart.label = "CodeArt")`.
#'
#' @keywords internal
#' @importFrom ambient fracture gen_simplex gen_perlin fbm ridged normalise
#' @importFrom farver decode_colour encode_colour convert_colour
"_PACKAGE"
