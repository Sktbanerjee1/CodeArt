# codeart 0.1.0

First public release.

## Styles

* `render_pinfield()`: an extruded pin-field terrain seen from an oblique camera.
* `render_silk()`: layered silk ribbons with crisp folds, cast shadows, bloom and film grain.
* `render_shards()`: stacked Voronoi crystal shards with posterised tones, drawn as vector polygons.
* `render_cubes()`: a wall of glossy rounded boxes, ray-cast with bevelled edges and a spotlight.
* `render_wires()`: tube-shaded wires following a flow field over smooth hills.

## Shared features

* One `*_params()` constructor per style; misspelled setting names are an error.
* Palette presets: 3 pin-field, 10 silk, and for shards, cubes and wires 10 mono base colours
  (or any hex colour) plus 10 multi-colour palettes (`tone_palette()`, `list_palettes()`,
  `preview_colours()`).
* Any aspect ratio (`aspect = "W:H"`), width in inches and dpi.
* Watermark "CodeArt" + project URL; render without it with `clean = TRUE`,
  `watermark_params(enabled = FALSE)` or session-wide with `set_watermark(FALSE)`.
* `render_variants()` renders every combination of palettes, seeds or other settings with
  unique file names.
