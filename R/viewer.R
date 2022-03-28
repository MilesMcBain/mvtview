#' View a local vector tileset on a map
#' 
#' Given a local .mtiles file containing a vector tiles database, this function
#' will start a local development server to serve the tiles and then return a htmlwidget map
#' that displays the tileset.
#' 
#' The map is powered by the awesome {rdeck} package, which is highly
#' recommended for making interactive WebGL maps in R.
#' 
#' The graphics options of this function are passed directed to '
#' [rdeck::add_mvt_layer()], and so suppourt {rdeck} color scales based on
#' attributes. See the {rdeck} helpfile for more detailed ' descriptions. 
#' 
#' The graphics parameters apply only to relvant geometries. For ' example:
#' 'fill color' is not used for line string features.
#' 
#' @inheritParams serve_mvt
#' @param get_fill_color the fill colour of plotted features.
#' @param get_line_color the line colour of plotted features.
#' @param get_line_width the line width of plotted features (in pixels by default).
#' @param line_width_units the units of the value supplied in `get_line_width`.
#'   "meters" may be preferred in some cases.
#' @param get_point_radius the radius of plotted point features (in pixels by default).
#' @param point_radius_units the units of the value supplied in `get_point_radius`.
#'   "meters" may be preferred in some cases.
#' @param stroked use a line on the borders of polygons or points? TRUE by default.
#' @param tooltip generate a tooltip for feature attributes? TRUE by default.
#' @param pickable allow map to react to features that get mouse hover? Needs to be
#'  enabled to view tooltips. TRUE by default.
#' @param ... further arguments forwarded to [rdeck::add_mvt_layer()].
#' @export
view_mvt <- function(
  tiles_path,
  get_fill_color = "#FFFFFF70",
  get_line_color = "#ffffffff",
  get_line_width = 2,
  line_width_units = "pixels",
  get_point_radius = 2,
  point_radius_units = "pixels",
  stroked = TRUE,
  tooltip = TRUE,
  pickable = TRUE,
  ...,
  .serve_mode = "in-memory"
) {

  server <- serve_mvt(tiles_path, .serve_mode = .serve_mode)

  rdeck::rdeck(
    initial_bounds = structure(unlist(server$tiles_metadata$bounds), crs = 4326, class = "bbox")
  ) %>%
    rdeck::add_mvt_layer(
      data = rdeck::tile_json(server$tiles_url),
      get_line_color = {{ get_line_color }},
      get_fill_color = {{ get_fill_color }},
      get_line_width = {{ get_line_width }},
      line_width_units = {{ line_width_units }},
      get_point_radius = {{ get_point_radius }},
      point_radius_units = {{ point_radius_units }},
      tooltip = {{ tooltip }},
      pickable = {{ pickable }},
      stroked = {{ stroked }},
      ...
    )
}
