SESSION <- new.env()

view_mvt <- function(
  tiles_path,
  get_fill_color = "#FFFFFF70",
  get_line_color = "#ffffffff",
  get_line_width = 2,
  line_width_units = "pixels",
  point_radius_units = "pixels",
  get_point_radius = 2,
  stroked = TRUE,
  tooltip = everything(),
  ...,
  .serve_mode = "in-memory"
) {
  host <- "0.0.0.0"
  port <- httpuv::randomPort(host = host)
  server <- callr::r_session$new()
  server$call(
    function(...) mbvr::serve_mvt(...),
    args = list(
      tiles_path = tiles_path,
      host = host,
      port = port,
      serve_mode = .serve_mode
    )
  )
  push_server(server) # so we can clean it up later

  Sys.sleep(3) # give the server a chance to spool up
  if (!server$is_alive()) {
    stop(server$read_all_error_lines())
  }
  

  tiles_url <-
    glue::glue("http://{host}:{port}/{tileset_name(tiles_path)}.json")

  tiles_metadata <- jsonlite::read_json(tiles_url)

  rdeck::rdeck(
    initial_bounds = structure(unlist(tiles_metadata$bounds), crs = 4326, class = "bbox")
  ) |>
    rdeck::add_mvt_layer(
      data = tiles_url,
      get_line_color = {{ get_line_color }},
      get_fill_color = {{ get_fill_color }},
      get_line_width = {{ get_line_width }},
      line_width_units = {{ line_width_units }},
      point_radius_units = {{ point_radius_units }},
      get_point_radius = {{ get_point_radius }},
      tooltip = FALSE,
      pickable = FALSE,
      stroked = {{ stroked }},
      ...
    )
}

push_server <- function(server) {
  if (is.null(SESSION$servers)) SESSION$servers <- list()

  SESSION$servers <- c(SESSION$servers, server)
}

clean_up <- function() {
  lapply(SESSION$servers, function(server) server$kill())
  SESSION$servers <- NULL
}