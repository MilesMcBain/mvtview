mbvr <- function(tiles_path) {
  if (!file.exists(tiles_path)) stop(
    "could not find the tile database: ", 
    tiles_path
  )
  if (!is_mbtiles(tiles_path)) stop(
    "{mbvr} only supports serving .mbtiles tile databases"
  )

  mvt_server <- create_mvt_server(tiles_path)
  mvt_server$start()

}

is_mbtiles <- function(tiles_path) {
  fs::path_ext(tiles_path) == "mbtiles"
}

tileset_name <- function(tiles_path) {
  fs::path_ext_remove(fs::path_file(tiles_path))
}

create_mvt_server <- function(tiles_path) {
  server <- ambiorix::Ambiorix$new()
  tile_db <- open_tile_db(tiles_path)
  tile_json_name <- glue::glue("{tileset_name(tiles_path)}.json")

  server$get("/", function(req, res) {
    res$send("{mbvr} a Mapbox vector tile viewer for R, powered by {rdeck} and {ambiorix}.")
  })

  server$get(tile_json_name, function(req, res) {
    tile_info <- get_tile_info(tile_db)
    tile_template_url <- 
      glue::glue("http://{req$HEADERS['host']}/{tile_json_name}/{{z}}/{{x}}/{{y}}.vector.pbf")
    tile_info$tiles <- tile_template_url
 
    res$json(tile_info)
  })
}