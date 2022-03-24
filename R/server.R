#' @export
serve_mvt <- function(tiles_path, host = "0.0.0.0", port = NULL, serve_mode = "in-memory") {
  if (!file.exists(tiles_path)) {
    stop(
      "could not find the tile database: ",
      tiles_path
    )
  }
  if (!is_mbtiles(tiles_path)) {
    stop(
      "{mbvr} only supports serving .mbtiles tile databases"
    )
  }

  if (!(serve_mode %in% c("in-memory", "disk"))) {
    stop(
      "support serve_mode for tiles is either 'in-memory' or 'disk'."
    )
  }

  mvt_server <- create_mvt_server(tiles_path, host, port, serve_mode)
  mvt_server$start()

}

is_mbtiles <- function(tiles_path) {
  fs::path_ext(tiles_path) == "mbtiles"
}

tileset_name <- function(tiles_path) {
  fs::path_ext_remove(fs::path_file(tiles_path))
}

create_mvt_server <- function(tiles_path, host, port, serve_mode) {
  server <- ambiorix::Ambiorix$new(host = host, port = port, log = FALSE)
  tile_db <- if (serve_mode == "in-memory") {
    read_tile_db(tiles_path) 
  } else { # "disk"
    open_tile_db(tiles_path)
  }
  tileset_id <- tileset_name(tiles_path)
  tile_json_name <- glue::glue("{tileset_id}.json")
  tileset_id <- tileset_name(tiles_path)
  tile_path <- glue::glue("/{tileset_id}/:z/:x/:y.vector.pbf")

  server$get("/", function(req, res) {
    res$send("{mbvr} a Mapbox vector tile viewer for R, powered by {rdeck} and {ambiorix}.")
  })

  server$get(tile_json_name, function(req, res) {
    tile_info <- get_tile_info(tile_db)
    tile_template_url <-
      glue::glue("http://{req$HEADERS['host']}/{tileset_id}/{{z}}/{{x}}/{{y}}.vector.pbf")
    tile_info$tiles <- tile_template_url

    res$header("Access-Control-Allow-Origin", "*")
    res$json(tile_info)
  })

  server$get(tile_path, function(req, res) {
    y <- as.numeric(stringr::str_extract(req$params$y.vector.pbf, "[0-9]+"))
    x <- as.numeric(req$params$x)
    z <- as.numeric(req$params$z)

    res$header("Access-Control-Allow-Origin", "*")
    res$header("Content-Type", "application/octect-stream")
    res$header("Content-Encoding", "gzip")
    tile_content <- get_tile(tile_db, z, x, y)
    if (length(tile_content) > 0) {
      res$send(
        tile_content
      )
    } else {
      res$set_status(204) # no content
      res$send("No content")
    }

  })

  server
}
