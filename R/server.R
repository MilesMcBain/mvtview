utils::globalVariables("SESSION")
SESSION <- new.env()

#' Serve a .mbtiles database of vectortiles
#' 
#' Starts a web server in a background R session serving vector tiles from a
#' supplied .mbtiles file.
#' 
#' @param tiles_path The path to an .mbtiles file.
#' @param .serve_mode The way in which the server handles the vector tiles
#' database. "in-memory" is the default and it will read the entire tile database
#' into R as a tibble. "disk" will read tiles from the .mbtiles file as an
#' SQLite database  from disk. The default is more performant. Use "disk" only if
#' you have a large vector tileset that would consume too much memory to hold in
#' RAM at once.
#' 
#' @seealso start_mvt_server for more control of server behaviour.
#'
#' @export
serve_mvt <- function(tiles_path, .serve_mode = "in-memory") {
  host <- "0.0.0.0"
  port <- httpuv::randomPort(host = host)
  server <- callr::r_session$new()
  server$call(
    function(...) mvtview::start_mvt_server(...),
    args = list(
      tiles_path = tiles_path,
      host = host,
      port = port,
      .serve_mode = .serve_mode
    )
  )
  push_server(structure(server, host = host, port = port)) # so we can clean it up later

  tiles_url <-
    glue::glue("http://{host}:{port}/{tileset_name(tiles_path)}.json")


  max_attempt_time <- 5
  tiles_metadata <- init_json_metadata(tiles_url, max_attempt_time)

  if (is.null(tiles_metadata)) {
    if (!server$is_alive()) {
      stop("mbtiles server died with error:", server$read_all_error_lines())
    } else {
      stop(
        "Could not receive mbtiles metadata from server, attempting for ",
        max_attempt_time,
        " seconds."
      )
    }
  }

  message(glue::glue("Serving your tile data from {tiles_url}.\nRun clean_mvt() to remove all server sessions."))

  invisible(
    structure(
      list(
        server = server,
        host = host,
        port = port,
        tiles_url = tiles_url,
        tiles_metadata = tiles_metadata
      ),
      class = "live_mvt_server"
    )
  )
}

#' Start an mvt_server in the current session
#'
#' 
#' Starts a web server serving vector tiles from a supplied .mbtiles file.
#' 
#' [serve_mvt()] is likely more convenient. Only use this if if you want more
#' control of the host and port on which your tiles are served on.
#' 
#' Where [serve_mvt()] verifies the server is actually up and responding, this
#' funciton does not. So that's up to you to take on.
#' 
#' Note: This server has been built minimising code written, not ' maximising
#' performance. It is intended for local development work, and will likely not
#' be performant enough for any production use-case.
#' 
#' @inheritParams serve_mvt
#' @param host the host to serve tiles on
#' @param port the port to serve tiles on
#' @export
start_mvt_server <- function(tiles_path, host = "0.0.0.0", port = NULL, .serve_mode = "in-memory") {
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

  if (!(.serve_mode %in% c("in-memory", "disk"))) {
    stop(
      "supported .serve_mode for tiles is either 'in-memory' or 'disk'."
    )
  }

  mvt_server <- create_mvt_server(tiles_path, host, port, .serve_mode)
  mvt_server$start()

  invisible(mvt_server)

}

is_mbtiles <- function(tiles_path) {
  fs::path_ext(tiles_path) == "mbtiles"
}

tileset_name <- function(tiles_path) {
  fs::path_ext_remove(fs::path_file(tiles_path))
}

create_mvt_server <- function(tiles_path, host, port, .serve_mode) {
  server <- ambiorix::Ambiorix$new(host = host, port = port, log = FALSE)
  tile_db <- if (.serve_mode == "in-memory") {
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

push_server <- function(server) {
  if (is.null(SESSION$servers)) SESSION$servers <- list()

  SESSION$servers <- c(SESSION$servers, server)
}

#' Stop all running vector tile servers
#' 
#' As you use serve_mvt or view_mvt servers will accumulate in child processes.
#' This function kills all child processes serving tiles. 
#'
#' @export
clean_mvt <- function() {
  lapply(SESSION$servers, function(server) {
    message(glue::glue("Removing http://{attr(server, 'host')}:{attr(server, 'port')}"))
    server$kill()
  })
  SESSION$servers <- NULL
}
