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
  message(glue::glue("Serving your tile data from {tiles_url}.\nRun clean_mvt() to remove all server sessions."))


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
  mvt_server <- create_mvt_server(tiles_path, .serve_mode)

  plumber::pr_run(mvt_server, host = host, port = port, docs = FALSE)
}

is_mbtiles <- function(tiles_path) {
  fs::path_ext(tiles_path) == "mbtiles"
}

tileset_name <- function(tiles_path) {
  fs::path_ext_remove(fs::path_file(tiles_path))
}

create_mvt_server <- function(tiles_path, .serve_mode) {
  tileset_id <- tileset_name(tiles_path)
  tile_json_name <- glue::glue("{tileset_id}.json")
  tile_json_route <- glue::glue("/{tile_json_name}")
  tileset_id <- tileset_name(tiles_path)
  tile_route <- glue::glue("/{tileset_id}/<z>/<x>/<y>")
  tiles_path <- tiles_path
  tile_db <- if (.serve_mode == "in-memory") {
    read_tile_db(tiles_path)
  } else { # "disk"
    open_tile_db(tiles_path)
  }

  app <-
    plumber::pr() %>%
    plumber::pr_get(
      "/",
      function() {
        "{mbvr} a Mapbox vector tile viewer for R, powered by {rdeck} and {plumber}"
      },
      serializer = plumber::serializer_html()
    ) %>%
    plumber::pr_get(
      tile_json_route,
      function(req, res) {
        tile_info <- get_tile_info(tile_db)
        tile_template_url <-
          glue::glue("http://{req$HTTP_HOST}/{tileset_id}/{{z}}/{{x}}/{{y}}.vector.pbf")
        tile_info$tiles <- tile_template_url
        res$headers$`Access-Control-Allow-Origin` <- "*"

        tile_info
      },
      serializer = plumber::serializer_unboxed_json()
    ) %>%
    plumber::pr_get(
      tile_route,
      function(req, res) {
        # tile_path has params z, x, y
        y <- as.numeric(stringr::str_extract(req$argsPath$y, "[0-9]+"))
        x <- as.numeric(req$argsPath$x)
        z <- as.numeric(req$argsPath$z)

        tile_content <- get_tile(tile_db, z, x, y)

        if (length(tile_content) > 0) {
          res$headers$`Access-Control-Allow-Origin` <- "*"
          res$headers$`Content-Type` <- "application/octect-stream"
          res$headers$`Content-Encoding` <- "gzip"
          res$body <- tile_content
          return(res)
        } else {
          res$status <- 204
          return(res)
        }
      }
    )

  app
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
