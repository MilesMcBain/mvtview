init_json_metadata <- function(tiles_url, max_attempt_time = 5) {

  tiles_metadata <- NULL
  attempt_start <- Sys.time()
  while (
    is.null(tiles_metadata) &&
      as.numeric(Sys.time() - attempt_start, unit = "secs") < max_attempt_time
  ) {
    tiles_metadata <- read_json_safely(tiles_url)
    if (is.null(tiles_metadata)) Sys.sleep(0.5)
  }

  tiles_metadata
}

read_json_safely <- function(tiles_url) {
  tiles_metadata <- tryCatch(
    jsonlite::read_json(tiles_url),
    error = function(e) NULL,
    warning = function(e) {
      if (grepl("Couldn't connect to server", e$message)) {
        NULL
      } else {
        e
      }
    }
  )
}