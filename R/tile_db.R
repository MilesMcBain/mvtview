open_tile_db <- function(path) {
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  db_con
}

read_tile_db <- function(path) {
  db_con <- open_tile_db(path)

  structure(
    list(
      metadata = get_tile_info(db_con),
      data = dplyr::collect(dplyr::tbl(db_con, "tiles"))
    ),
    class = "in_mem_tile_db"
  )
}

get_tile_info <- function(db) UseMethod("get_tile_info", db)

#' @export
get_tile_info.in_mem_tile_db <- function(db) {
  db$metadata
}

#' @export
get_tile_info.SQLiteConnection <- function(db) {
  metadata <-
    DBI::dbReadTable(db, "metadata") %>%
    purrr::pmap(
      \(name,value) switch(
        name,
        center = ,
        bounds = stats::setNames(list(as.numeric(strsplit(value, ",")[[1]])), name),
        minzoom = ,
        maxzoom = stats::setNames(list(as.numeric(value)), name),
        json = jsonlite::parse_json(value),
        stats::setNames(list(value), name)
      )
    ) %>%
    purrr::flatten()

    metadata$scheme <- "xyz"
    metadata$tilejson <- "2.2.0"

    metadata
}

get_tile <- function(db, z, x, y) UseMethod("get_tile", db)

#' @export
get_tile.in_mem_tile_db <- function(db, z, x, y) {
  get_tile_impl(db$data, z, x, y)
}

#' @export
get_tile.SQLiteConnection <- function(db, z, x, y) {
  get_tile_impl(dplyr::tbl(db, "tiles"), z, x, y)
}

#' @autoglobal
get_tile_impl <- function(tile_table, z, x, y) {

  flipped_y <- bitwShiftL(1, z) - 1 - y

  tile_record <-
    tile_table %>%
    dplyr::filter(
      zoom_level == z,
      tile_column == x,
      tile_row == flipped_y
    ) %>% 
    dplyr::collect()
  
  if (length(tile_record$tile_data) == 1)
    return(tile_record$tile_data[[1]])
    # get the raw vector out of the blob
  else
    return(NULL)
}

