open_tile_db <- function(path) {
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  db_con
}

get_tile_info <- function(dbcon) {
  metadata <-
    DBI::dbReadTable(db_con, "metadata") |>
    purrr::pmap(
      \(name,value) switch(
        name,
        center = ,
        bounds = setNames(list(as.numeric(strsplit(value, ",")[[1]])), name),
        minzoom = ,
        maxzoom = setNames(list(as.numeric(value)), name),
        json = jsonlite::parse_json(value),
        setNames(list(value), name)
      )
    ) |>
    purrr::flatten()

    metadata$scheme <- "xyz"
    metadata$tilejson <- "2.2.0"

    metadata
}

get_tile <- function(db_con, x, y, z) {

  flipped_y <- bitwShiftL(1, z) - 1 - y

  tile_record <-
    dplyr::tbl(db_con, "tiles") |>
    dplyr::filter(
      zoom_level == z,
      tile_column == x,
      tile_row == flipped_y
    ) |>
    dplyr::collect()
  
  tile_record$tile_data

}
