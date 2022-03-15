open_tile_db <- function(path) {
  db_con <- DBI::dbConnect(RSQLite::SQLite(), dbname = path)
  db_con
}

get_tile_info <- function(dbcon) {
  db_con |>
    DBI::dbListTables()

  metadata <-
    DBI::dbReadTable(db_con, "metadata") |>
    purrr::pmap(
      \(name, value)
       switch(name,
        center = ,
        bounds = setNames(list(as.numeric(strsplit(value, ",")[[1]])), name),
        minzoom = ,
        maxzoom = setNames(list(as.numeric(value)), name),
        json = jsonlite::parse_json(value),
        setNames(list(value), name)
       )
    ) |> 
    purrr::flatten()
}
