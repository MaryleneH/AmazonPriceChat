`%||%` <- function(x,y) if (is.null(x)) y else x

storage_path <- function(){
  dir <- Sys.getenv("DATA_DIR", unset = "data")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  list(
    duckdb  = file.path(dir, "prices.duckdb"),
    parquet = file.path(dir, "prices.parquet")
  )
}

storage_write_price <- function(info){
  stopifnot(is.list(info), !is.null(info$price$amount))
  rec <- data.frame(
    scraped_at = as.POSIXct(Sys.time(), tz = "UTC"),
    title      = info$title %||% NA_character_,
    amount     = as.numeric(info$price$amount),
    currency   = info$price$currency %||% "EUR",
    url        = info$url %||% NA_character_,
    source     = info$source %||% NA_character_,
    image      = info$image %||% NA_character_,
    handle     = sub(".*/products/([^/?#]+).*", "\\1", info$url %||% ""),
    variant_id = {
      v <- sub(".*[?&]variant=([0-9]+).*", "\\1", info$url %||% "")
      if (grepl("^[0-9]+$", v)) v else NA_character_
    },
    stringsAsFactors = FALSE
  )

  backend <- tolower(Sys.getenv("STORAGE_BACKEND", "duckdb"))
  paths <- storage_path()

  if (backend == "duckdb") {
    con <- DBI::dbConnect(duckdb::duckdb(paths$duckdb, read_only = FALSE))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS price_logs(
        scraped_at TIMESTAMP,
        title TEXT,
        amount DOUBLE,
        currency TEXT,
        url TEXT,
        source TEXT,
        image TEXT,
        handle TEXT,
        variant_id TEXT
      )")
    DBI::dbAppendTable(con, "price_logs", rec)
    invisible(paths$duckdb)
  } else {
    if (file.exists(paths$parquet)) {
      old <- arrow::read_parquet(paths$parquet)
      all <- rbind(old, rec)
    } else {
      all <- rec
    }
    arrow::write_parquet(all, paths$parquet)
    invisible(paths$parquet)
  }
}

storage_read_all <- function(){
  backend <- tolower(Sys.getenv("STORAGE_BACKEND", "duckdb"))
  paths <- storage_path()
  if (backend == "duckdb") {
    con <- DBI::dbConnect(duckdb::duckdb(paths$duckdb, read_only = TRUE))
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    if (!DBI::dbExistsTable(con, "price_logs")) return(utils::head(data.frame(), 0))
    DBI::dbReadTable(con, "price_logs")
  } else {
    if (!file.exists(paths$parquet)) return(utils::head(data.frame(), 0))
    arrow::read_parquet(paths$parquet)
  }
}
