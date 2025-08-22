#' @keywords internal
#' @noRd

`%||%` <- function(x,y) if (is.null(x)) y else x

.storage_dir <- function(){
  d <- Sys.getenv("DATA_DIR", "data")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

storage_backend <- function(){
  tolower(Sys.getenv("STORAGE_BACKEND", "duckdb"))
}

storage_write_price <- function(info){
  stopifnot(is.list(info))
  rec <- data.frame(
    ts = Sys.time(),
    url = info$url %||% NA_character_,
    title = info$title %||% NA_character_,
    price_amount = info$price$amount %||% NA_real_,
    price_currency = info$price$currency %||% NA_character_,
    image = info$image %||% NA_character_,
    source = info$source %||% NA_character_,
    stringsAsFactors = FALSE
  )

  if (identical(storage_backend(), "duckdb")){
    dbfile <- file.path(.storage_dir(), "prices.duckdb")
    con <- DBI::dbConnect(duckdb::duckdb(), dbfile, read_only = FALSE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS prices(
        ts TIMESTAMP, url TEXT, title TEXT,
        price_amount DOUBLE, price_currency TEXT,
        image TEXT, source TEXT
      )
    ")
    DBI::dbAppendTable(con, "prices", rec)
    invisible(TRUE)
  } else {
    # parquet (simple): un fichier par jour
    fn <- file.path(.storage_dir(), sprintf("prices-%s.parquet", format(Sys.Date(), "%Y-%m-%d")))
    if (file.exists(fn)) {
      old <- try(arrow::read_parquet(fn), silent = TRUE)
      if (!inherits(old, "try-error")) rec <- rbind(old, rec)
    }
    arrow::write_parquet(rec, fn)
    invisible(TRUE)
  }
}

storage_read_all <- function(){
  if (identical(storage_backend(), "duckdb")){
    dbfile <- file.path(.storage_dir(), "prices.duckdb")
    if (!file.exists(dbfile)) return(utils::head(data.frame(), 0))
    con <- DBI::dbConnect(duckdb::duckdb(), dbfile, read_only = TRUE)
    on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
    DBI::dbGetQuery(con, "SELECT * FROM prices ORDER BY ts DESC")
  } else {
    d <- .storage_dir()
    files <- Sys.glob(file.path(d, "prices-*.parquet"))
    if (!length(files)) return(utils::head(data.frame(), 0))
    ds <- arrow::open_dataset(files)
    as.data.frame(arrow::collect(ds))
  }
}
