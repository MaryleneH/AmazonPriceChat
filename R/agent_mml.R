#' @keywords internal
#' @noRd

`%||%` <- function(x,y) if (is.null(x)) y else x

mml_domain <- function(){
  dom <- Sys.getenv("MML_DOMAIN", "www.makemylemonade.com")
  dom <- trimws(sub("^https?://", "", dom))
  sub("/+$", "", dom)
}

mml_slugify <- function(x){
  x <- tolower(trimws(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-|-$)", "", x)
  x
}

mml_fetch_text <- function(url, accept = "application/json"){
  resp <- httr2::request(url) |>
    httr2::req_headers(
      `User-Agent`="Mozilla/5.0",
      `Accept`=accept,
      `Accept-Language`="fr-FR,fr;q=0.9,en;q=0.8"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  list(status = httr2::resp_status(resp),
       ct     = httr2::resp_content_type(resp),
       text   = httr2::resp_body_string(resp))
}

mml_search_suggest <- function(query, limit = 12){
  u <- sprintf(
    "https://%s/search/suggest.json?q=%s&resources[type]=product&resources[limit]=%d&resources[options][fields]=title,product_type,variants.title",
    mml_domain(),
    utils::URLencode(query, reserved = TRUE),
    limit
  )
  fx <- try(mml_fetch_text(u, accept = "application/json"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400 || !grepl("json", fx$ct, TRUE)) return(character())

  j <- try(jsonlite::fromJSON(fx$text), silent = TRUE)
  if (inherits(j,"try-error")) return(character())
  prods <- try(j$resources$results$products, silent = TRUE)
  if (inherits(prods,"try-error") || is.null(prods)) return(character())

  urls <- prods$url %||% character()
  urls <- urls[nzchar(urls)]
  if (!length(urls)) return(character())
  paste0("https://", mml_domain(), urls)
}

mml_collection_products_json <- function(slug, limit = 50){
  u <- sprintf("https://%s/collections/%s/products.json?limit=%d", mml_domain(), slug, limit)
  fx <- try(mml_fetch_text(u, accept = "application/json"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400 || !grepl("json", fx$ct, TRUE)) return(character())

  j <- try(jsonlite::fromJSON(fx$text), silent = TRUE)
  if (inherits(j,"try-error")) return(character())
  products <- j$products %||% NULL
  if (is.null(products) || is.null(products$handle)) return(character())
  handles <- products$handle
  handles <- handles[nzchar(handles)]
  unique(paste0("https://", mml_domain(), "/products/", handles))
}

mml_collection_products_html <- function(slug){
  u <- sprintf("https://%s/collections/%s", mml_domain(), slug)
  fx <- try(mml_fetch_text(u, accept = "text/html,application/xhtml+xml"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(character())

  doc <- xml2::read_html(fx$text)
  hrefs <- rvest::html_attr(rvest::html_elements(doc, css = "a[href^='/products/']"), "href")
  hrefs <- unique(hrefs[nzchar(hrefs)])
  paste0("https://", mml_domain(), hrefs)
}

mml_search_html <- function(query){
  u <- sprintf("https://%s/search?q=%s", mml_domain(), utils::URLencode(query, reserved = TRUE))
  fx <- try(mml_fetch_text(u, accept = "text/html,application/xhtml+xml"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(character())

  doc <- xml2::read_html(fx$text)
  hrefs <- rvest::html_attr(rvest::html_elements(doc, css = "a[href^='/products/']"), "href")
  hrefs <- unique(hrefs[nzchar(hrefs)])
  paste0("https://", mml_domain(), hrefs)
}

mml_discover_product_urls <- function(query, n = 10){
  urls <- character()
  urls <- c(urls, mml_search_suggest(query, limit = n * 2))

  if (length(urls) < n){
    slug <- mml_slugify(query)
    cands <- unique(c(slug, paste0(slug, "s"), sub("s$", "", slug)))
    for (s in cands){
      urls <- c(urls, mml_collection_products_json(s, limit = n * 2))
      if (length(unique(urls)) >= n) break
      urls <- c(urls, mml_collection_products_html(s))
      if (length(unique(urls)) >= n) break
    }
  }
  if (length(urls) < n) urls <- c(urls, mml_search_html(query))
  urls <- unique(urls)
  urls <- urls[grepl("/products/", urls)]
  head(urls, n)
}

# Agent R pur: retourne list(results=..., page_info=list(...))
mml_agent_prices <- function(query, n = 10, per_request_delay = 0){
  urls <- mml_discover_product_urls(query, n = n)
  if (!length(urls)) return(list(results = list(), page_info = list(has_next_page = FALSE)))

  items <- lapply(urls, function(u){
    info <- try(mml_normalize(mml_price_by_url(u)), silent = TRUE)
    if (per_request_delay > 0) Sys.sleep(per_request_delay)
    if (inherits(info, "try-error") || is.null(info)) return(NULL)
    list(
      title = info$title %||% "",
      price = info$price %||% list(amount = NA_real_, currency = "EUR"),
      url   = info$url %||% u,
      image = info$image %||% NULL
    )
  })
  items <- Filter(Negate(is.null), items)
  list(results = items, page_info = list(has_next_page = FALSE, end_cursor = NULL))
}
