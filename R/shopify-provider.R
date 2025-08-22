# --- Utils ---------------------------------------------------------------

sanitize_domain <- function(x){
  x <- trimws(x)
  x <- sub("^https?://", "", x)
  sub("/+$", "", x)
}
`%||%` <- function(x,y) if (is.null(x)) y else x

# --- Endpoint & requête JSON-RPC ----------------------------------------

shopify_mcp_endpoint <- function(){
  dom <- sanitize_domain(Sys.getenv("SHOPIFY_STORE_DOMAIN"))
  if (!nzchar(dom)) stop("SHOPIFY_STORE_DOMAIN manquant dans .Renviron")
  sprintf("https://%s/api/mcp", dom)
}

.shopify_req <- function(method, params = list(), id = 1L){
  endpoint <- shopify_mcp_endpoint()
  body <- list(jsonrpc = "2.0", method = method, id = id, params = params)
  resp <- httr2::request(endpoint) |>
    httr2::req_headers(`Content-Type`="application/json", `Accept`="application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  ct <- httr2::resp_content_type(resp)
  if (!grepl("json", tolower(ct))) stop("Réponse non-JSON depuis ", endpoint, " (Content-Type: ", ct, ").")
  httr2::resp_body_json(resp)
}

shopify_tools_list <- function(){
  .shopify_req("tools/list", params = list())
}

# --- Recherche produits ---------------------------------------------------

shopify_search <- function(q, limit = 10, after = NULL, country = NULL, language = NULL){
  args <- list(query = q, context = "reader", limit = limit)
  if (!is.null(after)) args$after <- after
  if (!is.null(country))  args$country  <- country
  if (!is.null(language)) args$language <- language

  # 1) sans locale
  rsp <- .shopify_req("tools/call", params = list(name="search_shop_catalog", arguments=args))
  items <- rsp$result$items

  # 2) fallback FR si 0
  if (length(items) == 0) {
    args$country  <- "FR"; args$language <- "FR"
    rsp <- .shopify_req("tools/call", params = list(name="search_shop_catalog", arguments=args))
    items <- rsp$result$items
  }

  results <- lapply(items %||% list(), function(x){
    amt <- suppressWarnings(as.numeric(x$price$amount %||% NA_real_))
    list(
      id    = x$variant_id %||% x$product_id %||% NA_character_,
      title = x$title %||% "",
      brand = x$vendor %||% "",
      price = list(amount = amt, currency = x$price$currency %||% "EUR"),
      url   = x$url %||% "#",
      image = x$image_url %||% NULL
    )
  })

  list(
    results = results,
    page_info = list(
      has_next_page = isTRUE(rsp$result$page_info$has_next_page),
      end_cursor    = rsp$result$page_info$end_cursor %||% NULL
    ),
    available_filters = rsp$result$available_filters %||% list(),
    cached_at = Sys.time()
  )
}

# --- Récupérer le prix d'un produit par URL -------------------------------

shopify_price_by_url <- function(product_url){
  # extrait la dernière partie utile (handle)
  path <- sub("^https?://[^/]+/", "", product_url)
  handle <- sub(".*/products/([^/?#]+).*", "\\1", path)
  if (identical(handle, path)) handle <- basename(path) # fallback

  res <- shopify_search(handle, limit = 10)
  items <- res$results %||% list()

  # essaie de matcher l'URL exacte (ignorer http/https, slash final)
  norm <- function(u) sub("/+$","", sub("^https?://", "", u))
  want <- norm(product_url)

  best <- NULL
  for (it in items){
    if (!is.null(it$url) && norm(it$url) == want) { best <- it; break }
  }
  if (is.null(best) && length(items)) best <- items[[1]]

  if (is.null(best)) return(NULL)
  list(
    title    = best$title,
    price    = best$price,
    url      = best$url,
    image    = best$image,
    matched  = !is.null(best$url) && norm(best$url) == want
  )
}

# --- Adapter "get_items" (MVP : re-cherche par id/texte) ------------------

shopify_get_items <- function(ids){
  found <- unlist(lapply(ids, function(id) shopify_search(id, limit = 10)$results), recursive = FALSE)
  list(items = found, fetched_at = Sys.time())
}
