# --- Shopify MCP provider -----------------------------------------------

#' shopify_mcp_endpoint
#'
#' @returns
#' @export
#'
#' @examples
shopify_mcp_endpoint <- function(){
  dom <- trimws(Sys.getenv("SHOPIFY_STORE_DOMAIN"))
  if (!nzchar(dom)) stop("SHOPIFY_STORE_DOMAIN manquant dans .Renviron")
  if (!grepl("\\.myshopify\\.com$", dom))
    warning("SHOPIFY_STORE_DOMAIN devrait être un domaine myshopify.com (dev store).")
  sprintf("https://%s/api/mcp", dom)
}

#' .shopify_req
#'
#' @param method
#' @param params
#' @param id
#'
#' @returns
#' @export
#'
#' @examples
.shopify_req <- function(method, params = list(), id = 1L){
  endpoint <- shopify_mcp_endpoint()
  body <- list(jsonrpc = "2.0", method = method, id = id, params = params)
  resp <- httr2::request(endpoint) |>
    httr2::req_headers(`Content-Type`="application/json", `Accept`="application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_perform()
  ct <- httr2::resp_content_type(resp)
  if (!grepl("json", tolower(ct))) {
    stop("Réponse non-JSON depuis ", endpoint, " (Content-Type: ", ct, ").")
  }
  httr2::resp_body_json(resp)
}

#' shopify_tools_list
#'
#' @returns
#' @export
#'
#' @examples
shopify_tools_list <- function(){
  .shopify_req("tools/list", params = list())
}


#' shopify_search
#'
#' @description
#' Recherche produits (query + context obligatoires ; country/lang/limit optionnels)
#'
#'
#' @param q
#' @param page
#' @param after
#' @param country
#' @param language
#' @param limit
#'
#' @returns
#' @export
#'
#' @examples
shopify_search <- function(q, page = 1, after = NULL,
                           country = "FR", language = "FR", limit = 10) {
  args <- list(
    query   = q,
    context = "reader",
    country = country,
    language = language,
    limit   = limit
  )
  if (!is.null(after)) args$after <- after

  rsp <- .shopify_req("tools/call",
                      params = list(name = "search_shop_catalog", arguments = args))
  items <- rsp$result$items %||% list()

  results <- lapply(items, function(x){
    amt <- suppressWarnings(as.numeric(x$price$amount %||% NA_real_))
    list(
      asin = x$variant_id %||% x$product_id %||% NA_character_, # identifiant interne
      title = x$title %||% "",
      brand = x$vendor %||% "",
      price = list(amount = amt, currency = x$price$currency %||% "EUR"),
      detail_page_url = x$url %||% "#",
      image_url = x$image_url %||% NULL
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

#' shopify_get_items
#'
#' @description
#' Récupération par IDs : pour ce MVP, on re-cherche par ID/texte
#'
#'
#' @param ids
#'
#' @returns
#' @export
#'
#' @examples
shopify_get_items <- function(ids) {
  found <- unlist(lapply(ids, function(id) shopify_search(id)$results), recursive = FALSE)
  list(items = found, fetched_at = Sys.time())
}

`%||%` <- function(x,y) if (is.null(x)) y else x
