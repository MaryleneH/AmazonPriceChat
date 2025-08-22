#' @keywords internal
#' @noRd

`%||%` <- function(x,y) if (is.null(x)) y else x

.mml_domain <- function(){
  dom <- Sys.getenv("MML_DOMAIN", "www.makemylemonade.com")
  dom <- trimws(sub("^https?://", "", dom))
  sub("/+$", "", dom)
}

.mml_handle_variant <- function(url){
  path <- sub("^https?://[^/]+/", "", url)
  handle <- sub(".*/products/([^/?#]+).*", "\\1", path)
  qs <- sub("^[^?]*\\??", "", url)
  variant <- if (grepl("variant=", qs)) sub(".*variant=([0-9]+).*", "\\1", qs) else NA_character_
  list(handle = handle, variant = variant)
}

.mml_fetch_json <- function(url){
  httr2::request(url) |>
    httr2::req_headers(
      `User-Agent`="Mozilla/5.0",
      `Accept`="application/json"
    ) |>
    httr2::req_timeout(20) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

.mml_price_from_productjs <- function(url){
  hv <- .mml_handle_variant(url)
  u  <- sprintf("https://%s/products/%s.js", .mml_domain(), hv$handle)
  j  <- .mml_fetch_json(u)

  variants <- j$variants
  if (is.null(variants) || !NROW(variants)) return(NULL)

  pick <- NULL
  if (!is.na(hv$variant)) {
    pick <- variants[variants$id == hv$variant, , drop = FALSE]
  }
  if (is.null(pick) || !NROW(pick)) pick <- variants[1, , drop = FALSE]

  # Shopify .js price are in cents
  price_eur <- suppressWarnings(as.numeric(pick$price)/100)
  img <- pick$featured_image %||% j$images[[1]] %||% NA_character_

  list(
    source = "product.js",
    title  = j$title %||% "",
    price  = list(amount = price_eur, currency = "EUR"),
    url    = url,
    image  = img
  )
}

.mml_price_from_productjson <- function(url){
  hv <- .mml_handle_variant(url)
  u  <- sprintf("https://%s/products/%s.json", .mml_domain(), hv$handle)
  j  <- .mml_fetch_json(u)

  p <- j$product
  if (is.null(p)) return(NULL)
  variants <- p$variants
  if (is.null(variants) || !NROW(variants)) return(NULL)

  pick <- NULL
  if (!is.na(hv$variant)) pick <- variants[variants$id == hv$variant, , drop = FALSE]
  if (is.null(pick) || !NROW(pick)) pick <- variants[1, , drop = FALSE]

  price_num <- suppressWarnings(as.numeric(pick$price))
  img <- pick$featured_image$src %||% p$image$src %||% NA_character_

  list(
    source = "product.json",
    title  = p$title %||% "",
    price  = list(amount = price_num, currency = "EUR"),
    url    = url,
    image  = img
  )
}

.mml_price_from_html_ld <- function(url){
  resp <- httr2::request(url) |>
    httr2::req_headers(`User-Agent`="Mozilla/5.0", `Accept`="text/html") |>
    httr2::req_timeout(20) |>
    httr2::req_perform()

  html <- httr2::resp_body_string(resp)
  doc  <- xml2::read_html(html)
  nodes <- rvest::html_elements(doc, "script[type='application/ld+json']")
  if (!length(nodes)) return(NULL)

  for (n in nodes){
    txt <- rvest::html_text2(n)
    j <- try(jsonlite::fromJSON(txt, simplifyVector = TRUE), silent = TRUE)
    if (inherits(j,"try-error")) next

    # parfois un tableau de graphes
    if (is.list(j) && !is.null(j[["@type"]]) && any(j[["@type"]] %in% c("Product","product"))){
      title <- j$name %||% ""
      offer <- j$offers
      amount <- suppressWarnings(as.numeric(offer$price %||% offer$lowPrice))
      currency <- offer$priceCurrency %||% "EUR"
      img <- (j$image %||% NA_character_)
      if (is.list(img)) img <- img[[1]]
      return(list(
        source = "json-ld",
        title  = title,
        price  = list(amount = amount, currency = currency),
        url    = url,
        image  = img
      ))
    }
  }
  NULL
}

#' Récupère le prix d'un produit à partir de son URL MML
#' @keywords internal
#' @noRd
mml_price_by_url <- function(product_url){
  # ordre: product.js -> product.json -> json-ld
  for (fn in list(.mml_price_from_productjs, .mml_price_from_productjson, .mml_price_from_html_ld)){
    out <- try(fn(product_url), silent = TRUE)
    if (!inherits(out, "try-error") && !is.null(out)) return(out)
  }
  NULL
}

#' Normalise l'info produit (URL d'image, etc.)
#' @keywords internal
#' @noRd
mml_normalize <- function(info){
  if (is.null(info)) return(NULL)
  if (!is.null(info$image) && nzchar(info$image) && grepl("^//", info$image)){
    info$image <- paste0("https:", info$image)
  }
  if (!is.null(info$price$amount))
    info$price$amount <- as.numeric(info$price$amount)
  info
}
