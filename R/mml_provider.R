# --- Utils -----------------------------------------------------------------
`%||%` <- function(x,y) if (is.null(x)) y else x

mml_extract <- function(url){
  list(
    domain     = sub("^https?://([^/]+)/.*", "\\1", url),
    handle     = sub(".*/products/([^/?#]+).*", "\\1", url),
    variant_id = {
      v <- sub(".*[?&]variant=([0-9]+).*", "\\1", url)
      if (grepl("^[0-9]+$", v)) v else NULL
    }
  )
}

mml_fetch_text <- function(url, accept = "application/json"){
  resp <- httr2::request(url) |>
    httr2::req_headers(
      `User-Agent`      = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125 Safari/537.36",
      `Accept`          = accept,
      `Accept-Language` = "fr-FR,fr;q=0.9,en;q=0.8"
    ) |>
    httr2::req_timeout(30) |>
    httr2::req_perform()
  list(status = httr2::resp_status(resp),
       ct     = httr2::resp_content_type(resp),
       text   = httr2::resp_body_string(resp))
}

mml_parse_price_num <- function(x){
  # gère "129.00" ou centimes "12900"
  n <- suppressWarnings(as.numeric(x))
  if (is.na(n)) return(NA_real_)
  if (n > 1000 && floor(n) == n) n <- n / 100
  n
}

# --- 1) Variant direct: /variants/{id}.json --------------------------------
mml_price_from_variant_json <- function(domain, variant_id){
  if (is.null(variant_id)) return(NULL)
  u <- sprintf("https://%s/variants/%s.json", domain, variant_id)
  fx <- try(mml_fetch_text(u, accept="application/json"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(NULL)
  pj <- try(jsonlite::fromJSON(fx$text), silent = TRUE)
  if (inherits(pj,"try-error") || is.null(pj$variant)) return(NULL)
  v <- pj$variant
  amt <- mml_parse_price_num(v$price %||% v$compare_at_price %||% NA_real_)
  if (is.na(amt)) return(NULL)
  list(
    source = "variant.json",
    title  = v$name %||% v$title %||% NA_character_,
    price  = list(amount = amt, currency = "EUR"),
    url    = sprintf("https://%s/products/%s?variant=%s", domain, v$product_handle %||% "", v$id),
    image  = v$featured_image %||% NA_character_
  )
}

# --- 2) Produit: /products/{handle}.js (classique Shopify public) ----------
mml_price_from_product_js <- function(domain, handle, variant_id = NULL){
  u <- sprintf("https://%s/products/%s.js", domain, handle)
  fx <- try(mml_fetch_text(u, accept="application/json"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(NULL)
  pj <- try(jsonlite::fromJSON(fx$text), silent = TRUE)
  if (inherits(pj,"try-error") || is.null(pj$variants)) return(NULL)

  pick <- NULL
  if (!is.null(variant_id)){
    idx <- which(as.character(pj$variants$id) == as.character(variant_id))
    if (length(idx)) pick <- pj$variants[idx[1], , drop = FALSE]
  }
  if (is.null(pick)){
    if (!is.null(pj$variants$available) && any(pj$variants$available)) {
      pick <- pj$variants[which(pj$variants$available)[1], , drop = FALSE]
    } else {
      pick <- pj$variants[1, , drop = FALSE]
    }
  }

  amt <- mml_parse_price_num(pick$price)
  if (is.na(amt)) {
    amt <- mml_parse_price_num(pick$compare_at_price %||% NA_real_)
  }
  if (is.na(amt)) return(NULL)

  img <- if (!is.null(pj$images) && length(pj$images)) pj$images[[1]] else NA_character_

  list(
    source = "product.js",
    title  = pj$title %||% NA_character_,
    price  = list(amount = amt, currency = "EUR"),
    url    = sprintf("https://%s/products/%s%s", domain, handle,
                     if (!is.null(variant_id)) paste0("?variant=", variant_id) else ""),
    image  = img
  )
}

# --- 3) Produit: /products/{handle}.json (souvent public aussi) ------------
mml_price_from_product_json <- function(domain, handle, variant_id = NULL){
  u <- sprintf("https://%s/products/%s.json", domain, handle)
  fx <- try(mml_fetch_text(u, accept="application/json"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(NULL)
  pj <- try(jsonlite::fromJSON(fx$text), silent = TRUE)
  if (inherits(pj,"try-error")) return(NULL)
  product <- pj$product %||% pj
  if (is.null(product$variants)) return(NULL)

  pick <- NULL
  if (!is.null(variant_id)){
    idx <- which(as.character(product$variants$id) == as.character(variant_id))
    if (length(idx)) pick <- product$variants[idx[1], , drop = FALSE]
  }
  if (is.null(pick)){
    if (!is.null(product$variants$available) && any(product$variants$available)) {
      pick <- product$variants[which(product$variants$available)[1], , drop = FALSE]
    } else {
      pick <- product$variants[1, , drop = FALSE]
    }
  }

  amt <- mml_parse_price_num(pick$price)
  if (is.na(amt)) amt <- mml_parse_price_num(pick$compare_at_price %||% NA_real_)
  if (is.na(amt)) return(NULL)

  img <- if (!is.null(product$images) && length(product$images)) product$images[[1]] else NA_character_

  list(
    source = "product.json",
    title  = product$title %||% NA_character_,
    price  = list(amount = amt, currency = "EUR"),
    url    = sprintf("https://%s/products/%s%s", domain, handle,
                     if (!is.null(variant_id)) paste0("?variant=", variant_id) else ""),
    image  = img
  )
}

# --- 4) Dernier recours: JSON-LD dans l'HTML --------------------------------
mml_price_from_jsonld <- function(url){
  fx <- try(mml_fetch_text(url, accept="text/html,application/xhtml+xml"), silent = TRUE)
  if (inherits(fx,"try-error") || fx$status >= 400) return(NULL)

  doc <- xml2::read_html(fx$text)
  scripts <- rvest::html_elements(doc, xpath = "//script[@type='application/ld+json']")
  if (!length(scripts)) return(NULL)
  for (sc in scripts){
    txt <- rvest::html_text2(sc)
    parsed <- try(jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
    if (inherits(parsed, "try-error")) next
    objs <- if (is.list(parsed) && is.null(parsed$`@type`)) parsed else list(parsed)
    for (o in objs){
      typ <- o$`@type` %||% NULL
      if (!is.null(typ) && (identical(typ,"Product") || (is.list(typ) && "Product" %in% typ))){
        offers <- o$offers %||% NULL
        if (is.null(offers)) next
        if (is.list(offers) && !is.null(offers$price)) {
          amt <- mml_parse_price_num(offers$price)
          if (!is.na(amt)) return(list(source="html-ld",
                                       title=o$name %||% NA_character_,
                                       price=list(amount=amt, currency=offers$priceCurrency %||% "EUR"),
                                       url=url, image=o$image %||% NA_character_))
        }
        if (is.list(offers) && !is.null(offers$lowPrice)) {
          amt <- mml_parse_price_num(offers$lowPrice)
          if (!is.na(amt)) return(list(source="html-ld",
                                       title=o$name %||% NA_character_,
                                       price=list(amount=amt, currency=offers$priceCurrency %||% "EUR"),
                                       url=url, image=o$image %||% NA_character_))
        }
        if (is.list(offers) && length(offers)>0 && !is.null(offers[[1]]$price)) {
          amt <- mml_parse_price_num(offers[[1]]$price)
          if (!is.na(amt)) return(list(source="html-ld",
                                       title=o$name %||% NA_character_,
                                       price=list(amount=amt, currency=offers[[1]]$priceCurrency %||% "EUR"),
                                       url=url, image=o$image %||% NA_character_))
        }
      }
    }
  }
  NULL
}

# --- API principale ---------------------------------------------------------
mml_price_by_url <- function(product_url){
  parts <- mml_extract(product_url)
  # 1) variante directe si on a un ?variant=
  r <- mml_price_from_variant_json(parts$domain, parts$variant_id)
  if (!is.null(r)) return(r)
  # 2) produit.js
  r <- mml_price_from_product_js(parts$domain, parts$handle, parts$variant_id)
  if (!is.null(r)) return(r)
  # 3) produit.json
  r <- mml_price_from_product_json(parts$domain, parts$handle, parts$variant_id)
  if (!is.null(r)) return(r)
  # 4) JSON-LD HTML
  r <- mml_price_from_jsonld(product_url)
  if (!is.null(r)) return(r)
  NULL
}

mml_normalize <- function(p){
  if (is.null(p)) return(NULL)
  if (!is.null(p$image) && nzchar(p$image) && grepl("^//", p$image)) {
    p$image <- paste0("https:", p$image)
  }
  p
}
