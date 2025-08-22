#' Make My Lemonade provider (scraping & JSON)
#'
#' Fonctions utilitaires pour récupérer des produits/prix depuis
#' https://www.makemylemonade.com/ sans MCP.
#'
#' @keywords internal
#' @noRd
NULL

# --- imports roxygen ----
#' @importFrom httr2 request req_url req_headers req_user_agent req_perform resp_status
#' @importFrom httr2 resp_body_string resp_content_type
#' @importFrom jsonlite fromJSON
#' @importFrom rvest read_html html_elements html_attr html_text2
#' @importFrom utils URLencode

# ----------------- Helpers internes -----------------

.mml_base <- function() {
  x <- Sys.getenv("MML_BASE", "https://www.makemylemonade.com")
  x <- sub("/+$", "", x)
  if (!nzchar(x)) stop("MML_BASE manquant.")
  x
}

.norm_img <- function(img) {
  # peut être NULL, list(src=..), vecteur, ou déjà complet
  if (is.null(img) || length(img) == 0) return(NA_character_)
  if (is.list(img)) {
    if (!is.null(img$src)) img <- img$src else img <- unlist(img, use.names = FALSE)[1]
  }
  img <- as.character(img)[1]
  if (!nzchar(img)) return(NA_character_)
  if (startsWith(img, "//")) img <- paste0("https:", img)
  img
}

.extract_handle <- function(x) {
  # accepte handle nu OU URL complète
  if (!nzchar(x)) return(NA_character_)
  if (grepl("^https?://", x)) {
    path <- sub("^https?://[^/]+/", "", x)
    h <- sub(".*/products/([^/?#]+).*", "\\1", path)
    return(h)
  }
  x
}

.pick_variant <- function(pjson, variant_id = NULL) {
  vars <- pjson$variants %||% list()
  if (!length(vars)) return(NULL)
  if (!is.null(variant_id)) {
    vv <- NULL
    for (v in vars) {
      if (!is.null(v$id) && as.character(v$id) == as.character(variant_id))
        vv <- v
    }
    if (!is.null(vv)) return(vv)
  }
  vars[[1]]
}

`%||%` <- function(x,y) if (is.null(x)) y else x

# ----------------- API publiques -----------------

#' Tester l'accès aux outils MML
#' @return TRUE si le site est accessible
#' @export
mml_tools_ok <- function() {
  ok <- TRUE
  try({
    u <- paste0(.mml_base(), "/robots.txt")
    httr2::request(u) |>
      httr2::req_user_agent("amazonPriceChat/0.1 (+R)") |>
      httr2::req_perform()
  }, silent = TRUE)
  isTRUE(ok)
}

#' Récupérer le JSON Shopify d'un produit par handle (product.js)
#' @param handle Handle du produit (ou URL produit)
#' @return liste JSON (ou NULL si échec)
#' @export
mml_product_js <- function(handle) {
  h <- .extract_handle(handle)
  if (is.na(h) || !nzchar(h)) return(NULL)

  url <- paste0(.mml_base(), "/products/", h, ".js")

  resp <- try(
    httr2::request(url) |>
      httr2::req_headers(Accept = "application/json,*/*;q=0.9") |>
      httr2::req_user_agent("amazonPriceChat/0.1 (+R)") |>
      httr2::req_perform(),
    silent = TRUE
  )
  if (inherits(resp, "try-error")) return(NULL)

  # Le site peut renvoyer text/javascript -> on parse à la main
  body <- httr2::resp_body_string(resp)
  if (!nzchar(body)) return(NULL)

  out <- NULL
  try(out <- jsonlite::fromJSON(body, simplifyVector = FALSE), silent = TRUE)
  out
}

#' Prix & infos d'un produit à partir d'une URL (ou handle)
#' @param product_url URL produit complète (ou handle)
#' @return list(title, price=list(amount,currency), url, image, source)
#' @export
mml_price_by_url <- function(product_url) {
  h <- .extract_handle(product_url)
  if (is.na(h)) return(NULL)

  # Si l'URL contient ?variant=..., on tente de prioriser ce variant
  q_variant <- sub(".*[?&]variant=([0-9]+).*", "\\1", product_url)
  variant_id <- if (grepl("variant=", product_url)) q_variant else NULL

  pj <- mml_product_js(h)
  if (is.null(pj)) return(NULL)

  # pick variant
  vv <- .pick_variant(pj, variant_id)

  # prix : selon les stores, integer en cents ou nombre direct
  amt <- NA_real_
  if (!is.null(vv$price)) {
    # vv$price peut être numerique (euros) ou entier (cents)
    a <- suppressWarnings(as.numeric(vv$price))
    if (!is.na(a)) {
      # heuristique : si > 5000 on suppose des cents
      if (a > 5000) a <- a / 100
      amt <- a
    }
  } else if (!is.null(pj$price)) {
    a <- suppressWarnings(as.numeric(pj$price))
    if (!is.na(a)) {
      if (a > 5000) a <- a / 100
      amt <- a
    }
  }

  title <- pj$title %||% ""
  img   <- pj$featured_image %||% pj$images %||% NA
  img   <- .norm_img(img)

  list(
    source  = "product.js",
    title   = title,
    price   = list(amount = amt, currency = "EUR"),
    url     = if (grepl("^https?://", product_url)) product_url else paste0(.mml_base(), "/products/", h),
    image   = img
  )
}

#' Recherche produits (predictive search + fallback scraping)
#' @param q requête textuelle
#' @param limit nombre max de résultats
#' @return list(results=..., page_info=..., cached_at=Sys.time())
#' @export
mml_search <- function(q, limit = 10L) {
  base <- .mml_base()
  limit <- max(1L, as.integer(limit))

  # 1) predictive search JSON
  ps_url <- paste0(
    base, "/search/suggest.json?q=", utils::URLencode(q, reserved = TRUE),
    "&resources[type]=product&resources[limit]=", limit,
    "&section_id=predictive-search"
  )

  items <- list()
  ok_json <- FALSE

  resp1 <- try(
    httr2::request(ps_url) |>
      httr2::req_headers(Accept = "application/json,*/*;q=0.9") |>
      httr2::req_user_agent("amazonPriceChat/0.1 (+R)") |>
      httr2::req_perform(),
    silent = TRUE
  )

  if (!inherits(resp1, "try-error")) {
    txt <- httr2::resp_body_string(resp1)
    if (nzchar(txt)) {
      j <- NULL
      try(j <- jsonlite::fromJSON(txt, simplifyVector = FALSE), silent = TRUE)
      if (!is.null(j$resources$results$products)) {
        ok_json <- TRUE
        prods <- j$resources$results$products
        for (p in prods) {
          title <- p$title %||% ""
          url   <- p$url %||% p$url_to_product %||% ""
          url   <- if (nzchar(url) && !grepl("^https?://", url)) paste0(base, url) else url
          # prix : souvent en cents dans predictive search : price_min
          amt <- NA_real_
          if (!is.null(p$price_min)) {
            a <- suppressWarnings(as.numeric(p$price_min))
            if (!is.na(a)) {
              if (a > 5000) a <- a / 100
              amt <- a
            }
          }
          img <- p$image %||% p$featured_image %||% NA
          img <- .norm_img(img)

          items[[length(items) + 1L]] <- list(
            id    = as.character(p$id %||% NA),
            title = title,
            brand = p$vendor %||% "",
            price = list(amount = amt, currency = "EUR"),
            url   = url,
            image = img
          )
          if (length(items) >= limit) break
        }
      }
    }
  }

  # 2) Fallback scraping HTML si rien
  if (!ok_json || !length(items)) {
    html_url <- paste0(base, "/search?q=", utils::URLencode(q, reserved = TRUE), "&type=product")
    page <- try(rvest::read_html(html_url), silent = TRUE)
    if (!inherits(page, "try-error")) {
      cards <- rvest::html_elements(page, "[data-product-card], .product-grid .product-item, article")
      for (c in cards) {
        # titre
        title <- rvest::html_text2(rvest::html_elements(c, ".product-item__title, .card__heading, a"))[1]
        if (is.na(title) || !nzchar(title)) next
        # url
        url <- rvest::html_attr(rvest::html_elements(c, "a"), "href")[1]
        if (is.na(url) || !nzchar(url)) next
        if (!grepl("^https?://", url)) url <- paste0(base, url)

        # prix (approx)
        price_txt <- rvest::html_text2(rvest::html_elements(c, ".price-item, .price__current, .price .money"))[1]
        amt <- suppressWarnings(as.numeric(gsub("[^0-9.,]", "", price_txt)))
        if (!is.na(amt) && amt > 5000) amt <- amt / 100

        # image
        img <- rvest::html_attr(rvest::html_elements(c, "img"), "src")[1]
        if (is.na(img) || !nzchar(img)) img <- rvest::html_attr(rvest::html_elements(c, "img"), "data-src")[1]
        img <- .norm_img(img)

        items[[length(items) + 1L]] <- list(
          id    = NA_character_,
          title = title %||% "",
          brand = "",
          price = list(amount = amt %||% NA_real_, currency = "EUR"),
          url   = url,
          image = img
        )
        if (length(items) >= limit) break
      }
    }
  }

  list(
    results = items,
    page_info = list(has_next_page = FALSE, end_cursor = NULL),
    available_filters = list(),
    cached_at = Sys.time()
  )
}

#' Récupérer plusieurs items par handle/URL
#' @param ids vecteur de handles ou d'URLs
#' @return list(items = list(...), fetched_at = Sys.time())
#' @export
mml_get_items <- function(ids) {
  ids <- as.character(ids)
  out <- list()
  for (x in ids) {
    it <- mml_price_by_url(x)
    if (!is.null(it)) out[[length(out) + 1L]] <- it
  }
  list(items = out, fetched_at = Sys.time())
}
