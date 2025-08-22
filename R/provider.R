get_provider <- function() tolower(Sys.getenv("PROVIDER","shopify"))

provider_search <- function(q, page=1){
  switch(get_provider(),
         "shopify" = shopify_search(q, limit = 10),
         "amazon"  = amazon_search(q, page),
         "ebay"    = ebay_search(q, page),
         stop("PROVIDER non supporté: ", get_provider())
  )
}

provider_get_items <- function(ids){
  switch(get_provider(),
         "shopify" = shopify_get_items(ids),
         "amazon"  = amazon_get_items(ids),
         "ebay"    = ebay_get_items(ids),
         stop("PROVIDER non supporté: ", get_provider())
  )
}
