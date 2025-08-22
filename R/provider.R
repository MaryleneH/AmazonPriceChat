#' Détecte le provider actif
#' @return "mml", "shopify" ou "amazon"
#' @export
get_provider <- function(){
  prov <- tolower(Sys.getenv("PROVIDER", "mml"))
  if (!prov %in% c("mml","shopify","amazon")) prov <- "mml"
  prov
}

#' Recherche produits selon provider
#' @param q requête
#' @param page page (amazon)
#' @export
provider_search <- function(q, page = 1){
  switch(get_provider(),
         mml     = mml_search(q, limit = 10),
         shopify = shopify_search(q, limit = 10),
         amazon  = amazon_search(q, page = page),
         mml_search(q, limit = 10)
  )
}

#' Items par identifiants/URLs selon provider
#' @param ids vecteur
#' @export
provider_get_items <- function(ids){
  switch(get_provider(),
         mml     = mml_get_items(ids),
         shopify = shopify_get_items(ids),
         amazon  = amazon_get_items(ids),
         mml_get_items(ids)
  )
}
