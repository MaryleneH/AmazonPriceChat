#' llm_route
#'
#' @param utterance
#'
#' @returns
#' @export
#'
#' @examples
llm_route <- function(utterance) {
  sys <- list(
    role="system",
    content=paste(
      "Tu es un routeur. Réponds UNIQUEMENT en JSON strict, sans texte autour.",
      "Deux formats possibles:",
      "{\"name\":\"search_amazon\",\"args\":{\"q\":\"...\",\"page\":1}}",
      "ou",
      "{\"name\":\"get_items\",\"args\":{\"asins\":[\"ID1\",\"ID2\"]}}",
      "Ne résume pas, n'ajoute rien."
    )
  )
  usr <- list(role="user", content=utterance)
  r <- llm_chat(messages=list(sys, usr))
  out <- tryCatch(jsonlite::fromJSON(r$text), error=function(e) NULL)
  if (is.null(out) || is.null(out$name)) list(name="search_amazon", args=list(q=utterance, page=1)) else out
}
