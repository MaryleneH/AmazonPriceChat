#' Client OpenAI-compatible (vLLM/Ollama)
#' @keywords internal
NULL

# utilitaires simples ----
.rtrim_slash <- function(x) sub("/+$", "", x)
.ltrim_slash <- function(x) sub("^/+", "", x)
`%||%` <- function(x, y) if (is.null(x)) y else x

# base URL, ex: https://llm.lab.sspcloud.fr + api/v1 -> https://llm.lab.sspcloud.fr/api/v1
llm_base_url <- function(){
  base <- Sys.getenv("LLM_BASE_URL")
  path <- Sys.getenv("LLM_PATH", "api/v1")
  if (!nzchar(base)) stop("LLM_BASE_URL manquant dans .Renviron")
  paste0(.rtrim_slash(base), "/", .ltrim_slash(path))
}

# en-têtes (Bearer) ----
.llm_headers <- function(){
  key <- Sys.getenv("LLM_API_KEY")
  hdr <- list(
    Authorization = if (nzchar(key)) paste("Bearer", key) else NULL,
    `Content-Type` = "application/json",
    Accept = "application/json"
  )
  Filter(Negate(is.null), hdr)
}

#' Appel chat/completions (OpenAI compatible)
#' @param messages liste de messages: list(list(role="user", content="..."), ...)
#' @param model nom du modèle (défaut: env LLM_MODEL)
#' @param temperature numérique
#' @param tools définitions d’outils (facultatif)
#' @param tool_choice "auto"/"none"/liste (facultatif)
#' @returns list(text=..., choice=..., raw=...)
#' @export
llm_chat <- function(messages,
                     model = Sys.getenv("LLM_MODEL"),
                     temperature = 0.2,
                     tools = NULL,
                     tool_choice = NULL,
                     stream = FALSE){
  if (!length(messages)) stop("messages manquant")
  if (!nzchar(model))   stop("LLM_MODEL manquant dans .Renviron")

  body <- list(
    model = model,
    messages = messages,
    temperature = temperature,
    stream = stream
  )
  if (!is.null(tools))       body$tools <- tools
  if (!is.null(tool_choice)) body$tool_choice <- tool_choice

  url <- llm_base_url()
  req <- httr2::request(url) |>
    httr2::req_url_path_append("chat/completions") |>
    httr2::req_headers(!!!.llm_headers()) |>
    httr2::req_body_json(body)

  resp <- httr2::req_perform(req)
  ct   <- httr2::resp_content_type(resp)
  if (!grepl("json", tolower(ct))) {
    stop("Le serveur LLM a renvoyé un contenu non-JSON (", ct, "). Vérifie LLM_BASE_URL/LLM_PATH.")
  }
  jj <- httr2::resp_body_json(resp)

  # extraction texte / tool calls
  ch <- jj$choices[[1]]
  msg <- ch$message
  text <- msg$content %||% ""
  # certains serveurs renvoient content en liste de segments
  if (is.list(text)) {
    # concatène les segments textuels si présents
    texts <- vapply(text, function(seg) seg$text %||% "", character(1))
    text <- paste(texts[nzchar(texts)], collapse = "")
  }

  out <- list(
    text = text %||% "",
    choice = ch,
    raw = jj
  )
  # expose tool_calls si présents
  if (!is.null(msg$tool_calls)) {
    out$tool_calls <- msg$tool_calls
  }
  out
}

#' Healthcheck simple sur /models
#' @returns list(ok=TRUE/FALSE, note=...)
#' @export
llm_healthcheck <- function(){
  url <- llm_base_url()
  req <- httr2::request(url) |>
    httr2::req_url_path_append("models") |>
    httr2::req_headers(!!!.llm_headers())

  out <- tryCatch({
    resp <- httr2::req_perform(req)
    ct   <- httr2::resp_content_type(resp)
    ok   <- grepl("json", tolower(ct))
    list(ok = ok, status = httr2::resp_status(resp), content_type = ct)
  }, error = function(e){
    list(ok = FALSE, note = conditionMessage(e))
  })
  class(out) <- c("llm_healthcheck", class(out))
  out
}
