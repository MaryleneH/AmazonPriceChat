# R/llm_tools_agent.R
# Agent outils (Make My Lemonade) pour backend OpenAI-compatible (vLLM SSPCloud)
# Compatible api/v1 et fallback functions.

# --- OUTILS ---------------------------------------------------------------

llm_agent_tools <- function(){
  list(
    list(
      type = "function",
      `function` = list(
        name = "mml_search",
        description = "Recherche des produits sur Make My Lemonade à partir d'une requête texte. Retourne jusqu'à 'limit' articles avec titre, URL, image et prix.",
        parameters = list(
          type = "object",
          properties = list(
            query    = list(type="string",  description="Requête, ex: 'jupes', 'robe noire'"),
            limit    = list(type="integer", description="Nombre max d'articles (1-20)", minimum=1L, maximum=20L, default=10L),
            country  = list(type="string",  description="Code pays ISO-2, ex: 'FR'"),
            language = list(type="string",  description="Code langue ISO-2, ex: 'FR'")
          ),
          required = c("query")
        )
      )
    ),
    list(
      type = "function",
      `function` = list(
        name = "mml_price_by_url",
        description = "Récupère le prix d'une page produit Make My Lemonade à partir de son URL.",
        parameters = list(
          type = "object",
          properties = list(
            url = list(type="string", description="URL produit complète")
          ),
          required = c("url")
        )
      )
    )
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# --- HTTP / JSON ----------------------------------------------------------

.parse_json <- function(resp) {
  ct <- httr2::resp_content_type(resp)
  if (!grepl("json", tolower(ct))) {
    list(
      error  = paste0("Non-JSON response (Content-Type=", ct, ")"),
      status = httr2::resp_status(resp),
      text   = rawToChar(httr2::resp_body_raw(resp))
    )
  } else {
    # IMPORTANT: garder la structure imbriquée telle quelle (pas de simplification)
    raw_txt <- rawToChar(httr2::resp_body_raw(resp))
    jsonlite::fromJSON(raw_txt, simplifyVector = FALSE)
  }
}

.perform_json_post <- function(url, body, headers = list(), tolerate_http_error = TRUE){
  req <- httr2::request(url) |>
    httr2::req_headers(!!!headers, `Accept`="application/json") |>
    httr2::req_body_json(body)

  if (isTRUE(tolerate_http_error)) {
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
  }
  resp <- httr2::req_perform(req)
  list(
    status  = httr2::resp_status(resp),
    content = .parse_json(resp)
  )
}

# --- BASE & HEADERS -------------------------------------------------------

.llm_openai_base <- function(){
  base <- Sys.getenv("LLM_BASE_URL", "")
  path <- Sys.getenv("LLM_PATH", "api/v1")   # par défaut api/v1 (SSPCloud)
  base <- sub("/+$","", base)
  path <- sub("^/+","", path)
  paste0(base, "/", path)
}

.llm_headers <- function(){
  key <- Sys.getenv("LLM_API_KEY", "")
  h <- list(`Content-Type`="application/json")
  if (nzchar(key)) h$Authorization <- paste("Bearer", key)
  h
}

# --- CHAT + FALLBACK TOOLS/FUNCTIONS --------------------------------------

llm_agent_chat <- function(messages,
                           tools = llm_agent_tools(),
                           tool_choice = "auto",
                           temperature = 0.2,
                           max_tokens = 800){

  model <- Sys.getenv("LLM_MODEL", "")
  base0 <- .llm_openai_base()

  bases <- unique(c(
    base0,
    sub("api/v1$", "openai/v1", base0),
    sub("api/v1$", "v1",       base0)
  ))
  url_of <- function(b) paste0(b, "/chat/completions")

  mode <- tolower(Sys.getenv("LLM_TOOLS_MODE", "auto"))  # auto | tools | functions

  body_tools <- list(
    model        = model,
    messages     = messages,
    tools        = tools,
    tool_choice  = tool_choice,
    temperature  = temperature,
    max_tokens   = max_tokens,
    stream       = FALSE
  )

  functions <- lapply(tools, function(t){
    list(
      name        = t$`function`$name,
      description = t$`function`$description,
      parameters  = t$`function`$parameters
    )
  })
  body_funcs <- list(
    model          = model,
    messages       = messages,
    functions      = functions,
    function_call  = "auto",
    temperature    = temperature,
    max_tokens     = max_tokens,
    stream         = FALSE
  )

  headers <- .llm_headers()
  try_variants <- switch(mode,
                         "tools"     = list(list(kind="tools",     body=body_tools)),
                         "functions" = list(list(kind="functions", body=body_funcs)),
                         list(list(kind="tools", body=body_tools), list(kind="functions", body=body_funcs))
  )

  for (b in bases) {
    u <- url_of(b)
    for (variant in try_variants) {
      res <- .perform_json_post(u, variant$body, headers, tolerate_http_error = TRUE)

      if (is.list(res$content) && is.null(res$content$error) && !is.null(res$content$choices)) {
        attr(res$content, "call_kind") <- variant$kind
        attr(res$content, "base_url")  <- b
        return(res$content)
      }

      if (res$status %in% c(400, 404)) next
    }
  }

  stop("Aucun endpoint n’a accepté la requête (tools/functions). ",
       "Vérifie LLM_BASE_URL/LLM_PATH (api/v1) et LLM_MODEL ; ",
       "tu peux aussi forcer LLM_TOOLS_MODE=functions.")
}

# --- EXTRACTION ROBUSTE DU CHOIX -------------------------------------------

.first_choice <- function(res){
  ch <- res$choices
  # cas classique: liste de listes
  if (is.list(ch) && !is.data.frame(ch)) {
    return(ch[[1]])
  }
  # cas data.frame (selon parseurs alternatifs)
  if (is.data.frame(ch)) {
    row1 <- ch[1, , drop = FALSE]
    # tenter de reconstruire un "choice" standard
    message <- NULL
    if (!is.null(row1$message) && is.list(row1$message[[1]])) {
      message <- row1$message[[1]]
    } else {
      # colonnes aplaties éventuelles
      msg_role    <- row1[["message.role"]][[1]]    %||% row1[["role"]][[1]]    %||% NULL
      msg_content <- row1[["message.content"]][[1]] %||% row1[["content"]][[1]] %||% NULL
      msg_tcalls  <- row1[["message.tool_calls"]][[1]] %||% NULL
      msg_fcall   <- row1[["message.function_call"]][[1]] %||% NULL
      message <- list(role = msg_role, content = msg_content, tool_calls = msg_tcalls, `function_call` = msg_fcall)
    }
    return(list(
      index = row1[["index"]][[1]] %||% 0L,
      message = message,
      finish_reason = row1[["finish_reason"]][[1]] %||% NULL
    ))
  }
  stop("Forme inattendue de 'choices'.")
}

# --- DISPATCH DES OUTILS ----------------------------------------------------

llm_agent_dispatch <- function(tool_call){
  fn   <- tool_call$`function`$name
  args <- try(jsonlite::fromJSON(tool_call$`function`$arguments, simplifyVector = TRUE), silent = TRUE)
  if (inherits(args, "try-error") || is.null(args)) args <- list()

  if (identical(fn, "mml_search")) {
    res <- mml_search(
      query    = args$query,
      limit    = args$limit    %||% 10L,
      country  = args$country  %||% NULL,
      language = args$language %||% NULL
    )
    return(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null"))
  }

  if (identical(fn, "mml_price_by_url")) {
    res <- mml_price_by_url(args$url)
    return(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null"))
  }

  jsonlite::toJSON(list(error = paste("Unknown tool:", fn)), auto_unbox = TRUE)
}

# --- BOUCLE AGENT -----------------------------------------------------------

agent_answer <- function(prompt, n_results = 10L){
  sys <- list(
    role    = "system",
    content = paste(
      "Tu es un assistant shopping focalisé sur le site Make My Lemonade.",
      "Quand on te demande des prix, utilise en priorité les OUTILS fournis:",
      "- mml_price_by_url quand une URL produit est donnée ;",
      "- mml_search pour trouver des produits par mots-clés.",
      "Présente des listes courtes (<= ", n_results, "), avec pour CHAQUE produit:",
      "1) le titre ; 2) le prix ; 3) l'URL exacte de la page produit.",
      "Toujours inclure l'URL explicite pour chaque produit."
    )
  )
  user <- list(role = "user", content = paste0(prompt, " (max ", n_results, " éléments)"))

  msgs <- list(sys, user)
  res  <- llm_agent_chat(msgs)

  loops <- 0L
  repeat {
    loops <- loops + 1L
    choice    <- .first_choice(res)
    call_kind <- attr(res, "call_kind") %||% "tools"
    msg       <- choice$message %||% list()

    if (identical(call_kind, "tools")) {
      tcalls <- msg$tool_calls %||% list()
      if (length(tcalls)) {
        # tracer l'appel assistant
        msgs <- append(msgs, list(list(role = "assistant", content = NULL, tool_calls = tcalls)))
        # exécuter les tools
        for (tc in tcalls) {
          out <- llm_agent_dispatch(tc)
          msgs <- append(msgs, list(list(role = "tool", content = out, tool_call_id = tc$id)))
        }
        # relance
        res <- llm_agent_chat(msgs)
        if (loops >= 3L) break
        next
      }
    } else { # functions (legacy)
      fcall <- msg$`function_call`
      if (length(fcall)) {
        tc <- list(id = paste0("fn_", as.integer(Sys.time())),
                   `function` = list(name = fcall$name, arguments = fcall$arguments))
        out <- llm_agent_dispatch(tc)
        msgs <- append(msgs, list(list(role = "function", name = fcall$name, content = out)))
        res <- llm_agent_chat(msgs)
        if (loops >= 3L) break
        next
      }
    }

    break
  }

  # message final
  final_choice <- .first_choice(res)
  list(
    text = final_choice$message$content %||% "",
    raw  = res
  )
}
