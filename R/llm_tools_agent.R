# R/llm_tools_agent.R

#' Spécification des outils (OpenAI Tools)
#' @keywords internal
#' @noRd
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
            query    = list(type="string",  description="Requête utilisateur, ex: 'jupes', 'robe noire'"),
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

# --- Client OpenAI-compatible (vLLM / SSPCloud) -----------------------------

.llm_openai_base <- function(){
  base <- Sys.getenv("LLM_BASE_URL", "")
  path <- Sys.getenv("LLM_PATH", "v1")
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

#' Appel chat/completions avec outils
#' @keywords internal
#' @noRd
llm_agent_chat <- function(messages, tools = llm_agent_tools(), tool_choice = "auto",
                           temperature = 0.2, max_tokens = 800){
  url  <- paste0(.llm_openai_base(), "/chat/completions")
  body <- list(
    model        = Sys.getenv("LLM_MODEL", ""),
    messages     = messages,
    tools        = tools,
    tool_choice  = tool_choice,
    temperature  = temperature,
    max_tokens   = max_tokens,
    stream       = FALSE
  )

  req <- httr2::request(url) |>
    httr2::req_headers(`Content-Type`="application/json")

  # ajoute Authorization si on a une clé
  key <- Sys.getenv("LLM_API_KEY", "")
  if (nzchar(key)) req <- httr2::req_headers(req, Authorization = paste("Bearer", key))

  resp <- req |> httr2::req_body_json(body) |> httr2::req_perform()
  httr2::resp_body_json(resp, simplifyVector = TRUE)
}

# --- Dispatch d'un tool_call ------------------------------------------------

#' Exécute un tool_call renvoyé par le LLM
#' @keywords internal
#' @noRd
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

# --- Boucle agent (tools -> réponse finale) ---------------------------------

#' Agent : prend une consigne utilisateur et renvoie la réponse finale du LLM
#' @param prompt texte utilisateur
#' @param n_results borne haute de résultats si applicable
#' @keywords internal
#' @noRd
agent_answer <- function(prompt, n_results = 10L){
  sys <- list(
    role    = "system",
    content = paste(
      "Tu es un assistant shopping focalisé sur le site Make My Lemonade.",
      "Quand on te demande des prix, utilise les OUTILS fournis.",
      "Présente des listes courtes, avec titre, prix et lien.",
      "Si aucun résultat, dis-le clairement."
    )
  )
  user <- list(role = "user", content = paste0(prompt, " (max ", n_results, " éléments)"))

  msgs <- list(sys, user)
  res  <- llm_agent_chat(msgs)

  loops <- 0L
  repeat {
    loops <- loops + 1L
    choice <- res$choices[[1]]
    tcalls <- choice$message$tool_calls

    if (length(tcalls)) {
      # ajouter le tour assistant avec tool_calls (contenu vide côté assistant)
      msgs <- append(msgs, list(list(role = "assistant", content = NULL, tool_calls = tcalls)))

      # exécuter chaque tool_call puis ajouter un message 'tool'
      for (tc in tcalls) {
        out <- llm_agent_dispatch(tc)
        msgs <- append(msgs, list(list(role = "tool", content = out, tool_call_id = tc$id)))
      }

      # relancer le LLM avec les sorties d'outils
      res <- llm_agent_chat(msgs)
      if (loops >= 3L) break
    } else {
      break
    }
  }

  list(
    text = res$choices[[1]]$message$content %||% "",
    raw  = res
  )
}
