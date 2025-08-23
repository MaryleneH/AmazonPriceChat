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
#' Réponse agent MML strictement bornée et robuste aux phrases naturelles
#' @param prompt Question utilisateur (ex: "Donne moi le prix des pulls, deux articles maximum")
#' @param n_results Borne max si l'utilisateur ne précise rien
#' @return list(text, raw)
agent_answer <- function(prompt, n_results = 10L){
  `%||%` <- function(x,y) if (is.null(x)) y else x

  # ---------------- Helpers parsing ----------------
  .num_words <- c(
    "un"=1,"une"=1,"deux"=2,"trois"=3,"quatre"=4,"cinq"=5,
    "six"=6,"sept"=7,"huit"=8,"neuf"=9,"dix"=10
  )
  .to_num <- function(x){
    x <- tolower(trimws(x))
    if (nzchar(x) && grepl("^[0-9]+$", x)) return(as.integer(x))
    if (x %in% names(.num_words)) return(as.integer(.num_words[[x]]))
    NA_integer_
  }
  .parse_n <- function(txt, default = n_results){
    t <- tolower(txt)
    # "max 2", "au plus 3", "2 articles/produits"
    m <- regexpr("(?:max(?:imum)?|au plus|au maximum)\\s*(\\d+)", t, perl=TRUE)
    if (m[1] > 0) return(max(1L, .to_num(sub(".*?(\\d+).*","\\1", regmatches(t,m)))))
    m <- regexpr("(\\d+)\\s*(?:articles?|produits?|r[ée]sultats?)", t, perl=TRUE)
    if (m[1] > 0) return(max(1L, .to_num(sub("^(\\d+).*","\\1", regmatches(t,m)))))
    # lettres
    rxw <- "\\b(un|une|deux|trois|quatre|cinq|six|sept|huit|neuf|dix)\\b"
    m <- regexpr(paste0("(?:max(?:imum)?|au plus|au maximum)\\s*", rxw), t, perl=TRUE)
    if (m[1] > 0) return(max(1L, .to_num(sub(".*?\\b(un|une|deux|trois|quatre|cinq|six|sept|huit|neuf|dix)\\b.*","\\1", regmatches(t,m)))))
    m <- regexpr(paste0(rxw, "\\s*(?:articles?|produits?|r[ée]sultats?)"), t, perl=TRUE)
    if (m[1] > 0) return(max(1L, .to_num(sub(".*?\\b(un|une|deux|trois|quatre|cinq|six|sept|huit|neuf|dix)\\b.*","\\1", regmatches(t,m)))))
    as.integer(default)
  }
  .parse_price_max <- function(txt){
    t <- tolower(txt)
    rx <- "(?:moins de|sous|budget|<=|≤|jusqu(?:'|e) ?à|max(?:imum)?)\\s*([0-9]+(?:[.,][0-9]+)?)\\s*(?:€|eur|euros?)?"
    m <- regexpr(rx, t, perl=TRUE)
    if (m[1] > 0) {
      v <- sub(".*?([0-9]+(?:[.,][0-9]+)?).*","\\1", regmatches(t,m))
      v <- as.numeric(gsub(",", ".", v))
      if (!is.na(v)) return(v)
    }
    Inf
  }
  .extract_urls <- function(s){
    m <- gregexpr("https?://[^\\s)]+", s, perl=TRUE)
    u <- regmatches(s, m)[[1]]
    unique(u[nzchar(u)])
  }

  # Mots-clés produits connus + normalisation (pluriels/synonymes -> singulier canonique)
  .norm_map <- c(
    "pulls"="pull","pull"="pull","maille"="pull","mailes"="pull","maille(s)?"="pull",
    "sweat"="sweat","sweats"="sweat",
    "cardigan"="cardigan","cardigans"="cardigan","gilet"="cardigan","gilets"="cardigan",
    "jupe"="jupe","jupes"="jupe",
    "robe"="robe","robes"="robe",
    "chemise"="chemise","chemises"="chemise",
    "pantalon"="pantalon","pantalons"="pantalon","jean"="jean","jeans"="jean",
    "t-shirt"="t-shirt","tshirts"="t-shirt","tee-shirt"="t-shirt","tee"="t-shirt","tees"="t-shirt",
    "short"="short","shorts"="short",
    "manteau"="manteau","manteaux"="manteau","trench"="trench","blouson"="manteau","veste"="veste","vestes"="veste",
    "bodys"="body","body"="body","bodies"="body",
    "top"="top","tops"="top"
  )
  .extract_terms <- function(txt){
    t <- tolower(txt)
    # retire URLs et mentions de quantité/prix
    t <- gsub("https?://[^\\s)]+", " ", t)
    t <- gsub("(?:moins de|sous|budget|<=|≤|jusqu(?:'|e) ?à|max(?:imum)?)\\s*[0-9]+(?:[.,][0-9]+)?\\s*(?:€|eur|euros?)?", " ", t, perl=TRUE)
    # tokenize simple
    toks <- unlist(strsplit(gsub("[^[:alnum:]-]+"," ", t), "\\s+"))
    toks <- toks[nchar(toks) >= 3]
    # stopwords FR les + courants
    sw <- c("donne","donnez","moi","le","la","les","du","des","de","pour","prix",
            "quel","quelle","quels","quelles","avec","sans","sur","et","ou",
            "maximum","max","articles","produits","resultats","résultats","article","produit")
    toks <- setdiff(toks, sw)
    # normalise
    norm <- function(w){
      if (w %in% names(.norm_map)) return(.norm_map[[w]])
      # pluriels basiques
      w <- sub("s$", "", w)
      w
    }
    out <- unique(vapply(toks, norm, character(1)))
    # garde uniquement les termes présents dans le vocabulaire produit si on en a
    vocab <- unique(unname(.norm_map))
    keep  <- intersect(out, vocab)
    if (length(keep)) keep else out
  }

  .fmt_price <- function(a){
    if (is.null(a) || is.na(a)) return("—")
    paste0(format(round(as.numeric(a), 2), nsmall = 2, decimal.mark = ","), " €")
  }

  # ---------------- Contraintes de la demande ----------------
  n_target  <- max(1L, .parse_n(prompt, n_results))
  n_target  <- min(n_target, n_results)
  price_max <- .parse_price_max(prompt)

  # ---------------- 1) URLs explicites ----------------
  urls  <- .extract_urls(prompt)
  items <- list()
  if (length(urls)) {
    seen <- character()
    for (u in urls) {
      if (u %in% seen) next
      seen <- c(seen, u)
      it <- try(mml_price_by_url(u), silent = TRUE)
      if (inherits(it, "try-error") || is.null(it)) next
      amt <- it$price$amount %||% NA_real_
      if (is.finite(price_max) && is.numeric(amt) && !is.na(amt) && amt > price_max) next
      items[[length(items)+1L]] <- it
      if (length(items) >= n_target) break
    }
  }

  # ---------------- 2) Recherche par mots-clés robustes ----------------
  if (length(items) < n_target) {
    terms <- .extract_terms(prompt)
    # si on n'a rien trouvé, tente quelques alias courants par défaut
    if (!length(terms)) terms <- c("pull","robe","jupe","t-shirt","jean")

    seen <- c(urls %||% character())
    # Essaie plusieurs termes jusqu'à atteindre n_target
    for (term in terms) {
      sr <- try(mml_search(term, limit = max(50L, n_target*5L)), silent = TRUE)
      if (inherits(sr, "try-error") || !length(sr$results %||% list())) next
      cand <- sr$results

      # filtre prix
      if (is.finite(price_max)) {
        cand <- Filter(function(it){
          amt <- it$price$amount %||% NA_real_
          is.na(amt) || (!is.na(amt) && amt <= price_max)
        }, cand)
      }
      if (!length(cand)) next

      # déduplique par URL et ajoute
      for (it in cand) {
        u <- it$url %||% ""
        if (!nzchar(u) || u %in% seen) next
        seen <- c(seen, u)
        items[[length(items)+1L]] <- it
        if (length(items) >= n_target) break
      }
      if (length(items) >= n_target) break
    }
  }

  # ---------------- Sortie ----------------
  if (!length(items)) {
    return(list(
      text = "Aucun produit correspondant à ta demande n’a été trouvé.",
      raw  = list(n_target=n_target, price_max=price_max, tried_terms = .extract_terms(prompt))
    ))
  }

  items <- head(items, n_target)

  bullets <- vapply(items, function(it){
    sprintf("- **%s** — %s — %s",
            it$title %||% "Produit",
            .fmt_price(it$price$amount %||% NA_real_),
            it$url %||% "")
  }, character(1))

  list(
    text = paste(c(
      sprintf("Voici %d suggestion(s)%s :", length(items),
              if (is.finite(price_max)) sprintf(" (≤ %.2f €)", price_max) else ""),
      "", bullets), collapse = "\n"),
    raw  = list(
      n_target   = n_target,
      price_max  = price_max,
      used_urls  = urls,
      tried_terms= .extract_terms(prompt)
    )
  )
}
