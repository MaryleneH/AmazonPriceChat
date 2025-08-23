# R/llm_tools_agent.R
# Agent Make My Lemonade (OpenAI-compatible / vLLM)
# - Tools: mml_search, mml_price_by_url
# - Prompting principal conservé
# - Filtre de second temps (max_items, price_max) déduit via llm_chat
# - L'agent renvoie du TEXTE avec des URLs (mod_chat.R fera les cartes)

# ---------------------------------------------------------------------
# Helpers génériques
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---------------------------------------------------------------------
# Définition des tools (OpenAI "tools" / legacy "functions")
llm_agent_tools <- function(){
  list(
    list(
      type = "function",
      `function` = list(
        name = "mml_search",
        description = "Recherche des produits Make My Lemonade à partir d'une requête texte. Retourne jusqu'à 'limit' articles (titre, URL, image, prix).",
        parameters = list(
          type = "object",
          properties = list(
            query    = list(type="string",  description="Requête, ex: 'jupes', 'pull noir'"),
            limit    = list(type="integer", description="Nombre maximum d'articles (1-20)", minimum=1L, maximum=20L, default=10L),
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

# ---------------------------------------------------------------------
# Base / headers pour l'endpoint OpenAI-compatible (SSPCloud vLLM)
.llm_openai_base <- function(){
  base <- Sys.getenv("LLM_BASE_URL", "")
  path <- Sys.getenv("LLM_PATH", "api/v1")  # SSPCloud: api/v1
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

.parse_json <- function(resp) {
  ct <- httr2::resp_content_type(resp)
  if (!grepl("json", tolower(ct))) {
    list(error  = paste0("Non-JSON (Content-Type=", ct, ")"),
         status = httr2::resp_status(resp),
         text   = rawToChar(httr2::resp_body_raw(resp)))
  } else {
    raw_txt <- rawToChar(httr2::resp_body_raw(resp))
    jsonlite::fromJSON(raw_txt, simplifyVector = FALSE)
  }
}

.perform_json_post <- function(url, body, headers = list(), tolerate_http_error = TRUE){
  req <- httr2::request(url) |>
    httr2::req_headers(!!!headers, `Accept`="application/json") |>
    httr2::req_body_json(body)
  if (isTRUE(tolerate_http_error)) req <- httr2::req_error(req, is_error = function(resp) FALSE)
  resp <- httr2::req_perform(req)
  list(status  = httr2::resp_status(resp),
       content = .parse_json(resp))
}

# ---------------------------------------------------------------------
# Chat completions compatible tools + fallback legacy functions
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
                         list(list(kind="tools", body=body_tools), list(kind="functions", body=body_funcs)))

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

# Extraction robuste du premier "choice"
.first_choice <- function(res){
  ch <- res$choices
  if (is.list(ch) && !is.data.frame(ch)) return(ch[[1]])
  if (is.data.frame(ch)) {
    row1 <- ch[1, , drop = FALSE]
    message <- NULL
    if (!is.null(row1$message) && is.list(row1$message[[1]])) {
      message <- row1$message[[1]]
    } else {
      msg_role    <- row1[["message.role"]][[1]]    %||% row1[["role"]][[1]]    %||% NULL
      msg_content <- row1[["message.content"]][[1]] %||% row1[["content"]][[1]] %||% NULL
      msg_tcalls  <- row1[["message.tool_calls"]][[1]] %||% NULL
      msg_fcall   <- row1[["message.function_call"]][[1]] %||% NULL
      message <- list(role = msg_role, content = msg_content, tool_calls = msg_tcalls, `function_call` = msg_fcall)
    }
    return(list(index = row1[["index"]][[1]] %||% 0L, message = message,
                finish_reason = row1[["finish_reason"]][[1]] %||% NULL))
  }
  stop("Forme inattendue de 'choices'.")
}

# ---------------------------------------------------------------------
# Dispatch R pour l'exécution des tools
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

# ---------------------------------------------------------------------
# Mini-agent extracteur de contraintes (2e temps)
.safely_parse_json <- function(txt){
  m <- regexpr("\\{.*\\}", txt, perl = TRUE)
  if (m[1] > 0) {
    json <- regmatches(txt, m)
    out <- try(jsonlite::fromJSON(json, simplifyVector = TRUE), silent = TRUE)
    if (!inherits(out, "try-error")) return(out)
  }
  list()
}

.extract_filters_llm <- function(prompt, default_n = 10L){
  # Nécessite llm_chat() (client simple OpenAI-compatible)
  sys <- list(
    role    = "system",
    content = paste(
      "Tu es un extracteur de contraintes pour des requêtes shopping Make My Lemonade.",
      "Réponds UNIQUEMENT en JSON compact valide, sans texte autour.",
      'Schéma: {"max_items": int, "price_max": number|null, "include": [string]}.',
      "Comprends les nombres en lettres (ex: 'deux' => 2).",
      "Si rien n'est précisé: max_items = ", default_n, ", price_max = null, include = []."
    )
  )
  usr <- list(role = "user", content = prompt)
  res <- llm_chat(list(sys, usr))
  msg <- .first_choice(res)$message$content %||% "{}"
  f   <- .safely_parse_json(msg)

  max_items <- as.integer(f$max_items %||% default_n)
  if (is.na(max_items) || max_items < 1L) max_items <- default_n
  max_items <- min(max_items, default_n)  # ne pas dépasser le paramètre n_results passé par l'appelant

  price_max <- suppressWarnings(as.numeric(f$price_max))
  if (is.null(price_max) || is.na(price_max)) price_max <- Inf

  include <- f$include %||% list()
  if (is.character(include)) include <- as.list(include)

  list(max_items = max_items, price_max = price_max, include = include)
}

# Heuristique de secours si l'extracteur n'a rien donné
.extract_keywords_heuristic <- function(txt){
  s <- tolower(txt)
  cats <- c("pull","pulls","jupe","jupes","robe","robes","chemise","chemises",
            "jean","jeans","pantalon","pantalons","veste","vestes","gilet","gilets",
            "cardigan","cardigans","top","tops","t-shirt","tee-shirt","short","shorts",
            "manteau","manteaux","blouse","blouses","sweat","sweats")
  hit <- cats[grepl(paste0("\\b(", paste(cats, collapse="|"), ")\\b"), s)]
  hit <- unique(hit)
  if (length(hit)) return(paste(unique(sub("s$","",hit)), collapse=" "))
  # fallback: on nettoie un peu
  s <- gsub("https?://\\S+"," ", s)
  s <- gsub("[^[:alnum:]àâäéèêëîïôöùûüç\\s-]", " ", s, perl = TRUE)
  s <- gsub("\\b(donne|donne-moi|le|la|les|des|de|du|un|une|prix|montre|trouve|articles?|maximum|max|moins|que|pour|à)\\b"," ", s)
  s <- gsub("\\s+"," ", s)
  trimws(s)
}

# URLs produits MML dans un texte
.extract_product_urls <- function(txt){
  if (is.null(txt) || !nzchar(txt)) return(character())
  m <- gregexpr("(https?://[^\\s)\\]]*makemylemonade\\.com[^\\s)\\]]*)", txt, perl = TRUE)
  u <- regmatches(txt, m)[[1]]
  unique(gsub("[\\)\\]\\.,]+$", "", u))
}

# Format prix (affichage texte)
.fmt_price <- function(price){
  if (is.null(price)) return("—")
  if (is.list(price) && !is.null(price$amount)) {
    amt <- suppressWarnings(as.numeric(price$amount))
  } else {
    amt <- suppressWarnings(as.numeric(price))
  }
  if (is.na(amt)) return("—")
  paste0(formatC(amt, big.mark = " ", decimal.mark = ",", digits = 2, format = "f"), " €")
}

# ---------------------------------------------------------------------
# Agent principal : prompting + tools, puis filtrage de second temps
agent_answer <- function(prompt, n_results = 10L){

  # 1) On déduit les contraintes (max_items, price_max, mots clés éventuels)
  flt <- .extract_filters_llm(prompt, default_n = n_results)

  # 2) Prompting principal (inchangé dans l'esprit)
  sys <- list(
    role    = "system",
    content = paste(
      "Tu es un assistant shopping focalisé sur le site Make My Lemonade.",
      "Quand on te demande des prix, utilise en priorité les OUTILS fournis :",
      "- mml_price_by_url quand une URL produit est donnée ;",
      "- mml_search pour trouver des produits par mots-clés.",
      "Tu DOIS appeler au moins un outil si la requête concerne des produits.",
      "Ne réponds JAMAIS 'Aucun résultat' sans avoir essayé mml_search.",
      "Pour choisir le nombre d'articles à afficher, tu cherches le prix demandé par l'utilisateur",
      "Tu choisis également le nombre d'articles, selon le nombre qu'on te demande",
      "Présente des listes courtes (<= ", flt$max_items, "), avec pour CHAQUE produit :",
      "1) le titre ; 2) le prix ; 3) l'URL exacte de la page produit.",
      "Tu filtres toujours la liste selon les instructions qu'on te donne",
      "Toujours inclure l'URL explicite pour chaque produit."
    )
  )
  user <- list(role = "user",
               content = paste0(prompt, " (max ", flt$max_items, " éléments)") )

  msgs <- list(sys, user)
  res  <- llm_agent_chat(msgs)

  # 3) Boucle d'exécution des tools (<= 3 tours)
  loops <- 0L
  repeat {
    loops <- loops + 1L
    choice    <- .first_choice(res)
    call_kind <- attr(res, "call_kind") %||% "tools"
    msg       <- choice$message %||% list()

    if (identical(call_kind, "tools")) {
      tcalls <- msg$tool_calls %||% list()
      if (length(tcalls)) {
        msgs <- append(msgs, list(list(role = "assistant", content = NULL, tool_calls = tcalls)))
        for (tc in tcalls) {
          out <- llm_agent_dispatch(tc)   # appelle mml_search / mml_price_by_url côté R
          msgs <- append(msgs, list(list(role = "tool", content = out, tool_call_id = tc$id)))
        }
        res <- llm_agent_chat(msgs)
        if (loops >= 3L) break
        next
      }
    } else { # compat function_call
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

  # 4) On récupère le texte final du modèle
  final_text <- .first_choice(res)$message$content %||% ""

  # 5) On construit la liste finale À PARTIR des données (URLs -> price_by_url OU mml_search),
  #    puis on filtre (prix max + max_items) et on renvoie un TEXTE avec URLs.
  #    => mod_chat.R détecte les URLs et affichera les cartes avec image.

  # a) URLs proposées par l'agent
  urls <- .extract_product_urls(final_text)

  # b) Collecte candidates par URLs, sinon fallback mml_search sur mots-clés
  candidates <- list()

  if (length(urls)) {
    for (u in urls) {
      p <- try(mml_price_by_url(u), silent = TRUE)
      if (!inherits(p, "try-error") && length(p)) {
        candidates[[length(candidates)+1L]] <- list(
          title = p$title %||% "",
          url   = p$url   %||% u,
          image = p$image %||% NULL,
          price = p$price %||% NULL
        )
      }
    }
  }

  if (!length(candidates)) {
    # mots-clés : utilise include[] si donné, sinon heuristique
    query <- if (length(flt$include)) paste(unlist(flt$include), collapse = " ") else .extract_keywords_heuristic(prompt)
    if (!nzchar(query)) query <- prompt
    r <- try(mml_search(query, limit = max(30L, flt$max_items * 5L)), silent = TRUE)
    if (!inherits(r, "try-error") && length(r$results)) {
      candidates <- lapply(r$results, function(x) list(
        title = x$title %||% "",
        url   = x$url   %||% x$link %||% "",
        image = x$image_url %||% x$image %||% NULL,
        price = x$price %||% NULL
      ))
    }
  }

  if (!length(candidates)) {
    return(list(
      text = "Aucun produit correspondant à ta demande n’a été trouvé.",
      raw  = list(filters = flt, agent_text = final_text)
    ))
  }

  # c) Normalisation + filtre prix + dédup + coupe à max_items
  norm <- lapply(candidates, function(it){
    amount <- if (is.list(it$price) && !is.null(it$price$amount)) {
      suppressWarnings(as.numeric(it$price$amount))
    } else {
      suppressWarnings(as.numeric(it$price))
    }
    list(
      title = it$title %||% "",
      url   = it$url   %||% "",
      image = it$image %||% NULL,
      price_amount = amount
    )
  })

  # filtre prix (on garde aussi ceux sans prix renseigné)
  if (is.finite(flt$price_max)) {
    norm <- Filter(function(it) is.na(it$price_amount) || it$price_amount <= flt$price_max + 1e-9, norm)
  }

  # dédup par URL canonique + coupe
  seen <- character(0); out <- list()
  for (it in norm) {
    u <- sub("/+$","", tolower(it$url %||% ""))
    if (!nzchar(u) || u %in% seen) next
    seen <- c(seen, u)
    out[[length(out)+1L]] <- it
    if (length(out) >= flt$max_items) break
  }

  if (!length(out)) {
    return(list(
      text = "Aucun produit correspondant à ta demande n’a été trouvé.",
      raw  = list(filters = flt, agent_text = final_text)
    ))
  }

  # d) Rend du TEXTE avec URLs (mod_chat.R fera les cartes via extraction URL)
  lines <- vapply(seq_along(out), function(i){
    it <- out[[i]]
    price_txt <- if (is.na(it$price_amount)) "—" else
      paste0(formatC(it$price_amount, big.mark = " ", decimal.mark = ",", digits = 2, format = "f"), " €")
    paste0(
      i, ". **", it$title, "** — ", price_txt, "\n",
      "   - URL : ", it$url
    )
  }, character(1))

  list(
    text = paste(lines, collapse = "\n"),
    raw  = list(filters = flt, agent_text = final_text, total_candidates = length(norm))
  )
}
