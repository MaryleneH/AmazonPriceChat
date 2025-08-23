#' chat UI Function
#'
#' @description Module de chat minimal avec rendu cartes produits.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import bslib htmltools
#' @importFrom shiny NS tagList
mod_chat_ui <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Amazon / MML Price Chat (MVP)"),
    shiny::div(
      id = ns("log"),
      style = "height:45vh; overflow:auto; border:1px solid #eee; padding:8px;"
    ),
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::textInput(
          ns("msg"), NULL,
          placeholder = "Ex : donne-moi le prix des jupes (2 articles max)",
          width = "100%"
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(ns("send"), "Envoyer", class = "btn-primary", width = "100%")
      )
    )
  )
}

#' chat Server Functions
#'
#' @noRd
mod_chat_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    `%||%` <- function(x,y) if (is.null(x)) y else x

    # -- Helpers -------------------------------------------------------------

    .format_price <- function(p){
      if (is.null(p)) return("")
      if (is.list(p) && !is.null(p$amount)) {
        cur <- p$currency %||% "EUR"
        amt <- suppressWarnings(as.numeric(p$amount))
      } else {
        cur <- "EUR"
        amt <- suppressWarnings(as.numeric(p))
      }
      if (is.na(amt)) return("")
      paste0(formatC(amt, big.mark = " ", decimal.mark = ",", digits = 2, format = "f"), " ", cur)
    }

    .extract_product_urls <- function(txt){
      if (is.null(txt) || !nzchar(txt)) return(character())
      m <- gregexpr("(https?://[^\\s)\\]]*makemylemonade\\.com[^\\s)\\]]*)", txt, perl = TRUE)
      urls <- regmatches(txt, m)[[1]]
      urls <- unique(gsub("[\\)\\]\\.,]+$", "", urls))
      urls
    }

    .card_html <- function(title, price_txt, url, img){
      htmltools::tags$div(
        class = "prod-card",
        style = "display:flex;gap:12px;align-items:flex-start;border:1px solid #eee;border-radius:12px;padding:10px;margin:8px 0;",
        if (!is.null(img) && nzchar(img))
          htmltools::tags$img(src = img, style = "width:80px;height:100px;object-fit:cover;border-radius:8px;"),
        htmltools::tags$div(
          style="flex:1;",
          htmltools::tags$div(style="font-weight:600;margin-bottom:4px;", title %||% ""),
          htmltools::tags$div(style="color:#555;margin-bottom:6px;", price_txt %||% ""),
          if (!is.null(url) && nzchar(url))
            htmltools::tags$a(href = url, target = "_blank", rel="noopener", "Voir le produit →")
        )
      )
    }

    .render_cards_from_results <- function(items, max_n = 10L){
      if (length(items) == 0) return(NULL)
      keep <- utils::head(items, max_n)
      htmltools::tagList(lapply(keep, function(it){
        img <- it$image %||% it$image_url %||% ""
        if (isTRUE(startsWith(img, "//"))) img <- paste0("https:", img)
        price_txt <- .format_price(it$price)
        .card_html(it$title %||% "", price_txt, it$url %||% "", img)
      }))
    }

    .render_cards_from_urls <- function(urls, max_n = 10L){
      if (length(urls) == 0) return(NULL)
      urls <- utils::head(urls, max_n)
      found <- lapply(urls, function(u){
        info <- tryCatch(mml_price_by_url(u), error = function(e) NULL)
        if (is.null(info)) return(NULL)
        price_txt <- .format_price(info$price)
        img <- info$image %||% ""
        if (isTRUE(startsWith(img, "//"))) img <- paste0("https:", img)
        .card_html(info$title %||% "", price_txt, info$url %||% u, img)
      })
      found <- Filter(Negate(is.null), found)
      if (!length(found)) return(NULL)
      htmltools::tagList(found)
    }

    .format_agent_reply <- function(x){
      if (is.null(x) || !nzchar(x)) return("")
      x <- gsub("\n", "<br/>", htmltools::htmlEscape(x), fixed = TRUE)
      x <- gsub("\\*\\*(.+?)\\*\\*", "<b>\\1</b>", x, perl = TRUE)
      x <- gsub("(?<!\\*)\\*(?!\\*)(.+?)(?<!\\*)\\*(?!\\*)", "<i>\\1</i>", x, perl = TRUE)
      x
    }

    append_msg <- function(role, text, is_html = FALSE){
      shiny::insertUI(
        selector = paste0("#", ns("log")),
        where = "beforeEnd",
        ui = htmltools::tags$div(
          class = paste("msg", ifelse(role == "user", "msg-user", "msg-assistant")),
          htmltools::tags$div(class = "bubble",
                              htmltools::tags$div(class = "label", if (role == "user") "🧑‍💻 Vous" else "🤖 Assistant"),
                              htmltools::tags$div(
                                class = "text",
                                if (isTRUE(is_html)) htmltools::HTML(text) else htmltools::HTML(htmltools::htmlEscape(text))
                              ),
                              htmltools::tags$div(class = "time", format(Sys.time(), "%H:%M"))
          )
        )
      )
    }

    # -- Observers -----------------------------------------------------------

    observeEvent(input$send, {
      req(nzchar(input$msg))
      userq <- input$msg
      append_msg("user", userq)
      shiny::updateTextInput(session, "msg", value = "")

      action <- llm_route(userq)

      if (identical(action$name, "search_amazon")) {
        res  <- provider_search(action$args$q, action$args$page %||% 1)
        html <- render_search(res)

      } else if (identical(action$name, "get_items")) {
        res  <- provider_get_items(action$args$asins %||% character())
        html <- render_items(res)

      } else {
        # ---- Chemin Agent Make My Lemonade --------------------------------
        # 1) Nombre max demandé par l’utilisateur (via util exportée)
        nres <- extract_n_results(userq, default = 10L)

        html <- tryCatch({
          shiny::withProgress(message = "Je cherche sur Make My Lemonade…",
                              value = 0, session = session, {
                                setProgress(0.1, detail = "Analyse de la requête…")

                                # 2) Appel agent LLM (avec n_results déterminé)
                                ans <- agent_answer(userq, n_results = nres)

                                setProgress(0.45, detail = "Récupération des produits…")

                                # 3) On ignore tout HTML éventuel de l’agent et on reconstruit NOS cartes
                                txt_html <- .format_agent_reply(ans$text %||% "")
                                urls     <- .extract_product_urls(ans$text %||% "")

                                # Essai via URLs détectées
                                cards <- NULL
                                if (length(urls)) {
                                  cards <- .render_cards_from_urls(urls, max_n = nres)  # <- coupe stricte
                                }

                                # Fallback via recherche directe, mais LIMITÉE
                                if (is.null(cards)) {
                                  srch <- tryCatch(mml_search(userq, limit = max(nres, 20L)), error = function(e) NULL)
                                  if (!is.null(srch) && length(srch$results)) {
                                    cards <- .render_cards_from_results(srch$results, max_n = nres)  # <- coupe stricte
                                  }
                                }

                                setProgress(0.9, detail = "Mise en forme…")

                                final <- if (!is.null(cards)) {
                                  paste0(if (nzchar(txt_html)) paste0(txt_html, "<hr/>") else "", as.character(cards))
                                } else {
                                  txt_html
                                }

                                setProgress(1)
                                final
                              })
        }, error = function(e){
          paste0("<div class='error'>", htmltools::htmlEscape(conditionMessage(e)), "</div>")
        })
      }

      append_msg("assistant", html, is_html = TRUE)
      session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
    })
  })
}
