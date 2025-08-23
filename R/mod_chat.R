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

    # >>> NOUVEAU : extraction robuste du nombre d'articles demandé
    .extract_n_results <- function(text, default = 10L){
      if (is.null(text) || !nzchar(text)) return(default)
      s <- tolower(trimws(text))

      # chiffres explicites
      nums <- regmatches(s, gregexpr("\\d+", s))[[1]]
      if (length(nums) > 0) {
        n <- suppressWarnings(as.integer(nums[1]))
        if (!is.na(n)) return(max(1L, min(n, default)))
      }

      # nombres en lettres (fr)
      dict <- c(
        "un"=1,"une"=1,"deux"=2,"trois"=3,"quatre"=4,"cinq"=5,
        "six"=6,"sept"=7,"huit"=8,"neuf"=9,"dix"=10,
        "quelques"=5,"quelque"=5
      )
      for (w in names(dict)) {
        if (grepl(paste0("\\b", w, "\\b"), s, perl = TRUE)) {
          return(max(1L, min(as.integer(dict[[w]]), default)))
        }
      }

      default
    }

    .format_price <- function(p){
      if (is.null(p) || is.null(p$amount)) return("")
      cur <- p$currency %||% "EUR"
      amt <- tryCatch(as.numeric(p$amount), error = function(e) NA_real_)
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
        class = "product-card",
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
        price_txt <- .format_price(it$price)
        .card_html(it$title %||% "", price_txt, it$url %||% "", it$image %||% "")
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
        nres <- .extract_n_results(userq, default = 10L)

        html <- tryCatch({
          shiny::withProgress(message = "Je cherche sur Make My Lemonade…", value = 0.1, {
            ans <- agent_answer(userq, n_results = nres)
            raw_txt <- ans$text %||% ""

            # Si l'agent renvoie déjà du HTML de cartes, on l'affiche tel quel
            if (grepl("<div|<img|class=\\\"prod-grid\\\"|class=\\\"product-card\\\"", raw_txt)) {
              final <- raw_txt
            } else {
              # Sinon on formate le texte + essaie d'afficher des cartes depuis les URLs
              txt_html <- .format_agent_reply(raw_txt)
              urls     <- .extract_product_urls(raw_txt)
              cards    <- .render_cards_from_urls(urls, max_n = nres)

              if (is.null(cards)) {
                # Fallback : recherche directe limitée à nres
                srch <- tryCatch(mml_search(userq, limit = nres), error = function(e) NULL)
                if (!is.null(srch) && length(srch$results)) {
                  cards <- .render_cards_from_results(srch$results, max_n = nres)
                }
              }

              final <- if (!is.null(cards)) {
                paste0(if (nzchar(txt_html)) paste0(txt_html, "<hr/>") else "", as.character(cards))
              } else {
                txt_html
              }
            }

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
