#' chat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import bslib htmltools
#' @importFrom shiny NS tagList
mod_chat_ui <- function(id){
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header("Amazon Price Chat (MVP)"),
    shiny::div(
      id = ns("log"),
      style = "height:45vh; overflow:auto; border:1px solid #eee; padding:8px;"
    ),
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::textInput(
          ns("msg"),
          NULL,
          placeholder = "Collez l’URL d’un produit ou posez une question…",
          width = "100%"
        )
      ),
      shiny::column(
        3,
        shiny::actionButton(
          ns("send"),
          "Envoyer",
          class = "btn-primary",
          width = "100%"
        )
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
    is_url <- function(x) grepl("^https?://", x, ignore.case = TRUE)

    append_msg <- function(role, text, is_html = FALSE){
      shiny::insertUI(
        selector = paste0("#", ns("log")),
        where = "beforeEnd",
        ui = shiny::div(
          class = paste("msg", ifelse(role == "user", "msg-user", "msg-assistant")),
          shiny::div(
            class = "bubble",
            shiny::div(class = "label", if (role == "user") "🧑‍💻 Vous" else "🤖 Assistant"),
            shiny::div(
              class = "text",
              if (isTRUE(is_html)) shiny::HTML(text) else shiny::HTML(htmltools::htmlEscape(text))
            ),
            shiny::div(class = "time", format(Sys.time(), "%H:%M"))
          )
        )
      )
    }

    observeEvent(input$send, {
      req(nzchar(input$msg))
      userq <- trimws(input$msg)
      append_msg("user", userq)
      shiny::updateTextInput(session, "msg", value = "")

      # 1) Cas URL produit -> extraction prix + log
      if (is_url(userq)) {
        info <- try(mml_normalize(mml_price_by_url(userq)), silent = TRUE)
        if (inherits(info, "try-error") || is.null(info)) {
          append_msg("assistant", "Désolé, je n’ai pas réussi à lire le prix de cette page.")
          session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
          return(invisible())
        }

        # Journalisation (best-effort, sans bloquer l'UI)
        try(storage_write_price(info), silent = TRUE)

        prix <- if (!is.null(info$price$amount))
          sprintf("%.2f %s", info$price$amount, info$price$currency) else "—"

        img <- if (!is.null(info$image) && nzchar(info$image)) {
          sprintf(
            "<img src='%s' style='width:72px;height:72px;object-fit:cover;border-radius:10px;border:1px solid #eee;margin-right:10px'/>",
            htmltools::htmlEscape(info$image)
          )
        } else ""

        html <- sprintf(
          "<div style='display:flex;align-items:center;gap:10px'>
             %s
             <div>
               <strong>%s</strong><br/>Prix : %s<br/>
               <a href='%s' target='_blank' rel='noopener'>Voir la page</a><br/>
               <small>source&nbsp;: %s</small>
             </div>
           </div>",
          img,
          htmltools::htmlEscape(info$title %||% "Produit"),
          prix,
          htmltools::htmlEscape(info$url %||% "#"),
          htmltools::htmlEscape(info$source %||% "?")
        )

        append_msg("assistant", html, is_html = TRUE)
        session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
        return(invisible())
      }

      # 2) Sinon, on garde ton flux LLM existant (search/get_items)
      action <- try(llm_route(userq), silent = TRUE)

      if (inherits(action, "try-error") || is.null(action$name)) {
        append_msg("assistant",
                   "Collez l’URL d’un produit Make My Lemonade et je récupère son prix 💶📎")
        session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
        return(invisible())
      }

      if (identical(action$name, "search_amazon")) {
        res  <- provider_search(action$args$q, action$args$page %||% 1)
        html <- render_search(res)                            # HTML sécurisé dans render_search()
      } else if (identical(action$name, "get_items")) {
        res  <- provider_get_items(action$args$asins %||% character())
        html <- render_items(res)
      } else {
        html <- "Je peux chercher un article (mots-clés) ou afficher le prix via identifiant/URL."
      }

      append_msg("assistant", html, is_html = TRUE)
      session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
    })
  })
}

## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")
