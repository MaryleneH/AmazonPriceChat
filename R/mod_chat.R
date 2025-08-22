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
    shiny::div(id = ns("log"),
               style = "height:45vh; overflow:auto; border:1px solid #eee; padding:8px;"),
    shiny::fluidRow(
      shiny::column(9, shiny::textInput(ns("msg"), NULL,
                                        placeholder = "Pose ta question (ex: prix ASIN B0...)", width = "100%")),
      shiny::column(3, shiny::actionButton(ns("send"), "Envoyer",
                                           class = "btn-primary", width = "100%"))
    )
  )
}

#' chat Server Functions
#'
#' @noRd
mod_chat_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    append_msg <- function(role, text){
      shiny::insertUI(
        selector = paste0("#", ns("log")),
        where = "beforeEnd",
        ui = shiny::div(
          class = paste("msg", ifelse(role == "user", "msg-user", "msg-assistant")),
          shiny::div(class = "bubble",
                     shiny::div(class = "label",
                                if (role == "user") "🧑‍💻 Vous" else "🤖 Assistant"
                     ),
                     shiny::div(class = "text", shiny::HTML(htmltools::htmlEscape(text))),
                     shiny::div(class = "time", format(Sys.time(), "%H:%M"))
          )
        )
      )
    }

    observeEvent(input$send, {
      req(nzchar(input$msg))
      userq <- input$msg
      append_msg(ns, "user", htmltools::htmlEscape(userq))
      updateTextInput(session, "msg", value = "")

      action <- llm_route(userq)  # ← décision par le LLM

      if (identical(action$name, "search_amazon")) {
        res  <- provider_search(action$args$q, action$args$page %||% 1)
        html <- render_search(res)
      } else if (identical(action$name, "get_items")) {
        res  <- provider_get_items(action$args$asins %||% character())
        html <- render_items(res)
      } else {
        html <- "Je peux chercher un article (mots-clés) ou afficher le prix via identifiant."
      }

      append_msg(ns, "assistant", html)
      session$sendCustomMessage("scroll_bottom", list(id = ns("log")))
    })

  })
}

## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")
