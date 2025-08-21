#' chat UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import bslib
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
      append_msg("user", userq)

      # placeholder avant LLM
      reply <- "LLM non configuré pour l’instant. Le squelette fonctionne !"
      append_msg("assistant", reply)

      updateTextInput(session, "msg", value = "")
    })
  })
}

## To be copied in the UI
# mod_chat_ui("chat_1")

## To be copied in the server
# mod_chat_server("chat_1")
