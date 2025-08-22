#' history UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_history_ui <- function(id){
  ns <- shiny::NS(id)

  bslib::card(
    bslib::card_header("Historique des relevés de prix"),
    # Résumé
    shiny::div(
      class = "row g-3",
      style = "margin-bottom:6px",
      shiny::div(class="col-sm-4", bslib::value_box(title = "Enregistrements", value = textOutput(ns("v_n"), inline = TRUE))),
      shiny::div(class="col-sm-4", bslib::value_box(title = "Prix médian",   value = textOutput(ns("v_med"), inline = TRUE))),
      shiny::div(class="col-sm-4", bslib::value_box(title = "Dernier relevé", value = textOutput(ns("v_last"), inline = TRUE)))
    ),

    # Filtres
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      shiny::dateRangeInput(ns("date"), "Date", start = Sys.Date()-7, end = Sys.Date()),
      shiny::textInput(ns("q"), "Rechercher (titre / url)", placeholder = "ex: jupe, jean, cardigan…"),
      shiny::selectInput(ns("source"), "Source", choices = c("Toutes" = ""), selected = ""),
      shiny::sliderInput(ns("price"), "Prix (EUR)", min = 0, max = 500, value = c(0, 500))
    ),

    # Actions
    shiny::div(
      class = "d-flex gap-2",
      shiny::actionButton(ns("refresh"), "Rafraîchir"),
      shiny::downloadButton(ns("dl_csv"), "Exporter CSV")
    ),

    # Table
    DT::dataTableOutput(ns("tbl"))
  )
}

#' Historique Server
#' @export
mod_history_server <- function(id){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    # --- lecture des données (au clic & au démarrage)
    read_now <- function(){
      df <- try(storage_read_all(), silent = TRUE)
      if (inherits(df, "try-error") || is.null(df)) return(utils::head(data.frame(), 0))
      df
    }
    data <- shiny::reactiveVal(read_now())
    shiny::observeEvent(input$refresh, ignoreInit = TRUE, { data(read_now()) })
    shiny::observeEvent(TRUE, { data(read_now()) }, once = TRUE)

    # --- mise à jour des filtres selon les données
    shiny::observeEvent(data(), {
      df <- data()
      if (!nrow(df)) return()

      # Date range
      dmin <- as.Date(min(df$ts, na.rm = TRUE))
      dmax <- as.Date(max(df$ts, na.rm = TRUE))
      shiny::updateDateRangeInput(session, "date", start = dmin, end = dmax, min = dmin, max = dmax)

      # Source
      srcs <- sort(unique(na.omit(df$source)))
      shiny::updateSelectInput(session, "source", choices = c("Toutes" = "", srcs), selected = "")

      # Prix
      p <- suppressWarnings(as.numeric(df$price_amount))
      p <- p[is.finite(p)]
      if (length(p)) {
        pmin <- floor(min(p, na.rm = TRUE))
        pmax <- ceiling(max(p, na.rm = TRUE))
        shiny::updateSliderInput(session, "price", min = max(0, pmin), max = max(1, pmax), value = c(max(0,pmin), max(1,pmax)))
      }
    })

    # --- filtrage
    filtered <- shiny::reactive({
      df <- data()
      if (!nrow(df)) return(df)

      # date
      if (!is.null(input$date) && length(input$date) == 2) {
        d1 <- as.Date(input$date[1]); d2 <- as.Date(input$date[2])
        df <- df[as.Date(df$ts) >= d1 & as.Date(df$ts) <= d2, , drop = FALSE]
      }

      # source
      if (nzchar(input$source)) df <- df[isTRUE(df$source == input$source), , drop = FALSE]

      # prix
      lo <- input$price[1]; hi <- input$price[2]
      p  <- suppressWarnings(as.numeric(df$price_amount))
      keep <- !is.na(p) & p >= lo & p <= hi
      df <- df[keep | is.na(p), , drop = FALSE]  # garde aussi lignes sans prix

      # recherche texte
      q <- trimws(input$q %||% "")
      if (nzchar(q)) {
        pat <- tolower(q)
        title_l <- tolower(df$title %||% "")
        url_l   <- tolower(df$url %||% "")
        hit <- grepl(pat, title_l, fixed = TRUE) | grepl(pat, url_l, fixed = TRUE)
        df <- df[hit, , drop = FALSE]
      }

      df
    })

    # --- valeurs de synthèse
    output$v_n <- shiny::renderText({ nrow(filtered()) })
    output$v_last <- shiny::renderText({
      df <- filtered(); if (!nrow(df)) return("—")
      format(max(df$ts, na.rm = TRUE), "%Y-%m-%d %H:%M")
    })
    output$v_med <- shiny::renderText({
      df <- filtered(); if (!nrow(df)) return("—")
      p <- suppressWarnings(as.numeric(df$price_amount))
      if (!length(p) || all(is.na(p))) return("—")
      sprintf("%.2f EUR", stats::median(p, na.rm = TRUE))
    })

    # --- table
    output$tbl <- DT::renderDataTable({
      df <- filtered()
      if (!nrow(df)) return(DT::datatable(df))

      # colonnes affichées + formatage
      safe <- function(x) htmltools::htmlEscape(x %||% "")
      img <- ifelse(
        nzchar(df$image %||% ""),
        sprintf("<img src='%s' style='width:42px;height:42px;object-fit:cover;border-radius:6px;border:1px solid #eee'/>",
                htmltools::htmlEscape(ifelse(grepl('^//', df$image), paste0('https:', df$image), df$image))),
        ""
      )
      price <- ifelse(
        is.finite(suppressWarnings(as.numeric(df$price_amount))),
        sprintf("%.2f %s", as.numeric(df$price_amount), safe(df$price_currency)),
        "—"
      )
      url <- ifelse(
        nzchar(df$url %||% ""),
        sprintf("<a href='%s' target='_blank' rel='noopener'>Ouvrir</a>", htmltools::htmlEscape(df$url)),
        ""
      )

      out <- data.frame(
        Date = format(df$ts, "%Y-%m-%d %H:%M"),
        Image = img,
        Titre = safe(df$title),
        Prix = price,
        URL = url,
        Source = safe(df$source),
        stringsAsFactors = FALSE
      )

      DT::datatable(
        out,
        escape = FALSE,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          autoWidth  = TRUE,
          order = list(list(0, "desc"))
        )
      )
    })

    # --- export CSV
    output$dl_csv <- shiny::downloadHandler(
      filename = function(){
        paste0("historique-", format(Sys.Date(), "%Y-%m-%d"), ".csv")
      },
      content = function(file){
        df <- filtered()
        utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
  })
}

# petit helper interne
`%||%` <- function(x, y) if (is.null(x)) y else x
