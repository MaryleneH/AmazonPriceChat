#' llm_healthcheck
#'
#' @description
#' Fonction pour vérifier si bien connecté au LLM
#'
#' @returns
#' @export
#' @import httr2
#'
#' @examples
llm_healthcheck <- function() {
  base <- .llm_base()
  key  <- .llm_key()

  # Chemins possibles selon les déploiements (vLLM, proxy, variante /openai/v1)
  candidates <- unique(c(
    .llm_endpoint("models"),
    paste0(sub("/+$","", base), "/v1/models"),
    paste0(sub("/+$","", base), "/openai/v1/models"),
    paste0(sub("/+$","", base), "/api/v1/models"),
    paste0(sub("/+$","", base), "/models")
  ))

  results <- lapply(candidates, function(u){
    ok <- TRUE; msg <- NULL; ct <- NULL; status <- NA_integer_; parsed <- NULL
    tryCatch({
      resp   <- .llm_req(u) |> httr2::req_perform()
      status <- httr2::resp_status(resp)
      ct     <- httr2::resp_content_type(resp)
      if (!grepl("json", tolower(ct %||% ""), fixed = TRUE)) {
        ok  <- FALSE
        msg <- sprintf("Non-JSON (%s). Souvent une page HTML (login/404).", ct)
      } else {
        parsed <- httr2::resp_body_json(resp)
      }
    }, error = function(e){
      ok  <- FALSE
      msg <- conditionMessage(e)
    })
    list(url=u, status=status, content_type=ct, ok=ok, parsed=parsed, note=msg)
  })

  # Choisit le premier OK avec JSON, sinon renvoie tout le diagnostic
  good <- Filter(function(x) isTRUE(x$ok) && !is.null(x$parsed), results)
  if (length(good)) {
    return(invisible(good[[1]]))
  } else {
    structure(results, class="llm_healthcheck_failed")
  }
}
