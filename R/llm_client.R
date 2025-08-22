.llm_base  <- function() Sys.getenv("LLM_BASE_URL", "http://127.0.0.1:8000")
.llm_model <- function() Sys.getenv("LLM_MODEL",    "mistral-small3.2:latest")
.llm_key   <- function() Sys.getenv("LLM_API_KEY",  "")
.llm_path  <- function() Sys.getenv("LLM_PATH",     "api/v1")   # ← on va mettre openai/v1

.llm_endpoint <- function(...){
  base <- sub("/+$", "", .llm_base())
  path <- sub("^/+", "", .llm_path())
  parts <- vapply(list(...), function(x) sub("^/+", "", x), character(1))
  paste(c(base, path, parts), collapse = "/")
}

.llm_req <- function(url){
  httr2::request(url) |>
    httr2::req_headers(
      "Accept" = "application/json",
      "Content-Type" = "application/json",
      Authorization = if (nzchar(.llm_key())) paste("Bearer", .llm_key()) else NULL
    )
}

llm_chat <- function(messages, tools = NULL, tool_choice = "auto", temperature = 0.2, timeout = 60){
  body <- list(model = .llm_model(), messages = messages, temperature = temperature)
  if (!is.null(tools)) { body$tools <- tools; body$tool_choice <- tool_choice }
  .llm_req(.llm_endpoint("chat/completions")) |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout) |>
    httr2::req_perform() |>
    httr2::resp_body_json() |>
    (\(resp){
      ch <- resp$choices[[1]]$message
      list(text = ch$content %||% "", tool_calls = ch$tool_calls %||% NULL, raw = resp)
    })()
}

llm_models <- function(){
  .llm_req(.llm_endpoint("models")) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

`%||%` <- function(x,y) if (is.null(x)) y else x
