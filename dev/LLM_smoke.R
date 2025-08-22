llm_smoke <- function(model = Sys.getenv("LLM_MODEL")) {
  msg <- list(
    list(role="system", content="Réponds par 'OK' si tu me reçois."),
    list(role="user",   content=sprintf("Quel est le modèle ? (%s)", model))
  )
  out <- tryCatch(llm_chat(msg), error=function(e) e)
  print(out)
}
llm_smoke()
