#' Extract the requested max number of items from French user text
#'
#' @param text Character scalar with the user request.
#' @param default Integer default if nothing is specified (default: 10).
#' @param hard_cap Integer hard maximum allowed (default: 20).
#' @return Integer(1) number of items to return.
#' @export
extract_n_results <- function(text, default = 10L, hard_cap = 20L){
  if (is.null(text) || !nzchar(text)) return(as.integer(default))
  s <- tolower(trimws(text))

  # 1) nombres explicites (1..99)
  m <- regmatches(s, gregexpr("\\b([0-9]{1,2})\\b", s, perl = TRUE))[[1]]
  if (length(m)) {
    n <- suppressWarnings(as.integer(m[1]))
    if (!is.na(n)) return(max(1L, min(n, as.integer(hard_cap))))
  }

  # 2) nombres FR en lettres (jusqu'à 20) + "quelques"
  fr_map <- c(
    "un"=1,"une"=1,"deux"=2,"trois"=3,"quatre"=4,"cinq"=5,
    "six"=6,"sept"=7,"huit"=8,"neuf"=9,
    "dix"=10,"onze"=11,"douze"=12,"treize"=13,"quatorze"=14,
    "quinze"=15,"seize"=16,"dix-sept"=17,"dix sept"=17,
    "dix-huit"=18,"dix huit"=18,"dix-neuf"=19,"dix neuf"=19,
    "vingt"=20,
    "quelques"=5,"quelque"=5
  )
  for (kw in names(fr_map)) {
    if (grepl(paste0("\\b", kw, "\\b"), s, perl = TRUE)) {
      n <- as.integer(fr_map[[kw]])
      return(max(1L, min(n, as.integer(hard_cap))))
    }
  }

  as.integer(default)
}
