#' @keywords internal
#' @noRd

`%||%` <- function(x, y) if (is.null(x)) y else x

.render_price <- function(price){
  if (is.null(price) || is.null(price$amount) || is.na(price$amount)) return("—")
  cur <- price$currency %||% ""
  sprintf("%.2f %s", as.numeric(price$amount), cur)
}

.render_img <- function(img){
  if (is.null(img) || !nzchar(img)) return("")
  if (grepl("^//", img)) img <- paste0("https:", img)
  sprintf(
    "<img src='%s' style='width:64px;height:64px;object-fit:cover;border-radius:8px;border:1px solid #eee'/>",
    htmltools::htmlEscape(img)
  )
}

.render_card <- function(it){
  title <- htmltools::htmlEscape(it$title %||% "")
  url   <- htmltools::htmlEscape(it$url   %||% "#")
  img   <- .render_img(it$image %||% "")
  ptxt  <- .render_price(it$price %||% list(amount = NA_real_, currency = ""))

  sprintf("
  <div class='search-item' style='display:flex;gap:10px;align-items:center;margin:8px 0'>
    %s
    <div>
      <strong>%s</strong><br/>%s<br/>
      <a href='%s' target='_blank' rel='noopener'>Voir</a>
    </div>
  </div>", img, title, ptxt, url)
}

#' @keywords internal
#' @noRd
render_search <- function(res){
  items <- res$results %||% res$items %||% list()
  if (!length(items)) return("Aucun résultat.")
  paste0(vapply(items, .render_card, FUN.VALUE = character(1)), collapse = "")
}

#' @keywords internal
#' @noRd
render_items <- function(res){
  items <- res$items %||% res$results %||% list()
  if (!length(items)) return("Aucun article.")
  paste0(vapply(items, .render_card, FUN.VALUE = character(1)), collapse = "")
}
