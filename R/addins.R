memtrack_selection <- function() {
  context <- rstudioapi::getActiveDocumentContext()
  text <- context$selection[[1]]$text

  browser()

  # if (all(nchar(text) == 0)) stop("No code selected")
  # out <- style_text(text)
  # rstudioapi::modifyRange(
  #   context$selection[[1]]$range, paste0(out, collapse = "\n"),
  #   id = context$id
  # )
  # if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
  #   rstudioapi::documentSave(context$id)
  # }
}