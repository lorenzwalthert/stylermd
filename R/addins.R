#' tidy current rmd file to fit 80 characters a line
#' @import rstudioapi
#' @import stringr
#' @importFrom purrr map map2
tidy_active_file <- function() {
  context <- get_rstudio_context()
  out <- tidy_text(context$content)
  rstudioapi::modifyRange(
    c(1, 1, length(context$contents) + 1, 1),
    paste0(append(out, ""), collapse = "\n"), id = context$id
  )
  may_save(context)
}

may_save <- function(context) {
  if (Sys.getenv("save_after_styling") == TRUE && context$path != "") {
    rstudioapi::documentSave(context$id)
  }
}


tidy_selection <- function() {
  context <- get_rstudio_context()
  text <- context$selection[[1]]$text
  if (all(nchar(text) == 0)) stop("Nothing selected")
  out <- tidy_text(text)
  rstudioapi::modifyRange(
    context$selection[[1]]$range, paste0(out, collapse = "\n"), id = context$id
  )
  may_save(context)
}


get_rstudio_context <- function() {
  rstudioapi::getActiveDocumentContext()
}

