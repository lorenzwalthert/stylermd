#' Tidy current rmd file to fit 80 characters a line
#'
#' @section Auto-Save Option:
#' By default, both of the RStudio Addins will apply styling to the (selected)
#' file contents without saving changes. Automatic saving can be enabled by
#' setting the environment variable `save_after_styling` to `TRUE`.
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
  rstudioapi::setCursorPosition(context$selection[[1]]$range)
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

