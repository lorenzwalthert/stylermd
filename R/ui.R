#' Tidy a file
#'
#' Tidies a markdown file, called for it's side effect.
#'
#' @return
#' If the file has been changed (ignoring the class attribute), `TRUE` is
#' returned, `NA` if an error occured and `FALSE` if it has not been changed.
#' @param path The path to the file.
#' @inheritParams cut_long
#' @export
#' @import glue
tidy_file <- function(path, width = getOption("stylermd.line_width")) {
  tryCatch({
    content_in <- readLines(path)
    content_out <- content_in %>%
    tidy_text(width) %>%
    writeLines(path)
    !identical(unclass(content_in), unclass(content_out))
  }, error = function(e) NA
  )
}

#' Tidy text
#'
#' Tidies text.
#' @param text The text to tidy.
#' @inheritParams cut_long
#' @importFrom purrr flatten_chr map2
#' @export
tidy_text <- function(text, width = getOption("stylermd.line_width")) {
  split <- split_text_into_paragraphs(text)
  text_width <- determine_text_with_from_paragraphs(split, target_width = width)
  map2(split, text_width, tidy_paragraph) %>%
    deconstruct_paragraph() %>%
    flatten_chr()
}
