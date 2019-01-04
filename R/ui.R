#' Tidy a file
#'
#' Tidies a markdown file.
#' @param path The path to the file.
#' @inheritParams cut_long
#' @export
#' @import glue
tidy_file <- function(path, width = getOption("stylermd.line_width")) {
  readLines(path) %>%
    tidy_text(width) %>%
    writeLines(path)
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
