#' Tidy a file
#'
#' Tidies a Markdown file
#' @param path The path to the file.
#' @inheritParams cut_long
#' @export
#' @import glue
tidy_file <- function(path, width = 80) {
  readLines(path) %>%
    tidy_text(width) %>%
    writeLines(path)

}

#' Tidy text
#' @param text The text to tidy.
#' @inheritParams cut_long
#' @importFrom purrr flatten_chr map
#' @export
tidy_text <- function(text, width = 80) {
  split <- split_text_into_paragraphs(text)
  map(split, tidy_paragraph, width = width) %>%
    deconstruct_paragraph() %>%
    flatten_chr()
}

