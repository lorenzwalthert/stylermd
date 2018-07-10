#' Tidy a file
#'
#' Tidies a Markdown file
#' @param path The path to the file.
#' @export
tidy_file <- function(path) {
  read_lines(path) %>%
    tidy_text() %>%
    write_lines(path)

}

#' Tidy text
#' @param text The text to tidy.
#' @importFrom purrr flatten_chr map
#' @export
tidy_text <- function(text) {
  split <- split_text_into_paragraphs(text)
  map(split, tidy_paragraph) %>%
    deconstruct_paragraph() %>%
    flatten_chr()
}

