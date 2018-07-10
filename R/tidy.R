#' tidy current rmd file to fit 80 characters a line
#' @import dplyr
#' @import rstudioapi
#' @import stringr
#' @import readr
#' @importFrom purrr map map2
tidy_active_file <- function() {
  file <- rstudioapi::getActiveDocumentContext()$path
  tidy_file(file)
}

tidy_file <- function(path = "README.Rmd") {
  read_lines(path) %>%
    tidy_text() %>%
    write_lines(path)

}

#' @importFrom purrr flatten_chr
tidy_text <- function(text) {
  split <- split_text_into_paragraphs(text)
  map(split, tidy_paragraph) %>%
    deconstruct_paragraph() %>%
    flatten_chr()
}

#' Split text into paragraphs
#'
#' Splits text into a list of elements, one corresponding to a paragraph,
#' for further processing. Each element
#' contains the text an the class of the chunks.
#' @param text Text to process.
#' @param header The header, in case there is one and it should be overwritten.
#' @importFrom rlang seq2
#' @importFrom purrr map_chr pmap
split_text_into_paragraphs <- function(text, header = NULL) {
  is_dash <- "---" == text
  if (is_dash[1]) {
    stop <- which(c(FALSE, is_dash[-1]))[1]
    header <- list(`0` = construct_paragraph(c(text[seq2(1L, stop)], ""), "header"))
    text <- text[-seq2(1L, stop)]
  }
  non_header <- split(text,
    cumsum(lag(as.integer(grepl("^\\s*$", text)), default = 0)) + 1L
  )
  non_header

  non_header_attrs <- map_chr(non_header, determine_class)
  non_header_lst <- pmap(list(non_header, non_header_attrs), ~list(text = ..1, class = ..2))
  append(non_header_lst, header, 0)
}

#' Determine the class of text chunk
#'
#' @param text Text to process.
#' @importFrom purrr when
determine_class <- function(text) {
  when(text,
    substr(.[1], 1, 3) == "```" ~ "code",
    substr(.[1], 1, 1) %in% enumeration_keys() ~ "enumeration",
    "ordinary text"
  )
}

enumeration_keys <- function() {
  c("*", "+", "-")
}


tidy_enumeration <- function(enumeration) {
  enumeration <- trimws(enumeration)
  if (length(enumeration) < 2L) return(enumeration)
  c(enumeration[1], paste0("  ", enumeration[-1], sep = ""))
}


#' Tidy up a paragraph
#'
#' After text has been split into pargraphs with
#' [split_text_into_paragraphs()], this is the workhorse for actually formatting
#' the text.
#' @param A paragraph to tidy.
tidy_paragraph <- function(paragraph) {
  text_without_blank <- paragraph$text[trimws(paragraph$text) != ""]
  class <- paragraph$class
  if (class %in% c("header", "code")) {
    return(paragraph)
  } else {
    if (length(text_without_blank) < 1L) return(character(0))
    out <- tidy_lines(text_without_blank, add_only = (class == "code"))
  }
  if (class %in% "enumeration") {
    paragraphs <- split(out, cumsum(substr(out, 1, 1) %in% enumeration_keys()))
    out <- map(paragraphs, tidy_enumeration) %>%
      flatten_chr()
  }
  out %>%
    paste(sep = "\n") %>%
    c("") %>%
    construct_paragraph(paragraph$class)
}


#' Tidy lines
#'
#' Tidyies mutliple line of code.
#' @param add_only If we should only add line breaks to lines that are too long
#'   or if line breaks should also be removed.
tidy_lines <- function(text, add_only = FALSE) {
  if (add_only) {
    text <- paste(text, collapse = " ")
  }
  map(text, tidy_line) %>%
    unlist()
}

#' Tidy one line
#'
#' Necessary to be called with `purrr::map(..., tidy_lines)` only if `add_only`
#' was set to `TRUE` because then we have multiple text lines to tidy.
#' @param text Text to prettify.
tidy_line <- function(text) {
  text %>%
    str_replace_all(" +", " ") %>%
    cut_long() %>%
    unlist() %>%
    map(trimws, "right")
}

#' Tools for manipulating paragraphs
#'
#' A paragraph is a list with text and other elements.
#' @param text The text of the paragraph.
#' @param class The class of the paragraph.
#' @name paragraph
NULL

#' @describeIn paragraph Constructs a paragraph.
construct_paragraph <- function(text, class = "ordinary text") {
  list(text = text, class = class)
}

#' @describeIn paragraph Exracts an element from a paragraph.
deconstruct_paragraph <- function(text, element = "text") {
  map(text, element)
}
#' Tidy highlighted region
tidy_active_region <- function() {
  file <- rstudioapi::getActiveDocumentContext()$path
  all <- read_lines(file)
  selection <- rstudioapi::getActiveDocumentContext()$selection
  start_row <- selection[[1]]$range$start[1]
  end_row <- selection[[1]]$range$end[1]

  pre <- all[seq_len(start_row - 1)]
  post <- if (end_row %in% c(length(all), length(all) + 1)) {
    post <- NULL
  } else {
    post <- all[(end_row + 1):length(all)]
  }
  process <- all[start_row:end_row] %>%
    tidy_text()
  write_lines(c(pre, process, post), file)

}
