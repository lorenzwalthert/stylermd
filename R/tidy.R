#' Split text into paragraphs
#'
#' Splits text into a list of elements, one corresponding to a paragraph,
#' for further processing. Each element
#' contains the text an the class of the chunks.
#' @param text Text to process.
#' @param header The header, in case there is one and it should be overwritten.
#' @importFrom rlang seq2
#' @importFrom purrr map_chr pmap
#' @keywords internal
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

#' @importFrom rlang seq2
lag <- function (x, n = 1L, default = NA) {
  c(rep(default, n), x[seq2(1L, length(x) - n)])
}

#' Determine the class of text chunk
#'
#' @param text Text to process.
#' @importFrom purrr when
#' @keywords internal
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
#' @param paragraph A paragraph to tidy.
#' @keywords internal
tidy_paragraph <- function(paragraph) {
  text_without_blank <- paragraph$text[trimws(paragraph$text) != ""]
  class <- paragraph$class
  if (class %in% c("header", "code")) {
    return(paragraph)
  } else {
    if (length(text_without_blank) < 1L) return(character(0))
    out <- tidy_lines(text_without_blank, add_only = (class == "enumeration"))
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
#' @param text Text to tidy.
#' @keywords internal
tidy_lines <- function(text, add_only = FALSE) {
  if (!add_only) {
    text <- paste(unlist(strsplit(text, "\n")), collapse = " ")
  } else {
  }
  map(text, tidy_line) %>%
    unlist()
}

#' Tidy one line
#'
#' Necessary to be called with `purrr::map(..., tidy_lines)` only if `add_only`
#' was set to `TRUE` because then we have multiple text lines to tidy.
#' @param text Text to prettify.
#' @keywords internal
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
#' @param element Which element to select.
#' @name paragraph
#' @keywords internal
NULL

#' @describeIn paragraph Constructs a paragraph.
construct_paragraph <- function(text, class = "ordinary text") {
  list(text = text, class = class)
}

#' @describeIn paragraph Exracts an element from a paragraph.
deconstruct_paragraph <- function(text, element = "text") {
  map(text, element)
}
