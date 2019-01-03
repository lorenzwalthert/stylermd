#' Tidy up a paragraph
#'
#' After text has been split into pargraphs with
#' [split_text_into_paragraphs()], this is the workhorse for actually formatting
#' the text.
#' @param paragraph A paragraph to tidy.
#' @param text_width The width of the text alone, not the same as the total
#'   line-width for lines containing bullet points ect.
#' @inheritParams cut_long
#' @keywords internal
tidy_paragraph <- function(paragraph, text_width) {
  text_without_blank <- paragraph$text[trimws(paragraph$text) != ""]
  if (length(text_without_blank) < 1L) return(character(0))
  if (paragraph$class %in% c("header", "code", "title")) {
    return(paragraph)
  } else {
    out <- tidy_lines(text_without_blank, width = text_width)
  }

  if (paragraph$class %in% c("bullet","enumeration")) {
    paragraphs <- split(out, cumsum(substr(out, 1, 1) %in% bullet_keys()))
    spaces_first <- paragraph$indent
    spaces_not_first <- ifelse(paragraph$class == "bullet", spaces_first + 2, spaces_first + 3)
    out <- map(paragraphs, tidy_listing, spaces_first = spaces_first, spaces_not_first = spaces_not_first) %>%
      flatten_chr()
  }
    out %>%
      ensure_empty_trailing_line() %>%
      construct_paragraph(paragraph$class)
}

#' Indents the first line of a listing by `spaces_first` and the remaining
#' lines by `spaces_not_first`.
tidy_listing <- function(bullet, spaces_first, spaces_not_first = 2) {
  bullet <- trimws(bullet)
  c(
    paste0(paste0(rep(" ", max(0, spaces_first)), collapse = ""), bullet[1]),
    paste0(paste0(rep(" ", spaces_not_first), collapse = ""), bullet[-1])
  )
}

ensure_empty_trailing_line <- function(x) {
  is_empty <- trimws(x, which = "both") == ""
  c(x[!is_empty], "")
}

determine_leading_spaces <- function(class) {
  when(class,
    . == "bullet 1" ~ 2,
    . == "bullet 2" ~ 4,
    . == "enumeration 1" ~ 3,
    . == "enumeration 2" ~ 5,
    ~ 2
  )
}


#' Drops hierarchival level of class
#'
#' E.g. enumeration 1 -> enumeration, but level -> level
drop_level_of_class <- function(class) {
  gsub("\\s*[0-9]+$", "", class)
}

#' Tidy lines
#'
#' Tidyies mutliple line of code.
#' @param add_only If we should only add line breaks to lines that are too long
#'   or if line breaks should also be removed.
#' @param text Text to tidy.
#' @inheritParams cut_long
#' @keywords internal
tidy_lines <- function(text, width, add_only = FALSE) {
  if (!add_only) {
    text <- paste(unlist(strsplit(text, "\n")), collapse = " ")
  } else {
  }
  map(text, tidy_line, width) %>%
    unlist()
}

#' Tidy one line
#'
#' Necessary to be called with `purrr::map(..., tidy_lines)` only if `add_only`
#' was set to `TRUE` because then we have multiple text lines to tidy.
#' @param text Text to prettify.
#' @inheritParams cut_long
#' @keywords internal
tidy_line <- function(text, width) {
  text %>%
    str_replace_all(" +", " ") %>%
    str_replace_all("^ ", "") %>%
    cut_long(width) %>%
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
