#' Split text into paragraphs
#'
#' Splits text into a list of elements, one corresponding to a paragraph,
#' for further processing. Each element
#' contains the text an the class of the chunks.
#' @inheritParams split_text_into_header_and_body
#' @importFrom rlang seq2
#' @importFrom purrr map_chr pmap flatten_int
#' @keywords internal
#' @import zeallot
split_text_into_paragraphs <- function(text, header = NULL) {
  c(header, body) %<-% split_text_into_header_and_body(text, header)
  c(code_start, is_code_start, code_stop) %<-% find_code_boundaries(body)
  is_code <- map2(code_start, code_stop, seq2) %>%
    flatten_int() %>%
    unwhich(length(body))
  trimmed_body <- trimws(body, which = "both")
  regex <- paste0(bullet_keys_collapsed(), "|[0-9]+\\.", sep = "")
  is_enumeration <- grepl(regex, trimmed_body)

  has_line_break_afterwards <- grepl("^\\s*$", lag(trimmed_body))

  is_split_point <- (has_line_break_afterwards | is_enumeration) &
    (!is_code) |
    is_code_start

  block <- cumsum(as.integer(is_split_point)) + 1L

  non_header <- split(body, block)
  non_header_attrs <- determine_class(non_header)
  non_header_lst <- pmap(
    list(
      non_header, non_header_attrs,
      lead(non_header_attrs, default = list(new_class_and_indent()))
    ),
    ~list(
      text = ..1, class = ..2[["class"]], class_after = ..3[["class"]],
      indent = ..2[["indent"]]
    )
  )

  append(non_header_lst, header, 0)
}

#' Find the start / stop of code lines
#'
#' @param body The body of a document, i.e. plain text.
find_code_boundaries <- function(body) {
  is_code_boundary <-
    (substr(body, 1, 3) ==  c("```")) |
    (substr(body, 1, 2) == "$$")
  code_boundary <- which(is_code_boundary)
  code_start <- odd(code_boundary)
  list(
    code_start = code_start,
    is_code_start = unwhich(code_start, n = length(body)),
    code_stop = even(code_boundary)
  )
}

#' Split text into header and body
#' @param text Text to process.
#' @param header The header, in case there is one and it should be overwritten.
split_text_into_header_and_body <- function(text, header = NULL) {
  is_dash <- "---" == text
  if (is_dash[1]) {
    stop <- which(c(FALSE, is_dash[-1]))[1]
    header <- list(
      `0` = construct_paragraph(c(text[seq2(1L, stop)], ""), "header")
    )
    text <- text[-seq2(1L, stop)]
  }
  list(header = header, body = text)
}

bullet_keys <- function() {
  c("* ", "+ ", "- ")
}

bullet_keys_collapsed <- function(trailing = "") {
  collapsed_keys <- glue_collapse(paste0("\\", bullet_keys()), sep = "|")
  glue("^{trailing}({collapsed_keys})")
}

#' Determine the class of paragraphs
#'
#' @param paragraphs A list of non-header paragraphs.
#' @importFrom purrr map
determine_class <- function(paragraphs) {
  map(paragraphs, determine_class_one)
}


#' List constructor
#' Helper function to construct a list with elements class and indent that
#' has sensible defaults.
#' @param class,indent Elements of the list to construct.
#' @keywords internal
new_class_and_indent <- function(class = NA, indent = NA) {
  list(class = class, indent = indent)
}

determine_text_with_from_paragraphs <- function(paragraphs, target_width) {
  classes <- map_chr(paragraphs, ~.x$class)
  purrr::when(classes,
              . == "bullet" ~ target_width - 2,
              . == "enumeration" ~ target_width - 3,
              target_width
  )
}
