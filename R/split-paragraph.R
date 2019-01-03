#' Split text into paragraphs
#'
#' Splits text into a list of elements, one corresponding to a paragraph,
#' for further processing. Each element
#' contains the text an the class of the chunks.
#' @inheritParams split_text_into_header_and_paragraph
#' @importFrom rlang seq2
#' @importFrom purrr map_chr pmap
#' @keywords internal
#' @import zeallot
split_text_into_paragraphs <- function(text, header = NULL) {
  c(header, body) %<-% split_text_into_header_and_paragraph(text, header)
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
      non_header, non_header_attrs, lead(non_header_attrs, default = list(new_class_and_indent()))
    ),
    ~list(
      text = ..1, class = ..2[["class"]], class_after = ..3[["class"]],
      indent = ..2[["indent"]]
    )
  )

  append(non_header_lst, header, 0)
}

#' Find the start / stop of code lines
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

#' @param text Text to process.
#' @param header The header, in case there is one and it should be overwritten.
split_text_into_header_and_paragraph <- function(text, header = NULL) {
  is_dash <- "---" == text
  if (is_dash[1]) {
    stop <- which(c(FALSE, is_dash[-1]))[1]
    header <- list(`0` = construct_paragraph(c(text[seq2(1L, stop)], ""), "header"))
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

#' Determine the class of text chunk
#'
#' @param paragraph A paragraph, i.e. a character vector.
#' @importFrom purrr when
#' @keywords internal
determine_class_one <- function(paragraph) {
  if (substr(paragraph[1], 1, 3) == "```" || substr(paragraph[1], 1, 2) == "$$") {
    return(new_class_and_indent("code"))
  }
  if (substr(paragraph[1], 1, 1) == "#") {
    return(new_class_and_indent("title"))
  }
  class_enumeration <- determine_class_enumeration(paragraph)
  if (!is.na(class_enumeration$class)) {
    return(class_enumeration)
  }
  class_bullet <- determine_class_bullet(paragraph)
  if (!is.na(class_bullet$class)) {
    return(class_bullet)
  }

  return(new_class_and_indent("ordinary text"))
}

#' Helper function to construct a list with elements class and indent that
#' has sensible defaults.
new_class_and_indent <- function(class = NA, indent = NA) {
  list(class = class, indent = indent)
}

#' @param t_nchar_to_indent The bare name of a function that transforms the
#'   number of leading spaces of the first line of the paragraph into an
#'   indention level, i.e. a mapping from number of characters to the indent.
#' @param class The class, if matched.
template_determine_class <- function(paragraph,
                                     regex,
                                     class) {
  length_enumeration <- str_match(paragraph[1], regex)[, 2]
  if (!is.na(length_enumeration)) {
    new_class_and_indent(
      class,
      nchar(length_enumeration)
    )
  } else {
    new_class_and_indent()
  }
}

determine_class_bullet <- purrr::partial(
  template_determine_class, regex = "^(\\s*)(\\* |\\+ |\\- )",
  class = "bullet"
)

determine_class_enumeration <- purrr::partial(
  template_determine_class, regex = "^(\\s*)[0-9]+\\.\\s+",
  class = "enumeration"
)

determine_text_with_from_paragraphs <- function(paragraphs, target_width) {
  classes <- map_chr(paragraphs, ~.x$class)
  purrr::when(classes,
              . == "bullet" ~ target_width - 2,
              . == "enumeration" ~ target_width - 3,
              target_width
  )
}
