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
      non_header, non_header_attrs, lead(non_header_attrs)
    ),
    ~list(text = ..1, class = ..2, class_after = ..3)
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

determine_class <- function(text) {
  map_chr(text, determine_class_one)
}

#' Determine the class of text chunk
#'
#' @param text Text to process.
#' @importFrom purrr when
#' @keywords internal
determine_class_one <- function(text) {
  class <- when(text,
       substr(.[1], 1, 3) == "```" ~ "code",
       substr(.[1], 1, 2) == "$$" ~ "code",
       substr(.[1], 1, 1) == "#" ~ "title",
       grepl("^[0-9]+\\.\\s+", .[1], perl = TRUE) ~ "enumeration 1",
       grepl("^\\s\\s+[0-9]+\\.\\s+", .[1]) ~ "enumeration 2",
       grepl(bullet_keys_collapsed(), .[1]) ~ "bullet 1",
       grepl(bullet_keys_collapsed("  +"), .[1]) ~ "bullet 2",
       "ordinary text"
  )
  fix_class_for_code(class)
}

#' @importFrom purrr map2 flatten_int
#' @importFrom rlang seq2
fix_class_for_code <- function(class) {
  start <- which(class == "code_start")
  stop <- which(class == "code_stop")
  if (length(start) < 1L | length(stop) < 1L) return(class)
  idx_code <- seq2(start, stop)
  class[idx_code] <- "code"
  class
}


determine_class_enumeration <- function(text) {

}
#' * determine class (bullet or enumeration)
#' * determine depth

determine_text_with_from_paragraphs <- function(paragraphs, target_width) {
  classes <- map_chr(paragraphs, ~.x$class)
  purrr::when(classes,
              . == "bullet" ~ target_width - 2,
              . == "enumeration" ~ target_width - 3,
              target_width
  )
}
