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
#' @import zeallot
split_text_into_paragraphs <- function(text, header = NULL) {
  c(header, body) %<-% split_text_into_heder_and_paragraph(text, header)
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

split_text_into_heder_and_paragraph <- function(text, header = NULL) {
  is_dash <- "---" == text
  if (is_dash[1]) {
    stop <- which(c(FALSE, is_dash[-1]))[1]
    header <- list(`0` = construct_paragraph(c(text[seq2(1L, stop)], ""), "header"))
    text <- text[-seq2(1L, stop)]
  }
  list(header = header, body = text)
}

bullet_keys_collapsed <- function(trailing = "") {
  collapsed_keys <- glue_collapse(paste0("\\", bullet_keys()), sep = "|")
  glue("^{trailing}({collapsed_keys})")
}

#' @importFrom rlang seq2
lag <- function (x, n = 1L, default = NA) {
  c(rep(default, n), x[seq2(1L, length(x) - n)])
}

#' @importFrom rlang seq2
lead <- function(x, n = 1L, default = NA) {
  c(x[seq2(n + 1, length(x))], rep(default, n))
}

determine_class <- function(text) {
  class <- map_chr(text, determine_class_one)
  fix_class_for_code(class)
}

#' @importFrom purrr map2 flatten_int
#' @importFrom rlang seq2
fix_class_for_code <- function(class) {
  start <- which(class == "code_start")
  stop <- which(class == "code_stop")
  idx_code <- map2(start, stop, seq2) %>% flatten_int()
  class[idx_code] <- "code"
  class
}

#' Determine the class of text chunk
#'
#' @param text Text to process.
#' @importFrom purrr when
#' @keywords internal
determine_class_one <- function(text) {
  when(text,
       substr(.[1], 1, 3) == "```" ~ "code",
       substr(.[1], 1, 2) == "$$" ~ "code",
       grepl("^[0-9]+\\.\\s+", .[1], perl = TRUE) ~ "enumeration 1",
       grepl("^\\s\\s+[0-9]+\\.\\s+", .[1]) ~ "enumeration 2",
       grepl(bullet_keys_collapsed(), .[1]) ~ "bullet 1",
       grepl(bullet_keys_collapsed("  +"), .[1]) ~ "bullet 2",

       substr(.[1], 1, 1) == "#" ~ "title",
       "ordinary text"
  )
}

bullet_keys <- function() {
  c("* ", "+ ", "- ")
}


tidy_listing <- function(bullet, spaces = 2, spaces_leading) {
  bullet <- trimws(bullet)
  c(
    paste0(paste0(rep(" ", max(0, spaces_leading)), collapse = ""), bullet[1]),
    paste0(paste0(rep(" ", spaces), collapse = ""), bullet[-1])
  )
}


#' Tidy up a paragraph
#'
#' After text has been split into pargraphs with
#' [split_text_into_paragraphs()], this is the workhorse for actually formatting
#' the text.
#' @param paragraph A paragraph to tidy.
#' @inheritParams cut_long
#' @keywords internal
tidy_paragraph <- function(paragraph, width) {
  text_without_blank <- paragraph$text[trimws(paragraph$text) != ""]
  class <- paragraph$class
  class_without_level <- drop_level_of_class(class)
  class_without_level_after <- drop_level_of_class(paragraph$class_after)
  if (class %in% c("header", "code", "title")) {
    return(paragraph)
  } else {
    if (length(text_without_blank) < 1L) return(character(0))
    out <- tidy_lines(text_without_blank, width = width)
  }

  if (class_without_level %in% c("bullet","enumeration")) {
    paragraphs <- split(out, cumsum(substr(out, 1, 1) %in% bullet_keys()))
    spaces <- determine_trailing_spaces(class)
    spaces_leading <- ifelse(class_without_level == "bullet", spaces - 2, spaces - 3)
    out <- map(paragraphs, tidy_listing, spaces = spaces, spaces_leading = spaces_leading) %>%
      flatten_chr()
  }
    out %>%
      ensure_empty_trailing_line() %>%
      construct_paragraph(paragraph$class)
}


ensure_empty_trailing_line <- function(x) {
  is_empty <- trimws(x, which = "both") == ""
  c(x[!is_empty], "")
}

determine_trailing_spaces <- function(class) {
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
