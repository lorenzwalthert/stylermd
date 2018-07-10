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
    deconstruct_text() %>%
    flatten_chr()
}

#' @importFrom rlang seq2
#' @importFrom purrr map_chr pmap
split_text_into_paragraphs <- function(text, header = NULL) {
  is_dash <- "---" == text
  if (is_dash[1]) {
    stop <- which(c(FALSE, is_dash[-1]))[1]
    header <- list(`0` = list(text = c(text[seq2(1L, stop)], ""), class = "header"))
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

determine_class <- function(text) {
  ifelse(substr(text[1], 1, 3) == "```", "code", "ordinary text")
}



tidy_paragraph <- function(text) {
  if (text$class != "ordinary text") {
    return(text)
  }
  text_without_blank <- text$text[text$text != ""]
  if (length(text_without_blank) < 1L) return(character(0))
  paste(text_without_blank, collapse = " ") %>%
    str_replace_all(" +", " ") %>%
    cut_long %>%
    unlist() %>%
    map(trimws, "right") %>%
    paste(sep = "\n") %>%
    c("") %>%
    construct_text(text$class)

}


construct_text <- function(text, class = "ordinary text") {
  list(text = text, class = class)
}

deconstruct_text <- function(text) {
  map(text, "text")
}

is_not_to_format <- function(text) {
  substr(trimws(text[1]), 1, 3) == "```"
}

cutting_points <- function(x, length = 80) {
  above <- first(which(x >= length))
  below <- last(which(x < length))
  if (is.na(below)) return(length)
  x[below]
}

split_pos <- function(string, pos) {
  if (length(pos) < 1) return(string)
  first <-  str_sub(string, 1, pos) # incl withespace
  if (nchar(string) <= pos) return(first)
  second <- str_sub(string, pos + 1, -1)
  c(first, second)
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


cut_long <- function(text, length = 80) {
  if (nchar(text) < length) return(text)

  blanks <- str_locate_all(text, " ")[[1]][, 1]

  optimal_length <- cutting_points(blanks, length)
  we_do <- str_sub(text, 1, optimal_length)
  postpone <- str_sub(text, optimal_length + 1, - 1)
  c(we_do, cut_long(postpone, length)) %>%
      unlist() %>%
      paste0(collapse = "\n") %>%
    str_split_fixed("\n", n = Inf) %>%
    as.vector()
}
