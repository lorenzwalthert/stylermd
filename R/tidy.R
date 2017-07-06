#' tidy current rmd file
#' @import tidyverse
#' @import rstudioapi
#' @import stringr
#' @import purrr
tidy_active <- function() {
  file <- rstudioapi::getActiveDocumentContext()$path
  tidy_file(file)
}

tidy_file <- function(path = "README.Rmd") {
  one_string <- read_lines(path)
  blanks <- str_locate_all(one_string, " ") %>%
    map(~.x[,1]) %>%
    map(cutting_points)
  newlines <- one_string %>%
    map2(blanks, split_pos) %>%
    unlist() %>%
    map(trimws, "right") %>%
    paste(sep = "\n") %>%
    write_lines(path)
}

cutting_points <- function(x) {
  above <- first(which(x >= 80))
  below <- last(which(x < 80))
  if (length(above) == 0 || is.na(above)) return(80)
  x[below]
}

split_pos <- function(string, pos) {
  if (length(pos) < 1) return(string)
  first <-  str_sub(string, 1, pos) # incl withespace
  if (nchar(string) <= pos) return(first)
  second <- str_sub(string, pos + 1, -1)
  c(first, second)
}
