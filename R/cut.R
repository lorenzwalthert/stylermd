#' Cut a string to a certain length
#' @param text The text to cut.
#' @param length The maximal lenght of a line.
#' @keywords internal
cut_long <- function(text, length = 80) {
  if (nchar(text) < length) return(text)

  blanks <- str_locate_all(text, " ")[[1]][, 1]
  if (all(is.na(blanks))) return(text)
  optimal_length <- cutting_points(blanks, length)
  we_do <- str_sub(text, 1, optimal_length)
  postpone <- str_sub(text, optimal_length + 1, - 1)
  c(we_do, cut_long(postpone, length)) %>%
    unlist() %>%
    paste0(collapse = "\n") %>%
    str_split_fixed("\n", n = Inf) %>%
    as.vector()
}

#' Find cutting points
#' @keywords internal
cutting_points <- function(x, length = 80) {
  above <- first(which(x >= length))
  below <- last(which(x < length))
  if (is.na(below)) return(x[1])
  x[below]
}

split_pos <- function(string, pos) {
  if (length(pos) < 1) return(string)
  first <-  str_sub(string, 1, pos) # incl withespace
  if (nchar(string) <= pos) return(first)
  second <- str_sub(string, pos + 1, -1)
  c(first, second)
}

