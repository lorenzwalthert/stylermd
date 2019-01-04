#' Cut a string to a certain width
#' @param text The text to cut.
#' @param width The maximal lenght of a line. If `length(width) > 1`, the
#'   elements are used sequentially, one for each iteration. If there are more
#'   iterations than values of `width`, the last value of `width` is used for
#'   all remaining iterations.
#' @param max_iter,current_iter The number of maximal / current iterations.
#'   For example, two iterations will result in two tidied lines and one line
#'   containing the remaining text.
#' @keywords internal
cut_long <- function(text, width, max_iter = Inf, current_iter = 1) {
  if (length(width) > 1) {
    width_postponed <- width[-1]
    width <- width[1]
  } else {
    width_postponed <- width
  }
  if (current_iter > max_iter | nchar(text) < width) return(text)

  blanks <- str_locate_all(text, " ")[[1]][, 1]
  if (all(is.na(blanks))) return(text)
  optimal_width <- cutting_points(blanks, width + 1L)
  we_do <- str_sub(text, 1, optimal_width)
  postpone <- str_sub(text, optimal_width + 1, -1)
  c(
    we_do,
    cut_long(postpone,
      width = width_postponed,
      max_iter = max_iter, current_iter = current_iter + 1L
    )
  ) %>%
    unlist() %>%
    paste0(collapse = "\n") %>%
    str_split_fixed("\n", n = Inf) %>%
    as.vector()
}

#' Find cutting points
#' @keywords internal
cutting_points <- function(x, width) {
  above <- (x > width)[1]
  below <- last(which(x <= width))
  if (is.na(below)) return(x[1])
  x[below]
}

last <- function(x) {
  if (length(x) < 1L) return(NA)
  x[length(x)]
}

split_pos <- function(string, pos) {
  if (length(pos) < 1) return(string)
  first <- str_sub(string, 1, pos) # incl withespace
  if (nchar(string) <= pos) return(first)
  second <- str_sub(string, pos + 1, -1)
  c(first, second)
}
