#' @importFrom rlang seq2
lag <- function (x, n = 1L, default = NA) {
  c(rep(default, n), x[seq2(1L, length(x) - n)])
}

#' @importFrom rlang seq2
lead <- function(x, n = 1L, default = NA) {
  c(x[seq2(n + 1, length(x))], rep(default, n))
}
