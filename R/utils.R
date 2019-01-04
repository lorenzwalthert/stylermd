unwhich <- function(x, n) {
  out <- rep(FALSE, n)
  out[x] <- TRUE
  out
}

odd <- function(x) {
  x[odd_index(x)]
}

odd_index <- function(x) {
  if (length(x) < 1) return(x)
  seq(1L, length(x), by = 2)
}

even <- function(x) {
  if (length(x) < 2) return(x)
  x[even_index(x)]
}

even_index <- function(x) {
  seq(2L, length(x), by = 2)
}

#' An example text
#'
#' Lorem ipsum and so on. You know it.
#'
#' @export
lorem_ipsum <- function() {
  c("Lorem ipsum dolor sit amet, consetetur sadipscing elitr, sed diam nonumy eirmod",
    "tempor invidunt ut labore et dolore magna aliquyam erat, sed diam voluptua. At",
    "vero eos et accusam et justo duo dolores et ea rebum. Stet clita kasd gubergren,",
    "no sea takimata sanctus est Lorem ipsum dolor sit amet. Lorem ipsum dolor sit",
    "amet, consetetur sadipscing elitr, sed diam nonumy eirmod tempor invidunt ut",
    "labore et dolore magna aliquyam erat, sed diam voluptua. At vero eos et accusam",
    "et justo duo dolores et ea rebum. Stet clita kasd gubergren, no sea takimata",
    "sanctus est Lorem ipsum dolor sit amet.")
}
