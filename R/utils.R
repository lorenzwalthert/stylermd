unwhich <- function(x, n) {
  out <- rep(FALSE, n)
  out[x] <- TRUE
  out
}
