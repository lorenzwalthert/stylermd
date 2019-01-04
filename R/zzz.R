.onLoad <- function(libname, pkgname) {
  op <- options()
  op.stylermd <- list(stylermd.line_width = 80)

  toset <- !(names(op.stylermd) %in% names(op))
  if (any(toset)) options(op.stylermd[toset])
  invisible()
}
