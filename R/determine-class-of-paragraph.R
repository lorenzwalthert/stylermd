#' Determine the class of text chunk
#'
#' @param paragraph A paragraph, i.e. a character vector, as defined in the
#'   vignette "Data structures".
#' @importFrom purrr when
#' @keywords internal
determine_class_one <- function(paragraph) {
  is_code <- substr(paragraph[1], 1, 3) == "```"
  is_latex <- substr(paragraph[1], 1, 2) == "$$"
  if (is_code || is_latex) {
    return(new_class_and_indent("code"))
  }
  if (substr(paragraph[1], 1, 1) == "#") {
    return(new_class_and_indent("title"))
  }
  class_enumeration <- determine_class_enumeration(paragraph)
  if (!is.na(class_enumeration$class)) {
    return(class_enumeration)
  }
  class_bullet <- determine_class_bullet(paragraph)
  if (!is.na(class_bullet$class)) {
    return(class_bullet)
  }

  return(new_class_and_indent("ordinary text"))
}

#' Determine the class and indent of a paragraph
#'
#' @inheritParams determine_class_one
#' @param regex The regular expression that matches listing, e.g. bullet of
#'    enumeration. Must the indention must be captured by a *capturing group*,
#'    e.g. `"^(\\s*)(\\* |\\+ |\\- )"`.
#' @param class The class, if matched.
#' @keywords internal
template_determine_class <- function(paragraph,
                                     regex,
                                     class) {
  length_enumeration <- str_match(paragraph[1], regex)[, 2]
  if (!is.na(length_enumeration)) {
    new_class_and_indent(
      class,
      nchar(length_enumeration)
    )
  } else {
    new_class_and_indent()
  }
}

determine_class_bullet <- purrr::partial(
  template_determine_class,
  regex = "^(\\s*)(\\* |\\+ |\\- )",
  class = "bullet"
)

determine_class_enumeration <- purrr::partial(
  template_determine_class,
  regex = "^(\\s*)([0-9]+|[a-z])\\.\\s+",
  class = "enumeration"
)
