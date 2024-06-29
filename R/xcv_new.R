#' Split a string by a given character
#'
#' @param x     A character vector
#' @param pattern A regular expression, as used in the stringr package. The default is ",".
#'          The pattern is used to describe where to split x.
#' @param n This determines the maximum length of each element of the output.
#'
#' @return A character vector
#' @export
#'
#' @examples
#' xcv_new("a,b,c", ",")
xcv_new <- function(x, pattern, n = Inf) {
  stopifnot(is.character(x), length(x) <= 1L)
  if (length(x) == 1L) {
    stringr::str_split(string = x, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}
