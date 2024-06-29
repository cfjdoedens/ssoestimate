xcv_new <- function(x, pattern, n = Inf) {
  stopifnot(is.character(x), length(x) <= 1L)
  if (length(x) == 1L) {
    stringr::str_split(string = x, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}
