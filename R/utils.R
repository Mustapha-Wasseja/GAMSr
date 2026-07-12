`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

is_string <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x)
}

as_list <- function(x) {
  if (is.null(x)) {
    list()
  } else if (is.list(x)) {
    x
  } else {
    list(x)
  }
}
