#' Ordered-set position and cardinality
#'
#' `gams_ord()` returns the one-based position of a set element in GAMS's
#' internal order. `gams_card()` returns the number of elements in a set.
#'
#' @param set A set created by [gams_set()] or [gams_alias()].
#'
#' @return A symbolic GAMSr expression.
#' @name set_functions
NULL

#' @rdname set_functions
#' @export
gams_ord <- function(set) {
  validate_set_function_input(set)
  new_set_function("ord", set, free_indices = set$name)
}

#' @rdname set_functions
#' @export
gams_card <- function(set) {
  validate_set_function_input(set)
  new_set_function("card", set)
}

validate_set_function_input <- function(set, call = rlang::caller_env()) {
  if (!is_gams_index_set(set)) {
    gamsr_abort(
      "`set` must be a GAMSr set or alias object.",
      class = "gamsr_error_invalid_domain",
      call = call
    )
  }
  invisible(set)
}
