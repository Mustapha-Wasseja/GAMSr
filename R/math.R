new_single_argument_math <- function(function_name, x) {
  new_math_function(function_name, list(x))
}

new_multi_argument_math <- function(function_name, ..., call = rlang::caller_env()) {
  arguments <- list(...)
  if (length(arguments) == 0L) {
    gamsr_abort(
      sprintf("`gams_%s()` requires at least one argument.", function_name),
      class = "gamsr_error_invalid_expression",
      call = call
    )
  }
  new_math_function(function_name, arguments)
}

#' GAMSr mathematical functions
#'
#' These helpers create symbolic math-function nodes. They do not evaluate their
#' arguments as ordinary R vectors.
#'
#' @param x A GAMSr symbolic expression or numeric scalar.
#' @param ... GAMSr symbolic expressions or numeric scalars.
#'
#' @return A `gams_expr_math_function` object.
#' @name gams_math
NULL

#' @rdname gams_math
#' @export
gams_abs <- function(x) {
  new_single_argument_math("abs", x)
}

#' @rdname gams_math
#' @export
gams_exp <- function(x) {
  new_single_argument_math("exp", x)
}

#' @rdname gams_math
#' @export
gams_log <- function(x) {
  new_single_argument_math("log", x)
}

#' @rdname gams_math
#' @export
gams_log10 <- function(x) {
  new_single_argument_math("log10", x)
}

#' @rdname gams_math
#' @export
gams_sqrt <- function(x) {
  new_single_argument_math("sqrt", x)
}

#' @rdname gams_math
#' @export
gams_min <- function(...) {
  new_multi_argument_math("min", ...)
}

#' @rdname gams_math
#' @export
gams_max <- function(...) {
  new_multi_argument_math("max", ...)
}
