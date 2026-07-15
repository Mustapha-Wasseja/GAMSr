new_variable_attr_expr <- function(reference, attribute) {
  reference <- normalize_variable_reference(reference)
  new_gams_expr(
    "variable_attribute",
    reference = reference,
    attribute = attribute,
    free_indices = reference$free_indices
  )
}

normalize_variable_reference <- function(reference, call = rlang::caller_env()) {
  if (inherits(reference, "gams_variable")) {
    reference <- as_gams_expr(reference, call = call)
  }
  if (
    !is_gams_expr(reference) ||
      !(reference$type %in% c("symbol_reference", "indexed_reference")) ||
      !inherits(reference$symbol, "gams_variable")
  ) {
    gamsr_abort(
      "`variable` must be a scalar variable or an indexed variable reference.",
      class = "gamsr_error_invalid_symbol",
      call = call
    )
  }
  reference
}

format_variable_attribute <- function(expression) {
  reference <- expression$reference
  base <- paste0(reference$symbol$name, ".", expression$attribute)
  if (identical(reference$type, "symbol_reference")) {
    return(base)
  }
  paste0(
    base,
    "(",
    paste(vapply(reference$indices, format_index, character(1L)), collapse = ","),
    ")"
  )
}

#' Symbolic variable attribute expressions
#'
#' These helpers create references to GAMS variable attributes for use in
#' assignments and dollar conditions.
#'
#' @param variable A scalar variable or an indexed variable reference.
#'
#' @return A symbolic variable-attribute expression.
#' @name variable_attribute_expressions
NULL

#' @rdname variable_attribute_expressions
#' @export
gams_level <- function(variable) {
  new_variable_attr_expr(variable, "l")
}

#' @rdname variable_attribute_expressions
#' @export
gams_marginal <- function(variable) {
  new_variable_attr_expr(variable, "m")
}

#' @rdname variable_attribute_expressions
#' @export
gams_lower_bound <- function(variable) {
  new_variable_attr_expr(variable, "lo")
}

#' @rdname variable_attribute_expressions
#' @export
gams_upper_bound <- function(variable) {
  new_variable_attr_expr(variable, "up")
}
