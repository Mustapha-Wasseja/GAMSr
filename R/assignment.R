#' Register an ordered parameter or dynamic-set assignment
#'
#' Assignments are emitted in registration order after input data and variable
#' attribute initialization. `condition` applies a dollar condition to the
#' assignment target, preserving existing values where the condition is false.
#'
#' @param target A scalar or indexed parameter reference, or an indexed dynamic
#'   set reference.
#' @param value A symbolic expression or scalar value.
#' @param condition Optional symbolic condition on the assignment target.
#'
#' @return A `gams_assignment` object, invisibly.
#' @export
gams_assign <- function(target, value, condition = NULL) {
  target <- normalize_assignment_target(target)
  symbol <- target$symbol
  model <- symbol$model
  logical_membership <- if (
    inherits(symbol, "gams_dynamic_set") &&
      is.logical(value) && length(value) == 1L && !is.na(value)
  ) {
    value
  } else {
    NULL
  }
  value <- as_gams_expr(value)
  condition <- if (is.null(condition)) NULL else as_gams_condition(condition)

  if (contains_direct_variable(value)) {
    gamsr_abort(
      "Assignments cannot contain decision variables directly.",
      i = "Use a symbolic variable-attribute expression such as `gams_level(x[i])`.",
      class = "gamsr_error_invalid_assignment"
    )
  }
  validate_assignment_symbols(model, value, condition)
  validate_assignment_scope(target, value, condition)

  assignment <- structure(
    list(
      model = model,
      target = target,
      value = value,
      condition = condition,
      logical_membership = logical_membership
    ),
    class = "gams_assignment"
  )
  model$add_assignment(assignment)
  invisible(assignment)
}

normalize_assignment_target <- function(target, call = rlang::caller_env()) {
  if (inherits(target, "gams_parameter")) {
    target <- as_gams_expr(target, call = call)
  }
  if (
    !is_gams_expr(target) ||
      !(target$type %in% c("symbol_reference", "indexed_reference"))
  ) {
    gamsr_abort(
      "`target` must be a scalar or indexed parameter/dynamic-set reference.",
      class = "gamsr_error_invalid_assignment",
      call = call
    )
  }

  symbol <- target$symbol
  valid_parameter <- inherits(symbol, "gams_parameter")
  valid_dynamic_set <- inherits(symbol, "gams_dynamic_set")
  if (!valid_parameter && !valid_dynamic_set) {
    gamsr_abort(
      "Assignment targets must be parameters or dynamic sets.",
      class = "gamsr_error_invalid_assignment",
      call = call
    )
  }
  target
}

validate_assignment_symbols <- function(model, value, condition,
                                        call = rlang::caller_env()) {
  expressions <- c(list(value), if (is.null(condition)) list() else list(condition))
  symbols <- unlist(lapply(expressions, expr_symbols), recursive = FALSE)
  if (any(!vapply(symbols, function(x) identical(x$model, model), logical(1L)))) {
    gamsr_abort(
      "Assignment expressions must use symbols from the target model.",
      class = "gamsr_error_invalid_assignment",
      call = call
    )
  }
  invisible(symbols)
}

validate_assignment_scope <- function(target, value, condition,
                                      call = rlang::caller_env()) {
  free <- value$free_indices
  if (!is.null(condition)) {
    free <- union(free, condition$free_indices)
  }
  unresolved <- setdiff(free, target$free_indices)
  if (length(unresolved) > 0L) {
    gamsr_abort(
      sprintf(
        "Assignment contains unresolved indices: `%s`.",
        paste(unresolved, collapse = "`, `")
      ),
      i = "Bind extra indices with `gams_sum()` or add them to the target domain.",
      class = "gamsr_error_unresolved_index",
      call = call
    )
  }
  invisible(target)
}

assignment_reference <- function(symbol, indices) {
  validate_symbol_indices(symbol, indices)
  if (length(indices) == 0L) {
    return(new_symbol_reference(symbol))
  }
  new_indexed_reference(symbol, indices)
}

#' @export
`[<-.gams_parameter` <- function(x, ..., value) {
  target <- assignment_reference(x, rlang::dots_list(..., .ignore_empty = "all"))
  gams_assign(target, value)
  x
}

#' @export
`[<-.gams_set` <- function(x, ..., value) {
  target <- assignment_reference(x, rlang::dots_list(..., .ignore_empty = "all"))
  gams_assign(target, value)
  x
}
