#' Sum a symbolic expression over one or more sets
#'
#' @param index A GAMSr set or a set vector created with `c(i, j)`.
#' @param expression A GAMSr symbolic expression.
#' @param condition Optional symbolic condition used to filter the summation
#'   domain.
#'
#' @return A `gams_expr_sum` object.
#' @export
gams_sum <- function(index, expression, condition = NULL) {
  indices <- normalize_sum_indices(index)
  new_sum_expression(indices, expression, condition = condition)
}

normalize_sum_indices <- function(index, call = rlang::caller_env()) {
  if (inherits(index, "gams_symbol_vector")) {
    index <- unclass(index)
  } else if (is_gams_index_set(index)) {
    index <- list(index)
  } else {
    gamsr_abort(
      "`index` must be a GAMSr set or a set vector created with `c(i, j)`.",
      class = "gamsr_error_invalid_domain",
      call = call
    )
  }

  if (length(index) == 0L) {
    gamsr_abort(
      "`index` must contain at least one set.",
      class = "gamsr_error_invalid_domain",
      call = call
    )
  }

  for (item in index) {
    if (!is_gams_index_set(item)) {
      gamsr_abort(
        "All summation indices must be GAMSr set or alias objects.",
        class = "gamsr_error_invalid_domain",
        call = call
      )
    }
  }

  names <- domain_names(index)
  if (anyDuplicated(tolower(names))) {
    gamsr_abort(
      "`index` must not contain duplicate set names.",
      class = "gamsr_error_duplicate_name",
      call = call
    )
  }

  index
}
