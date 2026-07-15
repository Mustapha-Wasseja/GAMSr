#' Create a GAMS variable
#'
#' @param model A model created by [gams_model()].
#' @param name GAMS identifier for the variable.
#' @param domain Optional set or set vector created with `c(i, j)`.
#' @param type Variable type. The MVP supports `free`, `positive`, `negative`,
#'   `binary`, and `integer`.
#' @param description Optional explanatory text.
#'
#' @return An immutable `gams_variable` symbol object registered with `model`.
#' @export
gams_variable <- function(model, name, domain = NULL,
                          type = c("free", "positive", "negative", "binary", "integer"),
                          description = NULL) {
  type <- match.arg(type)
  domain <- normalize_domain(domain, model)
  new_gams_symbol(
    model = model,
    name = name,
    kind = "variable",
    domain = domain,
    type = type,
    gams_attributes = list(),
    description = description
  )
}

#' Set variable lower bounds
#'
#' @param variable A variable created by [gams_variable()].
#' @param records Bound records in the same shape accepted by [gams_parameter()].
#'
#' @return A copy of `variable` with lower-bound records attached.
#' @export
set_lower_bound <- function(variable, records) {
  set_variable_attribute(variable, "lower", records)
}

#' Set variable upper bounds
#'
#' @inheritParams set_lower_bound
#'
#' @return A copy of `variable` with upper-bound records attached.
#' @export
set_upper_bound <- function(variable, records) {
  set_variable_attribute(variable, "upper", records)
}

#' Set variable starting levels
#'
#' @inheritParams set_lower_bound
#'
#' @return A copy of `variable` with level records attached.
#' @export
set_level <- function(variable, records) {
  set_variable_attribute(variable, "level", records)
}

#' Fix variable values
#'
#' @inheritParams set_lower_bound
#'
#' @return A copy of `variable` with fixed-value records attached.
#' @export
set_fixed <- function(variable, records) {
  set_variable_attribute(variable, "fixed", records)
}

set_variable_attribute <- function(variable, field, records, call = rlang::caller_env()) {
  if (!inherits(variable, "gams_variable")) {
    gamsr_abort(
      "`variable` must be created by `gams_variable()`.",
      class = "gamsr_error_invalid_symbol",
      call = call
    )
  }

  variable$gams_attributes[[field]] <- normalize_parameter_records(
    records,
    variable$domain,
    call = call
  )
  variable$model$update_symbol(variable)
  variable
}
