#' Create a GAMS equation
#'
#' @param model A model created by [gams_model()].
#' @param name GAMS identifier for the equation.
#' @param domain Optional set or set vector created with `c(i, j)`.
#' @param description Optional explanatory text.
#'
#' @return An immutable `gams_equation` symbol object registered with `model`.
#' @export
gams_equation <- function(model, name, domain = NULL, description = NULL) {
  domain <- normalize_domain(domain, model)
  new_gams_symbol(
    model = model,
    name = name,
    kind = "equation",
    domain = domain,
    definition = NULL,
    description = description
  )
}

#' @export
`[<-.gams_equation` <- function(x, ..., value) {
  indices <- normalize_equation_indices(
    x,
    rlang::dots_list(..., .ignore_empty = "all")
  )
  expression <- as_gams_expr(value)

  if (!inherits(expression, "gams_expr_comparison")) {
    gamsr_abort(
      "Equation definitions must be symbolic relationships.",
      i = "Use `<=`, `>=`, `==`, or `gams_eq()` to define an equation.",
      class = "gamsr_error_invalid_equation_definition"
    )
  }

  validate_equation_scope(x, indices, expression)
  x$definition <- list(indices = indices, expression = expression)
  x$model$update_symbol(x)
  x
}

normalize_equation_indices <- function(equation, indices, call = rlang::caller_env()) {
  expected <- length(equation$domain)
  actual <- length(indices)

  if (actual != expected) {
    gamsr_abort(
      sprintf(
        "Equation `%s` expects %d defining index%s, not %d.",
        equation$name,
        expected,
        if (expected == 1L) "" else "es",
        actual
      ),
      class = "gamsr_error_invalid_index",
      call = call
    )
  }

  for (i in seq_along(indices)) {
    index <- indices[[i]]
    domain <- equation$domain[[i]]
    if (!is_gams_index_set(index) || !identical(index, domain)) {
      gamsr_abort(
        sprintf(
          "Equation `%s` index %d must be the domain set `%s`.",
          equation$name,
          i,
          domain$name
        ),
        class = "gamsr_error_invalid_index",
        call = call
      )
    }
  }

  indices
}

validate_equation_scope <- function(equation, indices, expression, call = rlang::caller_env()) {
  allowed <- domain_names(indices)
  unresolved <- setdiff(expression$free_indices, allowed)

  if (length(unresolved) > 0L) {
    gamsr_abort(
      sprintf(
        "Equation `%s` contains unresolved free %s: `%s`.",
        equation$name,
        if (length(unresolved) == 1L) "index" else "indices",
        paste(unresolved, collapse = "`, `")
      ),
      i = "Bind extra indices with `gams_sum()` or add them to the equation domain.",
      class = "gamsr_error_unresolved_index",
      call = call
    )
  }

  invisible(expression)
}
