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
  gamsr_abort(
    "Symbolic equation assignment is not implemented in this milestone.",
    i = "The expression AST and indexed assignment layer are planned for Milestone 2.",
    class = "gamsr_error_not_implemented"
  )
}
