#' Create a dynamic subset
#'
#' Dynamic sets are computed by ordered assignment statements and are not
#' written to the input GDX file. They can be used as conditions after they
#' have been assigned.
#'
#' @param model A model created by [gams_model()].
#' @param name GAMS identifier for the dynamic set.
#' @param domain One or more static sets or aliases defining the subset domain.
#' @param description Optional explanatory text.
#'
#' @return A `gams_dynamic_set` registered with `model`.
#' @export
gams_dynamic_set <- function(model, name, domain, description = NULL) {
  domain <- normalize_domain(domain, model)
  if (length(domain) == 0L) {
    gamsr_abort(
      "`domain` must contain at least one set for a dynamic subset.",
      class = "gamsr_error_invalid_domain"
    )
  }

  symbol <- new_gams_symbol(
    model = model,
    name = name,
    kind = "set",
    domain = domain,
    records = NULL,
    dynamic = TRUE,
    description = description
  )
  class(symbol) <- c("gams_dynamic_set", class(symbol))
  model$update_symbol(symbol)
  symbol
}
