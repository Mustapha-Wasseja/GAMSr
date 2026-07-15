#' Create an alias for a GAMS set
#'
#' An alias provides another index name for an existing set. This is useful for
#' pairwise expressions such as `x[i, ip]` where `i` and `ip` range over the
#' same records.
#'
#' @param set A set created by [gams_set()].
#' @param name GAMS identifier for the alias.
#'
#' @return An immutable `gams_alias` symbol registered with the set's model.
#' @export
gams_alias <- function(set, name) {
  if (!inherits(set, "gams_set")) {
    gamsr_abort(
      "`set` must be created by `gams_set()`.",
      class = "gamsr_error_invalid_domain"
    )
  }

  new_gams_symbol(
    model = set$model,
    name = name,
    kind = "alias",
    domain = NULL,
    records = set$records,
    base_set = set
  )
}
