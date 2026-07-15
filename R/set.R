#' Create a GAMS set
#'
#' @param model A model created by [gams_model()].
#' @param name GAMS identifier for the set.
#' @param records Character labels, or a data frame whose first column contains
#'   labels and optional second column contains element text.
#' @param description Optional explanatory text.
#'
#' @return An immutable `gams_set` symbol object registered with `model`.
#' @export
gams_set <- function(model, name, records = character(), description = NULL) {
  new_gams_symbol(
    model = model,
    name = name,
    kind = "set",
    domain = NULL,
    records = normalize_set_records(records),
    dynamic = FALSE,
    description = description
  )
}

normalize_set_records <- function(records, call = rlang::caller_env()) {
  if (is.null(records)) {
    records <- character()
  }

  if (is.data.frame(records)) {
    if (ncol(records) < 1L || ncol(records) > 2L) {
      gamsr_abort(
        "`records` for a set data frame must have one or two columns.",
        class = "gamsr_error_invalid_records",
        call = call
      )
    }
    labels <- validate_label(as.character(records[[1L]]), "records", call = call)
    element_text <- if (ncol(records) == 2L) {
      as.character(records[[2L]])
    } else {
      rep("", length(labels))
    }
  } else {
    labels <- validate_label(as.character(records), "records", call = call)
    element_text <- rep("", length(labels))
  }

  check_unique_labels(labels, "records", call = call)
  data.frame(
    label = labels,
    element_text = element_text,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}
