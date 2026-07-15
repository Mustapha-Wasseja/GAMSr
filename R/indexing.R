#' @export
`[.gams_symbol` <- function(x, ..., drop = FALSE) {
  if (isTRUE(drop)) {
    gamsr_abort(
      "`drop = TRUE` is not supported for GAMSr symbolic indexing.",
      class = "gamsr_error_invalid_index"
    )
  }

  indices <- list(...)
  validate_symbol_indices(x, indices)
  new_indexed_reference(x, indices)
}

validate_symbol_indices <- function(symbol, indices, call = rlang::caller_env()) {
  expected <- length(symbol$domain)
  actual <- length(indices)

  if (actual != expected) {
    gamsr_abort(
      sprintf(
        "Symbol `%s` expects %d index value%s, not %d.",
        symbol$name,
        expected,
        if (expected == 1L) "" else "s",
        actual
      ),
      class = "gamsr_error_invalid_index",
      call = call
    )
  }

  for (i in seq_along(indices)) {
    index <- indices[[i]]
    domain <- symbol$domain[[i]]
    validate_index_value(index, symbol$model, domain, call = call)
  }

  invisible(indices)
}

validate_index_value <- function(index, model, domain = NULL, call = rlang::caller_env()) {
  if (is_gams_index_set(index)) {
    if (!identical(index$model, model)) {
      gamsr_abort(
        "Symbolic indices must belong to the same model as the symbol being indexed.",
        class = "gamsr_error_cross_model_domain",
        call = call
      )
    }
    if (!is.null(domain) && !identical(base_gams_set(index), base_gams_set(domain))) {
      gamsr_abort(
        sprintf("Index `%s` does not match expected domain `%s`.", index$name, domain$name),
        class = "gamsr_error_invalid_index",
        call = call
      )
    }
    return(invisible(index))
  }

  if (is.character(index) && length(index) == 1L && !is.na(index)) {
    validate_label(index, "index", call = call)
    if (!is.null(domain) && nrow(domain$records) > 0L && !(index %in% domain$records$label)) {
      gamsr_abort(
        sprintf("Label `%s` is not a record of domain set `%s`.", index, domain$name),
        class = "gamsr_error_invalid_index",
        call = call
      )
    }
    return(invisible(index))
  }

  gamsr_abort(
    "Symbolic indices must be GAMSr sets or single character labels.",
    class = "gamsr_error_invalid_index",
    call = call
  )
}

index_free_names <- function(indices) {
  vapply(
    Filter(is_gams_index_set, indices),
    function(index) index$name,
    character(1L),
    USE.NAMES = FALSE
  )
}
