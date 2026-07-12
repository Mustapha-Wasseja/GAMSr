new_gams_symbol <- function(model, name, kind, domain = NULL, description = NULL,
                            records = NULL, ...) {
  if (!is_gams_model(model)) {
    gamsr_abort(
      "`model` must be created by `gams_model()`.",
      class = "gamsr_error_invalid_model"
    )
  }

  symbol <- structure(
    list(
      model = model,
      name = validate_symbol_name(name),
      kind = kind,
      domain = normalize_domain(domain, model),
      description = validate_description(description),
      records = records,
      ...
    ),
    class = c(paste0("gams_", kind), "gams_symbol")
  )

  model$add_symbol(symbol)
  symbol
}

validate_description <- function(description, call = rlang::caller_env()) {
  if (is.null(description)) {
    return(NULL)
  }

  if (!is_string(description)) {
    gamsr_abort(
      "`description` must be `NULL` or a single non-missing character string.",
      class = "gamsr_error_invalid_description",
      call = call
    )
  }

  if (nchar(description, type = "chars") > 255L) {
    gamsr_abort(
      "`description` must be at most 255 characters for GAMS compatibility.",
      class = "gamsr_error_invalid_description",
      call = call
    )
  }

  enc2utf8(description)
}

normalize_domain <- function(domain, model, call = rlang::caller_env()) {
  if (is.null(domain)) {
    return(list())
  }

  if (inherits(domain, "gams_symbol_vector")) {
    domain <- unclass(domain)
  } else if (inherits(domain, "gams_symbol")) {
    domain <- list(domain)
  } else if (!is.list(domain)) {
    gamsr_abort(
      "`domain` must contain GAMSr set objects.",
      class = "gamsr_error_invalid_domain",
      call = call
    )
  }

  for (item in domain) {
    if (!inherits(item, "gams_set")) {
      gamsr_abort(
        "All domain entries must be GAMSr set objects.",
        class = "gamsr_error_invalid_domain",
        call = call
      )
    }
    if (!identical(item$model, model)) {
      gamsr_abort(
        "All domain sets must belong to the same model.",
        class = "gamsr_error_cross_model_domain",
        call = call
      )
    }
  }

  domain
}

domain_names <- function(domain) {
  vapply(domain, function(x) x$name, character(1L), USE.NAMES = FALSE)
}

#' @export
c.gams_symbol <- function(..., recursive = FALSE) {
  if (isTRUE(recursive)) {
    gamsr_abort(
      "`recursive = TRUE` is not supported for GAMSr symbols.",
      class = "gamsr_error_invalid_domain"
    )
  }
  structure(list(...), class = "gams_symbol_vector")
}

#' @export
print.gams_symbol <- function(x, ...) {
  cat(sprintf("<%s: %s>\n", x$kind, x$name))
  if (length(x$domain) > 0L) {
    cat(sprintf("Domain: %s\n", paste(domain_names(x$domain), collapse = ", ")))
  }
  if (!is.null(x$description)) {
    cat(sprintf("Description: %s\n", x$description))
  }
  invisible(x)
}
