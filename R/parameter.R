#' Create a GAMS parameter
#'
#' @param model A model created by [gams_model()].
#' @param name GAMS identifier for the parameter.
#' @param domain Optional set or set vector created with `c(i, j)`.
#' @param records Scalar numeric values, named vectors, matrices, or long-form
#'   data frames. Data frames must include a `value` column or use the final
#'   column as the value column.
#' @param description Optional explanatory text.
#'
#' @return An immutable `gams_parameter` symbol object registered with `model`.
#' @export
gams_parameter <- function(model, name, domain = NULL, records = NULL,
                           description = NULL) {
  domain <- normalize_domain(domain, model)
  new_gams_symbol(
    model = model,
    name = name,
    kind = "parameter",
    domain = domain,
    records = normalize_parameter_records(records, domain),
    description = description
  )
}

normalize_parameter_records <- function(records, domain, call = rlang::caller_env()) {
  names <- domain_names(domain)
  dim <- length(domain)

  if (is.null(records)) {
    out <- data.frame(value = numeric(), stringsAsFactors = FALSE, check.names = FALSE)
    if (dim > 0L) {
      out <- cbind(empty_key_frame(names), out)
    }
    return(out)
  }

  if (is.data.frame(records)) {
    return(normalize_parameter_data_frame(records, names, call = call))
  }

  if (is.matrix(records)) {
    if (dim != 2L) {
      gamsr_abort(
        "Matrix parameter records require exactly two domain sets.",
        class = "gamsr_error_invalid_records",
        call = call
      )
    }
    return(normalize_parameter_matrix(records, names, call = call))
  }

  if (is.atomic(records) && is.numeric(records)) {
    if (dim == 0L) {
      if (length(records) != 1L) {
        gamsr_abort(
          "Scalar parameter records must contain exactly one numeric value.",
          class = "gamsr_error_invalid_records",
          call = call
        )
      }
      return(validate_value_frame(data.frame(value = records, check.names = FALSE), call = call))
    }

    if (dim == 1L) {
      if (is.null(names(records)) || any(!nzchar(names(records)))) {
        gamsr_abort(
          "One-dimensional parameter vectors must be named.",
          class = "gamsr_error_invalid_records",
          call = call
        )
      }
      labels <- validate_label(names(records), "names(records)", call = call)
      check_unique_labels(labels, "names(records)", call = call)
      out <- data.frame(
        label = labels,
        value = as.numeric(records),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      names(out)[1L] <- names[1L]
      return(validate_value_frame(out, call = call))
    }
  }

  gamsr_abort(
    "`records` could not be normalized as parameter data.",
    class = "gamsr_error_invalid_records",
    call = call
  )
}

normalize_parameter_data_frame <- function(records, domain_names, call = rlang::caller_env()) {
  records <- as.data.frame(records, stringsAsFactors = FALSE)
  dim <- length(domain_names)

  if (ncol(records) < dim + 1L) {
    gamsr_abort(
      "Parameter record data frames must contain domain columns and a value column.",
      class = "gamsr_error_invalid_records",
      call = call
    )
  }

  value_col <- match("value", names(records), nomatch = 0L)
  if (value_col == 0L) {
    value_col <- ncol(records)
    names(records)[value_col] <- "value"
  }

  key_cols <- setdiff(seq_len(ncol(records)), value_col)
  if (length(key_cols) != dim) {
    gamsr_abort(
      "Parameter record data frames must have exactly one key column per domain set.",
      class = "gamsr_error_invalid_records",
      call = call
    )
  }

  out <- records[c(key_cols, value_col)]
  names(out) <- c(domain_names, "value")
  for (name in domain_names) {
    out[[name]] <- validate_label(as.character(out[[name]]), name, call = call)
  }
  validate_duplicate_keys(out, domain_names, call = call)
  validate_value_frame(out, call = call)
}

normalize_parameter_matrix <- function(records, domain_names, call = rlang::caller_env()) {
  row_labels <- rownames(records)
  col_labels <- colnames(records)
  if (is.null(row_labels) || is.null(col_labels)) {
    gamsr_abort(
      "Matrix parameter records must have row names and column names.",
      class = "gamsr_error_invalid_records",
      call = call
    )
  }

  row_labels <- validate_label(row_labels, "rownames(records)", call = call)
  col_labels <- validate_label(col_labels, "colnames(records)", call = call)
  check_unique_labels(row_labels, "rownames(records)", call = call)
  check_unique_labels(col_labels, "colnames(records)", call = call)

  grid <- expand.grid(
    row_labels,
    col_labels,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  names(grid) <- domain_names
  grid$value <- as.numeric(records[cbind(
    match(grid[[domain_names[1L]]], row_labels),
    match(grid[[domain_names[2L]]], col_labels)
  )])
  validate_value_frame(grid, call = call)
}

validate_value_frame <- function(records, call = rlang::caller_env()) {
  if (!is.numeric(records$value)) {
    gamsr_abort(
      "Parameter values must be numeric.",
      class = "gamsr_error_invalid_records",
      call = call
    )
  }

  if (anyNA(records$value)) {
    gamsr_abort(
      "Parameter values must not be missing in the MVP.",
      i = "GAMS special values will be handled by the transfer adapter milestone.",
      class = "gamsr_error_invalid_records",
      call = call
    )
  }

  records
}

empty_key_frame <- function(names) {
  out <- data.frame(matrix(character(), nrow = 0L, ncol = length(names)))
  names(out) <- names
  out
}

validate_duplicate_keys <- function(records, key_cols, call = rlang::caller_env()) {
  if (length(key_cols) == 0L || nrow(records) == 0L) {
    return(invisible(records))
  }

  keys <- apply(records[key_cols], 1L, function(row) paste(tolower(row), collapse = "\r"))
  duplicate_key <- duplicated(keys)
  if (any(duplicate_key)) {
    gamsr_abort(
      "Parameter records contain duplicate domain keys.",
      i = sprintf("First duplicate row: %d.", which(duplicate_key)[1L]),
      class = "gamsr_error_duplicate_records",
      call = call
    )
  }
  invisible(records)
}
