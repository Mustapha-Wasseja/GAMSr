#' Check for GAMS Transfer R
#'
#' @return `TRUE` when the optional `gamstransfer` package can be loaded.
#' @export
gams_transfer_available <- function() {
  requireNamespace("gamstransfer", quietly = TRUE)
}

#' Extract canonical transfer symbols
#'
#' @param problem A problem created by [gams_problem()], or a model created by
#'   [gams_model()].
#'
#' @return A `gams_transfer_symbols` object containing sets and parameters in
#'   canonical long-form records.
#' @export
transfer_symbols <- function(problem) {
  model <- transfer_model(problem)
  symbols <- model$symbols()

  structure(
    list(
      sets = lapply(
        Filter(
          function(symbol) inherits(symbol, "gams_set") && !isTRUE(symbol$dynamic),
          symbols
        ),
        canonical_transfer_set
      ),
      parameters = lapply(
        Filter(function(symbol) inherits(symbol, "gams_parameter"), symbols),
        canonical_transfer_parameter
      )
    ),
    class = "gams_transfer_symbols"
  )
}

transfer_model <- function(problem, call = rlang::caller_env()) {
  if (inherits(problem, "gams_problem")) {
    return(problem$model)
  }
  if (is_gams_model(problem)) {
    return(problem)
  }
  gamsr_abort(
    "`problem` must be a `gams_problem` or `gams_model` object.",
    class = "gamsr_error_invalid_problem",
    call = call
  )
}

canonical_transfer_set <- function(symbol) {
  list(
    name = symbol$name,
    description = symbol$description,
    records = symbol$records
  )
}

canonical_transfer_parameter <- function(symbol) {
  list(
    name = symbol$name,
    description = symbol$description,
    domain = domain_names(symbol$domain),
    records = symbol$records
  )
}

#' Create a mock transfer adapter
#'
#' The mock adapter is intended for unit tests. It does not create a valid GDX
#' file.
#'
#' @return A `gams_transfer_adapter` object.
#' @export
mock_transfer_adapter <- function() {
  structure(
    list(
      type = "mock",
      write_input = function(symbols, file, overwrite = FALSE) {
        validate_transfer_file(file, overwrite = overwrite)
        structure(
          list(
            adapter = "mock",
            file = normalize_transfer_path(file),
            symbols = symbols,
            written = FALSE
          ),
          class = "gams_transfer_write"
        )
      }
    ),
    class = c("gams_mock_transfer_adapter", "gams_transfer_adapter")
  )
}

#' Create a GAMS Transfer R adapter
#'
#' @param system_directory Optional GAMS system directory passed to
#'   `gamstransfer::Container$new()`.
#'
#' @return A `gams_transfer_adapter` object.
#' @export
gamstransfer_adapter <- function(system_directory = NULL) {
  structure(
    list(
      type = "gamstransfer",
      system_directory = system_directory,
      write_input = function(symbols, file, overwrite = FALSE) {
        write_input_with_gamstransfer(symbols, file, system_directory, overwrite)
      }
    ),
    class = c("gams_gamstransfer_adapter", "gams_transfer_adapter")
  )
}

#' Write input data to a GDX file
#'
#' @param problem A problem created by [gams_problem()], or a model created by
#'   [gams_model()].
#' @param file Output `.gdx` file path.
#' @param adapter Transfer adapter. Defaults to [gamstransfer_adapter()].
#' @param overwrite Whether to overwrite an existing file.
#'
#' @return A `gams_transfer_write` object.
#' @export
write_input_gdx <- function(problem, file, adapter = gamstransfer_adapter(),
                            overwrite = FALSE) {
  if (!inherits(adapter, "gams_transfer_adapter")) {
    gamsr_abort(
      "`adapter` must be created by `gamstransfer_adapter()` or `mock_transfer_adapter()`.",
      class = "gamsr_error_invalid_transfer_adapter"
    )
  }
  adapter$write_input(transfer_symbols(problem), file, overwrite = overwrite)
}

write_input_with_gamstransfer <- function(symbols, file, system_directory, overwrite) {
  validate_transfer_file(file, overwrite = overwrite)

  if (!gams_transfer_available()) {
    gamsr_abort(
      "The optional `gamstransfer` package is not installed.",
      i = "Install it with `install.packages(\"gamstransfer\")` to write GDX files.",
      class = "gamsr_error_missing_dependency"
    )
  }

  Container <- getExportedValue("gamstransfer", "Container")
  Set <- getExportedValue("gamstransfer", "Set")
  Parameter <- getExportedValue("gamstransfer", "Parameter")
  container <- if (is.null(system_directory)) {
    Container$new()
  } else {
    Container$new(systemDirectory = system_directory)
  }

  set_objects <- new.env(parent = emptyenv())
  for (set in symbols$sets) {
    set_object <- Set$new(container, name = set$name, records = set$records$label)
    assign(tolower(set$name), set_object, envir = set_objects)
  }

  for (parameter in symbols$parameters) {
    domain <- lapply(parameter$domain, function(name) {
      get(tolower(name), envir = set_objects, inherits = FALSE)
    })
    records <- gt_parameter_records(parameter)
    if (length(domain) == 0L) {
      Parameter$new(container, name = parameter$name, records = records)
    } else {
      Parameter$new(container, name = parameter$name, domain = domain, records = records)
    }
  }

  container$write(file)
  structure(
    list(
      adapter = "gamstransfer",
      file = normalizePath(file, winslash = "/", mustWork = TRUE),
      symbols = symbols,
      written = TRUE
    ),
    class = "gams_transfer_write"
  )
}

gt_parameter_records <- function(parameter) {
  if (length(parameter$domain) == 0L) {
    if (nrow(parameter$records) == 0L) {
      return(numeric())
    }
    return(parameter$records$value[[1L]])
  }

  parameter$records
}

validate_transfer_file <- function(file, overwrite = FALSE, call = rlang::caller_env()) {
  file <- validate_file_path(file, call = call)
  parent <- dirname(file)
  if (!dir.exists(parent)) {
    gamsr_abort(
      sprintf("Output directory does not exist: `%s`.", parent),
      class = "gamsr_error_invalid_path",
      call = call
    )
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    gamsr_abort(
      sprintf("Output file already exists: `%s`.", file),
      i = "Use `overwrite = TRUE` to replace it.",
      class = "gamsr_error_file_exists",
      call = call
    )
  }

  invisible(file)
}

normalize_transfer_path <- function(file) {
  parent <- normalizePath(dirname(file), winslash = "/", mustWork = TRUE)
  file.path(parent, basename(file))
}

#' @export
print.gams_transfer_symbols <- function(x, ...) {
  cat("<gams_transfer_symbols>\n")
  cat(sprintf("Sets: %d\n", length(x$sets)))
  cat(sprintf("Parameters: %d\n", length(x$parameters)))
  invisible(x)
}

#' @export
print.gams_transfer_write <- function(x, ...) {
  cat(sprintf("<gams_transfer_write: %s>\n", x$adapter))
  cat(sprintf("File: %s\n", x$file))
  cat(sprintf("Written: %s\n", if (isTRUE(x$written)) "yes" else "no"))
  invisible(x)
}
