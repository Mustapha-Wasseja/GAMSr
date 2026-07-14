new_gams_result <- function(problem, compilation, process, result_file, variables,
                            equations, objective, model_status, solver_status,
                            files_retained, summary = empty_solve_summary(),
                            files = list(), command = list()) {
  structure(
    list(
      problem = problem,
      compilation = compilation,
      process = process,
      result_file = result_file,
      files = files,
      command = command,
      variables = variables,
      equations = equations,
      objective = objective,
      summary = summary,
      status = list(
        model = model_status,
        solver = solver_status
      ),
      files_retained = files_retained
    ),
    class = "gams_result"
  )
}

.solve_summary_metrics <- c(
  "model_status",
  "solver_status",
  "objective_value",
  "objective_bound",
  "objective_variable_level",
  "resource_seconds",
  "elapsed_solve_seconds",
  "elapsed_solver_seconds",
  "iterations",
  "equations",
  "variables",
  "nonzeros",
  "domain_violations",
  "sum_infeasibilities",
  "max_infeasibility"
)

#' Read a solved result GDX file
#'
#' @param problem A problem created by [gams_problem()].
#' @param file Result `.gdx` file containing GAMSr status scalars and selected
#'   variable/equation records.
#'
#' @return A `gams_result_data` object.
#' @export
read_solution_gdx <- function(problem, file) {
  if (!inherits(problem, "gams_problem")) {
    gamsr_abort(
      "`problem` must be created by `gams_problem()`.",
      class = "gamsr_error_invalid_problem"
    )
  }
  file <- validate_file_path(file)
  if (!file.exists(file)) {
    gamsr_abort(
      sprintf("Result GDX file does not exist: `%s`.", file),
      class = "gamsr_error_invalid_path"
    )
  }
  if (!gams_transfer_available()) {
    gamsr_abort(
      "The optional `gamstransfer` package is not installed.",
      i = "Install it with `install.packages(\"gamstransfer\")` to read result GDX files.",
      class = "gamsr_error_missing_dependency"
    )
  }

  Container <- getExportedValue("gamstransfer", "Container")
  container <- Container$new(file)
  variable_symbols <- Filter(
    function(symbol) inherits(symbol, "gams_variable"),
    problem$model$symbols()
  )
  equation_symbols <- problem$equations

  structure(
    list(
      objective = read_result_scalar(container, "GAMSr_objective_value"),
      model_status = new_model_status(read_result_scalar(container, "GAMSr_modelstat")),
      solver_status = new_solver_status(read_result_scalar(container, "GAMSr_solvestat")),
      summary = read_solve_summary(container),
      variables = read_result_records(container, variable_symbols),
      equations = read_result_records(container, equation_symbols)
    ),
    class = "gams_result_data"
  )
}

read_result_scalar <- function(container, name) {
  records <- container[name]$records
  if (is.null(records) || nrow(records) == 0L || !("value" %in% names(records))) {
    gamsr_abort(
      sprintf("Result GDX is missing scalar `%s`.", name),
      class = "gamsr_error_invalid_result"
    )
  }
  as.numeric(records$value[[1L]])
}

read_result_records <- function(container, symbols) {
  records <- lapply(symbols, function(symbol) {
    out <- container[symbol$name]$records
    if (is.null(out)) {
      out <- data.frame()
    }
    out
  })
  stats::setNames(records, vapply(symbols, `[[`, "name", FUN.VALUE = character(1L)))
}

read_solve_summary <- function(container) {
  records <- container["GAMSr_solve_summary"]$records
  if (is.null(records) || nrow(records) == 0L || !("value" %in% names(records))) {
    return(empty_solve_summary())
  }

  metric_column <- setdiff(names(records), "value")[[1L]]
  values <- stats::setNames(rep(0, length(.solve_summary_metrics)), .solve_summary_metrics)
  present <- as.character(records[[metric_column]])
  values[present] <- as.numeric(records$value)
  data.frame(
    metric = names(values),
    value = as.numeric(values),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

empty_solve_summary <- function() {
  data.frame(
    metric = .solve_summary_metrics,
    value = rep(NA_real_, length(.solve_summary_metrics)),
    stringsAsFactors = FALSE
  )
}

#' Inspect solve status
#'
#' @param result A result created by `solve()`.
#'
#' @return A `gams_status_value` object.
#' @export
model_status <- function(result) {
  validate_gams_result(result)
  result$status$model
}

#' @rdname model_status
#' @export
solver_status <- function(result) {
  validate_gams_result(result)
  result$status$solver
}

#' Extract objective value
#'
#' @param result A result created by `solve()`.
#'
#' @return A numeric scalar.
#' @export
objective_value <- function(result) {
  validate_gams_result(result)
  result$objective
}

#' Extract solve summary metadata
#'
#' @param result A result created by `solve()`.
#'
#' @return A data frame with `metric` and `value` columns.
#' @export
solve_summary <- function(result) {
  validate_gams_result(result)
  result$summary
}

#' Inspect retained solve files
#'
#' @param result A result created by `solve()`.
#'
#' @return A named list of retained file paths. Paths are `NA` when `solve()`
#'   was run with `keep = FALSE`.
#' @export
result_files <- function(result) {
  validate_gams_result(result)
  result$files
}

#' Extract variable records
#'
#' @param result A result created by `solve()`.
#' @param variable A variable object or variable name.
#'
#' @return A data frame of variable records.
#' @export
variable_values <- function(result, variable) {
  validate_gams_result(result)
  result_record(result$variables, variable, "variable")
}

#' Extract equation records
#'
#' @param result A result created by `solve()`.
#' @param equation An equation object or equation name.
#'
#' @return A data frame of equation records.
#' @export
equation_values <- function(result, equation) {
  validate_gams_result(result)
  result_record(result$equations, equation, "equation")
}

validate_gams_result <- function(result, call = rlang::caller_env()) {
  if (!inherits(result, "gams_result")) {
    gamsr_abort(
      "`result` must be created by `solve()`.",
      class = "gamsr_error_invalid_result",
      call = call
    )
  }
  invisible(result)
}

result_record <- function(records, symbol, kind, call = rlang::caller_env()) {
  name <- if (inherits(symbol, "gams_symbol")) {
    symbol$name
  } else if (is_string(symbol)) {
    symbol
  } else {
    gamsr_abort(
      sprintf("`%s` must be a symbol object or name.", kind),
      class = "gamsr_error_invalid_result",
      call = call
    )
  }

  if (!(name %in% names(records))) {
    gamsr_abort(
      sprintf("No %s records named `%s` are available.", kind, name),
      class = "gamsr_error_invalid_result",
      call = call
    )
  }
  records[[name]]
}

#' @export
print.gams_result <- function(x, ...) {
  cat(sprintf("<gams_result: %s>\n", x$problem$name))
  cat(sprintf("Model status: %d (%s)\n", x$status$model$code, x$status$model$label))
  cat(sprintf("Solver status: %d (%s)\n", x$status$solver$code, x$status$solver$label))
  cat(sprintf("Objective value: %s\n", format(x$objective, scientific = FALSE, trim = TRUE)))
  elapsed <- summary_value(x$summary, "elapsed_solve_seconds")
  if (!is.na(elapsed)) {
    cat(sprintf("Solve time: %s seconds\n", format(elapsed, scientific = FALSE, trim = TRUE)))
  }
  if (!is.null(x$command$data)) {
    cat(sprintf("Data mode: %s\n", x$command$data))
  }
  invisible(x)
}

summary_value <- function(summary, metric) {
  if (!is.data.frame(summary) || !all(c("metric", "value") %in% names(summary))) {
    return(NA_real_)
  }

  value <- summary$value[summary$metric == metric]
  if (length(value) == 0L) {
    NA_real_
  } else {
    value[[1L]]
  }
}
