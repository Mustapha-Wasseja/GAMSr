#' Solve a GAMSr problem with a local GAMS installation
#'
#' This method writes deterministic GAMS source, runs a local GAMS executable
#' without invoking a shell, reads the result GDX, and returns tidy R records.
#'
#' @param a A problem created by [gams_problem()].
#' @param b Unused; present for compatibility with the base `solve()` generic.
#' @param ... Reserved for future options.
#' @param work_dir Optional work directory. When `NULL`, a temporary directory is
#'   created.
#' @param system_directory Optional GAMS system directory.
#' @param solver Optional solver name. When supplied, GAMSr passes a safe GAMS
#'   command-line solver override for the problem type, such as `lp=soplex`.
#' @param gams_options Named list of additional scalar GAMS command-line
#'   options, such as `list(reslim = 60, optcr = 0.01)`.
#' @param keep Whether to retain work files after reading results.
#' @param timeout Maximum process runtime in seconds.
#' @param echo Whether to echo GAMS output while it runs.
#'
#' @return A `gams_result` object.
#' @export
solve.gams_problem <- function(a, b, ..., work_dir = NULL, system_directory = NULL,
                               solver = NULL, gams_options = list(),
                               keep = FALSE, timeout = Inf, echo = FALSE) {
  if (!missing(b)) {
    gamsr_abort(
      "`b` is not used when solving a GAMSr problem.",
      class = "gamsr_error_invalid_problem"
    )
  }
  if (!inherits(a, "gams_problem")) {
    gamsr_abort(
      "`a` must be created by `gams_problem()`.",
      class = "gamsr_error_invalid_problem"
    )
  }
  if (!gams_transfer_available()) {
    gamsr_abort(
      "The optional `gamstransfer` package is not installed.",
      i = "Install it with `install.packages(\"gamstransfer\")` to read solve results.",
      class = "gamsr_error_missing_dependency"
    )
  }

  gams <- find_gams(system_directory %||% a$model$system_directory)
  if (is.na(gams)) {
    gamsr_abort(
      "No local GAMS executable was found.",
      i = "Install GAMS or pass `system_directory` to `solve()`.",
      class = "gamsr_error_gams_unavailable"
    )
  }

  if (is.null(work_dir)) {
    work_dir <- tempfile("GAMSr-solve-")
  }
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  work_dir <- normalizePath(work_dir, winslash = "/", mustWork = TRUE)
  if (!isTRUE(keep)) {
    on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)
  }

  compilation <- compile_gams(
    a,
    directory = work_dir,
    filename = paste0(a$name, ".gms"),
    overwrite = TRUE
  )
  source <- append_result_unload(compilation$source, model_ir(a), "results.gdx")
  source_file <- file.path(work_dir, paste0(a$name, "-solve.gms"))
  writeLines(source, con = source_file, useBytes = TRUE)
  args <- solve_command_args(
    a,
    source_file = source_file,
    solver = solver,
    gams_options = gams_options
  )

  process <- processx::run(
    command = gams,
    args = args,
    wd = work_dir,
    timeout = timeout,
    echo = echo,
    error_on_status = FALSE
  )
  result_file <- file.path(work_dir, "results.gdx")

  if (!file.exists(result_file)) {
    gamsr_abort(
      "GAMS did not produce a result GDX file.",
      i = sprintf("Process exit status: %s.", process$status),
      class = "gamsr_error_solve_failed"
    )
  }

  data <- read_solution_gdx(a, result_file)
  new_gams_result(
    problem = a,
    compilation = compilation,
    process = process,
    result_file = if (isTRUE(keep)) {
      normalizePath(result_file, winslash = "/", mustWork = TRUE)
    } else {
      NA_character_
    },
    variables = data$variables,
    equations = data$equations,
    objective = data$objective,
    model_status = data$model_status,
    solver_status = data$solver_status,
    files_retained = isTRUE(keep)
  )
}

solve_command_args <- function(problem, source_file, solver = NULL, gams_options = list()) {
  c(
    basename(source_file),
    "lo=2",
    solver_argument(problem$problem, solver),
    gams_option_arguments(gams_options)
  )
}

solver_argument <- function(problem_type, solver, call = rlang::caller_env()) {
  if (is.null(solver)) {
    return(character())
  }
  validate_gams_option_value(solver, "solver", call = call)
  validate_gams_option_token(solver, "solver", call = call)
  paste0(tolower(problem_type), "=", solver)
}

gams_option_arguments <- function(gams_options, call = rlang::caller_env()) {
  if (is.null(gams_options) || length(gams_options) == 0L) {
    return(character())
  }
  if (!is.list(gams_options)) {
    gamsr_abort(
      "`gams_options` must be a named list.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  option_names <- names(gams_options)
  if (is.null(option_names) || any(!nzchar(option_names))) {
    gamsr_abort(
      "`gams_options` must have non-empty names.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (anyDuplicated(tolower(option_names))) {
    gamsr_abort(
      "`gams_options` must not contain duplicate option names.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }

  mapply(
    function(name, value) {
      validate_gams_option_token(name, "gams_options", call = call)
      paste0(name, "=", format_gams_option_value(value, name, call = call))
    },
    option_names,
    gams_options,
    USE.NAMES = FALSE
  )
}

format_gams_option_value <- function(value, name, call = rlang::caller_env()) {
  validate_gams_option_value(value, name, call = call)

  if (is.logical(value)) {
    if (isTRUE(value)) "1" else "0"
  } else if (is.numeric(value)) {
    format(value, scientific = FALSE, trim = TRUE)
  } else {
    value
  }
}

validate_gams_option_value <- function(value, name, call = rlang::caller_env()) {
  if (length(value) != 1L || is.na(value)) {
    gamsr_abort(
      sprintf("GAMS option `%s` must be a single non-missing scalar value.", name),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (!(is.character(value) || is.numeric(value) || is.logical(value))) {
    gamsr_abort(
      sprintf("GAMS option `%s` must be character, numeric, or logical.", name),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (is.numeric(value) && !is.finite(value)) {
    gamsr_abort(
      sprintf("GAMS option `%s` must be finite.", name),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (is.character(value) && !nzchar(value)) {
    gamsr_abort(
      sprintf("GAMS option `%s` must not be empty.", name),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (is.character(value) && grepl("[\r\n]", value)) {
    gamsr_abort(
      sprintf("GAMS option `%s` must not contain line breaks.", name),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }

  invisible(value)
}

validate_gams_option_token <- function(value, arg, call = rlang::caller_env()) {
  if (!is_string(value) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", value)) {
    gamsr_abort(
      sprintf("`%s` must use GAMS identifier syntax.", arg),
      class = "gamsr_error_invalid_option",
      call = call
    )
  }

  invisible(value)
}

append_result_unload <- function(source, ir, result_file) {
  variable_names <- vapply(ir$variables, `[[`, "name", FUN.VALUE = character(1L))
  equation_names <- vapply(ir$equations, `[[`, "name", FUN.VALUE = character(1L))
  unload_symbols <- c(
    "GAMSr_modelstat",
    "GAMSr_solvestat",
    "GAMSr_objective_value",
    variable_names,
    equation_names
  )

  paste(
    source,
    "",
    "Scalar GAMSr_modelstat, GAMSr_solvestat, GAMSr_objective_value;",
    sprintf("GAMSr_modelstat = %s.modelstat;", ir$problem_name),
    sprintf("GAMSr_solvestat = %s.solvestat;", ir$problem_name),
    sprintf("GAMSr_objective_value = %s.l;", ir$objective_variable),
    sprintf(
      "execute_unload %s, %s;",
      quote_gams_text(result_file),
      paste(unload_symbols, collapse = ", ")
    ),
    sep = "\n"
  )
}
