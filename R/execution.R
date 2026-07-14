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
#' @param solver_options Optional named list of solver-specific option-file
#'   entries. Requires `solver`; GAMSr writes `<solver>.opt` in the work
#'   directory and passes `optfile=1` to GAMS.
#' @param data How input set and parameter records are supplied to GAMS.
#'   `"gdx"` writes and loads `input.gdx`; `"inline"` renders records directly
#'   into the generated `.gms` source.
#' @param keep Whether to retain work files after reading results.
#' @param timeout Maximum process runtime in seconds.
#' @param echo Whether to echo GAMS output while it runs.
#'
#' @return A `gams_result` object.
#' @export
solve.gams_problem <- function(a, b, ..., work_dir = NULL, system_directory = NULL,
                               solver = NULL, gams_options = list(),
                               solver_options = NULL,
                               data = c("gdx", "inline"),
                               keep = FALSE, timeout = Inf, echo = FALSE) {
  data_mode <- match.arg(data)
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

  ir <- model_ir(a)
  input_file <- maybe_write_solve_input_gdx(a, data_mode, work_dir, system_directory)
  source <- render_gams_ir(
    ir,
    data_source = data_mode,
    input_file = if (is.na(input_file)) NA_character_ else basename(input_file)
  )
  source <- append_result_unload(source, ir, "results.gdx")
  source_file <- file.path(work_dir, paste0(a$name, "-solve.gms"))
  writeLines(source, con = source_file, useBytes = TRUE)
  option_file <- write_solver_option_file(work_dir, solver, solver_options)
  args <- solve_command_args(
    a,
    source_file = source_file,
    solver = solver,
    gams_options = gams_options,
    use_solver_option_file = !is.na(option_file)
  )
  compilation <- new_gams_compilation(
    problem_name = a$name,
    source_file = normalizePath(source_file, winslash = "/", mustWork = TRUE),
    work_dir = work_dir,
    source = source,
    ir = ir,
    data_mode = data_mode,
    input_file = input_file,
    option_file = option_file
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

  result_data <- read_solution_gdx(a, result_file)
  files <- solve_files(
    work_dir = work_dir,
    source_file = source_file,
    input_file = input_file,
    result_file = result_file,
    option_file = option_file,
    listing_file = file.path(
      work_dir,
      paste0(tools::file_path_sans_ext(basename(source_file)), ".lst")
    ),
    retained = isTRUE(keep)
  )
  new_gams_result(
    problem = a,
    compilation = compilation,
    process = process,
    result_file = if (isTRUE(keep)) {
      normalizePath(result_file, winslash = "/", mustWork = TRUE)
    } else {
      NA_character_
    },
    variables = result_data$variables,
    equations = result_data$equations,
    objective = result_data$objective,
    model_status = result_data$model_status,
    solver_status = result_data$solver_status,
    files_retained = isTRUE(keep),
    summary = result_data$summary,
    files = files,
    command = list(
      executable = gams,
      args = args,
      solver = solver,
      gams_options = gams_options,
      solver_options = solver_options,
      data = data_mode
    )
  )
}

maybe_write_solve_input_gdx <- function(problem, data, work_dir, system_directory) {
  if (identical(data, "inline")) {
    return(NA_character_)
  }

  symbols <- transfer_symbols(problem)
  if (length(symbols$sets) == 0L && length(symbols$parameters) == 0L) {
    return(NA_character_)
  }

  input_file <- file.path(work_dir, "input.gdx")
  write_input_gdx(
    problem,
    input_file,
    adapter = gamstransfer_adapter(system_directory = system_directory),
    overwrite = TRUE
  )
  normalizePath(input_file, winslash = "/", mustWork = TRUE)
}

solve_command_args <- function(problem, source_file, solver = NULL, gams_options = list(),
                               use_solver_option_file = FALSE) {
  option_names <- names(gams_options) %||% character()
  if (isTRUE(use_solver_option_file) && any(tolower(option_names) == "optfile")) {
    gamsr_abort(
      "`gams_options` must not include `optfile` when `solver_options` is supplied.",
      class = "gamsr_error_invalid_option"
    )
  }

  c(
    basename(source_file),
    "lo=2",
    solver_argument(problem$problem, solver),
    if (isTRUE(use_solver_option_file)) "optfile=1" else character(),
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

write_solver_option_file <- function(work_dir, solver, solver_options) {
  if (is.null(solver_options)) {
    return(NA_character_)
  }
  if (is.null(solver)) {
    gamsr_abort(
      "`solver_options` requires an explicit `solver`.",
      class = "gamsr_error_invalid_option"
    )
  }
  validate_gams_option_value(solver, "solver")
  validate_gams_option_token(solver, "solver")

  lines <- solver_option_lines(solver_options)
  file <- file.path(work_dir, paste0(tolower(solver), ".opt"))
  writeLines(lines, con = file, useBytes = TRUE)
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

solver_option_lines <- function(solver_options, call = rlang::caller_env()) {
  if (!is.list(solver_options) || length(solver_options) == 0L) {
    gamsr_abort(
      "`solver_options` must be a non-empty named list.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }

  option_names <- names(solver_options)
  if (is.null(option_names) || any(!nzchar(option_names))) {
    gamsr_abort(
      "`solver_options` must have non-empty names.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }
  if (anyDuplicated(tolower(option_names))) {
    gamsr_abort(
      "`solver_options` must not contain duplicate option names.",
      class = "gamsr_error_invalid_option",
      call = call
    )
  }

  mapply(
    function(name, value) {
      validate_gams_option_token(name, "solver_options", call = call)
      paste(name, format_gams_option_value(value, name, call = call))
    },
    option_names,
    solver_options,
    USE.NAMES = FALSE
  )
}

solve_files <- function(work_dir, source_file, input_file, result_file, option_file,
                        listing_file, retained) {
  if (!isTRUE(retained)) {
    return(list(
      work_dir = NA_character_,
      source_file = NA_character_,
      input_file = NA_character_,
      result_file = NA_character_,
      option_file = NA_character_,
      listing_file = NA_character_
    ))
  }

  list(
    work_dir = normalizePath(work_dir, winslash = "/", mustWork = TRUE),
    source_file = normalizePath(source_file, winslash = "/", mustWork = TRUE),
    input_file = normalize_optional_file(input_file),
    result_file = normalize_optional_file(result_file),
    option_file = normalize_optional_file(option_file),
    listing_file = normalize_optional_file(listing_file)
  )
}

normalize_optional_file <- function(file) {
  if (is.null(file) || is.na(file) || !file.exists(file)) {
    return(NA_character_)
  }

  normalizePath(file, winslash = "/", mustWork = TRUE)
}

append_result_unload <- function(source, ir, result_file) {
  variable_names <- vapply(ir$variables, `[[`, "name", FUN.VALUE = character(1L))
  equation_names <- vapply(ir$equations, `[[`, "name", FUN.VALUE = character(1L))
  unload_symbols <- c(
    "GAMSr_modelstat",
    "GAMSr_solvestat",
    "GAMSr_objective_value",
    "GAMSr_solve_summary",
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
    "Set GAMSr_solve_metric /",
    "    \"model_status\", \"solver_status\", \"objective_value\", \"objective_bound\",",
    "    \"objective_variable_level\", \"resource_seconds\", \"elapsed_solve_seconds\",",
    "    \"elapsed_solver_seconds\", \"iterations\", \"equations\", \"variables\", \"nonzeros\",",
    "    \"domain_violations\", \"sum_infeasibilities\", \"max_infeasibility\"",
    "/;",
    "Parameter GAMSr_solve_summary(GAMSr_solve_metric);",
    sprintf("GAMSr_solve_summary('model_status') = %s.modelstat;", ir$problem_name),
    sprintf("GAMSr_solve_summary('solver_status') = %s.solvestat;", ir$problem_name),
    sprintf("GAMSr_solve_summary('objective_value') = %s.objval;", ir$problem_name),
    sprintf("GAMSr_solve_summary('objective_bound') = %s.objest;", ir$problem_name),
    sprintf(
      "GAMSr_solve_summary('objective_variable_level') = %s.l;",
      ir$objective_variable
    ),
    sprintf("GAMSr_solve_summary('resource_seconds') = %s.resusd;", ir$problem_name),
    sprintf("GAMSr_solve_summary('elapsed_solve_seconds') = %s.etsolve;", ir$problem_name),
    sprintf("GAMSr_solve_summary('elapsed_solver_seconds') = %s.etsolver;", ir$problem_name),
    sprintf("GAMSr_solve_summary('iterations') = %s.iterusd;", ir$problem_name),
    sprintf("GAMSr_solve_summary('equations') = %s.numequ;", ir$problem_name),
    sprintf("GAMSr_solve_summary('variables') = %s.numvar;", ir$problem_name),
    sprintf("GAMSr_solve_summary('nonzeros') = %s.numnz;", ir$problem_name),
    sprintf("GAMSr_solve_summary('domain_violations') = %s.domusd;", ir$problem_name),
    sprintf("GAMSr_solve_summary('sum_infeasibilities') = %s.suminfes;", ir$problem_name),
    sprintf("GAMSr_solve_summary('max_infeasibility') = %s.maxinfes;", ir$problem_name),
    sprintf(
      "execute_unload %s, %s;",
      quote_gams_text(result_file),
      paste(unload_symbols, collapse = ", ")
    ),
    sep = "\n"
  )
}
