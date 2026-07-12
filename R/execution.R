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
#' @param keep Whether to retain work files after reading results.
#' @param timeout Maximum process runtime in seconds.
#' @param echo Whether to echo GAMS output while it runs.
#'
#' @return A `gams_result` object.
#' @export
solve.gams_problem <- function(a, b, ..., work_dir = NULL, system_directory = NULL,
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

  process <- processx::run(
    command = gams,
    args = c(basename(source_file), "lo=2"),
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
