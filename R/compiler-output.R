validate_file_path <- function(file, arg = "file", call = rlang::caller_env()) {
  if (!is_string(file)) {
    gamsr_abort(
      sprintf("`%s` must be a single non-missing file path.", arg),
      class = "gamsr_error_invalid_path",
      call = call
    )
  }

  if (!nzchar(file)) {
    gamsr_abort(
      sprintf("`%s` must not be empty.", arg),
      class = "gamsr_error_invalid_path",
      call = call
    )
  }

  file
}

#' Write generated GAMS source to a file
#'
#' @param problem A problem created by [gams_problem()].
#' @param file Output `.gms` file path.
#' @param overwrite Whether to overwrite an existing file.
#' @param create_dir Whether to create the parent directory when it does not
#'   exist.
#'
#' @return The normalized output path, invisibly.
#' @export
write_gams <- function(problem, file, overwrite = FALSE, create_dir = FALSE) {
  file <- validate_file_path(file)
  parent <- dirname(file)

  if (!dir.exists(parent)) {
    if (isTRUE(create_dir)) {
      dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    } else {
      gamsr_abort(
        sprintf("Output directory does not exist: `%s`.", parent),
        i = "Use `create_dir = TRUE` or create the directory first.",
        class = "gamsr_error_invalid_path"
      )
    }
  }

  if (file.exists(file) && !isTRUE(overwrite)) {
    gamsr_abort(
      sprintf("Output file already exists: `%s`.", file),
      i = "Use `overwrite = TRUE` to replace it.",
      class = "gamsr_error_file_exists"
    )
  }

  writeLines(generated_gams(problem), con = file, useBytes = TRUE)
  invisible(normalizePath(file, winslash = "/", mustWork = TRUE))
}

#' Compile a problem to a deterministic `.gms` file
#'
#' `compile_gams()` is a compile-only helper. It writes the generated GAMS
#' source but does not run GAMS or create GDX files.
#'
#' @param problem A problem created by [gams_problem()].
#' @param directory Work directory for the generated `.gms` file.
#' @param filename Optional file name. Defaults to `<problem-name>.gms`.
#' @param overwrite Whether to overwrite an existing generated file.
#'
#' @return A `gams_compilation` object.
#' @export
compile_gams <- function(problem, directory = tempdir(), filename = NULL, overwrite = FALSE) {
  if (!inherits(problem, "gams_problem")) {
    gamsr_abort(
      "`problem` must be created by `gams_problem()`.",
      class = "gamsr_error_invalid_problem"
    )
  }

  directory <- validate_file_path(directory, "directory")
  filename <- filename %||% paste0(problem$name, ".gms")
  filename <- validate_file_path(filename, "filename")
  if (!identical(basename(filename), filename)) {
    gamsr_abort(
      "`filename` must be a file name, not a path.",
      class = "gamsr_error_invalid_path"
    )
  }

  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  source_file <- write_gams(
    problem,
    file.path(directory, filename),
    overwrite = overwrite,
    create_dir = TRUE
  )

  new_gams_compilation(
    problem_name = problem$name,
    source_file = source_file,
    work_dir = normalizePath(directory, winslash = "/", mustWork = TRUE),
    source = generated_gams(problem),
    ir = model_ir(problem),
    data_mode = "inline"
  )
}

new_gams_compilation <- function(problem_name, source_file, work_dir, source, ir,
                                 data_mode = "inline", input_file = NA_character_,
                                 option_file = NA_character_) {
  structure(
    list(
      problem_name = problem_name,
      source_file = source_file,
      work_dir = work_dir,
      source = source,
      ir = ir,
      data_mode = data_mode,
      input_file = input_file,
      option_file = option_file
    ),
    class = "gams_compilation"
  )
}

#' @export
print.gams_compilation <- function(x, ...) {
  cat(sprintf("<gams_compilation: %s>\n", x$problem_name))
  cat(sprintf("Source file: %s\n", x$source_file))
  invisible(x)
}
