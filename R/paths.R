#' Locate a GAMS executable
#'
#' @param system_directory Optional GAMS system directory.
#'
#' @return Path to the GAMS executable, or `NA_character_` when unavailable.
#' @export
find_gams <- function(system_directory = NULL) {
  if (!is.null(system_directory)) {
    return(normalize_gams_executable(file.path(system_directory, gams_executable_name())))
  }

  path <- Sys.which("gams")
  if (nzchar(path)) {
    return(normalize_gams_executable(path))
  }

  for (candidate in common_gams_candidates()) {
    normalized <- normalize_gams_executable(candidate)
    if (!is.na(normalized)) {
      return(normalized)
    }
  }

  NA_character_
}

#' Check whether GAMS is available
#'
#' @inheritParams find_gams
#'
#' @return `TRUE` when a GAMS executable can be located.
#' @export
gams_available <- function(system_directory = NULL) {
  !is.na(find_gams(system_directory))
}

#' Query the GAMS version
#'
#' @inheritParams find_gams
#'
#' @return A version number such as `"54.2.0"`, or `NA_character_` when the
#'   executable is unavailable or its output cannot be parsed. License metadata
#'   is never returned.
#' @export
gams_version <- function(system_directory = NULL) {
  exe <- find_gams(system_directory)
  if (is.na(exe)) {
    return(NA_character_)
  }

  process <- tryCatch(
    processx::run(exe, args = "?", timeout = 15, error_on_status = FALSE),
    error = function(err) NA_character_
  )
  if (is.character(process)) {
    return(process)
  }

  parse_gams_version(paste(c(process$stdout, process$stderr), collapse = "\n"))
}

parse_gams_version <- function(output) {
  pattern <- "GAMS Release[[:space:]]*:[[:space:]]*([0-9]+(?:\\.[0-9]+){1,2})"
  match <- regexec(pattern, output, perl = TRUE, ignore.case = TRUE)
  parts <- regmatches(output, match)[[1L]]
  if (length(parts) < 2L) {
    return(NA_character_)
  }
  parts[[2L]]
}

gams_executable_name <- function() {
  if (.Platform$OS.type == "windows") "gams.exe" else "gams"
}

normalize_gams_executable <- function(path) {
  if (!is_string(path) || !file.exists(path)) {
    return(NA_character_)
  }

  normalizePath(path, winslash = "/", mustWork = TRUE)
}

common_gams_candidates <- function() {
  candidates <- character()
  env_dirs <- Sys.getenv(c("GAMS_SYSDIR", "GAMSDIR", "GAMS_HOME"), unset = NA_character_)
  env_dirs <- env_dirs[!is.na(env_dirs) & nzchar(env_dirs)]
  candidates <- c(candidates, file.path(env_dirs, gams_executable_name()))

  if (.Platform$OS.type == "windows") {
    roots <- unique(c(
      Sys.getenv("GAMS_ROOT", unset = NA_character_),
      "C:/GAMS"
    ))
    roots <- roots[!is.na(roots) & nzchar(roots) & dir.exists(roots)]
    for (root in roots) {
      version_dirs <- list.dirs(root, full.names = TRUE, recursive = FALSE)
      candidates <- c(
        candidates,
        file.path(rev(sort(version_dirs)), gams_executable_name())
      )
    }
  }

  unique(candidates)
}
