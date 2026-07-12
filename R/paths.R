#' Locate a GAMS executable
#'
#' @param system_directory Optional GAMS system directory.
#'
#' @return Path to the GAMS executable, or `NA_character_` when unavailable.
#' @export
find_gams <- function(system_directory = NULL) {
  if (!is.null(system_directory)) {
    exe <- file.path(system_directory, if (.Platform$OS.type == "windows") "gams.exe" else "gams")
    if (file.exists(exe)) {
      return(normalizePath(exe, winslash = "/", mustWork = TRUE))
    }
    return(NA_character_)
  }

  path <- Sys.which("gams")
  if (nzchar(path)) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  } else {
    NA_character_
  }
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
#' @return Version output from GAMS, or `NA_character_` when unavailable.
#' @export
gams_version <- function(system_directory = NULL) {
  exe <- find_gams(system_directory)
  if (is.na(exe)) {
    return(NA_character_)
  }

  out <- tryCatch(
    system2(exe, args = "--version", stdout = TRUE, stderr = TRUE),
    error = function(err) NA_character_
  )
  paste(out, collapse = "\n")
}
