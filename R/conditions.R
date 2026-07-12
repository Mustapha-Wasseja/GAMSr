gamsr_abort <- function(message, ..., class = NULL, call = rlang::caller_env()) {
  cli::cli_abort(
    c(message, ...),
    class = c(class, "gamsr_error"),
    call = call
  )
}
