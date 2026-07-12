.model_status_labels <- c(
  "1" = "OptimalGlobal",
  "2" = "OptimalLocal",
  "3" = "Unbounded",
  "4" = "InfeasibleGlobal",
  "5" = "InfeasibleLocal",
  "6" = "InfeasibleIntermed",
  "7" = "Feasible",
  "8" = "Integer",
  "9" = "NonIntegerIntermed",
  "10" = "IntegerInfeasible",
  "11" = "LicenseError",
  "12" = "ErrorUnknown",
  "13" = "ErrorNoSolution",
  "14" = "NoSolutionReturned",
  "15" = "SolvedUnique",
  "16" = "Solved",
  "17" = "SolvedSingular",
  "18" = "UnboundedNoSolution",
  "19" = "InfeasibleNoSolution"
)

.solver_status_labels <- c(
  "1" = "Normal",
  "2" = "Iteration",
  "3" = "Resource",
  "4" = "Solver",
  "5" = "EvalError",
  "6" = "Capability",
  "7" = "License",
  "8" = "User",
  "9" = "SetupErr",
  "10" = "SolverErr",
  "11" = "InternalErr",
  "12" = "Skipped",
  "13" = "SystemErr"
)

new_status_value <- function(code, labels) {
  code <- as.integer(code)
  label <- labels[[as.character(code)]] %||% "Unknown"
  structure(
    list(code = code, label = label),
    class = "gams_status_value"
  )
}

new_model_status <- function(code) {
  new_status_value(code, .model_status_labels)
}

new_solver_status <- function(code) {
  new_status_value(code, .solver_status_labels)
}

#' @export
print.gams_status_value <- function(x, ...) {
  cat(sprintf("%d (%s)\n", x$code, x$label))
  invisible(x)
}
