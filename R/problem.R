.allowed_problem_types <- c(
  "LP", "MIP", "RMIP", "NLP", "DNLP", "QCP", "MIQCP", "MINLP", "RMINLP"
)

#' Create a GAMS solve problem
#'
#' `gams_problem()` binds a model context, a set of defined equations, an
#' objective expression, and solve metadata into a compileable problem object.
#'
#' @param model A model created by [gams_model()].
#' @param equations One or more equations, commonly `c(e1, e2)`.
#' @param objective A scalar GAMSr symbolic expression.
#' @param sense Optimisation direction, either `"min"` or `"max"`.
#' @param problem GAMS model type. The initial compiler supports deterministic
#'   source generation for common optimisation types.
#' @param name Optional problem name. Defaults to the model name.
#'
#' @return A `gams_problem` object.
#' @export
gams_problem <- function(model, equations, objective, sense = c("min", "max"),
                         problem = "LP", name = NULL) {
  if (!is_gams_model(model)) {
    gamsr_abort(
      "`model` must be created by `gams_model()`.",
      class = "gamsr_error_invalid_model"
    )
  }

  sense <- match.arg(sense)
  problem <- toupper(validate_symbol_name(problem, arg = "problem"))
  if (!(problem %in% .allowed_problem_types)) {
    gamsr_abort(
      sprintf("Problem type `%s` is not supported by the compiler MVP.", problem),
      i = sprintf("Supported types: %s.", paste(.allowed_problem_types, collapse = ", ")),
      class = "gamsr_error_invalid_problem"
    )
  }

  equations <- normalize_problem_equations(equations, model)
  objective <- as_gams_expr(objective)

  validate_problem_objective(objective)
  validate_problem_symbols(model, equations, objective)
  validate_discrete_variables(model, problem)

  structure(
    list(
      model = model,
      name = validate_model_name(name %||% paste0(model$name, "_problem")),
      equations = equations,
      objective = objective,
      sense = sense,
      problem = problem
    ),
    class = "gams_problem"
  )
}

normalize_problem_equations <- function(equations, model, call = rlang::caller_env()) {
  if (inherits(equations, "gams_symbol_vector")) {
    equations <- unclass(equations)
  } else if (inherits(equations, "gams_equation")) {
    equations <- list(equations)
  } else if (!is.list(equations)) {
    gamsr_abort(
      "`equations` must contain one or more equations.",
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }

  if (length(equations) == 0L) {
    gamsr_abort(
      "`equations` must contain at least one equation.",
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }

  names <- vapply(equations, `[[`, "name", FUN.VALUE = character(1L))
  if (anyDuplicated(tolower(names))) {
    gamsr_abort(
      "`equations` must not contain duplicate equation names.",
      class = "gamsr_error_duplicate_name",
      call = call
    )
  }

  lapply(equations, function(equation) {
    if (!inherits(equation, "gams_equation")) {
      gamsr_abort(
        "Every entry in `equations` must be created by `gams_equation()`.",
        class = "gamsr_error_invalid_problem",
        call = call
      )
    }
    if (!identical(equation$model, model)) {
      gamsr_abort(
        "Every equation must belong to the problem model.",
        class = "gamsr_error_invalid_problem",
        call = call
      )
    }

    refreshed <- model$get_symbol(equation$name)
    if (is.null(refreshed$definition)) {
      gamsr_abort(
        sprintf("Equation `%s` has no symbolic definition.", equation$name),
        i = sprintf(
          "Define it with `%s[...] <- lhs <= rhs` before creating the problem.",
          equation$name
        ),
        class = "gamsr_error_undefined_equation",
        call = call
      )
    }
    refreshed
  })
}

validate_problem_objective <- function(objective, call = rlang::caller_env()) {
  if (length(objective$free_indices) > 0L) {
    gamsr_abort(
      sprintf(
        "The objective expression must be scalar, but it has free %s: `%s`.",
        if (length(objective$free_indices) == 1L) "index" else "indices",
        paste(objective$free_indices, collapse = "`, `")
      ),
      i = "Bind objective indices with `gams_sum()`.",
      class = "gamsr_error_invalid_objective",
      call = call
    )
  }

  invisible(objective)
}

validate_problem_symbols <- function(model, equations, objective, call = rlang::caller_env()) {
  expressions <- c(
    lapply(equations, function(equation) equation$definition$expression),
    list(objective)
  )
  symbols <- unlist(lapply(expressions, expr_symbols), recursive = FALSE)

  for (symbol in symbols) {
    if (!identical(symbol$model, model)) {
      gamsr_abort(
        sprintf("Symbol `%s` belongs to a different model.", symbol$name),
        class = "gamsr_error_invalid_problem",
        call = call
      )
    }
  }

  invisible(symbols)
}

validate_discrete_variables <- function(model, problem, call = rlang::caller_env()) {
  if (problem != "LP") {
    return(invisible(model))
  }

  variables <- Filter(function(symbol) inherits(symbol, "gams_variable"), model$symbols())
  discrete <- Filter(function(variable) variable$type %in% c("binary", "integer"), variables)
  if (length(discrete) > 0L) {
    gamsr_abort(
      "Problem type `LP` cannot contain binary or integer variables.",
      i = "Use problem type `MIP` for discrete linear models.",
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }

  invisible(model)
}

expr_symbols <- function(expression) {
  switch(
    expression$type,
    "constant" = list(),
    "symbol_reference" = list(expression$symbol),
    "indexed_reference" = list(expression$symbol),
    "binary_operation" = c(expr_symbols(expression$lhs), expr_symbols(expression$rhs)),
    "unary_operation" = expr_symbols(expression$operand),
    "comparison" = c(expr_symbols(expression$lhs), expr_symbols(expression$rhs)),
    "sum" = expr_symbols(expression$expression),
    "math_function" = unlist(lapply(expression$arguments, expr_symbols), recursive = FALSE),
    "set_function" = list(expression$set),
    gamsr_abort(
      sprintf("Unsupported expression node type `%s`.", expression$type),
      class = "gamsr_error_invalid_expression"
    )
  )
}

#' @export
print.gams_problem <- function(x, ...) {
  cat(sprintf("<gams_problem: %s>\n", x$name))
  cat(sprintf("Problem type: %s\n", x$problem))
  cat(sprintf("Sense: %s\n", x$sense))
  cat(sprintf(
    "Equations: %s\n",
    paste(vapply(x$equations, `[[`, "name", FUN.VALUE = character(1L)), collapse = ", ")
  ))
  invisible(x)
}
