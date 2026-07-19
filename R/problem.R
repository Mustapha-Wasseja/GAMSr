.allowed_problem_types <- c(
  "LP", "MIP", "RMIP", "NLP", "DNLP", "QCP", "MIQCP", "RMIQCP",
  "MINLP", "RMINLP"
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
#' @param problem GAMS model type. GAMSr validates discrete-variable
#'   compatibility and rejects expressions whose polynomial degree exceeds the
#'   selected linear or quadratic model class.
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
  validate_problem_degree(equations, objective, problem)

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
  continuous_types <- c("LP", "NLP", "DNLP", "QCP")
  if (!(problem %in% continuous_types)) {
    return(invisible(model))
  }

  variables <- Filter(function(symbol) inherits(symbol, "gams_variable"), model$symbols())
  discrete <- Filter(function(variable) variable$type %in% c("binary", "integer"), variables)
  if (length(discrete) > 0L) {
    gamsr_abort(
      sprintf("Problem type `%s` cannot contain discrete variables.", problem),
      i = discrete_problem_hint(problem),
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }

  invisible(model)
}

discrete_problem_hint <- function(problem) {
  target <- switch(
    problem,
    "LP" = "MIP",
    "QCP" = "MIQCP",
    "NLP" = "MINLP",
    "DNLP" = "MINLP"
  )
  sprintf("Use problem type `%s` for this model.", target)
}

validate_problem_degree <- function(equations, objective, problem,
                                    call = rlang::caller_env()) {
  expressions <- c(
    lapply(equations, function(equation) equation$definition$expression),
    list(objective)
  )
  degree <- max(vapply(expressions, expression_degree, numeric(1L)))

  if (problem %in% c("LP", "MIP", "RMIP") && degree > 1) {
    gamsr_abort(
      sprintf("Problem type `%s` requires linear expressions.", problem),
      i = "Use `QCP`/`MIQCP` for quadratic models or an NLP model type.",
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }
  if (problem %in% c("QCP", "MIQCP", "RMIQCP") && degree > 2) {
    gamsr_abort(
      sprintf("Problem type `%s` supports at most quadratic expressions.", problem),
      i = "Use `NLP` or `MINLP` for higher-order and intrinsic expressions.",
      class = "gamsr_error_invalid_problem",
      call = call
    )
  }
  invisible(degree)
}

expression_degree <- function(expression) {
  switch(
    expression$type,
    "constant" = 0,
    "symbol_reference" = symbol_degree(expression$symbol),
    "indexed_reference" = symbol_degree(expression$symbol),
    "variable_attribute" = 0,
    "binary_operation" = binary_degree(expression),
    "unary_operation" = expression_degree(expression$operand),
    "comparison" = max(
      expression_degree(expression$lhs),
      expression_degree(expression$rhs)
    ),
    "sum" = max(
      expression_degree(expression$expression),
      if (is.null(expression$condition)) 0 else expression_degree(expression$condition)
    ),
    "math_function" = math_degree(expression),
    "set_function" = 0,
    "logical_operation" = max(
      expression_degree(expression$lhs),
      if (is.null(expression$rhs)) 0 else expression_degree(expression$rhs)
    ),
    "conditional" = max(
      expression_degree(expression$expression),
      expression_degree(expression$condition)
    ),
    "same_as" = 0,
    Inf
  )
}

symbol_degree <- function(symbol) {
  if (inherits(symbol, "gams_variable")) 1 else 0
}

binary_degree <- function(expression) {
  lhs <- expression_degree(expression$lhs)
  rhs <- expression_degree(expression$rhs)
  switch(
    expression$operator,
    "+" = max(lhs, rhs),
    "-" = max(lhs, rhs),
    "*" = lhs + rhs,
    "/" = if (rhs > 0) Inf else lhs,
    "**" = power_degree(expression, lhs, rhs),
    Inf
  )
}

power_degree <- function(expression, lhs, rhs) {
  if (lhs == 0) {
    return(0)
  }
  exponent <- expression$rhs
  if (rhs > 0 || !identical(exponent$type, "constant")) {
    return(Inf)
  }
  value <- exponent$value
  if (!isTRUE(value >= 0 && value == trunc(value))) {
    return(Inf)
  }
  lhs * value
}

math_degree <- function(expression) {
  degrees <- vapply(expression$arguments, expression_degree, numeric(1L))
  if (all(degrees == 0)) 0 else Inf
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
    "logical_operation" = c(
      expr_symbols(expression$lhs),
      if (is.null(expression$rhs)) list() else expr_symbols(expression$rhs)
    ),
    "conditional" = c(
      expr_symbols(expression$expression),
      expr_symbols(expression$condition)
    ),
    "same_as" = same_as_symbols(expression),
    "variable_attribute" = list(expression$reference$symbol),
    gamsr_abort(
      sprintf("Unsupported expression node type `%s`.", expression$type),
      class = "gamsr_error_invalid_expression"
    )
  )
}

same_as_symbols <- function(expression) {
  operands <- list(expression$lhs, expression$rhs)
  lapply(
    Filter(function(operand) identical(operand$type, "set"), operands),
    `[[`,
    "value"
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
