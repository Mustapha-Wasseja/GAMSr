new_gams_expr <- function(type, ..., free_indices = character()) {
  structure(
    list(
      type = type,
      ...,
      free_indices = unique(free_indices)
    ),
    class = c(paste0("gams_expr_", type), "gams_expr")
  )
}

is_gams_expr <- function(x) {
  inherits(x, "gams_expr")
}

as_gams_expr <- function(x, call = rlang::caller_env()) {
  if (is_gams_expr(x)) {
    return(x)
  }

  if (inherits(x, "gams_symbol")) {
    if (is_gams_index_set(x)) {
      gamsr_abort(
        sprintf("Set `%s` cannot be used as a numeric expression.", x$name),
        i = "Use `gams_ord()` or `gams_card()` when a numeric set value is required.",
        class = "gamsr_error_invalid_expression",
        call = call
      )
    }
    if (length(x$domain) > 0L) {
      gamsr_abort(
        sprintf("Symbol `%s` is indexed and must be referenced with `[]`.", x$name),
        i = sprintf("Use `%s[...]` with one index per domain set.", x$name),
        class = "gamsr_error_unindexed_symbol",
        call = call
      )
    }
    return(new_symbol_reference(x))
  }

  if (is.numeric(x) && length(x) == 1L && !is.na(x)) {
    return(new_gams_expr("constant", value = x))
  }

  if (is.logical(x) && length(x) == 1L && !is.na(x)) {
    return(new_gams_expr("constant", value = as.integer(x)))
  }

  gamsr_abort(
    "Object cannot be used as a GAMSr symbolic expression.",
    class = "gamsr_error_invalid_expression",
    call = call
  )
}

new_symbol_reference <- function(symbol) {
  new_gams_expr("symbol_reference", symbol = symbol)
}

new_indexed_reference <- function(symbol, indices) {
  new_gams_expr(
    "indexed_reference",
    symbol = symbol,
    indices = indices,
    free_indices = index_free_names(indices)
  )
}

new_binary_operation <- function(operator, lhs, rhs) {
  lhs <- as_gams_expr(lhs)
  rhs <- as_gams_expr(rhs)
  new_gams_expr(
    "binary_operation",
    operator = operator,
    lhs = lhs,
    rhs = rhs,
    free_indices = union(lhs$free_indices, rhs$free_indices)
  )
}

new_unary_operation <- function(operator, operand) {
  operand <- as_gams_expr(operand)
  new_gams_expr(
    "unary_operation",
    operator = operator,
    operand = operand,
    free_indices = operand$free_indices
  )
}

new_comparison <- function(relation, lhs, rhs) {
  lhs <- as_gams_expr(lhs)
  rhs <- as_gams_expr(rhs)
  new_gams_expr(
    "comparison",
    relation = relation,
    lhs = lhs,
    rhs = rhs,
    free_indices = union(lhs$free_indices, rhs$free_indices)
  )
}

new_sum_expression <- function(indices, expression, condition = NULL) {
  expression <- as_gams_expr(expression)
  condition <- if (is.null(condition)) NULL else as_gams_condition(condition)
  bound <- domain_names(indices)
  free_indices <- expression$free_indices
  if (!is.null(condition)) {
    free_indices <- union(free_indices, condition$free_indices)
  }
  new_gams_expr(
    "sum",
    indices = indices,
    expression = expression,
    condition = condition,
    free_indices = setdiff(free_indices, bound)
  )
}

new_math_function <- function(function_name, arguments) {
  arguments <- lapply(arguments, as_gams_expr)
  free <- unique(unlist(lapply(arguments, `[[`, "free_indices"), use.names = FALSE))
  new_gams_expr(
    "math_function",
    function_name = function_name,
    arguments = arguments,
    free_indices = free
  )
}

new_set_function <- function(function_name, set, free_indices = character()) {
  new_gams_expr(
    "set_function",
    function_name = function_name,
    set = set,
    free_indices = free_indices
  )
}

new_logical_operation <- function(operator, lhs, rhs = NULL) {
  lhs <- as_gams_condition(lhs)
  if (is.null(rhs)) {
    return(new_gams_expr(
      "logical_operation",
      operator = operator,
      lhs = lhs,
      rhs = NULL,
      free_indices = lhs$free_indices
    ))
  }

  rhs <- as_gams_condition(rhs)
  new_gams_expr(
    "logical_operation",
    operator = operator,
    lhs = lhs,
    rhs = rhs,
    free_indices = union(lhs$free_indices, rhs$free_indices)
  )
}

new_conditional_expression <- function(expression, condition) {
  expression <- as_gams_expr(expression)
  condition <- as_gams_condition(condition)
  new_gams_expr(
    "conditional",
    expression = expression,
    condition = condition,
    free_indices = union(expression$free_indices, condition$free_indices)
  )
}

new_same_as_expression <- function(lhs, rhs, call = rlang::caller_env()) {
  lhs <- normalize_same_as_operand(lhs, "lhs", call = call)
  rhs <- normalize_same_as_operand(rhs, "rhs", call = call)
  operands <- list(lhs, rhs)
  sets <- Filter(function(x) identical(x$type, "set"), operands)

  if (length(sets) == 0L) {
    gamsr_abort(
      "At least one `gams_same_as()` operand must be a GAMSr set or alias.",
      class = "gamsr_error_invalid_condition",
      call = call
    )
  }
  if (length(sets) == 2L && !identical(sets[[1L]]$value$model, sets[[2L]]$value$model)) {
    gamsr_abort(
      "Set operands in `gams_same_as()` must belong to the same model.",
      class = "gamsr_error_cross_model_domain",
      call = call
    )
  }

  free <- vapply(sets, function(x) x$value$name, character(1L), USE.NAMES = FALSE)
  new_gams_expr("same_as", lhs = lhs, rhs = rhs, free_indices = free)
}

normalize_same_as_operand <- function(x, arg, call = rlang::caller_env()) {
  if (is_gams_index_set(x)) {
    return(list(type = "set", value = x))
  }
  if (is.character(x) && length(x) == 1L && !is.na(x)) {
    return(list(type = "label", value = validate_label(x, arg, call = call)))
  }
  gamsr_abort(
    sprintf("`%s` must be a GAMSr set, alias, or single character label.", arg),
    class = "gamsr_error_invalid_condition",
    call = call
  )
}

as_gams_condition <- function(condition, call = rlang::caller_env()) {
  condition <- as_gams_expr(condition, call = call)
  if (contains_direct_variable(condition)) {
    gamsr_abort(
      "GAMS dollar conditions cannot contain decision variables.",
      i = "Use parameters, sets, ord/card, or symbolic variable attributes.",
      class = "gamsr_error_invalid_condition",
      call = call
    )
  }
  condition
}

contains_direct_variable <- function(expression) {
  switch(
    expression$type,
    "constant" = FALSE,
    "symbol_reference" = inherits(expression$symbol, "gams_variable"),
    "indexed_reference" = inherits(expression$symbol, "gams_variable"),
    "variable_attribute" = FALSE,
    "binary_operation" = contains_direct_variable(expression$lhs) ||
      contains_direct_variable(expression$rhs),
    "unary_operation" = contains_direct_variable(expression$operand),
    "comparison" = contains_direct_variable(expression$lhs) ||
      contains_direct_variable(expression$rhs),
    "sum" = contains_direct_variable(expression$expression) ||
      (!is.null(expression$condition) && contains_direct_variable(expression$condition)),
    "math_function" = any(vapply(
      expression$arguments,
      contains_direct_variable,
      logical(1L)
    )),
    "set_function" = FALSE,
    "logical_operation" = contains_direct_variable(expression$lhs) ||
      (!is.null(expression$rhs) && contains_direct_variable(expression$rhs)),
    "conditional" = contains_direct_variable(expression$expression) ||
      contains_direct_variable(expression$condition),
    "same_as" = FALSE,
    FALSE
  )
}

#' Apply a condition to an expression or equation relationship
#'
#' `gams_where()` applies a GAMS dollar condition. When `expression` is an
#' equation relationship assigned to an equation, the condition restricts the
#' equation's domain of definition. Otherwise, the expression contributes zero
#' when the condition is false.
#'
#' @param expression A symbolic algebraic expression or equation relationship.
#' @param condition A symbolic logical or numeric condition. Conditions cannot
#'   contain decision variables.
#'
#' @return A `gams_expr_conditional` object.
#' @export
gams_where <- function(expression, condition) {
  new_conditional_expression(expression, condition)
}

#' Compare set elements or labels
#'
#' @param lhs,rhs GAMSr sets, aliases, or single character labels. At least one
#'   operand must be a set or alias.
#'
#' @return A symbolic condition rendered with GAMS `sameAs()`.
#' @export
gams_same_as <- function(lhs, rhs) {
  new_same_as_expression(lhs, rhs)
}

#' Create a symbolic equality relation
#'
#' `gams_eq()` creates a symbolic equality relation for equation definitions.
#' It maps to the GAMS `=e=` equation relationship during rendering.
#'
#' @param lhs,rhs GAMSr symbolic expressions or numeric scalar constants.
#'
#' @return A `gams_expr_comparison` object.
#' @export
gams_eq <- function(lhs, rhs) {
  new_comparison("eq", lhs, rhs)
}

#' Format a symbolic expression as GAMS code
#'
#' @param expression A GAMSr symbolic expression.
#'
#' @return A single character string containing deterministic GAMS expression
#'   code.
#' @export
format_gams_expression <- function(expression) {
  expression <- as_gams_expr(expression)
  format_expr(expression, parent_precedence = 0L)
}

#' @export
print.gams_expr <- function(x, ...) {
  cat(format_gams_expression(x), "\n", sep = "")
  invisible(x)
}

gams_ops <- function(e1, e2) {
  if (missing(e2)) {
    switch(
      .Generic,
      "+" = as_gams_expr(e1),
      "-" = new_unary_operation("-", e1),
      "!" = new_logical_operation("not", e1),
      gamsr_abort(
        sprintf("Unary operator `%s` is not supported for GAMSr expressions.", .Generic),
        class = "gamsr_error_unsupported_operator"
      )
    )
  } else {
    gams_binary_or_comparison(.Generic, e1, e2)
  }
}

#' @export
Ops.gams_expr <- gams_ops

#' @export
Ops.gams_symbol <- gams_ops

gams_binary_or_comparison <- function(operator, lhs, rhs) {
  switch(
    operator,
    "+" = new_binary_operation("+", lhs, rhs),
    "-" = new_binary_operation("-", lhs, rhs),
    "*" = new_binary_operation("*", lhs, rhs),
    "/" = new_binary_operation("/", lhs, rhs),
    "^" = new_binary_operation("**", lhs, rhs),
    "<=" = new_comparison("le", lhs, rhs),
    ">=" = new_comparison("ge", lhs, rhs),
    "==" = new_comparison("eq", lhs, rhs),
    "<" = new_comparison("lt", lhs, rhs),
    ">" = new_comparison("gt", lhs, rhs),
    "!=" = new_comparison("ne", lhs, rhs),
    "&" = new_logical_operation("and", lhs, rhs),
    "|" = new_logical_operation("or", lhs, rhs),
    gamsr_abort(
      sprintf("Operator `%s` is not supported for GAMSr expressions.", operator),
      i = paste(
        "Supported operators are +, -, *, /, ^, <, <=, ==, !=, >=, >,",
        "&, |, !, and gams_eq()."
      ),
      class = "gamsr_error_unsupported_operator"
    )
  )
}

expr_precedence <- function(expression) {
  switch(
    expression$type,
    "comparison" = 10L,
    "logical_operation" = 5L,
    "conditional" = 5L,
    "binary_operation" = switch(
      expression$operator,
      "+" = 20L,
      "-" = 20L,
      "*" = 30L,
      "/" = 30L,
      "**" = 40L,
      20L
    ),
    "unary_operation" = 35L,
    100L
  )
}

format_expr <- function(expression, parent_precedence = 0L) {
  current_precedence <- expr_precedence(expression)
  out <- switch(
    expression$type,
    "constant" = format_constant(expression$value),
    "symbol_reference" = expression$symbol$name,
    "indexed_reference" = format_indexed_reference(expression),
    "binary_operation" = format_binary_operation(expression),
    "unary_operation" = paste0(expression$operator, format_expr(expression$operand, 35L)),
    "comparison" = format_comparison(expression),
    "sum" = format_sum_expression(expression),
    "math_function" = format_math_expression(expression),
    "set_function" = paste0(expression$function_name, "(", expression$set$name, ")"),
    "logical_operation" = format_condition_expr(expression),
    "conditional" = format_conditional_expression(expression),
    "same_as" = format_same_as_expression(expression),
    "variable_attribute" = format_variable_attribute(expression),
    gamsr_abort(
      sprintf("Unsupported expression node type `%s`.", expression$type),
      class = "gamsr_error_invalid_expression"
    )
  )

  if (current_precedence < parent_precedence) {
    paste0("(", out, ")")
  } else {
    out
  }
}

format_constant <- function(value) {
  if (!is.finite(value)) {
    gamsr_abort(
      "Only finite numeric constants are supported in symbolic expressions.",
      class = "gamsr_error_invalid_expression"
    )
  }
  format(value, scientific = FALSE, trim = TRUE)
}

format_indexed_reference <- function(expression) {
  paste0(
    expression$symbol$name,
    "(",
    paste(vapply(expression$indices, format_index, character(1L)), collapse = ","),
    ")"
  )
}

format_index <- function(index) {
  if (is_gams_index_set(index)) {
    return(index$name)
  }

  if (is.character(index) && length(index) == 1L && !is.na(index)) {
    return(quote_gams_label_literal(index))
  }

  gamsr_abort(
    "Unsupported symbolic index.",
    class = "gamsr_error_invalid_index"
  )
}

format_binary_operation <- function(expression) {
  if (identical(expression$operator, "**") && is_integer_power(expression$rhs)) {
    return(paste0(
      "power(",
      format_expr(expression$lhs, 0L),
      ", ",
      format_expr(expression$rhs, 0L),
      ")"
    ))
  }
  left <- format_expr(expression$lhs, expr_precedence(expression))
  right <- format_expr(expression$rhs, expr_precedence(expression) + 1L)
  paste(left, expression$operator, right)
}

is_integer_power <- function(expression) {
  identical(expression$type, "constant") &&
    isTRUE(expression$value == trunc(expression$value))
}

format_comparison <- function(expression) {
  relation <- switch(
    expression$relation,
    "eq" = "=e=",
    "le" = "=l=",
    "ge" = "=g=",
    "lt" = "<",
    "gt" = ">",
    "ne" = "<>",
    gamsr_abort(
      sprintf("Unsupported equation relation `%s`.", expression$relation),
      class = "gamsr_error_invalid_expression"
    )
  )
  paste(
    format_expr(expression$lhs, expr_precedence(expression)),
    relation,
    format_expr(expression$rhs, expr_precedence(expression))
  )
}

format_sum_expression <- function(expression) {
  index_text <- if (length(expression$indices) == 1L) {
    domain_names(expression$indices)
  } else {
    paste0("(", paste(domain_names(expression$indices), collapse = ","), ")")
  }
  if (!is.null(expression$condition)) {
    index_text <- paste0(index_text, "$(", format_condition_expr(expression$condition), ")")
  }

  paste0(
    "sum(",
    index_text,
    ", ",
    format_expr(expression$expression, 0L),
    ")"
  )
}

format_condition_expr <- function(expression, parent_precedence = 0L) {
  current_precedence <- condition_precedence(expression)
  out <- switch(
    expression$type,
    "comparison" = format_condition_comparison(expression),
    "logical_operation" = format_logical_operation(expression),
    "same_as" = format_same_as_expression(expression),
    format_expr(expression, 0L)
  )

  if (current_precedence < parent_precedence) paste0("(", out, ")") else out
}

condition_precedence <- function(expression) {
  if (identical(expression$type, "comparison")) {
    return(40L)
  }
  if (identical(expression$type, "logical_operation")) {
    return(switch(expression$operator, "not" = 30L, "and" = 20L, "or" = 10L, 10L))
  }
  100L
}

format_condition_comparison <- function(expression) {
  relation <- switch(
    expression$relation,
    "eq" = "=",
    "le" = "<=",
    "ge" = ">=",
    "lt" = "<",
    "gt" = ">",
    "ne" = "<>",
    gamsr_abort(
      sprintf("Unsupported condition relation `%s`.", expression$relation),
      class = "gamsr_error_invalid_condition"
    )
  )
  paste(format_expr(expression$lhs, 0L), relation, format_expr(expression$rhs, 0L))
}

format_logical_operation <- function(expression) {
  if (identical(expression$operator, "not")) {
    return(paste0("not (", format_condition_expr(expression$lhs, 0L), ")"))
  }
  precedence <- condition_precedence(expression)
  paste(
    format_condition_expr(expression$lhs, precedence),
    expression$operator,
    format_condition_expr(expression$rhs, precedence + 1L)
  )
}

format_conditional_expression <- function(expression) {
  term <- format_expr(expression$expression, 0L)
  if (!(expression$expression$type %in% c(
    "constant", "symbol_reference", "indexed_reference", "math_function", "set_function"
  ))) {
    term <- paste0("(", term, ")")
  }
  paste0(term, "$(", format_condition_expr(expression$condition), ")")
}

format_same_as_expression <- function(expression) {
  paste0(
    "sameAs(",
    format_same_as_operand(expression$lhs),
    ",",
    format_same_as_operand(expression$rhs),
    ")"
  )
}

format_same_as_operand <- function(operand) {
  if (identical(operand$type, "set")) {
    return(operand$value$name)
  }
  quote_gams_label_literal(operand$value)
}

quote_gams_label_literal <- function(label) {
  label <- validate_label(label)
  if (!grepl("'", label, fixed = TRUE)) {
    return(paste0("'", label, "'"))
  }
  if (!grepl('"', label, fixed = TRUE)) {
    return(paste0('"', label, '"'))
  }
  gamsr_abort(
    "Labels containing both single and double quotes are not supported yet.",
    class = "gamsr_error_invalid_label"
  )
}

format_math_expression <- function(expression) {
  paste0(
    expression$function_name,
    "(",
    paste(vapply(expression$arguments, format_expr, character(1L)), collapse = ", "),
    ")"
  )
}
