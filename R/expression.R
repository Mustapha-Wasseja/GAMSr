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

new_sum_expression <- function(indices, expression) {
  expression <- as_gams_expr(expression)
  bound <- domain_names(indices)
  new_gams_expr(
    "sum",
    indices = indices,
    expression = expression,
    free_indices = setdiff(expression$free_indices, bound)
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

#' @export
Ops.gams_expr <- function(e1, e2) {
  if (missing(e2)) {
    switch(
      .Generic,
      "+" = as_gams_expr(e1),
      "-" = new_unary_operation("-", e1),
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
Ops.gams_symbol <- function(e1, e2) {
  if (missing(e2)) {
    switch(
      .Generic,
      "+" = as_gams_expr(e1),
      "-" = new_unary_operation("-", e1),
      gamsr_abort(
        sprintf("Unary operator `%s` is not supported for GAMSr symbols.", .Generic),
        class = "gamsr_error_unsupported_operator"
      )
    )
  } else {
    gams_binary_or_comparison(.Generic, e1, e2)
  }
}

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
    gamsr_abort(
      sprintf("Operator `%s` is not supported for GAMSr expressions.", operator),
      i = "Supported operators are +, -, *, /, ^, <=, >=, ==, and gams_eq().",
      class = "gamsr_error_unsupported_operator"
    )
  )
}

expr_precedence <- function(expression) {
  switch(
    expression$type,
    "comparison" = 10L,
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
  if (inherits(index, "gams_set")) {
    return(index$name)
  }

  if (is.character(index) && length(index) == 1L && !is.na(index)) {
    return(format_gams_label(index))
  }

  gamsr_abort(
    "Unsupported symbolic index.",
    class = "gamsr_error_invalid_index"
  )
}

format_binary_operation <- function(expression) {
  left <- format_expr(expression$lhs, expr_precedence(expression))
  right <- format_expr(expression$rhs, expr_precedence(expression) + 1L)
  paste(left, expression$operator, right)
}

format_comparison <- function(expression) {
  relation <- switch(
    expression$relation,
    "eq" = "=e=",
    "le" = "=l=",
    "ge" = "=g=",
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

  paste0(
    "sum(",
    index_text,
    ", ",
    format_expr(expression$expression, 0L),
    ")"
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
