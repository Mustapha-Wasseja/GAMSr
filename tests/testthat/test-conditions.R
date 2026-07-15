test_that("logical conditions format with deterministic GAMS operators", {
  model <- gams_model("conditions")
  i <- gams_set(model, "i", records = c("a", "b", "c"))
  active <- gams_parameter(model, "active", domain = i, records = c(a = 1, b = 0, c = 1))

  condition <- (active[i] > 0) & !gams_same_as(i, "b")

  expect_identical(
    format_gams_expression(condition),
    "active(i) > 0 and not (sameAs(i,'b'))"
  )
  expect_identical(
    format_gams_expression((active[i] <= 1) | (active[i] != 0)),
    "active(i) <= 1 or active(i) <> 0"
  )
})

test_that("filtered sums bind condition indices", {
  model <- gams_model("filtered_sum")
  i <- gams_set(model, "i", records = c("a", "b"))
  j <- gams_set(model, "j", records = c("x", "y"))
  active <- gams_parameter(model, "active", domain = c(i, j))
  x <- gams_variable(model, "x", domain = c(i, j))

  expression <- gams_sum(j, x[i, j], condition = active[i, j] != 0)

  expect_identical(expression$free_indices, "i")
  expect_identical(
    format_gams_expression(expression),
    "sum(j$(active(i,j) <> 0), x(i,j))"
  )
})

test_that("gams_where supports conditional terms and equation domains", {
  model <- gams_model("where")
  i <- gams_set(model, "i", records = c("a", "b"))
  active <- gams_parameter(model, "active", domain = i, records = c(a = 1, b = 0))
  x <- gams_variable(model, "x", domain = i, type = "positive")
  floor <- gams_equation(model, "floor", domain = i)

  term <- gams_where(active[i] * 2, active[i] > 0)
  floor[i] <- gams_where(x[i] >= gams_ord(i), active[i] == 1)

  expect_identical(
    format_gams_expression(term),
    "(active(i) * 2)$(active(i) > 0)"
  )
  expect_s3_class(floor$definition$expression, "gams_expr_conditional")
  expect_identical(floor$definition$expression$free_indices, "i")
})

test_that("conditions reject variables and invalid equation relations", {
  model <- gams_model("condition_validation")
  i <- gams_set(model, "i", records = "a")
  p <- gams_parameter(model, "p", domain = i, records = c(a = 1))
  x <- gams_variable(model, "x", domain = i)
  e <- gams_equation(model, "e", domain = i)

  expect_error(
    gams_where(p[i], x[i] > 0),
    class = "gamsr_error_invalid_condition"
  )
  expect_error(
    gams_sum(i, p[i], condition = x[i] != 0),
    class = "gamsr_error_invalid_condition"
  )
  expect_error(
    e[i] <- x[i] < 1,
    class = "gamsr_error_invalid_equation_definition"
  )
})

test_that("sameAs validates operands and model ownership", {
  model <- gams_model("same_as")
  i <- gams_set(model, "i", records = "a")
  ip <- gams_alias(i, "ip")
  other_model <- gams_model("other")
  j <- gams_set(other_model, "j", records = "a")

  expect_identical(format_gams_expression(gams_same_as(i, ip)), "sameAs(i,ip)")
  expect_identical(gams_same_as(i, ip)$free_indices, c("i", "ip"))
  expect_error(gams_same_as("a", "b"), class = "gamsr_error_invalid_condition")
  expect_error(gams_same_as(i, j), class = "gamsr_error_cross_model_domain")
})

test_that("compiler renders conditional equations and algebraic terms", {
  problem <- conditional_lp_problem()
  source <- generated_gams(problem)

  expect_match(
    source,
    "floor(i)$(active(i) = 1).. x(i) =g= ord(i);",
    fixed = TRUE
  )
  expect_match(
    source,
    "sum(i$(ord(i) <= card(i)), x(i)$(active(i) = 1))",
    fixed = TRUE
  )
})
