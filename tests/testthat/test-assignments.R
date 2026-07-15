test_that("dynamic sets register as computed subsets", {
  model <- gams_model("dynamic_sets")
  i <- gams_set(model, "i", records = c("a", "b"))
  active <- gams_dynamic_set(model, "active", domain = i)

  expect_s3_class(active, "gams_dynamic_set")
  expect_identical(domain_names(active$domain), "i")
  expect_true(active$dynamic)
  expect_identical(model$get_symbol("active"), active)
  expect_error(
    gams_dynamic_set(model, "scalar_dynamic", domain = NULL),
    class = "gamsr_error_invalid_domain"
  )
})

test_that("replacement syntax registers ordered assignments", {
  model <- gams_model("assignment_order")
  i <- gams_set(model, "i", records = c("a", "b"))
  p <- gams_parameter(model, "p", domain = i, records = c(a = 1, b = 2))
  q <- gams_parameter(model, "q", domain = i, records = c(a = 0, b = 0))
  active <- gams_dynamic_set(model, "active", domain = i)

  active[i] <- p[i] > 1
  q[i] <- p[i] * 2

  assignments <- model$assignments()
  expect_length(assignments, 2L)
  expect_identical(assignments[[1L]]$target$symbol$name, "active")
  expect_identical(assignments[[2L]]$target$symbol$name, "q")
})

test_that("conditional parameter assignments validate scope and ownership", {
  model <- gams_model("assignment_validation")
  i <- gams_set(model, "i", records = c("a", "b"))
  j <- gams_set(model, "j", records = c("x", "y"))
  p <- gams_parameter(model, "p", domain = i, records = c(a = 1, b = 2))
  q <- gams_parameter(model, "q", domain = i, records = c(a = 0, b = 0))
  x <- gams_variable(model, "x", domain = i)
  other_model <- gams_model("other")
  other <- gams_parameter(other_model, "other", records = 1)

  expect_error(
    gams_assign(q[i], x[i]),
    class = "gamsr_error_invalid_assignment"
  )
  expect_error(
    gams_assign(q[i], p[i] + gams_ord(j)),
    class = "gamsr_error_unresolved_index"
  )
  expect_error(
    gams_assign(q[i], p[i] + other),
    class = "gamsr_error_invalid_assignment"
  )
  expect_error(
    gams_assign(i, TRUE),
    class = "gamsr_error_invalid_assignment"
  )
})

test_that("variable attributes are symbolic condition expressions", {
  model <- gams_model("attribute_expressions")
  i <- gams_set(model, "i", records = c("a", "b"))
  p <- gams_parameter(model, "p", domain = i, records = c(a = 1, b = 2))
  x <- gams_variable(model, "x", domain = i)

  expect_identical(format_gams_expression(gams_level(x[i])), "x.l(i)")
  expect_identical(format_gams_expression(gams_marginal(x[i])), "x.m(i)")
  expect_identical(format_gams_expression(gams_lower_bound(x[i])), "x.lo(i)")
  expect_identical(format_gams_expression(gams_upper_bound(x[i])), "x.up(i)")
  expect_identical(format_gams_expression(p["a"]), "p('a')")
  expect_identical(format_gams_expression(gams_level(x["a"])), "x.l('a')")
  expect_identical(
    format_gams_expression(gams_where(p[i], gams_level(x[i]) > 0)),
    "p(i)$(x.l(i) > 0)"
  )
  expect_error(
    gams_where(p[i], x[i] > 0),
    class = "gamsr_error_invalid_condition"
  )
})

test_that("compiler renders dynamic and conditional assignments in order", {
  source <- generated_gams(dynamic_assignment_problem())

  expect_match(source, "Set active(i);", fixed = TRUE)
  expect_match(source, "x.l('a') = 1;", fixed = TRUE)
  expect_match(source, "x.l('b') = 0;", fixed = TRUE)
  expect_match(source, "active(i) = x.l(i) > 0;", fixed = TRUE)
  expect_match(
    source,
    "target(i)$(active(i)) = p(i) * 2;",
    fixed = TRUE
  )
  expect_match(
    source,
    "floor(i)$(active(i)).. x(i) =g= target(i);",
    fixed = TRUE
  )
  expect_lt(
    regexpr("active\\(i\\) =", source)[[1L]],
    regexpr("target\\(i\\)\\$", source)[[1L]]
  )
})

test_that("GDX source declares but does not load dynamic sets", {
  source <- GAMSr:::render_gams_ir(
    model_ir(dynamic_assignment_problem()),
    data_source = "gdx",
    input_file = "input.gdx"
  )

  expect_match(source, "Set active(i);", fixed = TRUE)
  expect_match(source, "$load i p target", fixed = TRUE)
  expect_no_match(source, "$load i active", fixed = TRUE)
})

test_that("compiler renders initialized variable attributes", {
  model <- gams_model("initialized_attributes")
  i <- gams_set(model, "i", records = c("a", "b"))
  x <- gams_variable(model, "x", domain = i)
  x <- set_lower_bound(x, c(a = 0, b = 1))
  x <- set_upper_bound(x, c(a = 10, b = 20))
  x <- set_level(x, c(a = 2, b = 3))
  x <- set_fixed(x, c(a = 4, b = 5))
  balance <- gams_equation(model, "balance", domain = i)
  balance[i] <- x[i] >= 0
  problem <- gams_problem(
    model,
    equations = balance,
    objective = gams_sum(i, x[i]),
    sense = "min",
    problem = "LP"
  )
  source <- generated_gams(problem)

  expect_match(source, "x.lo('a') = 0;", fixed = TRUE)
  expect_match(source, "x.up('b') = 20;", fixed = TRUE)
  expect_match(source, "x.l('a') = 2;", fixed = TRUE)
  expect_match(source, "x.fx('b') = 5;", fixed = TRUE)
})
