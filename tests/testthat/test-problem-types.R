test_that("problem types reject incompatible polynomial degree", {
  model <- gams_model("problem_degree")
  x <- gams_variable(model, "x", type = "positive")
  quadratic <- gams_equation(model, "quadratic")
  cubic <- gams_equation(model, "cubic")
  quadratic[] <- x^2 <= 4
  cubic[] <- x^3 <= 8

  expect_error(
    gams_problem(model, quadratic, x, problem = "LP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_s3_class(
    gams_problem(model, quadratic, x, problem = "QCP"),
    "gams_problem"
  )
  expect_error(
    gams_problem(model, cubic, x, problem = "QCP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_s3_class(
    gams_problem(model, cubic, x, problem = "NLP"),
    "gams_problem"
  )
})

test_that("problem types validate discrete variables", {
  model <- gams_model("discrete_problem_types")
  x <- gams_variable(model, "x", type = "positive")
  y <- gams_variable(model, "y", type = "binary")
  link <- gams_equation(model, "link")
  link[] <- x <= y

  expect_error(
    gams_problem(model, link, x, problem = "NLP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_s3_class(
    gams_problem(model, link, x, problem = "MINLP"),
    "gams_problem"
  )
  expect_s3_class(
    gams_problem(model, link, x, problem = "RMIQCP"),
    "gams_problem"
  )
})

test_that("division and intrinsic functions require nonlinear model types", {
  model <- gams_model("nonpolynomial_degree")
  x <- gams_variable(model, "x", type = "positive")
  denominator <- gams_equation(model, "denominator")
  intrinsic <- gams_equation(model, "intrinsic")
  denominator[] <- 1 / (x + 1) <= 1
  intrinsic[] <- gams_sqrt(x + 1) <= 2

  expect_error(
    gams_problem(model, denominator, x, problem = "QCP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_error(
    gams_problem(model, intrinsic, x, problem = "QCP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_s3_class(
    gams_problem(model, intrinsic, x, problem = "NLP"),
    "gams_problem"
  )
})
