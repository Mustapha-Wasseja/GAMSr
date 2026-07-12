test_that("gams_problem validates objective and equations", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  j <- gams_set(model, "j", records = c("new-york", "chicago"))
  x <- gams_variable(model, "x", domain = c(i, j), type = "positive")
  supply <- gams_equation(model, "supply", domain = i)

  supply[i] <- gams_sum(j, x[i, j]) <= 1

  expect_error(
    gams_problem(model, equations = supply, objective = x[i, j]),
    class = "gamsr_error_invalid_objective"
  )
  expect_error(
    gams_problem(model, equations = c(supply, supply), objective = gams_sum(c(i, j), x[i, j])),
    class = "gamsr_error_duplicate_name"
  )
})

test_that("LP problems reject discrete variables", {
  model <- gams_model("discrete_model")
  x <- gams_variable(model, "x", type = "binary")
  e <- gams_equation(model, "e")
  e[] <- x <= 1

  expect_error(
    gams_problem(model, equations = e, objective = x, problem = "LP"),
    class = "gamsr_error_invalid_problem"
  )
  expect_s3_class(
    gams_problem(model, equations = e, objective = x, problem = "MIP"),
    "gams_problem"
  )
})

test_that("model_ir exposes normalized compiler data", {
  model <- gams_model("scalar_lp")
  x <- gams_variable(model, "x", type = "free")
  e <- gams_equation(model, "balance")
  e[] <- x >= 1

  problem <- gams_problem(model, equations = e, objective = x, sense = "min", problem = "LP")
  ir <- model_ir(problem)

  expect_s3_class(ir, "gams_ir")
  expect_identical(ir$problem_name, "scalar_lp_problem")
  expect_identical(ir$objective_variable, "GAMSr_obj_scalar_lp_problem")
  expect_identical(ir$objective_equation, "GAMSr_obj_def_scalar_lp_problem_def")
})

test_that("generated_gams renders a scalar LP deterministically", {
  expect_identical(generated_gams(scalar_lp_problem()), expected_gams_fixture("scalar-lp.gms"))
})

test_that("generated_gams renders the transportation model deterministically", {
  expect_identical(generated_gams(transport_problem()), expected_gams_fixture("transport.gms"))
})

test_that("write_gams writes source and protects existing files", {
  path <- file.path(withr::local_tempdir(), "scalar.gms")
  result <- write_gams(scalar_lp_problem(), path)

  expect_identical(result, normalizePath(path, winslash = "/", mustWork = TRUE))
  expect_identical(
    paste(readLines(path, warn = FALSE), collapse = "\n"),
    expected_gams_fixture("scalar-lp.gms")
  )
  expect_error(write_gams(scalar_lp_problem(), path), class = "gamsr_error_file_exists")
})

test_that("compile_gams writes a compile-only result object", {
  directory <- withr::local_tempdir()
  result <- compile_gams(transport_problem(), directory = directory)

  expect_s3_class(result, "gams_compilation")
  expect_true(file.exists(result$source_file))
  expect_identical(basename(result$source_file), "transport_problem.gms")
  expect_identical(result$source, expected_gams_fixture("transport.gms"))
})
