write_fake_result_gdx <- function(problem, file) {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")

  Container <- getExportedValue("gamstransfer", "Container")
  Set <- getExportedValue("gamstransfer", "Set")
  Variable <- getExportedValue("gamstransfer", "Variable")
  Equation <- getExportedValue("gamstransfer", "Equation")
  Parameter <- getExportedValue("gamstransfer", "Parameter")

  model <- problem$model
  container <- Container$new()
  set_objects <- new.env(parent = emptyenv())
  for (set in Filter(function(symbol) inherits(symbol, "gams_set"), model$symbols())) {
    assign(
      tolower(set$name),
      Set$new(container, set$name, records = set$records$label),
      envir = set_objects
    )
  }

  x <- model$get_symbol("x")
  Variable$new(
    container,
    "x",
    type = x$type,
    domain = lapply(domain_names(x$domain), function(name) {
      get(tolower(name), envir = set_objects, inherits = FALSE)
    }),
    records = data.frame(
      i = c("seattle", "seattle", "san-diego", "san-diego"),
      j = c("new-york", "chicago", "new-york", "chicago"),
      level = c(10, 20, 30, 40),
      marginal = c(0, 0, 0, 0),
      lower = c(0, 0, 0, 0),
      upper = c(Inf, Inf, Inf, Inf)
    )
  )

  supply <- model$get_symbol("supply")
  Equation$new(
    container,
    "supply",
    type = "leq",
    domain = lapply(domain_names(supply$domain), function(name) {
      get(tolower(name), envir = set_objects, inherits = FALSE)
    }),
    records = data.frame(
      i = c("seattle", "san-diego"),
      level = c(30, 70),
      marginal = c(0, 0),
      lower = c(-Inf, -Inf),
      upper = c(350, 600)
    )
  )

  demand <- model$get_symbol("demand")
  Equation$new(
    container,
    "demand",
    type = "geq",
    domain = lapply(domain_names(demand$domain), function(name) {
      get(tolower(name), envir = set_objects, inherits = FALSE)
    }),
    records = data.frame(
      j = c("new-york", "chicago", "topeka"),
      level = c(40, 60, 0),
      marginal = c(0, 0, 0),
      lower = c(325, 300, 275),
      upper = c(Inf, Inf, Inf)
    )
  )

  Parameter$new(container, "GAMSr_modelstat", records = 1)
  Parameter$new(container, "GAMSr_solvestat", records = 1)
  Parameter$new(container, "GAMSr_objective_value", records = 123)
  container$write(file)
}

test_that("read_solution_gdx imports status and symbol records", {
  problem <- transport_problem()
  path <- file.path(withr::local_tempdir(), "results.gdx")
  write_fake_result_gdx(problem, path)

  data <- read_solution_gdx(problem, path)

  expect_s3_class(data, "gams_result_data")
  expect_identical(data$model_status$code, 1L)
  expect_identical(data$model_status$label, "OptimalGlobal")
  expect_identical(data$solver_status$code, 1L)
  expect_identical(data$solver_status$label, "Normal")
  expect_identical(data$objective, 123)
  expect_equal(nrow(data$variables$x), 4)
  expect_equal(nrow(data$equations$supply), 2)
})

test_that("result accessors return statuses and tidy records", {
  problem <- transport_problem()
  path <- file.path(withr::local_tempdir(), "results.gdx")
  write_fake_result_gdx(problem, path)
  data <- read_solution_gdx(problem, path)

  result <- GAMSr:::new_gams_result(
    problem = problem,
    compilation = NULL,
    process = list(status = 0, stdout = "", stderr = ""),
    result_file = path,
    variables = data$variables,
    equations = data$equations,
    objective = data$objective,
    model_status = data$model_status,
    solver_status = data$solver_status,
    files_retained = TRUE
  )

  expect_identical(objective_value(result), 123)
  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(nrow(variable_values(result, "x")), 4)
  expect_equal(nrow(variable_values(result, problem$model$get_symbol("x"))), 4)
  expect_equal(nrow(equation_values(result, "supply")), 2)
})

test_that("solve fails clearly when GAMS is unavailable", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if(gams_available(), "GAMS is installed")

  expect_error(
    solve(scalar_lp_problem()),
    class = "gamsr_error_gams_unavailable"
  )
})

test_that("solve builds safe GAMS command arguments", {
  args <- GAMSr:::solve_command_args(
    scalar_lp_problem(),
    source_file = "scalar_lp_problem-solve.gms",
    solver = "soplex",
    gams_options = list(reslim = 60, threads = 2, optcr = 0.01, profile = TRUE)
  )

  expect_identical(
    args,
    c(
      "scalar_lp_problem-solve.gms",
      "lo=2",
      "lp=soplex",
      "reslim=60",
      "threads=2",
      "optcr=0.01",
      "profile=1"
    )
  )
  expect_error(
    GAMSr:::solve_command_args(scalar_lp_problem(), "model.gms", solver = "bad-name"),
    class = "gamsr_error_invalid_option"
  )
  expect_error(
    GAMSr:::solve_command_args(
      scalar_lp_problem(),
      "model.gms",
      gams_options = list(`bad-name` = 1)
    ),
    class = "gamsr_error_invalid_option"
  )
  expect_error(
    GAMSr:::solve_command_args(
      scalar_lp_problem(),
      "model.gms",
      gams_options = list(reslim = c(1, 2))
    ),
    class = "gamsr_error_invalid_option"
  )
})

test_that("solve runs a scalar LP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(scalar_lp_problem(), keep = TRUE, gams_options = list(limrow = 0, limcol = 0))

  expect_s3_class(result, "gams_result")
  expect_identical(model_status(result)$code, 1L)
  expect_identical(solver_status(result)$code, 1L)
  expect_equal(objective_value(result), 1)
  expect_true(file.exists(result$result_file))
  expect_equal(variable_values(result, "x")$level[[1L]], 1)
})

test_that("solve runs the transportation LP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(transport_problem(), gams_options = list(limrow = 0, limcol = 0))
  x <- variable_values(result, "x")

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 153.675)
  expect_equal(nrow(x), 6)
  expect_equal(sum(x$level), 900)
})

test_that("execution source appends result unload statements", {
  problem <- scalar_lp_problem()
  source <- GAMSr:::append_result_unload(generated_gams(problem), model_ir(problem), "results.gdx")

  expect_match(
    source,
    "Scalar GAMSr_modelstat, GAMSr_solvestat, GAMSr_objective_value",
    fixed = TRUE
  )
  expect_match(source, "GAMSr_modelstat = scalar_lp_problem.modelstat", fixed = TRUE)
  expect_match(source, "execute_unload \"results.gdx\"", fixed = TRUE)
})
