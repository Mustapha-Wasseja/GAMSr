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
  metric <- Set$new(container, "GAMSr_solve_metric", records = GAMSr:::.solve_summary_metrics)
  Parameter$new(
    container,
    "GAMSr_solve_summary",
    domain = list(metric),
    records = data.frame(
      GAMSr_solve_metric = c("model_status", "solver_status", "objective_value", "iterations"),
      value = c(1, 1, 123, 4)
    )
  )
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
  expect_equal(data$summary$value[data$summary$metric == "objective_value"], 123)
  expect_equal(data$summary$value[data$summary$metric == "iterations"], 4)
  expect_equal(data$summary$value[data$summary$metric == "sum_infeasibilities"], 0)
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
    files_retained = TRUE,
    summary = data$summary,
    files = list(result_file = path),
    command = list(data = "gdx")
  )

  expect_identical(objective_value(result), 123)
  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(solve_summary(result)$value[solve_summary(result)$metric == "iterations"], 4)
  expect_identical(result_files(result)$result_file, path)
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
    gams_options = list(reslim = 60, threads = 2, optcr = 0.01, profile = TRUE),
    use_solver_option_file = TRUE
  )

  expect_identical(
    args,
    c(
      "scalar_lp_problem-solve.gms",
      "lo=2",
      "lp=soplex",
      "optfile=1",
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
  expect_error(
    GAMSr:::solve_command_args(
      scalar_lp_problem(),
      "model.gms",
      gams_options = list(optfile = 2),
      use_solver_option_file = TRUE
    ),
    class = "gamsr_error_invalid_option"
  )
})

test_that("solver option files are written safely", {
  directory <- withr::local_tempdir()
  path <- GAMSr:::write_solver_option_file(
    directory,
    solver = "soplex",
    solver_options = list(feasibility_tolerance = 1e-7, display = FALSE)
  )

  expect_identical(basename(path), "soplex.opt")
  expect_identical(
    readLines(path, warn = FALSE),
    c("feasibility_tolerance 0.0000001", "display 0")
  )
  expect_error(
    GAMSr:::write_solver_option_file(
      directory,
      solver = NULL,
      solver_options = list(display = FALSE)
    ),
    class = "gamsr_error_invalid_option"
  )
  expect_error(
    GAMSr:::write_solver_option_file(
      directory,
      solver = "soplex",
      solver_options = list(`bad-name` = 1)
    ),
    class = "gamsr_error_invalid_option"
  )
})

test_that("execution source can load input data from GDX", {
  source <- GAMSr:::render_gams_ir(
    model_ir(transport_problem()),
    data_source = "gdx",
    input_file = "input.gdx"
  )

  expect_match(source, "$gdxIn \"input.gdx\"", fixed = TRUE)
  expect_match(source, "$load i j a b c", fixed = TRUE)
  expect_no_match(source, "seattle, san-diego", fixed = TRUE)
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
  expect_true(is.na(result_files(result)$input_file))
  expect_equal(variable_values(result, "x")$level[[1L]], 1)
})

test_that("solve runs the transportation LP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(transport_problem(), keep = TRUE, gams_options = list(limrow = 0, limcol = 0))
  x <- variable_values(result, "x")

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 153.675)
  expect_equal(nrow(x), 6)
  expect_equal(sum(x$level), 900)
  expect_true(file.exists(result_files(result)$input_file))
  expect_true(file.exists(result_files(result)$listing_file))
  summary <- solve_summary(result)
  expect_equal(summary$value[summary$metric == "objective_value"], 153.675)
  expect_equal(summary$value[summary$metric == "sum_infeasibilities"], 0)
})

test_that("solve runs a binary MIP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(binary_mip_problem(), gams_options = list(limrow = 0, limcol = 0))

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 1)
  expect_equal(variable_values(result, "y")$level[[1L]], 1)
})

test_that("solve runs an alias and ordered-set LP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(ordered_alias_problem(), gams_options = list(limrow = 0, limcol = 0))
  x <- variable_values(result, "x")

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 2)
  expect_equal(x$level, c(1 / 3, 2 / 3, 1), tolerance = 1e-8)
})

test_that("solve runs a conditionally generated LP with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(conditional_lp_problem(), gams_options = list(limrow = 0, limcol = 0))
  x <- variable_values(result, "x")
  floor <- equation_values(result, "floor")

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 4)
  expect_equal(as.character(x$i), c("a", "c"))
  expect_equal(x$level, c(1, 3))
  expect_equal(as.character(floor$i), c("a", "c"))
})

test_that("solve runs dynamic and conditional assignments with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(
    dynamic_assignment_problem(),
    gams_options = list(limrow = 0, limcol = 0)
  )
  x <- variable_values(result, "x")
  floor <- equation_values(result, "floor")

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 8)
  expect_equal(as.character(x$i), c("a", "c"))
  expect_equal(x$level, c(2, 6))
  expect_equal(as.character(floor$i), c("a", "c"))
})

test_that("solve reports infeasible LP status with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(infeasible_lp_problem(), gams_options = list(limrow = 0, limcol = 0))

  expect_identical(model_status(result)$label, "InfeasibleNoSolution")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(solve_summary(result)$value[solve_summary(result)$metric == "model_status"], 19)
})

test_that("solve runs nonlinear and quadratic model classes with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  nlp <- solve(
    nonlinear_nlp_problem(),
    gams_options = list(limrow = 0, limcol = 0)
  )
  qcp <- solve(
    quadratic_qcp_problem(),
    solver = "cplex",
    gams_options = list(limrow = 0, limcol = 0)
  )

  expect_true(model_status(nlp)$label %in% c("OptimalGlobal", "OptimalLocal"))
  expect_identical(solver_status(nlp)$label, "Normal")
  expect_equal(objective_value(nlp), 0, tolerance = 1e-8)
  expect_equal(variable_values(nlp, "x")$level[[1L]], 3, tolerance = 1e-6)
  expect_identical(model_status(qcp)$label, "OptimalGlobal")
  expect_identical(solver_status(qcp)$label, "Normal")
  expect_equal(objective_value(qcp), 2, tolerance = 1e-7)
})

test_that("solve runs a mixed-integer nonlinear model with local GAMS", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(
    mixed_integer_nlp_problem(),
    solver = "sbb",
    gams_options = list(limrow = 0, limcol = 0)
  )

  expect_true(model_status(result)$label %in% c("OptimalGlobal", "OptimalLocal"))
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 0, tolerance = 1e-7)
  expect_equal(variable_values(result, "x")$level[[1L]], 2, tolerance = 1e-5)
  expect_equal(variable_values(result, "y")$level[[1L]], 1)
})

test_that("unbounded status is stable across local LP solvers", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  solvers <- c("cplex", "highs", "soplex")
  for (solver in solvers) {
    solver_options <- if (identical(solver, "cplex")) list(preind = 0) else NULL
    result <- solve(
      unbounded_lp_problem(),
      solver = solver,
      solver_options = solver_options,
      gams_options = list(limrow = 0, limcol = 0)
    )

    expect_true(model_status(result)$label %in% c("Unbounded", "UnboundedNoSolution"))
    expect_identical(solver_status(result)$label, "Normal")
  }
})

test_that("licensed GAMS solves a model beyond the demo limit", {
  skip_if_not(
    identical(tolower(Sys.getenv("GAMSR_RUN_LICENSED_TESTS")), "true"),
    "licensed integration tests are disabled"
  )
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")
  skip_if_not(gams_available(), "GAMS is not installed")

  result <- solve(
    large_licensed_lp_problem(),
    solver = "highs",
    gams_options = list(limrow = 0, limcol = 0)
  )

  expect_identical(model_status(result)$label, "OptimalGlobal")
  expect_identical(solver_status(result)$label, "Normal")
  expect_equal(objective_value(result), 2100)
  expect_equal(nrow(variable_values(result, "x")), 2100)
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
  expect_match(source, "Parameter GAMSr_solve_summary", fixed = TRUE)
  expect_match(source, "execute_unload \"results.gdx\"", fixed = TRUE)
})
