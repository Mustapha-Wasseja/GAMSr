test_that("transfer_symbols extracts canonical sets and parameters", {
  symbols <- transfer_symbols(transport_problem())

  expect_s3_class(symbols, "gams_transfer_symbols")
  expect_identical(unname(vapply(symbols$sets, `[[`, "name", FUN.VALUE = "")), c("i", "j"))
  expect_identical(
    unname(vapply(symbols$parameters, `[[`, "name", FUN.VALUE = "")),
    c("a", "b", "c")
  )
  expect_identical(symbols$sets[[1]]$records$label, c("seattle", "san-diego"))
  expect_identical(symbols$parameters[[3]]$domain, c("i", "j"))
  expect_equal(nrow(symbols$parameters[[3]]$records), 6)
})

test_that("mock transfer adapter records intended GDX writes without writing GDX", {
  path <- file.path(withr::local_tempdir(), "input.gdx")
  result <- write_input_gdx(transport_problem(), path, adapter = mock_transfer_adapter())

  expect_s3_class(result, "gams_transfer_write")
  expect_identical(result$adapter, "mock")
  expect_false(result$written)
  expect_false(file.exists(path))
  expect_identical(basename(result$file), "input.gdx")
  expect_identical(length(result$symbols$sets), 2L)
  expect_identical(length(result$symbols$parameters), 3L)
})

test_that("dynamic sets stay out of input transfer data", {
  symbols <- transfer_symbols(dynamic_assignment_problem())

  expect_identical(
    unname(vapply(symbols$sets, `[[`, "name", FUN.VALUE = "")),
    "i"
  )
  expect_identical(
    unname(vapply(symbols$parameters, `[[`, "name", FUN.VALUE = "")),
    c("p", "target")
  )
})

test_that("write_input_gdx protects existing files", {
  path <- file.path(withr::local_tempdir(), "input.gdx")
  writeLines("not a gdx", path)

  expect_error(
    write_input_gdx(transport_problem(), path, adapter = mock_transfer_adapter()),
    class = "gamsr_error_file_exists"
  )
})

test_that("gamstransfer adapter fails clearly when dependency is unavailable", {
  skip_if(gams_transfer_available(), "gamstransfer is installed")

  path <- file.path(withr::local_tempdir(), "input.gdx")
  expect_error(
    write_input_gdx(transport_problem(), path),
    class = "gamsr_error_missing_dependency"
  )
})

test_that("gamstransfer adapter writes an actual GDX when available", {
  skip_if_not(gams_transfer_available(), "gamstransfer is not installed")

  path <- file.path(withr::local_tempdir(), "input.gdx")
  result <- write_input_gdx(transport_problem(), path)

  expect_true(file.exists(path))
  expect_true(result$written)
  expect_identical(result$adapter, "gamstransfer")

  Container <- getExportedValue("gamstransfer", "Container")
  container <- Container$new(path)
  expect_identical(as.character(container["i"]$records$uni), c("seattle", "san-diego"))
  expect_equal(nrow(container["c"]$records), 6)
})
