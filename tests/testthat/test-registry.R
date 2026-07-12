test_that("name registry preserves insertion order", {
  registry <- GAMSr:::new_name_registry("symbol")
  registry$add("i", 1)
  registry$add("j", 2)

  expect_identical(registry$names(), c("i", "j"))
  expect_identical(unname(registry$values()), list(1, 2))
  expect_true(registry$exists("I"))
  expect_identical(registry$get("J"), 2)
})

test_that("name registry rejects duplicate GAMS identifiers", {
  registry <- GAMSr:::new_name_registry("symbol")
  registry$add("cost")
  expect_error(registry$add("Cost"), class = "gamsr_error_duplicate_name")
})
