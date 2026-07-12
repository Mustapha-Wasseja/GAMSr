test_that("gams_model creates an empty model context", {
  model <- gams_model("transport")

  expect_s3_class(model, "GamsModel")
  expect_identical(model$name, "transport")
  expect_identical(model$symbols(), list())
})

test_that("models reject invalid names", {
  expect_error(gams_model("model"), class = "gamsr_error_reserved_name")
})

test_that("model symbol lookup is case-insensitive", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))

  expect_identical(model$get_symbol("I"), i)
  expect_error(gams_set(model, "I"), class = "gamsr_error_duplicate_name")
})
