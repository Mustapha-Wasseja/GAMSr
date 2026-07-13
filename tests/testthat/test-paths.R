test_that("find_gams validates explicit system directories", {
  directory <- withr::local_tempdir()

  expect_true(is.na(find_gams(directory)))
})

test_that("gams_version probes local GAMS with the console help command", {
  skip_if_not(gams_available(), "GAMS is not installed")

  version <- gams_version()

  expect_type(version, "character")
  expect_match(version, "GAMS", fixed = TRUE)
  expect_match(version, "Release", fixed = TRUE)
})
