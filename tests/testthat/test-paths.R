test_that("find_gams validates explicit system directories", {
  directory <- withr::local_tempdir()

  expect_true(is.na(find_gams(directory)))
})

test_that("gams_version probes local GAMS with the console help command", {
  skip_if_not(gams_available(), "GAMS is not installed")

  version <- gams_version()

  expect_type(version, "character")
  expect_match(version, "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_no_match(version, "License", fixed = TRUE)
})

test_that("version parsing never returns license metadata", {
  output <- paste(
    "*** GAMS Release     : 54.2.0 build platform",
    "*** License          : C:/private/gamslice.txt",
    "*** Person Name and private-license-data",
    sep = "\n"
  )

  expect_identical(GAMSr:::parse_gams_version(output), "54.2.0")
  expect_true(is.na(GAMSr:::parse_gams_version("unrecognized output")))
})
