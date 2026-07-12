test_that("symbol names follow GAMS identifier rules", {
  expect_identical(GAMSr:::validate_symbol_name("plant_1"), "plant_1")
  expect_error(GAMSr:::validate_symbol_name("1plant"), class = "gamsr_error_invalid_name")
  expect_error(GAMSr:::validate_symbol_name("plant-cost"), class = "gamsr_error_invalid_name")
  expect_error(GAMSr:::validate_symbol_name("set"), class = "gamsr_error_reserved_name")
  expect_error(
    GAMSr:::validate_symbol_name(paste0("a", strrep("x", 63))),
    class = "gamsr_error_invalid_name"
  )
})

test_that("labels support GAMS label syntax and reject ambiguous values", {
  labels <- c("seattle", "san-diego", "new york", "10%-INCR")
  expect_identical(GAMSr:::validate_label(labels), labels)
  expect_error(GAMSr:::validate_label(NA_character_), class = "gamsr_error_invalid_label")
  expect_error(GAMSr:::validate_label("bad\nlabel"), class = "gamsr_error_invalid_label")
  expect_error(GAMSr:::validate_label("label "), class = "gamsr_error_invalid_label")
})

test_that("labels format deterministically for generated GAMS", {
  expect_identical(GAMSr:::format_gams_label("seattle"), "seattle")
  expect_identical(GAMSr:::format_gams_label("new york"), "'new york'")
  expect_identical(GAMSr:::format_gams_label("has'quote"), "\"has'quote\"")
  expect_error(GAMSr:::format_gams_label("has'and\"quotes"), class = "gamsr_error_invalid_label")
})

test_that("duplicate labels are detected case-insensitively", {
  expect_error(
    GAMSr:::check_unique_labels(c("A", "a")),
    class = "gamsr_error_duplicate_label"
  )
})
