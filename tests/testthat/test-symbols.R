test_that("sets normalize records and preserve order", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))

  expect_s3_class(i, "gams_set")
  expect_identical(i$records$label, c("seattle", "san-diego"))
})

test_that("parameters normalize scalar, vector, data frame, and matrix records", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  j <- gams_set(model, "j", records = c("new-york", "chicago"))

  scalar <- gams_parameter(model, "freight", records = 90)
  expect_identical(scalar$records$value, 90)

  capacity <- gams_parameter(
    model,
    "capacity",
    domain = i,
    records = c(seattle = 350, `san-diego` = 600)
  )
  expect_identical(names(capacity$records), c("i", "value"))
  expect_identical(capacity$records$value, c(350, 600))

  demand <- gams_parameter(
    model,
    "demand",
    domain = j,
    records = data.frame(j = c("new-york", "chicago"), value = c(325, 300))
  )
  expect_identical(demand$records$value, c(325, 300))

  distance_matrix <- matrix(
    c(2.5, 1.7, 2.5, 1.8),
    nrow = 2,
    dimnames = list(c("seattle", "san-diego"), c("new-york", "chicago"))
  )
  distance <- gams_parameter(model, "distance", domain = c(i, j), records = distance_matrix)
  expect_identical(names(distance$records), c("i", "j", "value"))
  expect_equal(nrow(distance$records), 4)
})

test_that("domain objects must belong to the same model", {
  model_a <- gams_model("model_a")
  model_b <- gams_model("model_b")
  i <- gams_set(model_a, "i")

  expect_error(
    gams_parameter(model_b, "p", domain = i),
    class = "gamsr_error_cross_model_domain"
  )
})

test_that("variables support immutable attribute setters", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  x <- gams_variable(model, "x", domain = i, type = "positive")
  x2 <- set_upper_bound(x, c(seattle = 500, `san-diego` = 700))

  expect_identical(x$type, "positive")
  expect_null(x$gams_attributes$upper)
  expect_identical(x2$gams_attributes$upper$value, c(500, 700))
})

test_that("equations store symbolic definitions", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  capacity <- gams_parameter(
    model,
    "capacity",
    domain = i,
    records = c(seattle = 1, `san-diego` = 2)
  )
  x <- gams_variable(model, "x", domain = i, type = "positive")
  e <- gams_equation(model, "supply", domain = i)

  e[i] <- x[i] <= capacity[i]

  expect_s3_class(e$definition$expression, "gams_expr_comparison")
  expect_identical(
    format_gams_expression(e$definition$expression),
    "x(i) =l= capacity(i)"
  )
  expect_identical(model$get_symbol("supply")$definition$expression, e$definition$expression)
})
