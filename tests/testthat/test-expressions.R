test_that("symbolic indexing creates deterministic references", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  j <- gams_set(model, "j", records = c("new-york", "chicago"))
  x <- gams_variable(model, "x", domain = c(i, j), type = "positive")

  expect_identical(format_gams_expression(x[i, j]), "x(i,j)")
  expect_identical(
    format_gams_expression(x["seattle", "new-york"]),
    "x('seattle','new-york')"
  )
  expect_error(x[j, i], class = "gamsr_error_invalid_index")
  expect_error(x["missing", "new-york"], class = "gamsr_error_invalid_index")
})

test_that("arithmetic expressions preserve precedence", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = "seattle")
  x <- gams_variable(model, "x", domain = i, type = "positive")
  a <- gams_parameter(model, "a", domain = i, records = c(seattle = 10))

  expect_identical(format_gams_expression(x[i] + 2 * a[i]), "x(i) + 2 * a(i)")
  expect_identical(format_gams_expression((x[i] + a[i]) * 2), "(x(i) + a(i)) * 2")
  expect_identical(format_gams_expression(-x[i]), "-x(i)")
  expect_identical(format_gams_expression(x[i]^2), "x(i) ** 2")
})

test_that("scalar symbols and expressions compose in either operand order", {
  model <- gams_model("scalar_arithmetic")
  x <- gams_variable(model, "x")
  y <- gams_variable(model, "y")

  expect_identical(format_gams_expression((x + 1) + y), "x + 1 + y")
  expect_identical(format_gams_expression(y + (x + 1)), "y + (x + 1)")
  expect_identical(format_gams_expression(2 * y + x), "2 * y + x")
})

test_that("aliases and ordered-set functions format and bind indices", {
  model <- gams_model("ordered_sets")
  i <- gams_set(model, "i", records = c("a", "b", "c"))
  ip <- gams_alias(i, "ip")
  x <- gams_variable(model, "x", domain = i)

  expression <- gams_sum(ip, x[ip] * gams_ord(ip)) / gams_card(i)

  expect_identical(expression$free_indices, character())
  expect_identical(
    format_gams_expression(expression),
    "sum(ip, x(ip) * ord(ip)) / card(i)"
  )
  expect_error(x + 1, class = "gamsr_error_unindexed_symbol")
  expect_error(i + 1, class = "gamsr_error_invalid_expression")
})

test_that("comparisons and gams_eq create symbolic relationships", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = "seattle")
  x <- gams_variable(model, "x", domain = i)

  expect_identical(format_gams_expression(x[i] <= 1), "x(i) =l= 1")
  expect_identical(format_gams_expression(x[i] >= 1), "x(i) =g= 1")
  expect_identical(format_gams_expression(gams_eq(x[i], 1)), "x(i) =e= 1")
  expect_identical(format_gams_expression(x[i] == 1), "x(i) =e= 1")
  expect_identical(format_gams_expression(x[i] < 1), "x(i) < 1")
  expect_identical(format_gams_expression(x[i] != 1), "x(i) <> 1")
})

test_that("gams_sum binds indices and formats deterministically", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  j <- gams_set(model, "j", records = c("new-york", "chicago"))
  x <- gams_variable(model, "x", domain = c(i, j), type = "positive")
  a <- gams_parameter(model, "a", domain = i, records = c(seattle = 1, `san-diego` = 2))

  expression <- gams_sum(j, x[i, j]) <= a[i]

  expect_identical(expression$free_indices, "i")
  expect_identical(format_gams_expression(expression), "sum(j, x(i,j)) =l= a(i)")
  expect_error(gams_sum(c(j, j), x[i, j]), class = "gamsr_error_duplicate_name")
})

test_that("math helpers create symbolic function calls", {
  model <- gams_model("math_model")
  i <- gams_set(model, "i", records = "a")
  x <- gams_variable(model, "x", domain = i)

  expect_identical(
    format_gams_expression(gams_sqrt(gams_abs(x[i] - 10))),
    "sqrt(abs(x(i) - 10))"
  )
  expect_identical(format_gams_expression(gams_min(x[i], 3, 4)), "min(x(i), 3, 4)")
})

test_that("equation assignment validates free index scope", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = c("seattle", "san-diego"))
  j <- gams_set(model, "j", records = c("new-york", "chicago"))
  x <- gams_variable(model, "x", domain = c(i, j), type = "positive")
  a <- gams_parameter(model, "a", domain = i, records = c(seattle = 1, `san-diego` = 2))
  b <- gams_parameter(model, "b", domain = j, records = c(`new-york` = 1, chicago = 2))
  supply <- gams_equation(model, "supply", domain = i)
  demand <- gams_equation(model, "demand", domain = j)

  supply[i] <- gams_sum(j, x[i, j]) <= a[i]
  demand[j] <- gams_sum(i, x[i, j]) >= b[j]

  expect_identical(
    format_gams_expression(supply$definition$expression),
    "sum(j, x(i,j)) =l= a(i)"
  )
  expect_identical(
    format_gams_expression(demand$definition$expression),
    "sum(i, x(i,j)) =g= b(j)"
  )

  expect_error(
    supply[i] <- x[i, j] <= a[i],
    class = "gamsr_error_unresolved_index"
  )
})

test_that("indexed symbols must be explicitly indexed", {
  model <- gams_model("transport")
  i <- gams_set(model, "i", records = "seattle")
  x <- gams_variable(model, "x", domain = i)

  expect_error(x + 1, class = "gamsr_error_unindexed_symbol")
})
