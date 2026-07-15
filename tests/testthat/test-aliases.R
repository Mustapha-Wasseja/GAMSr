test_that("aliases are registered and compile after their base sets", {
  model <- gams_model("alias_model")
  i <- gams_set(model, "i", records = c("a", "b"))
  ip <- gams_alias(i, "ip")
  x <- gams_variable(model, "x", domain = i, type = "positive")
  floor <- gams_equation(model, "floor", domain = ip)

  floor[ip] <- x[ip] >= gams_ord(ip) / gams_card(i)
  problem <- gams_problem(
    model,
    equations = floor,
    objective = gams_sum(ip, x[ip]),
    sense = "min",
    problem = "LP"
  )
  source <- generated_gams(problem)

  expect_s3_class(ip, "gams_alias")
  expect_identical(ip$base_set, i)
  expect_match(source, "Set i / a, b /;", fixed = TRUE)
  expect_match(source, "Alias (i, ip);", fixed = TRUE)
  expect_match(source, "floor(ip).. x(ip) =g= ord(ip) / card(i);", fixed = TRUE)
  expect_lt(regexpr("Set i", source, fixed = TRUE), regexpr("Alias (i, ip)", source, fixed = TRUE))
})

test_that("aliases preserve base-set domain validation", {
  model <- gams_model("alias_domains")
  i <- gams_set(model, "i", records = "a")
  j <- gams_set(model, "j", records = "b")
  ip <- gams_alias(i, "ip")
  x <- gams_variable(model, "x", domain = i)

  expect_s3_class(x[ip], "gams_expr_indexed_reference")
  expect_error(x[j], class = "gamsr_error_invalid_index")
  expect_error(gams_alias(ip, "ipp"), class = "gamsr_error_invalid_domain")
})
