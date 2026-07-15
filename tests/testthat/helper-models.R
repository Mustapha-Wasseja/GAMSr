expected_gams_fixture <- function(name) {
  path <- testthat::test_path("..", "fixtures", "expected-gams", name)
  paste(readLines(path, warn = FALSE), collapse = "\n")
}

scalar_lp_problem <- function() {
  model <- gams_model("scalar_lp")
  x <- gams_variable(model, "x", type = "free")
  e <- gams_equation(model, "balance")
  e[] <- x >= 1

  gams_problem(
    model,
    name = "scalar_lp_problem",
    equations = e,
    objective = x,
    sense = "min",
    problem = "LP"
  )
}

binary_mip_problem <- function() {
  model <- gams_model("binary_mip")
  x <- gams_variable(model, "x", type = "binary")
  y <- gams_variable(model, "y", type = "binary")
  capacity <- gams_equation(model, "capacity")

  capacity[] <- x + y <= 1

  gams_problem(
    model,
    name = "binary_mip_problem",
    equations = capacity,
    objective = y,
    sense = "max",
    problem = "MIP"
  )
}

infeasible_lp_problem <- function() {
  model <- gams_model("infeasible_lp")
  x <- gams_variable(model, "x", type = "positive")
  lower <- gams_equation(model, "lower")
  upper <- gams_equation(model, "upper")

  lower[] <- x >= 1
  upper[] <- x <= 0

  gams_problem(
    model,
    name = "infeasible_lp_problem",
    equations = c(lower, upper),
    objective = x,
    sense = "min",
    problem = "LP"
  )
}

ordered_alias_problem <- function() {
  model <- gams_model("ordered_alias")
  i <- gams_set(model, "i", records = c("a", "b", "c"))
  ip <- gams_alias(i, "ip")
  x <- gams_variable(model, "x", domain = i, type = "positive")
  floor <- gams_equation(model, "floor", domain = ip)

  floor[ip] <- x[ip] >= gams_ord(ip) / gams_card(i)

  gams_problem(
    model,
    name = "ordered_alias_problem",
    equations = floor,
    objective = gams_sum(ip, x[ip]),
    sense = "min",
    problem = "LP"
  )
}

conditional_lp_problem <- function() {
  model <- gams_model("conditional_lp")
  i <- gams_set(model, "i", records = c("a", "b", "c"))
  active <- gams_parameter(
    model,
    "active",
    domain = i,
    records = c(a = 1, b = 0, c = 1)
  )
  x <- gams_variable(model, "x", domain = i, type = "positive")
  floor <- gams_equation(model, "floor", domain = i)

  floor[i] <- gams_where(x[i] >= gams_ord(i), active[i] == 1)

  gams_problem(
    model,
    name = "conditional_lp_problem",
    equations = floor,
    objective = gams_sum(
      i,
      gams_where(x[i], active[i] == 1),
      condition = gams_ord(i) <= gams_card(i)
    ),
    sense = "min",
    problem = "LP"
  )
}

dynamic_assignment_problem <- function() {
  model <- gams_model("dynamic_assignment")
  i <- gams_set(model, "i", records = c("a", "b", "c"))
  p <- gams_parameter(model, "p", domain = i, records = c(a = 1, b = 2, c = 3))
  target <- gams_parameter(model, "target", domain = i)
  x <- gams_variable(model, "x", domain = i, type = "positive")
  x <- set_level(x, c(a = 1, b = 0, c = 1))
  active <- gams_dynamic_set(model, "active", domain = i)
  active[i] <- gams_level(x[i]) > 0
  gams_assign(target[i], p[i] * 2, condition = active[i])
  floor <- gams_equation(model, "floor", domain = i)
  floor[i] <- gams_where(x[i] >= target[i], active[i])

  gams_problem(
    model,
    name = "dynamic_assignment_problem",
    equations = floor,
    objective = gams_sum(i, x[i], condition = active[i]),
    sense = "min",
    problem = "LP"
  )
}

transport_problem <- function() {
  model <- gams_model("transport")
  i <- gams_set(
    model,
    "i",
    records = c("seattle", "san-diego"),
    description = "canning plants"
  )
  j <- gams_set(
    model,
    "j",
    records = c("new-york", "chicago", "topeka"),
    description = "markets"
  )
  a <- gams_parameter(
    model,
    "a",
    domain = i,
    records = c(seattle = 350, `san-diego` = 600)
  )
  b <- gams_parameter(
    model,
    "b",
    domain = j,
    records = c(`new-york` = 325, chicago = 300, topeka = 275)
  )
  c <- gams_parameter(
    model,
    "c",
    domain = c(i, j),
    records = data.frame(
      i = c("seattle", "seattle", "seattle", "san-diego", "san-diego", "san-diego"),
      j = c("new-york", "chicago", "topeka", "new-york", "chicago", "topeka"),
      value = c(0.225, 0.153, 0.162, 0.225, 0.162, 0.126)
    )
  )
  x <- gams_variable(model, "x", domain = c(i, j), type = "positive")
  supply <- gams_equation(model, "supply", domain = i)
  demand <- gams_equation(model, "demand", domain = j)

  supply[i] <- gams_sum(j, x[i, j]) <= a[i]
  demand[j] <- gams_sum(i, x[i, j]) >= b[j]

  gams_problem(
    model,
    name = "transport_problem",
    equations = c(supply, demand),
    objective = gams_sum(c(i, j), c[i, j] * x[i, j]),
    sense = "min",
    problem = "LP"
  )
}
