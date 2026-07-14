# GAMSr

`GAMSr` is an experimental R package for defining, validating, and eventually
compiling algebraic optimisation models for execution by a separately installed
GAMS system.

This project is an independent community interface. It is not affiliated with or
endorsed by GAMS Development Corporation unless explicitly stated otherwise.
GAMS is a separate product and may require its own installation and licence.

## Status

This repository is in early active development. The package currently provides:

- validated GAMS-compatible names and labels;
- an ordered model symbol registry;
- immutable S3 symbol objects for sets, parameters, variables, and equations;
- canonical records for basic set and parameter inputs;
- symbolic indexing, arithmetic, equation relationships, and `gams_sum()`;
- deterministic expression formatting for compile-only inspection;
- `gams_problem()`, `model_ir()`, and `generated_gams()` for compile-only
  GAMS source generation;
- `write_gams()` and `compile_gams()` for durable compile-only files;
- a starter transfer adapter layer for canonical input data and optional GDX
  writing through GAMS Transfer R;
- safe local `solve()` execution with result-GDX reading when GAMS is installed;
- optional solver selection and scalar GAMS command-line options in `solve()`;
- GDX-backed solve execution that writes input data to `input.gdx` instead of
  rendering records inline;
- solver-specific option-file generation through `solver_options`;
- result helpers: `model_status()`, `solver_status()`, `objective_value()`,
  `variable_values()`, `equation_values()`, `solve_summary()`, and
  `result_files()`;
- compile-only architecture notes and ADRs.

It includes gated local GAMS integration tests, but broad solver and model-class
coverage is still under development.

## Example

```r
library(GAMSr)

m <- gams_model("transport")

i <- gams_set(
  m,
  "i",
  records = c("seattle", "san-diego"),
  description = "canning plants"
)

j <- gams_set(
  m,
  "j",
  records = c("new-york", "chicago", "topeka"),
  description = "markets"
)

a <- gams_parameter(
  m,
  "a",
  domain = i,
  records = c(seattle = 350, `san-diego` = 600)
)

x <- gams_variable(m, "x", domain = c(i, j), type = "positive")
supply <- gams_equation(m, "supply", domain = i)

supply[i] <- gams_sum(j, x[i, j]) <= a[i]

format_gams_expression(supply$definition$expression)
#> sum(j, x(i,j)) =l= a(i)

problem <- gams_problem(
  m,
  equations = supply,
  objective = gams_sum(c(i, j), x[i, j]),
  sense = "min",
  problem = "LP"
)

cat(generated_gams(problem))

compile <- compile_gams(problem, directory = tempdir())
compile$source_file

m
```

## GDX Transfer Status

`GAMSr` can now extract canonical input symbols and has an adapter boundary for
GAMS Transfer R:

```r
transfer_symbols(problem)
gams_transfer_available()
```

Actual GDX writing requires the optional `gamstransfer` package and a compatible
GAMS setup. Tests use `mock_transfer_adapter()` so core checks stay independent
of GAMS.

## Solving Status

When GAMS and `gamstransfer` are available, `solve()` writes input data to GDX,
runs GAMS without invoking a shell, and reads status, objective, variable,
equation, and solve-summary records back into R:

```r
result <- solve(problem)
model_status(result)
solver_status(result)
objective_value(result)
variable_values(result, x)
equation_values(result, supply)
solve_summary(result)

result <- solve(
  problem,
  solver = "soplex",
  gams_options = list(reslim = 60, optcr = 0.01),
  solver_options = list(display = 0)
)
```

On machines without GAMS, `solve()` fails early with a clear error while
compile-only workflows continue to work.

## GAMS Availability

The package must install and load without GAMS. Runtime execution requires a
local GAMS installation and the optional `gamstransfer` package.

```r
gams_available()
find_gams()
gams_version()
```

## Naming Note

The package name `GAMSr` is provisional. Research notes record that names based
on GAMS need legal/trademark review before any public release.
