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
- compile-only architecture notes and ADRs.

It does not yet implement GDX transfer or process execution.

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

m
```

## GAMS Availability

The package must install and load without GAMS. Runtime execution will be added
later and will require a local GAMS installation.

```r
gams_available()
find_gams()
```

## Naming Note

The package name `GAMSr` is provisional. Research notes record that names based
on GAMS need legal/trademark review before any public release.
