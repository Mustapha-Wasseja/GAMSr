# Supported Features

Last updated: 2026-07-13.

## Implemented

- Package skeleton.
- GAMS-compatible symbol name validation.
- GAMS label validation and deterministic simple quoting.
- Ordered, case-insensitive name registry.
- `gams_model()` R6 model context.
- `gams_set()` with ordered one-dimensional records.
- `gams_parameter()` with scalar, named-vector, matrix, and long data frame
  record normalization.
- `gams_variable()` for `free`, `positive`, `negative`, `binary`, and
  `integer` declarations.
- Variable attribute record setters for lower, upper, level, and fixed values.
- `gams_equation()` declaration.
- Symbolic indexing with `[]` for sets and literal labels.
- Symbolic arithmetic with `+`, `-`, `*`, `/`, `^`, and unary minus.
- Symbolic relationships with `<=`, `>=`, `==`, and `gams_eq()`.
- `gams_sum()` over one or more sets.
- Symbolic math helpers: `gams_abs()`, `gams_exp()`, `gams_log()`,
  `gams_log10()`, `gams_sqrt()`, `gams_min()`, and `gams_max()`.
- Equation definition assignment with free-index validation.
- `format_gams_expression()` for deterministic expression inspection.
- `gams_problem()` for binding equations, objective, sense, and model type.
- `model_ir()` for normalized compiler inspection.
- `generated_gams()` for deterministic compile-only `.gms` source.
- `write_gams()` for writing generated source to a chosen file.
- `compile_gams()` for writing compile-only `.gms` files and returning a
  `gams_compilation` object.
- Inline rendering for sets, scalar/one-dimensional/multidimensional
  parameters, variables, equations, objective equations, model declarations, and
  solve statements.
- `transfer_symbols()` for canonical input set/parameter extraction.
- `gamstransfer_adapter()`, `mock_transfer_adapter()`, and `write_input_gdx()`
  as the initial transfer adapter boundary.
- Optional `gamstransfer` detection through `gams_transfer_available()`.
- Real input GDX writing through GAMS Transfer R when `gamstransfer` is
  installed.
- Safe `solve()` method for `gams_problem` objects using `processx` argument
  vectors, no shell command construction.
- Optional solver selection and scalar GAMS command-line options in `solve()`.
- Result GDX reading through `read_solution_gdx()`.
- Result accessors: `model_status()`, `solver_status()`, `objective_value()`,
  `variable_values()`, and `equation_values()`.
- GAMS executable discovery helpers, including common Windows install
  locations.
- Gated end-to-end solve tests against a local GAMS installation.

## Not Yet Implemented

- Solver option files.
- Broad full-model GAMS syntax validation across problem classes.
- Richer result metadata, including listing/log summaries, solve time,
  iteration counts, resource usage, infeasibility summaries, and solver-specific
  details.
- GDX-backed model execution that loads input data from GDX instead of rendering
  all records inline.
- Remote execution through GAMS Engine or other hosted solve backends.
