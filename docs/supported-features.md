# Supported Features

Last updated: 2026-07-15.

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
- Symbolic variable-attribute expressions for level, marginal, lower-bound, and
  upper-bound values in assignments and conditions.
- `gams_equation()` declaration.
- Symbolic indexing with `[]` for sets and literal labels.
- Symbolic arithmetic with `+`, `-`, `*`, `/`, `^`, and unary minus.
- Mixed scalar symbols and expression trees in either operand order.
- Symbolic relationships with `<=`, `>=`, `==`, and `gams_eq()`.
- Logical comparisons with `<`, `<=`, `==`, `!=`, `>=`, and `>` inside
  conditions.
- Logical condition composition with `&`, `|`, and `!`.
- `gams_where()` for conditional equation domains and algebraic terms.
- `gams_sum()` over one or more sets, with optional domain filtering through
  its `condition` argument.
- `gams_same_as()` for set-element and literal-label matching.
- `gams_dynamic_set()` for computed subsets over one or more static domains.
- Ordered parameter and dynamic-set assignments with indexed replacement
  syntax.
- `gams_assign()` for assignment targets with optional dollar conditions.
- `gams_alias()` for using multiple index names over the same base set.
- Ordered-set helpers `gams_ord()` and `gams_card()`.
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
- GDX-backed local solve execution that writes `input.gdx` and uses `$load`.
- Solver-specific option-file generation through `solver_options`.
- Result GDX reading through `read_solution_gdx()`.
- Result accessors: `model_status()`, `solver_status()`, `objective_value()`,
  `variable_values()`, `equation_values()`, `solve_summary()`, and
  `result_files()`.
- Solve-summary metadata for objective values/bounds, resource time, elapsed
  solve time, solver iterations, model size, and infeasibility attributes when
  GAMS reports them.
- GAMS executable discovery helpers, including common Windows install
  locations.
- Gated end-to-end solve tests against a local GAMS installation for LP, MIP,
  and infeasible LP fixtures.

## Not Yet Implemented

- Broad full-model GAMS syntax validation across problem classes.
- Listing/log parsing and solver-specific detail extraction beyond the current
  GAMS model attributes.
- Stable unbounded-model classification tests across solvers.
- Broader modeling syntax: lead/lag, advanced set operations and domain
  forwarding, and more intrinsic functions.
- Remote execution through GAMS Engine or other hosted solve backends.
