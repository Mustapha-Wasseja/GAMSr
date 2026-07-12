# Supported Features

Last updated: 2026-07-11.

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
- GAMS executable discovery helpers.

## Not Yet Implemented

- Symbolic expression AST.
- Indexed equation assignment.
- `gams_sum()` and equation relationships.
- Compiler IR and GAMS source generation.
- GDX read/write.
- GAMS process execution.
- Result objects.
- Integration tests against a real GAMS installation.
