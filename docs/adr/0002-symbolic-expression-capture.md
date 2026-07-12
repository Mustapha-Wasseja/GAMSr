# ADR 0002: Symbolic Expression Capture

## Context

Users need to write R-like algebra while the package captures symbolic
expressions instead of evaluating R vectors.

## Options Considered

- Explicit constructors and operator methods.
- Formula or quosure-based DSL.
- Parse user strings.

## Chosen Option

Use explicit constructors and operator methods for the MVP. Use `gams_sum()` for
aggregation and `gams_eq()` for equality. Do not parse user strings.

## Consequences

The initial DSL is explicit and easier to validate. Some syntax will be less
magical than full R formulas, but failures can be made clearer.

## Rejected Alternatives

Quosure/formula capture may be useful later but increases evaluation ambiguity.
String parsing is rejected for security and diagnostics.

## How This Will Be Tested

Expression-node tests will verify AST construction, dimension inference, scope
validation, equality handling, and prevention of accidental logical comparison.
