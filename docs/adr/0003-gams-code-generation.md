# ADR 0003: GAMS Code Generation

## Context

Generated GAMS source must be deterministic, readable, and safe to inspect.

## Options Considered

- Concatenate strings throughout constructors.
- Build a normalized intermediate representation and render it.
- Emit GAMS only through templates.

## Chosen Option

Build a normalized IR and render it through dedicated compiler functions.

## Consequences

Compilation is more work up front, but deterministic output, golden tests, and
source maps become practical.

## Rejected Alternatives

Ad hoc string concatenation spreads escaping and ordering bugs through the code.
Templates alone are insufficient without a validated IR.

## How This Will Be Tested

Golden-file tests will compare generated `.gms` output for scalar LP,
transportation, diet, and MIP examples. Snapshot tests will avoid unstable paths.
