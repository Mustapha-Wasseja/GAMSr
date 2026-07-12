# ADR 0005: Execution and Security

## Context

The package will execute an external GAMS binary and pass user-controlled model
metadata into generated files.

## Options Considered

- Use `system(paste(...))`.
- Use `system2()` or `processx` with argument vectors.
- Defer execution until the compiler and transfer layers are validated.

## Chosen Option

Use `processx::run()` with argument vectors and avoid shell invocation.

## Consequences

The execution layer writes deterministic source to an isolated work directory,
passes arguments directly to GAMS, appends a result-GDX unload block, and reads
results through GAMS Transfer R. Solver names and options must be validated
before being passed to GAMS.

## Rejected Alternatives

Shell command concatenation is rejected because it is injection-prone.

## How This Will Be Tested

Security tests will cover malicious names, paths with spaces and quotes, solver
option validation, temporary work directory isolation, and absence of shell
string construction.

## Implementation Note

`solve.gams_problem()` now checks for `gamstransfer` and a local GAMS
executable, writes a solve-specific `.gms` file, invokes GAMS through
`processx::run()` without a shell, and imports result records from GDX. On this
development machine GAMS is not installed, so tests verify clear unavailable-GAMS
errors and fake-result GDX parsing.
