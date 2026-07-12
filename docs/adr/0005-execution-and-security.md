# ADR 0005: Execution and Security

## Context

The package will execute an external GAMS binary and pass user-controlled model
metadata into generated files.

## Options Considered

- Use `system(paste(...))`.
- Use `system2()` or `processx` with argument vectors.
- Defer execution until the compiler and transfer layers are validated.

## Chosen Option

Defer execution beyond the skeleton. When implemented, use argument vectors and
avoid shell invocation.

## Consequences

The MVP can remain compile-only until the command construction surface is small
and tested. Solver names and options must be validated before being passed to
GAMS.

## Rejected Alternatives

Shell command concatenation is rejected because it is injection-prone.

## How This Will Be Tested

Security tests will cover malicious names, paths with spaces and quotes, solver
option validation, temporary work directory isolation, and absence of shell
string construction.
