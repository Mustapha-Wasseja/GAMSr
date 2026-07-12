# ADR 0006: Error Model

## Context

Users need modelling errors that explain what failed, where, why, and how to
fix it.

## Options Considered

- Base `stop()` strings.
- `rlang::abort()` with custom classes.
- `cli::cli_abort()` with structured messages and classes.

## Chosen Option

Use `cli::cli_abort()` with package-specific condition classes.

## Consequences

Errors are testable by class and can include bullets and contextual hints.
Messages can stay readable in interactive R sessions.

## Rejected Alternatives

Plain `stop()` is too hard to classify in tests. Raw `rlang::abort()` works but
lacks the structured formatting used by `cli`.

## How This Will Be Tested

Tests assert error classes for invalid names, duplicate labels, cross-model
domains, invalid records, and later expression-scope failures.
