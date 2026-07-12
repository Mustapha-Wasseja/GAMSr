# ADR 0001: Object System

## Context

The package needs mutable model/container state and symbolic objects that can be
validated, copied, printed, and snapshot-tested.

## Options Considered

- R6 for all objects.
- S3 for all objects.
- S4 for all objects.
- R6 model context with immutable S3 symbols and expressions.

## Chosen Option

Use R6 for the model context and registries, and immutable S3 objects for
symbols and expression nodes.

## Consequences

The model can own mutable state such as insertion order and generated names.
Symbols remain simple values that are easy to inspect and test. Public
constructors must validate every object.

## Rejected Alternatives

All-R6 would make symbolic expressions too mutable. All-S3 would make model
state management cumbersome. S4 adds formal dispatch but more ceremony than the
MVP needs.

## How This Will Be Tested

Unit tests verify model registration order, duplicate-name detection, symbol
classing, and copy-on-modify behavior for variable attributes.
