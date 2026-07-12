# ADR 0004: Data Transfer Strategy

## Context

The package must install and compile models without GAMS, but solved models need
GDX input and output.

## Options Considered

- Depend directly on `gamstransfer`.
- Make `gamstransfer` optional behind an adapter.
- Write GDX files from scratch.

## Chosen Option

Keep `gamstransfer` optional and use an adapter layer when data transfer is
implemented.

## Consequences

Core modelling and compilation can run on machines without GAMS or
`gamstransfer`. Integration tests can use mocks until a GAMS-enabled runner is
configured.

## Rejected Alternatives

A hard dependency would make installation more fragile. Writing GDX from
scratch is unnecessary and risky.

## How This Will Be Tested

Mock adapter tests will validate canonical records. Integration tests will be
skipped clearly when GAMS or `gamstransfer` is unavailable.

## Implementation Note

The first transfer slice adds `transfer_symbols()`, `gamstransfer_adapter()`,
`mock_transfer_adapter()`, `gams_transfer_available()`, and
`write_input_gdx()`. The mock adapter intentionally does not create GDX files;
it records the intended write for tests. Real GDX writing is routed through
optional GAMS Transfer R and fails clearly when `gamstransfer` is unavailable.
