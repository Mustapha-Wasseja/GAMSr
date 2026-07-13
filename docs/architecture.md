# Architecture

`GAMSr` is split into layers:

1. R-side model graph: an R6 model context owns symbol objects and registries.
2. Symbol layer: immutable S3 objects represent sets, parameters, variables,
   and equations.
3. Expression layer: S3 AST nodes represent algebra, indexing, relationships,
   math functions, and summation.
4. Compiler layer: normalized IR and deterministic full-model GAMS renderer for
   compile-only workflows.
5. Transfer layer: adapter boundary over optional GAMS Transfer R plus a mock
   adapter for tests.
6. Execution layer: safe process invocation, result-GDX reading, and result
   accessors.

The current repository implements layers 1 through 6 for inline-data
compile-only workflows, optional GDX input writing, and solve/result APIs.
When GAMS is installed, gated integration tests solve scalar and transportation
LP fixtures and read the resulting GDX records back into R.
