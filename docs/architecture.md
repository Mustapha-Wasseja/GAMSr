# Architecture

`GAMSr` is split into layers:

1. R-side model graph: an R6 model context owns symbol objects and registries.
2. Symbol layer: immutable S3 objects represent sets, parameters, variables,
   and equations.
3. Expression and statement layer: S3 AST nodes represent algebra, indexing,
   relationships, logical conditions, variable attributes, conditional terms,
   math functions, summation, and ordered parameter/set assignments.
4. Compiler layer: normalized IR and deterministic full-model GAMS renderer for
   compile-only workflows.
5. Transfer layer: adapter boundary over optional GAMS Transfer R, input-GDX
   writing, and a mock adapter for tests.
6. Execution layer: GDX-backed input loading, solver option-file writing, safe
   process invocation, result-GDX reading, and result accessors.

The current repository implements layers 1 through 6 for inline-data
compile-only workflows, GDX-backed local solves, optional solver option files,
and solve/result APIs. When GAMS is installed, gated integration tests solve
scalar LP, transportation LP, binary MIP, and infeasible LP fixtures and read
the resulting GDX records back into R. Ordered-set and conditional LP fixtures
also verify aliases, `ord/card`, filtered sums, conditional equation domains,
dynamic subsets, parameter assignments, and variable-attribute conditions.
