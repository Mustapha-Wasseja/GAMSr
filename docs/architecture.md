# Architecture

`GAMSr` is split into layers:

1. R-side model graph: an R6 model context owns symbol objects and registries.
2. Symbol layer: immutable S3 objects represent sets, parameters, variables,
   and equations.
3. Expression layer: S3 AST nodes represent algebra, indexing, relationships,
   math functions, and summation.
4. Compiler layer: normalized IR and deterministic full-model GAMS renderer for
   compile-only workflows.
5. Transfer layer: planned adapter over optional GAMS Transfer R.
6. Execution layer: planned safe process invocation and result parsing.

The current repository implements layers 1 through 4 for inline-data
compile-only workflows.
