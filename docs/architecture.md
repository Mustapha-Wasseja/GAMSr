# Architecture

`GAMSr` is split into layers:

1. R-side model graph: an R6 model context owns symbol objects and registries.
2. Symbol layer: immutable S3 objects represent sets, parameters, variables,
   and equations.
3. Expression layer: planned S3 AST nodes will represent algebra, indexing,
   relationships, and aggregations.
4. Compiler layer: planned normalized IR and deterministic GAMS renderer.
5. Transfer layer: planned adapter over optional GAMS Transfer R.
6. Execution layer: planned safe process invocation and result parsing.

The current repository implements layers 1 and part of 2.
