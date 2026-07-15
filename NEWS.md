# GAMSr 0.0.1

- Created the package skeleton.
- Added research notes and initial ADRs.
- Added validated name, label, registry, model, and symbol constructors.
- Added the first symbolic expression layer with indexing, arithmetic,
  relationships, `gams_sum()`, math helpers, and equation definition storage.
- Added the first compiler layer with `gams_problem()`, `model_ir()`, and
  `generated_gams()` for deterministic compile-only `.gms` output.
- Added `write_gams()` and `compile_gams()` file-output helpers.
- Added expected `.gms` fixtures for scalar LP and transportation examples.
- Added the initial GDX transfer adapter boundary with canonical transfer
  symbols, a mock adapter, and optional GAMS Transfer R routing.
- Added real input GDX write/read tests when `gamstransfer` is installed.
- Added safe `solve()` execution scaffolding, result-GDX import, status helpers,
  objective values, variable records, and equation records.
- Added gated local GAMS solve tests for scalar and transportation LP fixtures.
- Added solver selection and scalar GAMS command-line options to `solve()`.
- Improved local GAMS discovery and version probing.
- Added GDX-backed solve execution with `input.gdx` as the default data path.
- Added solver option-file generation through `solver_options`.
- Added solve-summary metadata, `solve_summary()`, and `result_files()`.
- Added local GAMS integration tests for MIP and infeasible LP fixtures.
- Fixed mixed scalar-symbol/expression arithmetic dispatch so expressions can
  be composed naturally in either operand order.
- Added `gams_alias()` plus `gams_ord()` and `gams_card()`, with a gated
  end-to-end GAMS solve test using GDX-backed set data.
- Added conditional modeling with `gams_where()`, filtered `gams_sum()`,
  natural relational and logical operators, and `gams_same_as()`.
- Added validation that rejects decision variables in dollar conditions and
  distinguishes strict logical comparisons from equation relationships.
- Added an end-to-end GDX-backed conditional LP solve test.
- Added dynamic subsets with `gams_dynamic_set()` and deterministic ordered
  assignments for parameters and dynamic sets.
- Added conditional assignment targets through `gams_assign()` and natural
  indexed replacement syntax for unconditional assignments.
- Added symbolic `.l`, `.m`, `.lo`, and `.up` variable-attribute expressions
  for assignments and dollar conditions.
- Added end-to-end GDX-backed tests that compute a dynamic subset, conditionally
  assign parameter data, generate equations over that subset, and solve it.
