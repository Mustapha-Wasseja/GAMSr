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
