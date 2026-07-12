# GAMSr 0.0.1

- Created the package skeleton.
- Added research notes and initial ADRs.
- Added validated name, label, registry, model, and symbol constructors.
- Added the first symbolic expression layer with indexing, arithmetic,
  relationships, `gams_sum()`, math helpers, and equation definition storage.
- Added the first compiler layer with `gams_problem()`, `model_ir()`, and
  `generated_gams()` for deterministic compile-only `.gms` output.
