# Contributing

Thank you for helping build `GAMSr`.

## Development Principles

- Keep changes focused on one milestone or feature.
- Add tests before or alongside implementation.
- Keep the package usable without GAMS installed.
- Do not commit generated GAMS work directories, licence files, credentials, or
  proprietary binaries.
- Do not copy source code, examples, tests, or documentation wording from
  GAMSPy.

## Local Checks

```r
devtools::document()
devtools::test()
devtools::check()
```

Integration tests that require GAMS must be skipped clearly when GAMS is not
available.
