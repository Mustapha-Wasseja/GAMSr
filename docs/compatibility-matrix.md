# Compatibility Matrix

Last updated: 2026-07-19.

| Component | Status |
| --- | --- |
| R 4.1+ | Declared minimum in `DESCRIPTION`. |
| R release/oldrel/devel | Checked on GitHub Actions across Windows, macOS, and Ubuntu. |
| R 4.5.2 on Windows | Used for local R, Positron Ark, and GAMS integration checks. |
| GAMS 54.2 on Windows | Installed and used for LP, MIP, NLP, QCP, and MINLP solves. |
| Non-demo personal GAMS license | Verified with a 2,100-row HiGHS LP; commercial solver entitlements are not assumed. |
| `gamstransfer` | Installed and used for real input and result GDX round trips. |
| CRAN-style checks | `R CMD check --as-cran --no-manual` passes with the expected new-submission note. |
