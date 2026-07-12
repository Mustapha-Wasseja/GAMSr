# Research Notes

Last reviewed: 2026-07-11.

## Sources Reviewed

- GAMS latest documentation is currently version 54 according to the docs
  version selector: <https://www.gams.com/latest/docs/>.
- GAMS R API overview: <https://www.gams.com/latest/docs/API_R_OVERVIEW.html>.
- GDX user guide: <https://www.gams.com/latest/docs/UG_GDX.html>.
- GAMS program lexical rules: <https://www.gams.com/latest/docs/UG_GAMSPrograms.html>.
- GAMS variables: <https://www.gams.com/latest/docs/UG_Variables.html>.
- GAMS equations: <https://www.gams.com/latest/docs/UG_Equations.html>.
- GAMS model and solve statements:
  <https://www.gams.com/latest/docs/UG_ModelSolve.html>.
- GAMS Transfer R examples:
  <https://transfer-r.readthedocs.io/en/latest/user_guide/examples/index.html>.
- CRAN `gamstransfer` page:
  <https://cran.r-project.org/web/packages/gamstransfer/index.html>.
- GAMSPy overview and basics:
  <https://gamspy.readthedocs.io/en/latest/user/whatisgamspy.html>.

## Findings

### GAMS identifiers and labels

GAMS identifiers start with a letter, may contain letters, digits, and
underscores, and are limited to 63 characters. Names are case-insensitive.
GAMS labels are also limited to 63 characters. Unquoted labels can start with a
letter or digit and can include letters, digits, underscores, plus, and minus.
Quoted labels can include broader legal characters. Leading blanks are
significant, but trailing blanks are trimmed by GAMS, so this package rejects
trailing blanks in labels for deterministic round-trips.

### Reserved words

The official GAMS program documentation lists reserved words and reserved
operator tokens. The MVP rejects reserved words as package-level identifiers and
as unquoted labels. Reserved labels can still be represented by quoting in code
generation.

### Equations

Equations must be declared before definition. The common relation operators are
`=e=` for equality, `=l=` for less-than-or-equal, and `=g=` for
greater-than-or-equal. The MVP should map R-side comparisons to these explicit
GAMS operators and prefer a dedicated `gams_eq()` function for equality.

### Variables and solve statements

GAMS variables must be declared before reference. A solve statement uses a model
name, a model type, a direction for optimisation models, and a scalar free
objective variable. GAMS validates that symbolic equations are defined, the
objective variable is scalar and free, and the model fits the requested problem
class.

### GDX and GAMS Transfer R

GDX stores symbol values for sets, parameters, variables, and equations. It does
not store model formulations or executable statements. The GAMS R API overview
points to GAMS Transfer R. `gamstransfer` is on CRAN as version 3.0.8, published
2026-01-09, with imports on `Rcpp`, `R6`, `R.utils`, and `collections`, and a
C++17 system requirement. It should remain optional for `GAMSr` until the data
transfer adapter is implemented.

### GAMSPy lessons

GAMSPy emphasizes set-based symbolic algebra, mathematical-model generation
instead of eager instance expansion, sparsity-aware data handling, generated
GAMS inspection, and multiple backends. `GAMSr` should borrow the product shape,
not source code, examples, wording, or tests. The R design should use R idioms
and avoid Python-to-R transliteration.

### Package name

The name `GAMSr` is provisional. Web search found prior GitHub usage under
`christophe-gouel/gamsr` and older forum references to a package with a similar
name. No CRAN package named exactly `gamsr` appeared in the search results, but
formal CRAN and trademark checks remain required before publication because the
name contains `GAMS` and could imply endorsement.

## Open Questions

1. Exact escaping rule for GAMS labels that contain both single and double
   quotes.
2. Whether `gamstransfer` should be `Suggests` or an adapter-specific optional
   dependency for the first CRAN release.
3. Exact model and solver status mapping to expose in R result objects.
4. GAMS Transfer R special-value representation for `EPS`, `INF`, `-INF`, `NA`,
   and `UNDF`.
5. Final public package name and disclaimer wording.
