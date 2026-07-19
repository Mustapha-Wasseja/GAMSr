# Model Classes

`gams_problem()` validates the selected model class before source generation.
The checks catch two common errors early:

1. Binary and integer variables require a discrete or relaxed-discrete model
   class such as MIP, MIQCP, MINLP, RMIP, RMIQCP, or RMINLP.
2. LP/MIP expressions must be linear, while QCP/MIQCP expressions may be at
   most quadratic. Higher-order powers, variable denominators, and intrinsic
   functions require NLP or MINLP.

```r
m <- gams_model("quadratic")
x <- gams_variable(m, "x", type = "positive")
bound <- gams_equation(m, "bound")
bound[] <- x^2 <= 4

problem <- gams_problem(
  m,
  equations = bound,
  objective = x,
  sense = "max",
  problem = "QCP"
)
```

Integer exponents use GAMS `power()` rather than `**`. This allows expressions
such as `(x - 3)^2` to remain defined when the base is temporarily negative.
Real-valued exponents retain GAMS `**` semantics and therefore require a base
in the function's valid domain.

## Licensed Integration Test

The regular GAMS integration tests run whenever GAMS and `gamstransfer` are
available. The larger licensed test is opt-in because development and CI
machines may intentionally use a demo installation:

```powershell
$env:GAMSR_RUN_LICENSED_TESTS = "true"
Rscript -e "devtools::test()"
```

That fixture creates 2,100 indexed rows and variables and solves with HiGHS.
It verifies operation beyond the GAMS demo model-size ceiling without assuming
a separate commercial solver entitlement.
