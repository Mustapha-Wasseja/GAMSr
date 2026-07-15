# Conditional Modeling

GAMSr supports GAMS dollar conditions in equation domains, indexed sums,
algebraic terms, and assignment targets.

```r
m <- gams_model("conditional_model")
i <- gams_set(m, "i", records = c("a", "b", "c"))
active <- gams_parameter(m, "active", domain = i, records = c(a = 1, b = 0, c = 1))
x <- gams_variable(m, "x", domain = i, type = "positive")
floor <- gams_equation(m, "floor", domain = i)

floor[i] <- gams_where(x[i] >= gams_ord(i), active[i] == 1)

objective <- gams_sum(i, x[i], condition = active[i] == 1)
```

The equation compiles to a condition on the domain of definition:

```gams
floor(i)$(active(i) = 1).. x(i) =g= ord(i);
```

Conditions may combine `<`, `<=`, `==`, `!=`, `>=`, and `>` with `&`, `|`,
and `!`. Use `gams_same_as(i, "a")` to compare a set element with another set,
alias, or literal label.

```r
condition <- (active[i] > 0) & !gams_same_as(i, "b")
term <- gams_where(active[i] * 2, condition)
```

GAMS dollar conditions cannot contain decision variables directly. GAMSr
validates this before compilation, while allowing symbolic variable attributes:

```r
x <- set_level(x, c(a = 1, b = 0, c = 1))
selected <- gams_dynamic_set(m, "selected", domain = i)
selected[i] <- gams_level(x[i]) > 0
```

The available attribute expressions are `gams_level()`, `gams_marginal()`,
`gams_lower_bound()`, and `gams_upper_bound()`.

Parameter and dynamic-set assignments are emitted in registration order.
Indexed replacement syntax registers an unconditional assignment:

```r
selected[i] <- active[i] > 0
active[i] <- active[i] * 2
```

Use `gams_assign()` when the assignment target itself has a dollar condition.
Values outside the condition retain their previous values:

```r
gams_assign(active[i], active[i] * 2, condition = selected[i])
```

Dynamic sets are declared in generated GAMS source and excluded from input GDX
data because their membership is computed by these assignments.
