# Conditional Modeling

GAMSr supports GAMS dollar conditions in equation domains, indexed sums, and
algebraic terms.

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

GAMS dollar conditions cannot contain decision variables. GAMSr validates this
before compilation. Variable-attribute expressions are not supported yet.
