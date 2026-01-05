# interaction detector

Identify the interaction between different risk factors, that is, assess
whether factors X1 and X2 together increase or decrease the explanatory
power of the dependent variable Y, or whether the effects of these
factors on Y are independent of each other.

## Usage

``` r
interaction_detector(y, x1, x2)
```

## Arguments

- y:

  Dependent variable, continuous numeric vector.

- x1:

  Covariate \\X_1\\, `factor`, `character` or `discrete numeric`.

- x2:

  Covariate \\X_2\\, `factor`, `character` or `discrete numeric`.

## Value

A list.

- `Variable1 Q-statistics`:

  Q-statistics for variable1

- `Variable2 Q-statistics`:

  Q-statistics for variable2

- `Variable1 and Variable2 interact Q-statistics`:

  Q-statistics for variable1 and variable2 interact

- `Interaction`:

  the interact result type

- `Variable1 P-value`:

  P-value of the Q-statistic for Variable1

- `Variable2 P-value`:

  P-value of the Q-statistic for Variable2

- `Variable1 and Variable2 interact P-value`:

  P-value of the Q-statistic for variable1 and variable2 interact

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
interaction_detector(y = 1:7,
                     x1 = c('x',rep('y',3),rep('z',3)),
                     x2 = c(rep('a',2),rep('b',2),rep('c',3)))
#> $`Variable1 Q-statistics`
#> [1] 0.7714286
#> 
#> $`Variable2 Q-statistics`
#> [1] 0.8928571
#> 
#> $`Variable1 and Variable2 interact Q-statistics`
#> [1] 0.75
#> 
#> $Interaction
#> [1] "Weaken, nonlinear"
#> 
#> $`Variable1 P-value`
#> [1] 0.07936477
#> 
#> $`Variable2 P-value`
#> [1] 0.03167582
#> 
#> $`Variable1 and Variable2 interact P-value`
#> [1] 0.09807732
#> 
```
