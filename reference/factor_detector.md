# factor detector

The factor detector q-statistic measures the spatial stratified
heterogeneity of a variable Y, or the determinant power of a covariate X
of Y.

## Usage

``` r
factor_detector(y, x, confintv = FALSE, alpha = 0.95)
```

## Arguments

- y:

  Variable Y, continuous numeric vector.

- x:

  Covariate X, `factor`, `character` or `discrete numeric`.

- confintv:

  (optional) Whether to compute the confidence interval for the q
  statistic, default is `FALSE`.

- alpha:

  (optional) Confidence level of the interval, default is `0.95`.

## Value

A list.

- `Q-statistic`:

  the q statistic for factor detector

- `P-value`:

  the p value for factor detector

- `CIL`:

  the confidence interval lower bound

- `CIU`:

  the confidence interval upper bound

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
factor_detector(y = 1:7,x = c('x',rep('y',3),rep('z',3)))
#> $`Q-statistic`
#> [1] 0.7714286
#> 
#> $`P-value`
#> [1] 0.07936477
#> 
```
