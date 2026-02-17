# ecological detector

Compare the effects of two factors \\X_1\\ and \\X_2\\ on the spatial
distribution of the attribute \\Y\\.

## Usage

``` r
ecological_detector(y, x1, x2, alpha = 0.95)
```

## Arguments

- y:

  Dependent variable, continuous numeric vector.

- x1:

  Covariate \\X_1\\, `factor`, `character` or `discrete numeric`.

- x2:

  Covariate \\X_2\\, `factor`, `character` or `discrete numeric`.

- alpha:

  (optional) Confidence level of the interval,default is `0.95`.

## Value

A list.

- `F-statistic`:

  the result of F statistic for ecological detector

- `P-value`:

  the result of P value for ecological detector

- `Ecological`:

  is there a significant difference between the two factors \\X_1\\ and
  \\X_2\\ on the spatial distribution of the attribute \\Y\\

## Examples

``` r
ecological_detector(y = 1:7,
                    x1 = c('x',rep('y',3),rep('z',3)),
                    x2 = c(rep('a',2),rep('b',2),rep('c',3)))
#> $`F-statistic`
#> [1] 2.133333
#> 
#> $`P-value`
#> [1] 0.189319
#> 
#> $Ecological
#> [1] No
#> Levels: Yes No
#> 
```
