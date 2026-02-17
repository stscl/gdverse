# risk detector

Determine whether there is a significant difference between the
attribute means of two sub regions.

## Usage

``` r
risk_detector(y, x, alpha = 0.95)
```

## Arguments

- y:

  Variable Y, continuous numeric vector.

- x:

  Covariate X, `factor`, `character` or `discrete numeric`.

- alpha:

  (optional) Confidence level of the interval, default is `0.95`.

## Value

A tibble.

## Examples

``` r
risk_detector(y = 1:7,
              x = c('x',rep('y',3),rep('z',3)))
#> # A tibble: 3 × 6
#>   zone1st zone2nd `T-statistic` `Degree-freedom` `P-value` Risk 
#>   <chr>   <chr>           <dbl>            <dbl>     <dbl> <fct>
#> 1 zonex   zoney            0                   0    1      No   
#> 2 zonex   zonez            0                   0    1      No   
#> 3 zoney   zonez           -3.67                4    0.0213 Yes  
```
