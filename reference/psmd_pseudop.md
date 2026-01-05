# power of spatial and multilevel discretization determinant(PSMD) and the corresponding pseudo-p value

Function for calculate power of spatial and multilevel discretization
determinant and the corresponding pseudo-p value.

## Usage

``` r
psmd_pseudop(
  yobs,
  xobs,
  wt,
  discnum = 3:8,
  discmethod = "quantile",
  cores = 1,
  seed = 123456789,
  permutations = 0,
  ...
)
```

## Arguments

- yobs:

  Variable Y

- xobs:

  The original undiscretized covariable X.

- wt:

  The spatial weight matrix.

- discnum:

  (optional) Number of multilevel discretization. Default will use
  `3:8`.

- discmethod:

  (optional) The discretization methods. Default will use `quantile`. If
  `discmethod` is set to `robust`, the function
  [`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md)
  will be used. Conversely, if `discmethod` is set to `rpart`, the
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md)
  function will be used. Others use
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html).
  Currently, only one `discmethod` can be used at a time.

- cores:

  (optional) A positive integer(default is 1). If cores \> 1, use
  parallel computation.

- seed:

  (optional) Random seed number, default is `123456789`.

- permutations:

  (optional) The number of permutations for the PSD computation. Default
  is `0`, which means no pseudo-p values are calculated.

- ...:

  (optional) Other arguments passed to
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html),[`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md)
  or
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A tibble of power of spatial and multilevel discretization determinant
and the corresponding pseudo-p value.

## Details

The power of spatial and multilevel discretization determinant formula
is \\PSMDQ_s = MEAN(Q_s)\\

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
psmd_pseudop(sim$y,sim$xa,wt)
#> # A tibble: 1 × 2
#>   `Q-statistic` `P-value`        
#>           <dbl> <chr>            
#> 1         0.310 No Pseudo-P Value
```
