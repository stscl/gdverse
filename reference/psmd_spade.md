# power of spatial and multilevel discretization determinant(PSMD)

Function for calculate power of spatial and multilevel discretization
determinant `PSMDQ_s`.

## Usage

``` r
psmd_spade(
  yobs,
  xobs,
  wt,
  discnum = 3:8,
  discmethod = "quantile",
  cores = 1,
  seed = 123456789,
  ...
)
```

## Arguments

- yobs:

  Variable Y

- xobs:

  The original continuous covariable X.

- wt:

  The spatial weight matrix.

- discnum:

  (optional) Number of multilevel discretizations. Default will use
  `3:8`.

- discmethod:

  (optional) The discretize methods. Default will use `quantile`. If
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

- ...:

  (optional) Other arguments passed to
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html),[`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md)
  or
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A value of power of spatial and multilevel discretization determinant
`PSMDQ_s`.

## Details

The power of spatial and multilevel discretization determinant formula
is \\PSMDQ_s = MEAN(Q_s)\\

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
psmd_spade(sim$y,sim$xa,wt)
#> [1] 0.3099817
```
