# optimal spatial data discretization based on SPADE q-statistics

Function for determining the optimal spatial data discretization based
on SPADE q-statistics.

## Usage

``` r
cpsd_disc(
  formula,
  data,
  wt,
  discnum = 3:8,
  discmethod = "quantile",
  strategy = 2L,
  increase_rate = 0.05,
  cores = 1,
  seed = 123456789,
  ...
)
```

## Arguments

- formula:

  A formula of optimal spatial data discretization.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- wt:

  The spatial weight matrix.

- discnum:

  (optional) A vector of number of classes for discretization. Default
  is `3:8`.

- discmethod:

  (optional) The discretization methods. Default all use `quantile`.
  Noted that `rpart` will use
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md);
  Others use
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html).

- strategy:

  (optional) Discretization strategy. When `strategy` is `1L`, choose
  the highest SPADE model q-statistics to determinate optimal spatial
  data discretization parameters. When `strategy` is `2L`, The optimal
  discrete parameters of spatial data are selected by combining LOESS
  model.

- increase_rate:

  (optional) The critical increase rate of the number of discretization.
  Default is `5%`.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- seed:

  (optional) Random seed number, default is `123456789`.

- ...:

  (optional) Other arguments passed to
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html)
  or
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A list.

- `x`:

  discretization variable name

- `k`:

  optimal number of spatial data discreteization

- `method`:

  optimal spatial data discretization method

- `disc`:

  the result of optimal spatial data discretization

## Note

When the `discmethod` is configured to `robust`, it will operate at a
significantly reduced speed. Consequently, the use of robust
discretization is not advised.

## References

Yongze Song & Peng Wu (2021) An interactive detector for spatial
associations, International Journal of Geographical Information Science,
35:8, 1676-1701, DOI:10.1080/13658816.2021.1882680

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
cpsd_disc(y ~ xa + xb + xc, data = sim, wt = wt)
#> $x
#> [1] "xa" "xb" "xc"
#> 
#> $k
#> [1] 5 5 7
#> 
#> $method
#> [1] "quantile" "quantile" "quantile"
#> 
#> $disv
#> # A tibble: 80 × 3
#>       xa    xb    xc
#>    <int> <int> <int>
#>  1     1     4     3
#>  2     4     4     6
#>  3     2     5     3
#>  4     1     3     2
#>  5     4     4     5
#>  6     2     4     4
#>  7     4     4     6
#>  8     1     2     2
#>  9     4     3     4
#> 10     5     1     6
#> # ℹ 70 more rows
#> 
```
