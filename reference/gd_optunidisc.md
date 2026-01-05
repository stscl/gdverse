# optimal univariate discretization based on geodetector q-statistic

optimal univariate discretization based on geodetector q-statistic

## Usage

``` r
gd_optunidisc(
  formula,
  data,
  discnum = 3:8,
  discmethod = c("sd", "equal", "geometric", "quantile", "natural"),
  cores = 1,
  seed = 123456789,
  ...
)
```

## Arguments

- formula:

  A formula.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- discnum:

  (optional) A vector of numbers of discretization. Default is `3:8`.

- discmethod:

  (optional) A vector of methods for discretization, default is using
  `c("sd","equal","geometric","quantile","natural")` by invoking
  `sdsfun`.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- seed:

  (optional) Random seed number, default is `123456789`.

- ...:

  (optional) Other arguments passed to
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html).

## Value

A list.

- `x`:

  the name of the variable that needs to be discretized

- `k`:

  optimal discretization number

- `method`:

  optimal discretization method

- `qstatistic`:

  optimal q-statistic

- `disc`:

  optimal discretization results

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
gd_optunidisc(y ~ xa + xb + xc,
              data = sim,
              discnum = 3:6)
#> $x
#> [1] "xa" "xb" "xc"
#> 
#> $k
#> [1] 6 6 6
#> 
#> $method
#> [1] "geometric" "geometric" "geometric"
#> 
#> $qstatistic
#> [1] 0.3267163 0.4800064 0.6225785
#> 
#> $disc
#> # A tibble: 80 × 3
#>       xa    xb    xc
#>    <int> <int> <int>
#>  1     2     5     4
#>  2     5     5     5
#>  3     3     5     4
#>  4     3     4     3
#>  5     5     5     5
#>  6     4     5     4
#>  7     5     5     5
#>  8     2     3     2
#>  9     5     4     4
#> 10     6     3     5
#> # ℹ 70 more rows
#> 
```
