# spatial association detector (SPADE) model

spatial association detector (SPADE) model

## Usage

``` r
spade(
  formula,
  data,
  wt = NULL,
  discvar = NULL,
  discnum = 3:8,
  discmethod = "quantile",
  cores = 1,
  seed = 123456789,
  permutations = 0,
  ...
)
```

## Arguments

- formula:

  A formula of spatial association detector (SPADE) model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- wt:

  (optional) The spatial weight matrix. When `data` is not an `sf`
  object, must provide `wt`.

- discvar:

  (optional) Name of continuous variable columns that need to be
  discretized. Noted that when `formula` has `discvar`, `data` must have
  these columns. By default, all independent variables are used as
  `discvar`.

- discnum:

  (optional) Number of multilevel discretization. Default will use
  `3:8`.

- discmethod:

  (optional) The discretization methods. Default all use `quantile`.
  Note that when using different `discmethod` for `discvar`, please
  ensure that the lengths of both are consistent. Noted that `robust`
  will use
  [`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md);
  `rpart` will use
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md);
  Others use
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html).

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- seed:

  (optional) Random number seed, default is `123456789`.

- permutations:

  (optional) The number of permutations for the PSD computation. Default
  is `0`, which means no pseudo-p values are calculated.

- ...:

  (optional) Other arguments passed to
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html),[`robust_disc()`](https://stscl.github.io/gdverse/reference/robust_disc.md)
  or
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A list.

- `factor`:

  the result of SPADE model

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Examples

``` r
data('sim')
sim1 = sf::st_as_sf(sim,coords = c('lo','la'))
g = spade(y ~ ., data = sim1)
g
#> ***         Spatial Association Detector         
#> 
#> | variable | Q-statistic |      P-value      |
#> |:--------:|:-----------:|:-----------------:|
#> |    xc    |  0.5662369  | No Pseudo-P Value |
#> |    xb    |  0.4351567  | No Pseudo-P Value |
#> |    xa    |  0.3099817  | No Pseudo-P Value |
```
