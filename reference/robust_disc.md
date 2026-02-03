# univariate discretization based on offline change point detection

Determines discretization interval breaks using an optimization
algorithm for variance-based change point detection.

## Usage

``` r
robust_disc(formula, data, discnum, minsize = 1, cores = 1)
```

## Arguments

- formula:

  A formula of univariate discretization.

- data:

  A data.frame or tibble of observation data.

- discnum:

  A numeric vector of discretized classes of columns that need to be
  discretized.

- minsize:

  (optional) The min size of each discretization group. Default all use
  `1`.

- cores:

  (optional) A positive integer(default is 1). If cores \> 1, use
  `python` `joblib` package to parallel computation.

## Value

A `tibble`.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
# \donttest{
tryCatch({
  robust_disc(y ~ xa, data = sim, discnum = 5)
  robust_disc(y ~ .,
              data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
              discnum = 5, cores = 3)
}, error = \(e) message("Skipping Python-dependent example: ", e$message))
#> Skipping Python-dependent example: cannot coerce class ‘c("pandas.DataFrame", "pandas.core.generic.NDFrame", "pandas.core.base.PandasObject", ’ to a data.frame
# }
```
