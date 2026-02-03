# robust interaction detector(RID) model

robust interaction detector(RID) model

## Usage

``` r
rid(
  formula,
  data,
  discvar = NULL,
  discnum = 3:8,
  minsize = 1,
  strategy = 2L,
  increase_rate = 0.05,
  cores = 1
)
```

## Arguments

- formula:

  A formula of RGD model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- discvar:

  Name of continuous variable columns that need to be discretized. Noted
  that when `formula` has `discvar`, `data` must have these columns. By
  default, all independent variables are used as `discvar`.

- discnum:

  A numeric vector of discretized classes of columns that need to be
  discretized. Default all `discvar` use `3:8`.

- minsize:

  (optional) The min size of each discretization group. Default all use
  `1`.

- strategy:

  (optional) Optimal discretization strategy. When `strategy` is `1L`,
  choose the highest q-statistics to determinate optimal spatial data
  discretization parameters. When `strategy` is `2L`, The optimal
  discrete parameters of spatial data are selected by combining LOESS
  model.

- increase_rate:

  (optional) The critical increase rate of the number of discretization.
  Default is `5%`.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

## Value

A list.

- `interaction`:

  the result of RID model

## References

Zhang, Z., Song, Y., Karunaratne, L., & Wu, P. (2024). Robust
interaction detector: A case of road life expectancy analysis. Spatial
Statistics, 59(100814), 100814.
https://doi.org/10.1016/j.spasta.2024.100814

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
# \donttest{
tryCatch({
  g = rid(y ~ .,
          data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
          discnum = 3:6, cores = 1)
  g
}, error = \(e) message("Skipping Python-dependent example: ", e$message))
#> Skipping Python-dependent example: cannot coerce class ‘c("pandas.DataFrame", "pandas.core.generic.NDFrame", "pandas.core.base.PandasObject", ’ to a data.frame
# }
```
