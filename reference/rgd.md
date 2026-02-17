# robust geographical detector(RGD) model

robust geographical detector(RGD) model

## Usage

``` r
rgd(
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

- `factor`:

  robust power of determinant

- `opt_disc`:

  optimal robust discrete results

- `allfactor`:

  factor detection results corresponding to different number of robust
  discreteizations

- `alldisc`:

  all robust discrete results

## References

Zhang, Z., Song, Y.\*, & Wu, P., 2022. Robust geographical detector.
International Journal of Applied Earth Observation and Geoinformation.
109, 102782. DOI: 10.1016/j.jag.2022.102782.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
# \donttest{
tryCatch({
    g = rgd(y ~ .,
           data = dplyr::select(sim,-dplyr::any_of(c('lo','la'))),
           discnum = 3:6, cores = 1)
    g
  }, error = \(e) message("Skipping Python-dependent example: ", e$message))
#> Downloading uv...
#> Done!
#> ***      Robust Geographical Detector    
#> 
#> | variable | Q-statistic |   P-value    |
#> |:--------:|:-----------:|:------------:|
#> |    xc    |  0.7011119  | 7.650000e-10 |
#> |    xb    |  0.6032094  | 1.186300e-08 |
#> |    xa    |  0.4969454  | 4.191848e-06 |
#> 
# }
```
