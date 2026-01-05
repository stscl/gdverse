# optimal parameters-based geographical detector(OPGD) model

optimal parameters-based geographical detector(OPGD) model

## Usage

``` r
opgd(
  formula,
  data,
  discvar = NULL,
  discnum = 3:8,
  discmethod = c("sd", "equal", "geometric", "quantile", "natural"),
  cores = 1,
  type = "factor",
  alpha = 0.95,
  ...
)
```

## Arguments

- formula:

  A formula of OPGD model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- discvar:

  Name of continuous variable columns that need to be discretized. Noted
  that when `formula` has `discvar`, `data` must have these columns. By
  default, all independent variables are used as `discvar`.

- discnum:

  (optional) A vector of number of classes for discretization. Default
  is `3:8`.

- discmethod:

  (optional) A vector of methods for discretization, default is using
  `c("sd","equal","geometric","quantile","natural")` by invoking
  `sdsfun`.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- type:

  (optional) The type of geographical detector, which must be
  `factor`(default), `interaction`, `risk`, `ecological`. You can run
  one or more types at one time.

- alpha:

  (optional) Specifies the size of confidence level. Default is `0.95`.

- ...:

  (optional) Other arguments passed to
  [`gd_optunidisc()`](https://stscl.github.io/gdverse/reference/gd_optunidisc.md).
  A useful parameter is `seed`, which is used to set the random number
  seed.

## Value

A list.

- `opt_param`:

  optimal discretization parameter

- `factor`:

  the result of factor detector

- `interaction`:

  the result of interaction detector

- `risk`:

  the result of risk detector

- `ecological`:

  the result of ecological detector

## References

Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based
geographical detector model enhances geographic characteristics of
explanatory variables for spatial heterogeneity analysis: Cases with
different types of spatial data, GIScience & Remote Sensing, 57(5),
593-610. doi: 10.1080/15481603.2020.1760434.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
opgd(y ~ xa + xb + xc, data = sim,
     discvar = paste0('x',letters[1:3]),
     discnum = 3:6)
#> ***   Optimal Parameters-based Geographical Detector     
#>                 Factor Detector            
#> 
#> | variable | Q-statistic |   P-value   |
#> |:--------:|:-----------:|:-----------:|
#> |    xc    |  0.6225785  | 5.72000e-10 |
#> |    xb    |  0.4800064  | 1.00905e-07 |
#> |    xa    |  0.3267163  | 7.64250e-05 |
#> 
```
