# interactive detector for spatial associations(IDSA) model

interactive detector for spatial associations(IDSA) model

## Usage

``` r
idsa(
  formula,
  data,
  wt = NULL,
  discnum = 3:8,
  discmethod = "quantile",
  overlay = "and",
  strategy = 2L,
  increase_rate = 0.05,
  cores = 1,
  seed = 123456789,
  alpha = 0.95,
  ...
)
```

## Arguments

- formula:

  A formula of IDSA model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- wt:

  (optional) The spatial weight matrix. When `data` is not an `sf`
  object, must provide `wt`.

- discnum:

  (optional) Number of multilevel discretization. Default will use
  `3:8`.

- discmethod:

  (optional) The discretization methods. Default all use `quantile`.
  Noted that `rpart` will use
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md);
  Others use
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html).

- overlay:

  (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
  Default is `and`.

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

  (optional) Random number seed, default is `123456789`.

- alpha:

  (optional) Specifies the size of confidence level. Default is `0.95`.

- ...:

  (optional) Other arguments passed to
  [`cpsd_disc()`](https://stscl.github.io/gdverse/reference/cpsd_disc.md).

## Value

A list.

- `interaction`:

  the interaction result of IDSA model

- `risk`:

  whether values of the response variable between a pair of overlay
  zones are significantly different

- `number_individual_explanatory_variables`:

  the number of individual explanatory variables used for examining the
  interaction effects

- `number_overlay_zones`:

  the number of overlay zones

- `percentage_finely_divided_zones`:

  the percentage of finely divided zones that are determined by the
  interaction of variables

## Note

**Please note that all variables in the IDSA model need to be continuous
data**.

The IDSA model requires at least \\2^n-1\\ calculations when has \\n\\
explanatory variables. When there are more than 10 explanatory
variables, carefully consider the computational burden of this model.
When there are a large number of explanatory variables, the data
dimensionality reduction method can be used to ensure the trade-off
between analysis results and calculation speed.

## References

Yongze Song & Peng Wu (2021) An interactive detector for spatial
associations, International Journal of Geographical Information Science,
35:8, 1676-1701, DOI:10.1080/13658816.2021.1882680

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
sim1 = sf::st_as_sf(sim,coords = c('lo','la'))
g = idsa(y ~ ., data = sim1)
g
#> ***     Interactive Detector For Spatial Associations 
#> 
#> |   variable   |    PID    |
#> |:------------:|:---------:|
#> |   xa ∩ xb    | 0.5540009 |
#> |   xb ∩ xc    | 0.5467294 |
#> |      xc      | 0.5411351 |
#> |   xa ∩ xc    | 0.4515003 |
#> | xa ∩ xb ∩ xc | 0.4275805 |
#> 
#>  --------- IDSA model performance evaluation: --------
#>  * Number of overlay zones :  9 
#>  * Percentage of finely divided zones :  0 
#>  * Number of individual explanatory variables :  2 
#>  
#>  ## Different of response variable between a pair of overlay zones:
#> 
#> | zone1st  | zone2nd  | Risk |
#> |:--------:|:--------:|:----:|
#> | zonexa_1 | zonexa_2 |  No  |
#> | zonexa_1 | zonexa_3 | Yes  |
#> | zonexa_1 | zonexa_4 |  No  |
#> | zonexa_1 | zonexb_1 | Yes  |
#> | zonexa_1 | zonexb_2 |  No  |
#> 
#>  #### Only the first five pairs of interactions and overlay zones are displayed! ####
```
