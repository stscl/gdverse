# geographically optimal zones-based heterogeneity(GOZH) model

geographically optimal zones-based heterogeneity(GOZH) model

## Usage

``` r
gozh(formula, data, cores = 1, type = "factor", alpha = 0.95, ...)
```

## Arguments

- formula:

  A formula of GOZH model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

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
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A list.

- `factor`:

  the result of factor detector

- `interaction`:

  the result of interaction detector

- `risk`:

  the result of risk detector

- `ecological`:

  the result of ecological detector

## References

Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L.
(2022). Identifying determinants of spatio-temporal disparities in soil
moisture of the Northern Hemisphere using a geographically optimal
zones-based heterogeneity model. ISPRS Journal of Photogrammetry and
Remote Sensing: Official Publication of the International Society for
Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
https://doi.org/10.1016/j.isprsjprs.2022.01.009

## Examples

``` r
data('ndvi')
g = gozh(NDVIchange ~ ., data = ndvi)
g
#> ***   Geographically Optimal Zones-based Heterogeneity Model       
#>                 Factor Detector            
#> 
#> |   variable    | Q-statistic | P-value  |
#> |:-------------:|:-----------:|:--------:|
#> | Precipitation | 0.87255056  | 4.52e-10 |
#> |  Climatezone  | 0.82129550  | 2.50e-10 |
#> |  Tempchange   | 0.33324945  | 1.12e-10 |
#> |  Popdensity   | 0.22321863  | 3.00e-10 |
#> |    Mining     | 0.13982859  | 6.00e-11 |
#> |      GDP      | 0.09170153  | 3.96e-10 |
#> 
```
