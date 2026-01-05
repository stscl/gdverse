# comparison of size effects of spatial units based on GOZH

comparison of size effects of spatial units based on GOZH

## Usage

``` r
sesu_gozh(
  formula,
  datalist,
  su,
  cores = 1,
  strategy = 2L,
  increase_rate = 0.05,
  alpha = 0.95,
  ...
)
```

## Arguments

- formula:

  A formula of comparison of size effects of spatial units.

- datalist:

  A list of `data.frame` or `tibble`.

- su:

  A vector of sizes of spatial units.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- strategy:

  (optional) Calculation strategies of Q statistics at different scales.
  Default is `2L`, see `details` for more contents.

- increase_rate:

  (optional) The critical increase rate of the number of discretization.
  Default is `5%`.

- alpha:

  (optional) Specifies the size of confidence level. Default is `0.95`.

- ...:

  (optional) Other arguments passed to
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A list.

- `sesu`:

  a tibble representing size effects of spatial units

- `optsu`:

  optimal spatial unit

- `strategy`:

  the optimal analytical scale selection strategy

- `increase_rate`:

  the critical increase rate of q value

## Details

When `strategy` is `1`, use the same process as
[`sesu_opgd()`](https://stscl.github.io/gdverse/reference/sesu_opgd.md).If
not, all explanatory variables are used to generate a unique Q statistic
corresponding to the data in the datalist based on
[`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md)
and [`gd()`](https://stscl.github.io/gdverse/reference/gd.md), and then
[`loess_optscale()`](https://stscl.github.io/gdverse/reference/loess_optscale.md)is
used to determine the optimal analysis scale.

## References

Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based
geographical detector model enhances geographic characteristics of
explanatory variables for spatial heterogeneity analysis: Cases with
different types of spatial data, GIScience & Remote Sensing, 57(5),
593-610. doi: 10.1080/15481603.2020.1760434.

Luo, P., Song, Y., Huang, X., Ma, H., Liu, J., Yao, Y., & Meng, L.
(2022). Identifying determinants of spatio-temporal disparities in soil
moisture of the Northern Hemisphere using a geographically optimal
zones-based heterogeneity model. ISPRS Journal of Photogrammetry and
Remote Sensing: Official Publication of the International Society for
Photogrammetry and Remote Sensing (ISPRS), 185, 111–128.
https://doi.org/10.1016/j.isprsjprs.2022.01.009

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
if (FALSE) { # \dontrun{
## The following code takes a long time to run:
library(tidyverse)
fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
fvc = terra::rast(paste0("/vsicurl/",fvcpath))
fvc1000 = fvc %>%
  terra::as.data.frame(na.rm = T) %>%
  as_tibble()
fvc5000 = fvc %>%
  terra::aggregate(fact = 5) %>%
  terra::as.data.frame(na.rm = T) %>%
  as_tibble()
sesu_gozh(fvc ~ .,
          datalist = list(fvc1000,fvc5000),
          su = c(1000,5000),
          cores = 6)
} # }
```
