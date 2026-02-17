# comparison of size effects of spatial units based on OPGD

comparison of size effects of spatial units based on OPGD

## Usage

``` r
sesu_opgd(
  formula,
  datalist,
  su,
  discvar,
  discnum = 3:8,
  discmethod = c("sd", "equal", "geometric", "quantile", "natural"),
  cores = 1,
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

- discvar:

  Name of continuous variable columns that need to be discretized.Noted
  that when `formula` has `discvar`, `data` must have these columns.

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

- increase_rate:

  (optional) The critical increase rate of the number of discretization.
  Default is `5%`.

- alpha:

  (optional) Specifies the size of confidence level. Default is `0.95`.

- ...:

  (optional) Other arguments passed to `gd_bestunidisc()`.

## Value

A list.

- `sesu`:

  a tibble representing size effects of spatial units

- `optsu`:

  optimal spatial unit

- `increase_rate`:

  the critical increase rate of q value

## Details

Firstly, the `OPGD` model is executed for each data in the datalist (all
`significant` Q statistic of each data are averaged to represent the
spatial association strength under this spatial unit), and then the
`loess_optscale` function is used to select the optimal spatial analysis
scale.

## References

Song, Y., Wang, J., Ge, Y. & Xu, C. (2020) An optimal parameters-based
geographical detector model enhances geographic characteristics of
explanatory variables for spatial heterogeneity analysis: Cases with
different types of spatial data, GIScience & Remote Sensing, 57(5),
593-610. doi: 10.1080/15481603.2020.1760434.

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
sesu_opgd(fvc ~ .,
          datalist = list(fvc1000,fvc5000),
          su = c(1000,5000),
          discvar = names(select(fvc5000,-c(fvc,lulc))),
          cores = 6)
} # }
```
