
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdverse <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/gdverse)](https://CRAN.R-project.org/package=gdverse)
[![r-universe](https://spatlyu.r-universe.dev/badges/gdverse)](https://spatlyu.r-universe.dev/gdverse)
<!-- badges: end -->

The goal of gdverse is to support the geodetector model and its
variants.

## Installation

You can install the development version of gdverse from
[GitHub](https://github.com/SpatLyu/gdverse) with:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/gdverse",build_vignettes = T,dep = T)
```

or install `gdverse` from `r-universe`:

``` r
install.packages('gdverse', repos='https://spatlyu.r-universe.dev')
```

### OPGD model

``` r
library(sf)
library(terra)
library(tidyverse)
library(gdverse)
fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
fvc = terra::rast(paste0("/vsicurl/",fvcpath))
fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
head(fvc)
## # A tibble: 6 × 13
##     fvc premax premin presum tmpmax tmpmin tmpavg    pop   ntl  lulc  elev slope
##   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 0.198   163.   7.95  3956.   20.8  -7.53   8.05  1.90   6.60    10 1758.  2.65
## 2 0.193   161.   6.80  3892.   20.7  -7.55   8.02  1.20   4.91    10 1754.  3.45
## 3 0.192   160.   5.24  3842.   20.9  -7.48   8.15  0.547  3.75    10 1722.  3.96
## 4 0.189   159.   5     3808.   21.1  -7.39   8.35  0.542  3.99    10 1672.  2.90
## 5 0.208   164.   9.98  4051.   20.6  -7.59   7.97 10.4    7.10    10 1780.  1.94
## 6 0.196   163.   8.15  3973.   20.7  -7.53   8.03  9.31   6.56    10 1755.  3.01
## # ℹ 1 more variable: aspect <dbl>
```

``` r
set.seed(12345678)
tictoc::tic()
fvc_gd = opgd(fvc ~ .,data = fvc,
              discvar = names(select(fvc,-c(fvc,lulc))),
              cores = 6, type = 'factor')
tictoc::toc()
## 14.33 sec elapsed
```

``` r
fvc_gd
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6402    | 6.669e-10 |
|   lulc   |   0.5533    | 9.106e-10 |
|  premin  |   0.4433    | 6.004e-10 |
|  tmpmin  |   0.4065    | 4.706e-10 |
|  tmpmax  |   0.2284    | 5.111e-10 |
|   elev   |    0.209    |  1.5e-10  |
|  tmpavg  |    0.197    | 6.833e-10 |
|  slope   |   0.1937    | 8.865e-10 |
|   pop    |   0.1856    | 3.221e-10 |
|  premax  |   0.1324    | 2.448e-10 |
|   ntl    |   0.02125   | 6.277e-10 |
|  aspect  |   0.00741   | 5.448e-10 |
