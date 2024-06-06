
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdverse

<!-- badges: start -->
![CRAN](https://www.r-pkg.org/badges/version/gdverse)
<!-- badges: end -->

The goal of gdverse is to support the geodetector model and its
variants.

## Installation

You can install the development version of gdverse from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/gdverse",build_vignettes = T,dep = T)
```

## Example

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
tictoc::tic()
g = gd_bestunidisc(fvc ~ .,data = select(fvc,-lulc),discnum = 2:15,cores = 6)
tictoc::toc()
## 14.33 sec elapsed
```

``` r
new.fvc = bind_cols(select(fvc,fvc,lulc),g$disv)
gd(fvc ~ .,data = new.fvc,type = 'factor')
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6404    | 6.057e-10 |
|   lulc   |   0.5533    | 9.106e-10 |
|  premin  |   0.4381    | 5.372e-10 |
|  tmpmin  |    0.407    | 8.491e-10 |
|  tmpmax  |   0.2284    | 5.111e-10 |
|   elev   |    0.209    |  1.5e-10  |
|  tmpavg  |   0.1972    | 3.432e-10 |
|  slope   |   0.1945    | 4.206e-10 |
|   pop    |   0.1856    | 3.221e-10 |
|  premax  |    0.133    | 3.39e-10  |
|   ntl    |   0.02159   | 6.877e-10 |
|  aspect  |   0.00741   | 5.448e-10 |

``` r
gd(fvc ~ .,data = new.fvc,type = 'interaction')
## Spatial Stratified Heterogeneity Test 
##  
##          Interaction detector
```

| Interactive variable |    Interaction     |
|:--------------------:|:------------------:|
|    lulc ∩ aspect     | Enhance, nonlinear |
|     lulc ∩ elev      |    Enhance, bi-    |
|      lulc ∩ ntl      | Enhance, nonlinear |
|      lulc ∩ pop      |    Enhance, bi-    |
|    lulc ∩ premax     |    Enhance, bi-    |
|    lulc ∩ premin     |    Enhance, bi-    |
|    lulc ∩ presum     |    Enhance, bi-    |
|     lulc ∩ slope     |    Enhance, bi-    |
|    lulc ∩ tmpavg     |    Enhance, bi-    |
|    lulc ∩ tmpmax     |    Enhance, bi-    |
|    lulc ∩ tmpmin     |    Enhance, bi-    |
|    aspect ∩ elev     | Enhance, nonlinear |
|     aspect ∩ ntl     | Enhance, nonlinear |
|     aspect ∩ pop     | Enhance, nonlinear |
|   aspect ∩ premax    | Enhance, nonlinear |
|   aspect ∩ premin    | Enhance, nonlinear |
|   aspect ∩ presum    |    Weaken, uni-    |
|    aspect ∩ slope    | Enhance, nonlinear |
|   aspect ∩ tmpavg    | Enhance, nonlinear |
|   aspect ∩ tmpmax    | Enhance, nonlinear |
|   aspect ∩ tmpmin    | Enhance, nonlinear |
|      elev ∩ ntl      | Enhance, nonlinear |
|      elev ∩ pop      |    Enhance, bi-    |
|    elev ∩ premax     | Enhance, nonlinear |
|    elev ∩ premin     |    Enhance, bi-    |
|    elev ∩ presum     |    Enhance, bi-    |
|     elev ∩ slope     |    Enhance, bi-    |
|    elev ∩ tmpavg     |    Enhance, bi-    |
|    elev ∩ tmpmax     | Enhance, nonlinear |
|    elev ∩ tmpmin     |    Enhance, bi-    |
|      ntl ∩ pop       | Enhance, nonlinear |
|     ntl ∩ premax     | Enhance, nonlinear |
|     ntl ∩ premin     | Enhance, nonlinear |
|     ntl ∩ presum     | Enhance, nonlinear |
|     ntl ∩ slope      | Enhance, nonlinear |
|     ntl ∩ tmpavg     | Enhance, nonlinear |
|     ntl ∩ tmpmax     | Enhance, nonlinear |
|     ntl ∩ tmpmin     | Enhance, nonlinear |
|     pop ∩ premax     | Enhance, nonlinear |
|     pop ∩ premin     |    Enhance, bi-    |
|     pop ∩ presum     |    Enhance, bi-    |
|     pop ∩ slope      |    Enhance, bi-    |
|     pop ∩ tmpavg     | Enhance, nonlinear |
|     pop ∩ tmpmax     | Enhance, nonlinear |
|     pop ∩ tmpmin     |    Enhance, bi-    |
|   premax ∩ premin    | Enhance, nonlinear |
|   premax ∩ presum    |    Enhance, bi-    |
|    premax ∩ slope    | Enhance, nonlinear |
|   premax ∩ tmpavg    | Enhance, nonlinear |
|   premax ∩ tmpmax    | Enhance, nonlinear |
|   premax ∩ tmpmin    | Enhance, nonlinear |
|   premin ∩ presum    |    Enhance, bi-    |
|    premin ∩ slope    |    Enhance, bi-    |
|   premin ∩ tmpavg    |    Enhance, bi-    |
|   premin ∩ tmpmax    |    Enhance, bi-    |
|   premin ∩ tmpmin    |    Enhance, bi-    |
|    presum ∩ slope    |    Enhance, bi-    |
|   presum ∩ tmpavg    |    Enhance, bi-    |
|   presum ∩ tmpmax    |    Enhance, bi-    |
|   presum ∩ tmpmin    |    Enhance, bi-    |
|    slope ∩ tmpavg    |    Enhance, bi-    |
|    slope ∩ tmpmax    |    Enhance, bi-    |
|    slope ∩ tmpmin    |    Enhance, bi-    |
|   tmpavg ∩ tmpmax    | Enhance, nonlinear |
|   tmpavg ∩ tmpmin    | Enhance, nonlinear |
|   tmpmax ∩ tmpmin    | Enhance, nonlinear |
