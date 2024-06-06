
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
## 14.39 sec elapsed
```

``` r
new.fvc = bind_cols(select(fvc,fvc,lulc),g$disv)
gd(fvc ~ .,data = new.fvc,type = 'factor')
## $factor
## # A tibble: 12 × 3
##    variable `Q-statistic` `P-value`
##    <chr>            <dbl>     <dbl>
##  1 presum         0.642    5.67e-10
##  2 lulc           0.553    9.11e-10
##  3 premin         0.441    3.84e-10
##  4 tmpmin         0.406    7.59e-10
##  5 tmpmax         0.228    5.11e-10
##  6 elev           0.209    1.50e-10
##  7 tmpavg         0.197    6.83e-10
##  8 slope          0.194    4.96e-10
##  9 pop            0.186    3.22e-10
## 10 premax         0.134    3.75e-10
## 11 ntl            0.0216   6.33e-10
## 12 aspect         0.00741  5.45e-10
## 
## attr(,"class")
## [1] "factor_detector"
```

``` r
gd(fvc ~ .,data = new.fvc,type = 'interaction')
## $interaction
## # A tibble: 66 × 6
##    variable1 variable2 Interaction Variable1 Q-statisti…¹ Variable2 Q-statisti…²
##    <chr>     <chr>     <chr>                        <dbl>                  <dbl>
##  1 lulc      aspect    Enhance, n…                  0.553                0.00741
##  2 lulc      elev      Enhance, b…                  0.553                0.209  
##  3 lulc      ntl       Enhance, n…                  0.553                0.0216 
##  4 lulc      pop       Enhance, b…                  0.553                0.186  
##  5 lulc      premax    Enhance, b…                  0.553                0.134  
##  6 lulc      premin    Enhance, b…                  0.553                0.441  
##  7 lulc      presum    Enhance, b…                  0.553                0.642  
##  8 lulc      slope     Enhance, b…                  0.553                0.194  
##  9 lulc      tmpavg    Enhance, b…                  0.553                0.197  
## 10 lulc      tmpmax    Enhance, b…                  0.553                0.228  
## # ℹ 56 more rows
## # ℹ abbreviated names: ¹​`Variable1 Q-statistics`, ²​`Variable2 Q-statistics`
## # ℹ 1 more variable: `Variable1 and Variable2 interact Q-statistics` <dbl>
## 
## attr(,"class")
## [1] "interaction_detector"
```
