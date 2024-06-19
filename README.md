
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdverse <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/gdverse)](https://CRAN.R-project.org/package=gdverse)
[![r-universe](https://spatlyu.r-universe.dev/badges/gdverse)](https://spatlyu.r-universe.dev/gdverse)
<!-- badges: end -->

The goal of **gdverse** is to *support the geodetector model and its variants*.

## Installation

You can install the development version of **gdverse** from [github](https://github.com/SpatLyu/gdverse) with:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/gdverse",build_vignettes = T,dep = T)
```

or install **gdverse** from [r-universe](https://spatlyu.r-universe.dev/gdverse):

```r
install.packages('gdverse', repos='https://spatlyu.r-universe.dev')
```

### Load data and package

``` r
library(sf)
library(terra)
library(tidyverse)
library(gdverse)
fvcpath = "https://github.com/SpatLyu/rdevdata/raw/main/FVC.tif"
fvc = terra::rast(paste0("/vsicurl/",fvcpath))
fvc = terra::aggregate(fvc,fact = 5)
fvc = as_tibble(terra::as.data.frame(fvc,na.rm = T))
head(fvc)
## # A tibble: 6 × 13
##     fvc premax premin presum tmpmax tmpmin tmpavg   pop   ntl  lulc  elev slope
##   <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
## 1 0.188   163.   6.86  3992.   21.2  -7.09   8.54  5.64  9.10    10 1645.  2.96
## 2 0.162   162.   5.23  3922.   21.7  -6.90   8.92 23.1  10.5     10 1539.  1.86
## 3 0.168   168.   4.15  4040.   21.2  -7.22   8.53  9.73  5.58    10 1611.  3.19
## 4 0.186   174.   5.99  4254.   20.8  -7.42   8.21  6.84  2.89    10 1677.  3.32
## 5 0.189   164.   7.86  4047.   21.2  -7.00   8.58  2.36 12.3     10 1643.  2.79
## 6 0.171   161.   5.23  3944.   21.7  -6.85   8.91  3.17 10.1     10 1553.  1.93
## # ℹ 1 more variable: aspect <dbl>
```

### OPGD model

``` r
tictoc::tic()
fvc_opgd = opgd(fvc ~ ., data = fvc, discnum = 3:15,
                discvar = names(select(fvc,-c(fvc,lulc))),
                cores = 6, type = 'factor')
tictoc::toc()
## 3.11 sec elapsed
```

``` r
fvc_opgd
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6626    | 9.145e-10 |
|   lulc   |   0.6597    | 8.782e-10 |
|  premin  |   0.4654    | 4.831e-10 |
|  tmpmin  |   0.4323    | 4.367e-10 |
|  tmpmax  |   0.2543    | 4.168e-10 |
|   elev   |   0.2353    | 2.885e-10 |
|  slope   |   0.2338    | 7.849e-10 |
|  tmpavg  |   0.2214    | 7.616e-10 |
|   pop    |   0.1953    | 2.428e-10 |
|  premax  |   0.1442    | 1.682e-10 |
|   ntl    |   0.02272   |  0.0301   |
|  aspect  |   0.0186    |  0.7491   |

### GOZH model

``` r
g = gozh(fvc ~ ., data = fvc, cores = 6)
g
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

|             variable              | Q-statistic |  P-value  |
|:---------------------------------:|:-----------:|:---------:|
| Explanatory Variables Association |   0.8052    | 7.146e-10 |
|              presum               |   0.6372    | 9.492e-11 |
|               lulc                |   0.6106    | 4.797e-10 |
|              premin               |   0.4658    | 5.484e-10 |
|              tmpmin               |   0.4112    | 2.948e-10 |
|              tmpmax               |   0.2478    | 7.308e-10 |
|               slope               |   0.2286    | 6.05e-10  |
|                pop                |   0.2238    | 3.753e-10 |
|               elev                |   0.2237    | 4.671e-10 |
|              tmpavg               |   0.2188    | 6.543e-10 |
|              premax               |   0.1259    | 1.436e-10 |
|                ntl                |   0.02365   | 1.832e-10 |
|              aspect               |   0.01413   | 8.938e-09 |

### RGD model

To run `RGD`,remember to set up your python dependence, see `RGD`
vignette to get more details.

``` r
reticulate::use_condaenv('geocompy')
tictoc::tic()
fvc_rgd = rgd(fvc ~ ., data = fvc, discnum = 10, 
              discvar = names(select(fvc,-c(fvc,lulc))),
              cores = 6, type = 'factor')
tictoc::toc()
## 1886.14 sec elapsed
```

``` r
fvc_rgd
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6678    | 4.284e-10 |
|   lulc   |   0.6597    | 8.782e-10 |
|  premin  |   0.4781    | 7.04e-10  |
|  tmpmin  |    0.448    | 5.718e-10 |
|  tmpmax  |   0.2692    | 3.296e-10 |
|   elev   |    0.248    | 4.734e-10 |
|  slope   |   0.2464    | 8.852e-10 |
|   pop    |   0.2446    | 9.414e-10 |
|  tmpavg  |   0.2405    | 5.228e-10 |
|  premax  |   0.1572    | 5.383e-10 |
|  aspect  |   0.02759   | 0.003456  |
|   ntl    |   0.02334   | 4.623e-10 |