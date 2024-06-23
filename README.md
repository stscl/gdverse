
<!-- README.md is generated from README.Rmd. Please edit that file -->

# gdverse <img src="man/figures/logo.png" align="right" height="120"/>

<!-- badges: start -->

[![CRAN](https://www.r-pkg.org/badges/version/gdverse)](https://CRAN.R-project.org/package=gdverse)
[![r-universe](https://spatlyu.r-universe.dev/badges/gdverse)](https://spatlyu.r-universe.dev/gdverse)
<!-- badges: end -->

The goal of **gdverse** is to *support the geodetector model and its
variants*.

## Overview

Full document of the most recent release of **gdverse** is online:
<https://spatlyu.github.io/gdverse/>

Current models and functions provided by **gdverse** are:

| geodetector model | *gdverse* function | support status |
|-------------------|--------------------|----------------|
| **GeoDetector**   | `gd()`             | ✔️             |
| **OPGD**          | `opgd()`           | ✔️             |
| **GOZH**          | `gozh()`           | ✔️             |
| **LESH**          | `lesh()`           | ✔️             |
| **SPADE**         | `spade()`          | ✔️             |
| **IDSA**          | `idsa()`           | ❌             |
| **RGD**           | `rgd()`            | ✔️             |
| **RID**           | `rid()`            | ❌             |

## Installation

You can install the development version of **gdverse** from
[github](https://github.com/SpatLyu/gdverse) with:

``` r
# install.packages("devtools")
devtools::install_github("SpatLyu/gdverse",build_vignettes = T,dep = T)
```

or install **gdverse** from
[r-universe](https://spatlyu.r-universe.dev/gdverse):

``` r
install.packages('gdverse', repos='https://spatlyu.r-universe.dev')
```

## Example

``` r
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
## 3.26 sec elapsed
```

``` r
fvc_opgd
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6609    | 8.344e-10 |
|   lulc   |   0.6597    | 8.782e-10 |
|  premin  |   0.4655    | 3.606e-10 |
|  tmpmin  |   0.4356    | 8.537e-10 |
|  tmpmax  |   0.2536    | 7.849e-10 |
|  slope   |   0.2357    | 8.826e-10 |
|   elev   |   0.2355    | 7.287e-10 |
|  tmpavg  |   0.2214    | 7.616e-10 |
|   pop    |   0.1953    | 2.428e-10 |
|  premax  |   0.1451    | 5.961e-10 |
|  aspect  |   0.0186    |  0.7491   |
|   ntl    |   0.01726   |  0.1384   |

### GOZH model

``` r
g = gozh(fvc ~ ., data = fvc, cores = 6, type = 'factor')
g
## Spatial Stratified Heterogeneity Test 
##  
##           Factor detector
```

| variable | Q-statistic |  P-value  |
|:--------:|:-----------:|:---------:|
|  presum  |   0.6372    | 9.492e-11 |
|   lulc   |   0.6106    | 4.797e-10 |
|  premin  |   0.4658    | 5.484e-10 |
|  tmpmin  |   0.4112    | 2.948e-10 |
|  tmpmax  |   0.2478    | 7.308e-10 |
|  slope   |   0.2286    | 6.05e-10  |
|   pop    |   0.2238    | 3.753e-10 |
|   elev   |   0.2237    | 4.671e-10 |
|  tmpavg  |   0.2188    | 6.543e-10 |
|  premax  |   0.1259    | 1.436e-10 |
|   ntl    |   0.02365   | 1.832e-10 |
|  aspect  |   0.01413   | 8.938e-09 |
