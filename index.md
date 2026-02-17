# gdverse

**Analysis of Spatial Stratified Heterogeneity**

## Overview

*gdverse* consolidates cutting-edge SSH methodologies into a unified
toolkit, redefining spatial association measurement as the evolutionary
successor to
[geodetector](https://CRAN.R-project.org/package=geodetector) and
[GD](https://CRAN.R-project.org/package=GD) in the R ecosystem.

Current models and functions provided by **gdverse** are:

| *Model*                                                 | *Function*                                                      | *Support* |
|---------------------------------------------------------|-----------------------------------------------------------------|-----------|
| [GD](https://doi.org/10.1080/13658810802443457)         | [`gd()`](https://stscl.github.io/gdverse/reference/gd.md)       | ✔️        |
| [OPGD](https://doi.org/10.1080/15481603.2020.1760434)   | [`opgd()`](https://stscl.github.io/gdverse/reference/opgd.md)   | ✔️        |
| [GOZH](https://doi.org/10.1016/j.isprsjprs.2022.01.009) | [`gozh()`](https://stscl.github.io/gdverse/reference/gozh.md)   | ✔️        |
| [LESH](https://doi.org/10.1080/17538947.2023.2271883)   | [`lesh()`](https://stscl.github.io/gdverse/reference/lesh.md)   | ✔️        |
| [SPADE](https://doi.org/10.1080/13658816.2018.1476693)  | [`spade()`](https://stscl.github.io/gdverse/reference/spade.md) | ✔️        |
| [IDSA](https://doi.org/10.1080/13658816.2021.1882680)   | [`idsa()`](https://stscl.github.io/gdverse/reference/idsa.md)   | ✔️        |
| [RGD](https://doi.org/10.1016/j.jag.2022.102782)        | [`rgd()`](https://stscl.github.io/gdverse/reference/rgd.md)     | ✔️        |
| [RID](https://doi.org/10.1016/j.spasta.2024.100814)     | [`rid()`](https://stscl.github.io/gdverse/reference/rid.md)     | ✔️        |
| [SRSGD](https://doi.org/10.1016/j.ins.2021.12.019)      | [`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md) | ✔️        |

## Installation

- Install from [CRAN](https://CRAN.R-project.org/package=gdverse) with:

``` r
install.packages("gdverse", dep = TRUE)
```

- Install development binary version from
  [R-universe](https://stscl.r-universe.dev/gdverse) with:

``` r
install.packages('gdverse',
                 repos = c("https://stscl.r-universe.dev",
                           "https://cloud.r-project.org"),
                 dep = TRUE)
```

- Install development source version from
  [GitHub](https://github.com/stscl/gdverse) with:

``` r
# install.packages("devtools")
devtools::install_github("stscl/gdverse",
                         build_vignettes = TRUE,
                         dep = TRUE)
```

✨ Please ensure that **Rcpp** is properly installed and the appropriate
**C++** compilation environment is configured in advance if you want to
install **gdverse** from github.

✨ The **gdverse** package supports the use of robust discretization for
the robust geographical detector and robust interaction detector. For
details on using them, please refer to
<https://stscl.github.io/gdverse/articles/rgdrid.html>.

## Example

``` r
library(gdverse)
data("ndvi")
ndvi
## # A tibble: 713 × 7
##    NDVIchange Climatezone Mining Tempchange Precipitation    GDP Popdensity
##         <dbl> <chr>       <fct>       <dbl>         <dbl>  <dbl>      <dbl>
##  1    0.116   Bwk         low         0.256          237.  12.6      1.45  
##  2    0.0178  Bwk         low         0.273          214.   2.69     0.801 
##  3    0.138   Bsk         low         0.302          449.  20.1     11.5   
##  4    0.00439 Bwk         low         0.383          213.   0        0.0462
##  5    0.00316 Bwk         low         0.357          205.   0        0.0748
##  6    0.00838 Bwk         low         0.338          201.   0        0.549 
##  7    0.0335  Bwk         low         0.296          210.  11.9      1.63  
##  8    0.0387  Bwk         low         0.230          236.  30.2      4.99  
##  9    0.0882  Bsk         low         0.214          342. 241       20.0   
## 10    0.0690  Bsk         low         0.245          379.  42.0      7.50  
## # ℹ 703 more rows
```

### OPGD model

``` r
discvar = names(ndvi)[-1:-3]
discvar
## [1] "Tempchange"    "Precipitation" "GDP"           "Popdensity"
ndvi_opgd = opgd(NDVIchange ~ ., data = ndvi, 
                 discvar = discvar, cores = 6)
ndvi_opgd
## ***   Optimal Parameters-based Geographical Detector     
##                 Factor Detector            
## 
## |   variable    | Q-statistic | P-value  |
## |:-------------:|:-----------:|:--------:|
## | Precipitation |  0.8693505  | 2.58e-10 |
## |  Climatezone  |  0.8218335  | 7.34e-10 |
## |  Tempchange   |  0.3330256  | 1.89e-10 |
## |  Popdensity   |  0.1990773  | 6.60e-11 |
## |    Mining     |  0.1411154  | 6.73e-10 |
## |      GDP      |  0.1004568  | 3.07e-10 |
```

### GOZH model

``` r
g = gozh(NDVIchange ~ ., data = ndvi)
g
## ***   Geographically Optimal Zones-based Heterogeneity Model       
##                 Factor Detector            
## 
## |   variable    | Q-statistic | P-value  |
## |:-------------:|:-----------:|:--------:|
## | Precipitation | 0.87255056  | 4.52e-10 |
## |  Climatezone  | 0.82129550  | 2.50e-10 |
## |  Tempchange   | 0.33324945  | 1.12e-10 |
## |  Popdensity   | 0.22321863  | 3.00e-10 |
## |    Mining     | 0.13982859  | 6.00e-11 |
## |      GDP      | 0.09170153  | 3.96e-10 |
```

## CITATION

Please cite **gdverse** as:

``` R
Lv, W., Lei, Y., Liu, F., Yan, J., Song, Y. and Zhao, W. (2025), gdverse: An R Package for Spatial Stratified Heterogeneity Family. Transactions in GIS, 29(2). https://doi.org/10.1111/tgis.70032
```

A BibTeX entry for LaTeX users is:

``` bib
@article{lyu2025gdverse, 
    title={{gdverse}: An {R} Package for Spatial Stratified Heterogeneity Family}, 
    volume={29}, 
    ISSN={1467-9671},
    DOI={10.1111/tgis.70032},
    number={2}, 
    journal={Transactions in GIS}, 
    publisher={Wiley}, 
    author={Lv, Wenbo and Lei, Yangyang and Liu, Fangmei and Yan, Jianwu and Song, Yongze and Zhao, Wufan},
    year={2025}, 
    month={mar}
}
```
