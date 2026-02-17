# locally explained stratified heterogeneity(LESH) model

locally explained stratified heterogeneity(LESH) model

## Usage

``` r
lesh(formula, data, cores = 1, ...)
```

## Arguments

- formula:

  A formula of LESH model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- ...:

  (optional) Other arguments passed to
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A list.

- `interaction`:

  the interaction result of LESH model

- `spd_lesh`:

  a tibble of the shap power of determinants

## Note

The LESH model requires at least \\2^n-1\\ calculations when has \\n\\
explanatory variables. When there are more than 10 explanatory
variables, carefully consider the computational burden of this model.
When there are a large number of explanatory variables, the data
dimensionality reduction method can be used to ensure the trade-off
between analysis results and calculation speed.

## References

Li, Y., Luo, P., Song, Y., Zhang, L., Qu, Y., & Hou, Z. (2023). A
locally explained heterogeneity model for examining wetland disparity.
International Journal of Digital Earth, 16(2), 4533–4552.
https://doi.org/10.1080/17538947.2023.2271883

## Examples

``` r
data('ndvi')
g = lesh(NDVIchange ~ ., data = ndvi)
g
#> ***       Locally Explained Stratified Heterogeneity Model         
#> 
#> |    Interactive variable     |    Interaction     | Variable1 SPD | Variable2 SPD |
#> |:---------------------------:|:------------------:|:-------------:|:-------------:|
#> |    Climatezone ∩ Mining     |    Weaken, uni-    |  0.75353265   |  0.06776285   |
#> |  Climatezone ∩ Tempchange   |    Weaken, uni-    |  0.64437728   |  0.17691822   |
#> | Climatezone ∩ Precipitation |    Enhance, bi-    |  0.39405554   |  0.48986045   |
#> |      Climatezone ∩ GDP      |    Enhance, bi-    |  0.79843017   |  0.05246998   |
#> |  Climatezone ∩ Popdensity   |    Enhance, bi-    |  0.74240657   |  0.11069841   |
#> |     Mining ∩ Tempchange     |    Enhance, bi-    |  0.10161351   |  0.31023743   |
#> |   Mining ∩ Precipitation    |    Weaken, uni-    |  0.05886173   |  0.81368883   |
#> |        Mining ∩ GDP         |    Enhance, bi-    |  0.12735177   |  0.09306564   |
#> |     Mining ∩ Popdensity     |    Enhance, bi-    |  0.13123771   |  0.21760488   |
#> | Tempchange ∩ Precipitation  |    Enhance, bi-    |  0.16187198   |  0.73291613   |
#> |      Tempchange ∩ GDP       | Enhance, nonlinear |  0.35277116   |  0.08443737   |
#> |   Tempchange ∩ Popdensity   |    Enhance, bi-    |  0.28786726   |  0.15633619   |
#> |     Precipitation ∩ GDP     |    Enhance, bi-    |  0.84089496   |  0.04445297   |
#> | Precipitation ∩ Popdensity  |    Enhance, bi-    |  0.79267181   |  0.09507756   |
#> |      GDP ∩ Popdensity       |    Weaken, uni-    |  0.06828443   |  0.15493420   |
```
