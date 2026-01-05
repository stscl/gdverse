# shap power of determinants

Function for calculate shap power of determinants \\SPD\\.

## Usage

``` r
spd_lesh(formula, data, cores = 1, ...)
```

## Arguments

- formula:

  A formula of calculate shap power of determinants.

- data:

  A data.frame or tibble of observation data.

- cores:

  (optional) Positive integer (default is 1). When cores are greater
  than 1, use multi-core parallel computing.

- ...:

  (optional) Other arguments passed to
  [`rpart_disc()`](https://stscl.github.io/gdverse/reference/rpart_disc.md).

## Value

A tibble with variable and its corresponding \\SPD\\ value.

## Details

The power of shap power of determinants formula is

\\\theta\_{x_j} \left( S \right) = \sum\limits\_{s \in M \setminus
\\x_j\\} \frac{\|S\|! \left(\|M\| - \|S\| - 1\right)!}{\|M\|!}\left(v
\left(S \cup \left\\x_j\right\\ \right) - v\left(S\right)\right)\\.

shap power of determinants (SPD) is the contribution of variable \\x_j\\
to the power of determinants.

## Note

The shap power of determinants (SPD) requires at least \\2^n-1\\
calculations when has \\n\\ explanatory variables. When there are more
than 10 explanatory variables, carefully consider the computational
burden of this model. When there are a large number of explanatory
variables, the data dimensionality reduction method can be used to
ensure the trade-off between analysis results and calculation speed.

## References

Li, Y., Luo, P., Song, Y., Zhang, L., Qu, Y., & Hou, Z. (2023). A
locally explained heterogeneity model for examining wetland disparity.
International Journal of Digital Earth, 16(2), 4533–4552.
https://doi.org/10.1080/17538947.2023.2271883

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('ndvi')
g = spd_lesh(NDVIchange ~ ., data = ndvi)
g
#> # A tibble: 6 × 2
#>   variable      spd_theta
#>   <chr>             <dbl>
#> 1 Precipitation    0.218 
#> 2 Climatezone      0.176 
#> 3 Tempchange       0.0482
#> 4 Popdensity       0.0262
#> 5 Mining           0.0158
#> 6 GDP              0.0115
```
