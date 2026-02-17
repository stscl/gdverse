# spatial rough set-based geographical detector(SRSGD) model

spatial rough set-based geographical detector(SRSGD) model

## Usage

``` r
srsgd(formula, data, wt = NULL, type = "factor", alpha = 0.95)
```

## Arguments

- formula:

  A formula of spatial rough set-based geographical detector model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- wt:

  Spatial adjacency matrix. If `data` is a `sf` polygon object, the
  queen adjacency matrix is used when no `wt` object is provided. In
  other cases, you must provide a `wt` object.

- type:

  (optional) The type of geographical detector, which must be one of
  `factor`(default), `interaction` and `ecological`.

- alpha:

  (optional) Specifies the size of the alpha (confidence level). Default
  is `0.95`.

## Value

A list.

- `factor`:

  the result of spatial rough set-based factor detector

- `interaction`:

  the result of spatial rough set-based interaction detector

- `ecological`:

  the result of spatial rough set-based ecological detector

## Note

The Spatial Rough Set-based Geographical Detector Model (SRSGD) conducts
spatial hierarchical heterogeneity analysis utilizing a geographical
detector for data where *the dependent variable* is *discrete*. Given
the complementary relationship between SRSGD and the native version of
geographical detector, I strive to maintain consistency with
[`gd()`](https://stscl.github.io/gdverse/reference/gd.md) function when
establishing `srsgd()` function. This implies that all input variable
data in srsgd must *be discretized prior to use*.

## References

Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough
set-based geographical detectors for nominal target variables.
Information Sciences, 586, 525–539.
https://doi.org/10.1016/j.ins.2021.12.019

## Examples

``` r
data('srs_table')
data('srs_wt')
srsgd(d ~ a1 + a2 + a3, data = srs_table, wt = srs_wt,
      type = c('factor','interaction','ecological'))
#> ***     Spatial Rough Set-based Geographical Detector      
#>                 Factor Detector            
#> 
#> | variable |    PD     |  SE_PD   |
#> |:--------:|:---------:|:--------:|
#> |    a3    | 0.4898268 | 3.290192 |
#> |    a2    | 0.4716450 | 3.304379 |
#> |    a1    | 0.4595238 | 3.282169 |
#> 
#>                 Interaction Detector         
#> 
#> | Interactive variable | Interaction  |
#> |:--------------------:|:------------:|
#> |       a1 ∩ a2        | Enhance, bi- |
#> |       a1 ∩ a3        | Enhance, bi- |
#> |       a2 ∩ a3        | Enhance, bi- |
#> 
#>                 Ecological Detector         
#> 
#> |   | a2 | a3 |
#> |:--|:--:|:--:|
#> |a1 | No | No |
#> |a2 | NA | No |
#> 
```
