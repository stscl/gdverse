# spatial rough set-based geographical detector

spatial rough set-based geographical detector

## Usage

``` r
srs_geodetector(formula, data, wt = NULL, type = "factor", alpha = 0.95)
```

## Arguments

- formula:

  A formula of spatial rough set-based geographical detector model.

- data:

  A data.frame, tibble or sf object of observation data.

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

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('srs_table')
data('srs_wt')
srs_geodetector(d ~ a1 + a2 + a3, data = srs_table, wt = srs_wt)
#> Spatial Rough Set-based Factor Detector 
#>                 Factor Detector            
#> 
#> | variable |    PD     |  SE_PD   |
#> |:--------:|:---------:|:--------:|
#> |    a3    | 0.4898268 | 3.290192 |
#> |    a2    | 0.4716450 | 3.304379 |
#> |    a1    | 0.4595238 | 3.282169 |
srs_geodetector(d ~ a1 + a2 + a3, data = srs_table,
                wt = srs_wt, type = 'interaction')
#> Spatial Rough Set-based Interaction Detector 
#>                 Interaction Detector         
#> 
#> | Interactive variable | Interaction  |
#> |:--------------------:|:------------:|
#> |       a1 ∩ a2        | Enhance, bi- |
#> |       a1 ∩ a3        | Enhance, bi- |
#> |       a2 ∩ a3        | Enhance, bi- |
srs_geodetector(d ~ a1 + a2 + a3, data = srs_table,
                wt = srs_wt, type = 'ecological')
#> Spatial Rough Set-based Ecological Detector 
#>                 Ecological Detector         
#> 
#> |   | a2 | a3 |
#> |:--|:--:|:--:|
#> |a1 | No | No |
#> |a2 | NA | No |
```
