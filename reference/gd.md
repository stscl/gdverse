# native geographical detector(GD) model

native geographical detector(GD) model

## Usage

``` r
gd(formula, data, type = "factor", alpha = 0.95)
```

## Arguments

- formula:

  A formula of geographical detector model.

- data:

  A `data.frame`, `tibble` or `sf` object of observation data.

- type:

  (optional) The type of geographical detector, which must be one of
  `factor`(default), `interaction`, `risk`, `ecological`. You can run
  one or more types at one time.

- alpha:

  (optional) Specifies the size of the alpha (confidence level). Default
  is `0.95`.

## Value

A list.

- `factor`:

  the result of factor detector

- `interaction`:

  the result of interaction detector

- `risk`:

  the result of risk detector

- `ecological`:

  the result of ecological detector

## References

Jin‐Feng Wang, Xin‐Hu Li, George Christakos, Yi‐Lan Liao, Tin Zhang,
XueGu & Xiao‐Ying Zheng (2010) Geographical Detectors‐Based Health Risk
Assessment and its Application in the Neural Tube Defects Study of the
Heshun Region, China, International Journal of Geographical Information
Science, 24:1, 107-127, DOI: 10.1080/13658810802443457

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data("NTDs")
g = gd(incidence ~ watershed + elevation + soiltype,
       data = NTDs,type = c('factor','interaction'))
g
#>                 Factor Detector            
#> 
#> | variable  | Q-statistic |   P-value   |
#> |:---------:|:-----------:|:-----------:|
#> | watershed |  0.6377737  | 0.000128803 |
#> | elevation |  0.6067087  | 0.043382244 |
#> | soiltype  |  0.3857168  | 0.372145486 |
#> 
#>                 Interaction Detector         
#> 
#> | Interactive variable  | Interaction  |
#> |:---------------------:|:------------:|
#> | watershed ∩ elevation | Enhance, bi- |
#> | watershed ∩ soiltype  | Enhance, bi- |
#> | elevation ∩ soiltype  | Enhance, bi- |
#> 
```
