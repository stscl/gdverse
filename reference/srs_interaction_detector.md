# spatial rough set-based interaction detector

spatial rough set-based interaction detector

## Usage

``` r
srs_interaction_detector(y, x1, x2, wt)
```

## Arguments

- y:

  Dependent variable, `factor`, `character` or `discrete numeric`.

- x1:

  Covariate \\X_1\\, `factor`, `character` or `discrete numeric`.

- x2:

  Covariate \\X_2\\, `factor`, `character` or `discrete numeric`.

- wt:

  Spatial adjacency matrix.

## Value

A list.

- `Variable1 PD`:

  the average local explanatory power for variable1

- `Variable2 PD`:

  the average local explanatory power for variable2

- `Variable1 and Variable2 interact PD`:

  the average local explanatory power for variable1 and variable2
  interact

- `Variable1 SE_PD`:

  the degree of spatial heterogeneity of the local explanatory power for
  variable1

- `Variable2 SE_PD`:

  the degree of spatial heterogeneity of the local explanatory power for
  variable2

- `Variable1 and Variable2 SE_PD`:

  the degree of spatial heterogeneity of the local explanatory power for
  variable1 and variable2 interact

- `Interaction`:

  the interact result type

## References

Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough
set-based geographical detectors for nominal target variables.
Information Sciences, 586, 525–539.
https://doi.org/10.1016/j.ins.2021.12.019

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('srs_table')
data('srs_wt')
srs_interaction_detector(srs_table$d,srs_table$a1,srs_table$a2,srs_wt)
#> $`Variable1 PD`
#> [1] 0.4595238
#> 
#> $`Variable2 PD`
#> [1] 0.471645
#> 
#> $`Variable1 and Variable2 interact PD`
#> [1] 0.5080087
#> 
#> $`Variable1 SE_PD`
#> [1] 3.282169
#> 
#> $`Variable2 SE_PD`
#> [1] 3.304379
#> 
#> $`Variable1 and Variable2 interact SE_PD`
#> [1] 3.278937
#> 
#> $Interaction
#> [1] "Enhance, bi-"
#> 
```
