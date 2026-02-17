# spatial rough set-based factor detector

spatial rough set-based factor detector

## Usage

``` r
srs_factor_detector(y, x, wt)
```

## Arguments

- y:

  Variable Y, `factor`, `character` or `discrete numeric`.

- x:

  Covariate X, `factor`, `character` or `discrete numeric`.

- wt:

  Spatial adjacency matrix.

## Value

A list.

- `PD`:

  the average local explanatory power

- `SE_PD`:

  the degree of spatial heterogeneity of the local explanatory power

## References

Bai, H., Li, D., Ge, Y., Wang, J., & Cao, F. (2022). Spatial rough
set-based geographical detectors for nominal target variables.
Information Sciences, 586, 525–539.
https://doi.org/10.1016/j.ins.2021.12.019

## Examples

``` r
data('srs_table')
data('srs_wt')
srs_factor_detector(srs_table$d,srs_table$a1,srs_wt)
#> $PD
#> [1] 0.4595238
#> 
#> $SE_PD
#> [1] 3.282169
#> 
```
