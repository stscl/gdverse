# spatial rough set-based ecological detector

spatial rough set-based ecological detector

## Usage

``` r
srs_ecological_detector(y, x1, x2, wt, alpha = 0.95)
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

- alpha:

  (optional) Confidence level of the interval,default is `0.95`.

## Value

A list.

- `T-statistic`:

  the result of T statistic for spatial rough set-based ecological
  detector

- `P-value`:

  the result of P value for spatial rough set-based ecological detector

- `Ecological`:

  does one spatial feature \\X_1\\ play a more important role than
  \\X_2\\

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
srs_ecological_detector(srs_table$d,srs_table$a1,srs_table$a2,srs_wt)
#> $`T-statistic`
#>          t 
#> -0.1160247 
#> 
#> $`P-value`
#> [1] 0.9087941
#> 
#> $Ecological
#> [1] No
#> Levels: Yes No
#> 
```
