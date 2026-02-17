# calculate power of spatial determinant(PSD) and the corresponding pseudo-p value

Function for calculate power of spatial determinant \\q_s\\.

## Usage

``` r
psd_pseudop(y, x, wt, cores = 1, seed = 123456789, permutations = 0)
```

## Arguments

- y:

  Variable Y, continuous numeric vector.

- x:

  Covariable X, `factor`, `character` or `discrete numeric`.

- wt:

  The spatial weight matrix.

- cores:

  (optional) A positive integer(default is 1). If cores \> 1, use
  parallel computation.

- seed:

  (optional) Random seed number, default is `123456789`.

- permutations:

  (optional) The number of permutations for the PSD computation. Default
  is `0`, which means no pseudo-p values are calculated.

## Value

A tibble of power of spatial determinant and the corresponding pseudo-p
value.

## Details

The power of spatial determinant formula is \\q_s = 1 -
\frac{\sum\_{h=1}^L N_h \Gamma_h}{N \Gamma}\\

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')),
                                  power = 2)
psd_pseudop(sim$y,sdsfun::discretize_vector(sim$xa,5),wt)
#> # A tibble: 1 × 2
#>   `Q-statistic` `P-value`        
#>           <dbl> <chr>            
#> 1         0.393 No Pseudo-P Value
```
