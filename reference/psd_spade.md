# power of spatial determinant(PSD)

Function for calculate power of spatial determinant `q_s`

## Usage

``` r
psd_spade(y, x, wt)
```

## Arguments

- y:

  Variable Y, continuous numeric vector.

- x:

  Covariable X, `factor`, `character` or `discrete numeric`.

- wt:

  The spatial weight matrix.

## Value

A value of power of spatial determinant `q_s`.

## Details

The power of spatial determinant formula is

\\q_s = 1 - \frac{\sum\_{h=1}^L N_h \Gamma_h}{N \Gamma}\\

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')),
                                  power = 2)
psd_spade(sim$y,sdsfun::discretize_vector(sim$xa,5),wt)
#> [1] 0.3928036
```
