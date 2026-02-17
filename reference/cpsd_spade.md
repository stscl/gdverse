# compensated power of spatial determinant(CPSD)

Function for calculate compensated power of spatial determinant `Q_s`.

## Usage

``` r
cpsd_spade(yobs, xobs, xdisc, wt)
```

## Arguments

- yobs:

  Variable Y

- xobs:

  The original undiscretized covariable X.

- xdisc:

  The discretized covariable X.

- wt:

  The spatial weight matrix.

## Value

A value of compensated power of spatial determinant `Q_s`.

## Details

The power of compensated spatial determinant formula is

\\Q_s = \frac{q_s}{q\_{s\_{inforkep}}} = \frac{1 - \frac{\sum\_{h=1}^L
N_h \Gamma\_{kdep}}{N \Gamma\_{totaldep}}}{1 - \frac{\sum\_{h=1}^L N_h
\Gamma\_{hind}}{N \Gamma\_{totalind}}}\\

## References

Xuezhi Cang & Wei Luo (2018) Spatial association detector
(SPADE),International Journal of Geographical Information Science,
32:10, 2055-2075, DOI: 10.1080/13658816.2018.1476693

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
xa = sim$xa
xa_disc = sdsfun::discretize_vector(xa,5)
cpsd_spade(sim$y,xa,xa_disc,wt)
#> [1] 0.3530927
```
