# PSD of an interaction of explanatory variables (PSD-IEV)

PSD of an interaction of explanatory variables (PSD-IEV)

## Usage

``` r
psd_iev(discdata, spzone, wt)
```

## Arguments

- discdata:

  Observed data with discrete explanatory variables. A `tibble` or
  `data.frame` .

- spzone:

  Fuzzy overlay spatial zones. Returned from `st_fuzzyoverlay()`.

- wt:

  Spatial weight matrix

## Value

The Value of `PSD-IEV`

## Details

\\\phi = 1 - \frac{\sum\_{i=1}^m
\sum\_{k=1}^{n_i}N\_{i,k}\tau\_{i,k}}{\sum\_{i=1}^m N_i \tau_i}\\

## References

Yongze Song & Peng Wu (2021) An interactive detector for spatial
associations, International Journal of Geographical Information Science,
35:8, 1676-1701, DOI:10.1080/13658816.2021.1882680

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
sim1 = dplyr::mutate(sim,dplyr::across(xa:xc,\(.x) sdsfun::discretize_vector(.x,5)))
sz = sdsfun::fuzzyoverlay(y ~ xa + xb + xc, data = sim1)
psd_iev(dplyr::select(sim1,xa:xc),sz,wt)
#> [1] 0.8031768
```
