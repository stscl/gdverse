# IDSA Q-saistics `PID`

IDSA Q-saistics `PID`

## Usage

``` r
pid_idsa(formula, rawdata, discdata, wt, overlaymethod = "and")
```

## Arguments

- formula:

  A formula for IDSA Q-saistics

- rawdata:

  Raw observation data

- discdata:

  Observed data with discrete explanatory variables

- wt:

  Spatial weight matrix

- overlaymethod:

  (optional) Spatial overlay method. One of `and`, `or`, `intersection`.
  Default is `and`.

## Value

The value of IDSA Q-saistics `PID`.

## Details

\\Q\_{IDSA} = \frac{\theta_r}{\phi}\\

## Examples

``` r
data('sim')
wt = sdsfun::inverse_distance_swm(sf::st_as_sf(sim,coords = c('lo','la')))
sim1 = dplyr::mutate(sim,dplyr::across(xa:xc,\(.x) sdsfun::discretize_vector(.x,5)))
pid_idsa(y ~ xa + xb + xc, rawdata = sim,
         discdata = sim1, wt = wt)
#> [1] 0.591589
```
