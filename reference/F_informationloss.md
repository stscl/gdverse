# measure information loss by information entropy

Function for measure information loss by shannon information entropy.

## Usage

``` r
F_informationloss(xvar, xdisc)
```

## Arguments

- xvar:

  The original undiscretized vector.

- xdisc:

  The discretized vector.

## Value

A numeric value of information loss measured by information entropy.

## Details

The information loss measured by information entropy formula is \\F =
-\sum\limits\_{i=1}^N p\_{(i)}\log_2 p\_{(i)} -
\left(-\sum\limits\_{h=1}^L p\_{(h)}\log_2 p\_{(h)}\right)\\

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
F_informationloss(1:7,c('x',rep('y',3),rep('z',3)))
#> [1] 1.358539
```
