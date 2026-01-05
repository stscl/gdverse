# assign values by weight

assign values by weight

## Usage

``` r
weight_assign(x, w, list = FALSE)
```

## Arguments

- x:

  A numeric value

- w:

  A weight vector

- list:

  (optional) Return list or not. if `list` is `TRUE`, return a list,
  otherwise return a vector. Default is `FALSE`.

## Value

A numeric Vector.

## Examples

``` r
weight_assign(0.875,1:3)
#> [1] 0.1458333 0.2916667 0.4375000
```
