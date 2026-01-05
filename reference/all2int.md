# convert all discretized vectors to integer

convert all discretized vectors to integer

## Usage

``` r
all2int(x)
```

## Arguments

- x:

  A discretized vector.

## Value

An integer vector.

## Examples

``` r
all2int(factor(letters[1:3],levels = c('b','a','c')))
#> [1] 2 1 3
all2int(letters[1:3])
#> [1] 1 2 3
```
