# plot SPADE power of spatial and multilevel discretization determinant

S3 method to plot output for SPADE power of spatial and multilevel
discretization determinant from
[`spade()`](https://stscl.github.io/gdverse/reference/spade.md).

## Usage

``` r
# S3 method for class 'spade_result'
plot(x, slicenum = 2, alpha = 0.95, keep = TRUE, ...)
```

## Arguments

- x:

  Return by
  [`spade()`](https://stscl.github.io/gdverse/reference/spade.md).The
  number of labels facing inward.

- slicenum:

  (optional) The number of labels facing inward. Default is `2`.

- alpha:

  (optional) Confidence level.Default is `0.95`.

- keep:

  (optional) Whether to keep Q-value results for insignificant
  variables, default is `TRUE`.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer.
