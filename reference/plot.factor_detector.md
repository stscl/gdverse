# plot factor detector result

S3 method to plot output for factor detector in
[`geodetector()`](https://stscl.github.io/gdverse/reference/geodetector.md).

## Usage

``` r
# S3 method for class 'factor_detector'
plot(x, slicenum = 2, alpha = 0.95, keep = TRUE, qlabelsize = 3.88, ...)
```

## Arguments

- x:

  Return by
  [`geodetector()`](https://stscl.github.io/gdverse/reference/geodetector.md).

- slicenum:

  (optional) The number of labels facing inward. Default is `2`.

- alpha:

  (optional) Confidence level. Default is `0.95`.

- keep:

  (optional) Whether to keep Q-value results for insignificant
  variables, default is `TRUE`.

- qlabelsize:

  (optional) Set the font size of the q-value text labels in the plot.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>
