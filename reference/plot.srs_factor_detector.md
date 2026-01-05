# plot spatial rough set-based factor detector result

S3 method to plot output for spatial rough set-based factor detector in
[`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md).

## Usage

``` r
# S3 method for class 'srs_factor_detector'
plot(x, slicenum = 2, ...)
```

## Arguments

- x:

  Return by
  [`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md).

- slicenum:

  (optional) The number of labels facing inward. Default is `2`.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>
