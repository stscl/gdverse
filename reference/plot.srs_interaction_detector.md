# plot spatial rough set-based interaction detector result

S3 method to plot output for spatial rough set-based interaction
detector in
[`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md).

## Usage

``` r
# S3 method for class 'srs_interaction_detector'
plot(x, alpha = 1, ...)
```

## Arguments

- x:

  Return by
  [`srsgd()`](https://stscl.github.io/gdverse/reference/srsgd.md).

- alpha:

  (optional) Picture transparency. Default is `1`.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer

## Author

Wenbo Lv <lyu.geosocial@gmail.com>
