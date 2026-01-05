# plot RID result

S3 method to plot output for RID model from
[`rid()`](https://stscl.github.io/gdverse/reference/rid.md).

## Usage

``` r
# S3 method for class 'rid_result'
plot(x, alpha = 1, ...)
```

## Arguments

- x:

  Return by [`rid()`](https://stscl.github.io/gdverse/reference/rid.md).

- alpha:

  (optional) Picture transparency. Default is `1`.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer

## Author

Wenbo Lv <lyu.geosocial@gmail.com>
