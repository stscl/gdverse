# plot LESH model result

S3 method to plot output for LESH model interaction result in
[`lesh()`](https://stscl.github.io/gdverse/reference/lesh.md).

## Usage

``` r
# S3 method for class 'lesh_result'
plot(
  x,
  pie = TRUE,
  scatter = FALSE,
  scatter_alpha = 1,
  pieradius_factor = 15,
  pielegend_x = 0.99,
  pielegend_y = 0.1,
  pielegend_num = 3,
  ...
)
```

## Arguments

- x:

  x Return by
  [`lesh()`](https://stscl.github.io/gdverse/reference/lesh.md).

- pie:

  (optional) Whether to draw the interaction contributions. Default is
  `TRUE`.

- scatter:

  (optional) Whether to draw the interaction direction diagram. Default
  is `FALSE`.

- scatter_alpha:

  (optional) Picture transparency. Default is `1`.

- pieradius_factor:

  (optional) The radius expansion factor of interaction contributions
  pie plot. Default is `15`.

- pielegend_x:

  (optional) The X-axis relative position of interaction contributions
  pie plot legend. Default is `0.99`.

- pielegend_y:

  (optional) The Y-axis relative position of interaction contributions
  pie plot legend. Default is `0.1`.

- pielegend_num:

  (optional) The number of interaction contributions pie plot legend.
  Default is `3`.

- ...:

  (optional) Other arguments passed to
  [`ggplot2::theme()`](https://ggplot2.tidyverse.org/reference/theme.html).

## Value

A ggplot2 layer.

## Note

When both `scatter` and `pie` are set to `TRUE` in RStudio, enlarge the
drawing frame for normal display.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>
