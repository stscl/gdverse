# geographical detector

geographical detector

## Usage

``` r
geodetector(formula, data, type = "factor", alpha = 0.95)
```

## Arguments

- formula:

  A formula of geographical detector model.

- data:

  A data.frame or tibble of observation data.

- type:

  (optional) The type of geographical detector, which must be one of
  `factor`(default), `interaction`, `risk`, `ecological`.

- alpha:

  (optional) Specifies the size of the alpha (confidence level). Default
  is `0.95`.

## Value

A list.

- `factor`:

  the result of factor detector

- `interaction`:

  the result of interaction detector

- `risk`:

  the result of risk detector

- `ecological`:

  the result of ecological detector

## Note

Note that only one type of geodetector is supported at a time in
`geodetector()`.

## Author

Wenbo Lv <lyu.geosocial@gmail.com>

## Examples

``` r
geodetector(y ~ x1 + x2,
   tibble::tibble(y = 1:7,
                  x1 = c('x',rep('y',3),rep('z',3)),
                  x2 = c(rep('a',2),rep('b',2),rep('c',3))))
#>                 Factor Detector            
#> 
#> | variable | Q-statistic |  P-value   |
#> |:--------:|:-----------:|:----------:|
#> |    x2    |  0.8928571  | 0.03167582 |
#> |    x1    |  0.7714286  | 0.07936477 |

geodetector(y ~ x1 + x2,
   tibble::tibble(y = 1:7,
                  x1 = c('x',rep('y',3),rep('z',3)),
                  x2 = c(rep('a',2),rep('b',2),rep('c',3))),
   type = 'interaction')
#>                 Interaction Detector         
#> 
#> | Interactive variable |    Interaction    |
#> |:--------------------:|:-----------------:|
#> |       x1 ∩ x2        | Weaken, nonlinear |

geodetector(y ~ x1 + x2,
   tibble::tibble(y = 1:7,
                  x1 = c('x',rep('y',3),rep('z',3)),
                  x2 = c(rep('a',2),rep('b',2),rep('c',3))),
   type = 'risk',alpha = 0.95)
#>                 Risk Detector            
#> 
#>  Variable x1:
#> 
#> | zone  | zonex | zoney |
#> |:-----:|:-----:|:-----:|
#> | zoney |  No   |  NA   |
#> | zonez |  No   |  Yes  |
#> 
#>  Variable x2:
#> 
#> | zone  | zonea | zoneb |
#> |:-----:|:-----:|:-----:|
#> | zoneb |  No   |  NA   |
#> | zonec |  Yes  |  Yes  |

geodetector(y ~ x1 + x2,
   tibble::tibble(y = 1:7,
                  x1 = c('x',rep('y',3),rep('z',3)),
                  x2 = c(rep('a',2),rep('b',2),rep('c',3))),
   type = 'ecological',alpha = 0.95)
#>                 Ecological Detector         
#> 
#> |   | x2 |
#> |:--|:--:|
#> |x1 | No |
```
