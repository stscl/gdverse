# generate permutations

generate permutations

## Usage

``` r
gen_permutations(x, seed = 123456789)
```

## Arguments

- x:

  A vector.

- seed:

  (optional) Random seed number. Default is `123456789`.

## Value

A permutations vector.

## Examples

``` r
gen_permutations(1:100,42)
#>   [1]  49  65  25  74  18 100  47  24  71  89  37  20  26   3  41  27  36   5
#>  [19]  34  87  58  42  93  30  43  15  22  80   8  84  68  96   4  50  95  88
#>  [37]  67   6  63   2  52  81  54  21  61  55  38  10  40  83  33  66  39  92
#>  [55]  45  79   9  29  12  91  44  35  76  16  28  51  94  56  69  78  17  32
#>  [73]  48  14  72  23  99  57  70  97   7  53  73  13  82  62  59  75  46   1
#>  [91]  60  19  90  77  85  11  64  31  86  98
```
