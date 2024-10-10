# gdverse 1.1.0

* The general variable discretization in **gdverse** now utilizes `sdsfun::discretize_vector()` (#6).

* Algorithm functions are migrated to `sdsfun` (#8).

* Clear the `WORDLIST` to ensure the source code remains clean and organized.

# gdverse 1.0-3

* Update the `RGD` Model API Settings (#2).

* Fix bug caused by changes in default parameters of `opgd` in `sesu_opgd` (#4).

* Maintain the same results for `st_unidisc` and `ClassInt::classify_intervals` (#5).

* The parameter `overlaymethod` in `rid` and `idsa` has been renamed to `overlay`.

* Add `readr` as a dependence of type `Suggests`.

* Recompile vignettes due to internal function changes.

# gdverse 1.0-2

* When the `discvar` input for the `opgd`, `rgd`, `rid`, `spade` functions is `NULL`, 
  it is assumed that all independent variables in the `formula` need to be discretized.
  
* Updating the S3 method for plotting various factor detectors to better conform 
  to academic publication requirements.
  
* Using new example data in the vignettes for `spade` and `idsa`.

* Adding the `esp` function to the package.

# gdverse 1.0-1

* Unify all vignettes filenames to lowercase.

* Support for using the `sf` object as input in all models.

# gdverse 1.0.0

* First stable release.
