# gdverse 1.3-2

# gdverse 1.3-1

* Fix bugs of `gd`,`opgd`,`gozh`,`srsgd` plot s3 methods (#23).

# gdverse 1.3

* Redundant functions consolidated and migrated to `sdsfun` for maintenance (#22).

* Reorganize the parallel computing section in gdverse (#21).

* Align the RGD model with the original algorithm presented in paper (#20).

* Now gdverse requires sdsfun with a minimum version of `0.4.3` (#19).

* Transfer `isp`-related concepts and functions to the `cisp` package (#18).

* Modify the default font settings in the gdverse S3 plotting methods (#17).

# gdverse 1.2

* Set the default number of discretizations in gdverse to range from 3 to 8 (#15).

* Optimize the Python integration setup in gdverse (#14).

* Now `opgd()` returns optimal discretization parameters (#13).

* Force `data` to `tibble` format in **gdverse** GDMs model function (#12).

* Align the RID model and algorithm with the original framework presented in paper (#9).

* Beautify the narrative and other writing details in the vignettes, without making 
  any changes at the user level.

# gdverse 1.1-1

* Clear the `WORDLIST` to ensure the source code remains clean and organized.

* Migrate the source code from `ausgis/gdverse` to `stscl/gdverse` on GitHub.

# gdverse 1.1.0

* The general variable discretization in **gdverse** now utilizes `sdsfun::discretize_vector()` (#6).

* Algorithm functions are migrated to `sdsfun` (#8).

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
