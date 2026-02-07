# Changelog

## gdverse 1.6

- Update package startup message and citation guidance
  ([\#97](https://github.com/stscl/gdverse/issues/97)).

- Report significance of interaction detection results in geographical
  detector ([\#95](https://github.com/stscl/gdverse/issues/95)).

- Improve robustness of non-centrality parameter estimation for
  q-statistic confidence intervals
  ([\#93](https://github.com/stscl/gdverse/issues/93)).

## gdverse 1.5.1

CRAN release: 2025-10-15

- Wrap python-dependent examples in `tryCatch` to avoid check failures
  ([\#82](https://github.com/stscl/gdverse/issues/82)).

## gdverse 1.5

CRAN release: 2025-10-10

- Refactor `cpd_disc()` for improved parallel stability and reticulate
  compatibility ([\#77](https://github.com/stscl/gdverse/issues/77)).

- Update python dependency configuration and usage
  ([\#74](https://github.com/stscl/gdverse/issues/74)).

- Eliminate warnings about the size aesthetic for lines in plot S3
  methods ([\#71](https://github.com/stscl/gdverse/issues/71)).

## gdverse 1.3-3

CRAN release: 2025-04-02

- Add pkg citation information
  ([\#61](https://github.com/stscl/gdverse/issues/61)).

## gdverse 1.3-2

CRAN release: 2025-02-17

- Experimentally support computing confidence intervals for the q
  statistic in factor_detector
  ([\#51](https://github.com/stscl/gdverse/issues/51)).

- Support adjusting the font size of q-value labels in the factor
  detection result plot
  ([\#49](https://github.com/stscl/gdverse/issues/49)).

- Rename `gd_bestunidisc()` as
  [`gd_optunidisc()`](https://stscl.github.io/gdverse/reference/gd_optunidisc.md)
  ([\#39](https://github.com/stscl/gdverse/issues/39)).

- Specify that the `cores` parameter in all functions of the `gdverse`
  package currently supports only positive integer inputs
  ([\#36](https://github.com/stscl/gdverse/issues/36)).

- Update the documentation on Python dependency configuration in the
  `rgdrid` vignette
  ([\#30](https://github.com/stscl/gdverse/issues/30)).

- Discuss the consistency with the results of existing geographical
  detector R packages
  ([\#28](https://github.com/stscl/gdverse/issues/28)).

- Update the calculation method of pseudo p-values for the SPADE model
  ([\#26](https://github.com/stscl/gdverse/issues/26)).

## gdverse 1.3-1

CRAN release: 2024-12-14

- Fix bugs of `gd`,`opgd`,`gozh`,`srsgd` plot s3 methods
  ([\#23](https://github.com/stscl/gdverse/issues/23)).

## gdverse 1.3

CRAN release: 2024-11-16

- Redundant functions consolidated and migrated to `sdsfun` for
  maintenance ([\#22](https://github.com/stscl/gdverse/issues/22)).

- Reorganize the parallel computing section in gdverse
  ([\#21](https://github.com/stscl/gdverse/issues/21)).

- Align the RGD model with the original algorithm presented in paper
  ([\#20](https://github.com/stscl/gdverse/issues/20)).

- Now gdverse requires sdsfun with a minimum version of `0.4.3`
  ([\#19](https://github.com/stscl/gdverse/issues/19)).

- Transfer `isp`-related concepts and functions to the `cisp` package
  ([\#18](https://github.com/stscl/gdverse/issues/18)).

- Modify the default font settings in the gdverse S3 plotting methods
  ([\#17](https://github.com/stscl/gdverse/issues/17)).

## gdverse 1.2

CRAN release: 2024-11-07

- Set the default number of discretizations in gdverse to range from 3
  to 8 ([\#15](https://github.com/stscl/gdverse/issues/15)).

- Optimize the Python integration setup in gdverse
  ([\#14](https://github.com/stscl/gdverse/issues/14)).

- Now [`opgd()`](https://stscl.github.io/gdverse/reference/opgd.md)
  returns optimal discretization parameters
  ([\#13](https://github.com/stscl/gdverse/issues/13)).

- Force `data` to `tibble` format in **gdverse** GDMs model function
  ([\#12](https://github.com/stscl/gdverse/issues/12)).

- Align the RID model and algorithm with the original framework
  presented in paper ([\#9](https://github.com/stscl/gdverse/issues/9)).

- Beautify the narrative and other writing details in the vignettes,
  without making any changes at the user level.

## gdverse 1.1-1

CRAN release: 2024-10-17

- Clear the `WORDLIST` to ensure the source code remains clean and
  organized.

- Migrate the source code from `ausgis/gdverse` to `stscl/gdverse` on
  GitHub.

## gdverse 1.1.0

CRAN release: 2024-10-10

- The general variable discretization in **gdverse** now utilizes
  [`sdsfun::discretize_vector()`](https://stscl.github.io/sdsfun/reference/discretize_vector.html)
  ([\#6](https://github.com/stscl/gdverse/issues/6)).

- Algorithm functions are migrated to `sdsfun`
  ([\#8](https://github.com/stscl/gdverse/issues/8)).

## gdverse 1.0-3

CRAN release: 2024-09-30

- Update the `RGD` Model API Settings
  ([\#2](https://github.com/stscl/gdverse/issues/2)).

- Fix bug caused by changes in default parameters of `opgd` in
  `sesu_opgd` ([\#4](https://github.com/stscl/gdverse/issues/4)).

- The parameter `overlaymethod` in `rid` and `idsa` has been renamed to
  `overlay`.

- Add `readr` as a dependence of type `Suggests`.

- Recompile vignettes due to internal function changes.

## gdverse 1.0-2

CRAN release: 2024-09-23

- When the `discvar` input for the `opgd`, `rgd`, `rid`, `spade`
  functions is `NULL`, it is assumed that all independent variables in
  the `formula` need to be discretized.

- Updating the S3 method for plotting various factor detectors to better
  conform to academic publication requirements.

## gdverse 1.0-1

CRAN release: 2024-09-16

- Unify all vignettes filename to lowercase.

- Support for using the `sf` object as input in all models.

## gdverse 1.0.0

CRAN release: 2024-09-09

- First stable release.
