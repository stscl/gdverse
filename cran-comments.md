## Resubmission

This is a resubmission. In this submission I have:

* Add Rd-tags `\value` for pipe.Rd.

* Update package description details in `DESCRIPTION` file.

* Remove unnecessary use of `\dontrun{}` in package examples and only retain `\dontrun{}`   where `robust_disc`, `rgd`, and `rid` (which require configuration of Python dependencies) and `loess_optscale`, `sesu_opgd`, and `sesu_gozh` (which take a long time  to run) are involved. Additionally, we have added the necessary prompts for the examples that use `\dontrun{}`.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
