## R CMD check results

0 errors | 0 warnings | 0 note

## This submission

This update addresses the recent check issue reported by CRAN
(<https://cran.r-project.org/web/checks/check_results_gdverse.html>).

The reported errors were caused by missing Python modules (e.g., pandas)
in some CRAN test environments.

To prevent such errors, all Python-dependent examples and functions are now
wrapped in `tryCatch()` blocks, printing an informative message via
`message()` when Python or required modules are unavailable.

This ensures that CRAN checks will not fail even if Python is not installed,
while maintaining full functionality when Python is available.

No other functionality or user-facing behavior has changed.
