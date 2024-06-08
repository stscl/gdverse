# Pre-compiled vignettes that need parallel computation.
# reference: https://ropensci.org/blog/2019/12/08/precompute-vignettes/
# Must manually move image files from `gdverse/` to `gdverse/vignettes/` after knit.

devtools::load_all()

knitr::knit("vignettes/OPGD.Rmd.orig",
            "vignettes/OPGD.Rmd")

knitr::knit("vignettes/RGD.Rmd.orig",
            "vignettes/RGD.Rmd")
