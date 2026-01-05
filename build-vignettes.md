# NA

Due to the time-consuming computations involved in the vignettes of the
*gdverse* package, it is necessary to pre-build the vignettes prior to
package submission.

``` r
.prebuild_vignettes = \(name){
  out = paste0("vignettes/",name,".Rmd")
  inp = paste0(out,".orig")
  knitr::knit(inp,out)
}
```

- Compile all vignettes at once

``` r
for (v in c("consistency", "gd", "opgd", "sesu", "shegd", "spade", "idsa", "rgdrid")) {
  .prebuild_vignettes(v)
}
```

- Build vignettes separately

``` r
.prebuild_vignettes("consistency")
.prebuild_vignettes("gd")
.prebuild_vignettes("opgd")
.prebuild_vignettes("sesu")
.prebuild_vignettes("shegd")
.prebuild_vignettes("spade")
.prebuild_vignettes("idsa")
.prebuild_vignettes("rgdrid")
```
