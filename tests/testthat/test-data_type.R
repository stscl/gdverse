test_that("data.frame works", {
  expect_no_error({
    data("ndvi")
    g = opgd(NDVIchange ~ ., data = as.data.frame(ndvi),
             discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
             discnum = 3:6, cores = 1)
  })
})
