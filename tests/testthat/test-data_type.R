test_that("data.frame works", {
  expect_no_error({
    g = opgd(NDVIchange ~ ., data = GD::ndvi_50,
             discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
             discnum = 3:6, cores = 1)
  })
})
