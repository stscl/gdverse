test_that("multicores works", {
  expect_equal(opgd(NDVIchange ~ ., data = GD::ndvi_50,
                    discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
                    discnum = 3:6, cores = 1),
               opgd(NDVIchange ~ ., data = GD::ndvi_50,
                    discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
                    discnum = 3:6, cores = 2))
})
