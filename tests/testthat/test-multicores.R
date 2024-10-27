test_that("multicores works", {
  data("ndvi")
  expect_equal(opgd(NDVIchange ~ ., data = ndvi,
                    discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
                    discnum = 3:6, cores = 1),
               opgd(NDVIchange ~ ., data = ndvi,
                    discvar = c("Tempchange","Precipitation","GDP","Popdensity"),
                    discnum = 3:6, cores = 2))
})
