test_that("test whether spvar function works well", {
  y = c(42,56,73)
  wt1 = inverse_distance_weight(1:length(y),1:length(y))
  expect_equal(spvar(y,wt1), 193.1)
})
