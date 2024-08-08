test_that("test whether factor_detector function works well", {
  fd = factor_detector(y = 1:7,x = c('x',rep('y',3),rep('z',3)))
  fd = lapply(fd, round, 5)
  expect_equal(fd,
               list(`Q-statistic` = 0.77143,
                    `P-value` = 0.07936))
})
