test_that("test whether srs_factor_detector function works well", {
  data('srs_table')
  data('srs_wt')
  srsfd = srs_factor_detector(srs_table$d,srs_table$a1,srs_wt)
  srsfd = lapply(srsfd, round, 6)
  expect_equal(srsfd,
               list(PD = 0.459524,
                    SE_PD = 3.282169))
})
