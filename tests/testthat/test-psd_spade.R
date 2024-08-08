test_that("test whether psd_spade function works well", {
  data('NTDs')
  wt = inverse_distance_weight(NTDs$X,NTDs$Y,power = 2)
  p = round(psd_spade(NTDs$incidence,NTDs$soiltype,wt),6)
  expect_equal(p,0.256653)
})
