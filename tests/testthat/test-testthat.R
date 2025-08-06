test_that("Chelsa Clim Bio 15 size", {
  expect_equal(ClimDatDownloadR::getDownloadSize("https://os.zhdk.cloud.switch.ch/chelsav2/GLOBAL/climatologies/1981-2010/bio/CHELSA_bio15_1981-2010_V.2.1.tif"), 188.43)
})
