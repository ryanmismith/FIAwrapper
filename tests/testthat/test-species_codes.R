test_that("fia_to_fvs converts known FIA codes to FVS codes", {
  expect_equal(fia_to_fvs(833), "RO")
  expect_equal(fia_to_fvs(12), "BF")
  expect_equal(fia_to_fvs(129), "WP")
})

test_that("fia_to_fvs returns OH for unknown species", {
  expect_equal(fia_to_fvs(99999), "OH")
})

test_that("fvs_to_fia converts known FVS codes to FIA codes", {
  expect_equal(fvs_to_fia("RO"), 833L)
  expect_equal(fvs_to_fia("BF"), 12L)
})

test_that("fvs_to_fia returns 999 for unknown FVS code", {
  expect_equal(fvs_to_fia("ZZ"), 999L)
})

test_that("get_common_name returns correct common names", {
  expect_equal(get_common_name(833), "northern red oak")
  expect_equal(get_common_name(12), "balsam fir")
})

test_that("pef_to_fvs converts known PEF codes", {
  expect_equal(pef_to_fvs(1), "BF")
  expect_equal(pef_to_fvs(4), "RS")
})

test_that("pef_to_fvs returns OH for unknown PEF code", {
  expect_equal(pef_to_fvs(999), "OH")
})
