# Tests for fetch_fia.R

test_that(".state_abbr_to_code returns correct FIPS codes", {
  fn <- FIAwrapper:::.state_abbr_to_code
  expect_equal(fn("ME"), c(ME = 23))
  expect_equal(fn("CA"), c(CA = 6))
  expect_equal(fn(c("NH", "VT")), c(NH = 33, VT = 50))
})

test_that(".state_abbr_to_code is case-insensitive", {
  fn <- FIAwrapper:::.state_abbr_to_code
  expect_equal(fn("me"), c(ME = 23))
  expect_equal(fn("ca"), c(CA = 6))
})

test_that(".state_abbr_to_code drops invalid abbreviations silently", {
  fn <- FIAwrapper:::.state_abbr_to_code
  result <- fn("ZZ")
  expect_length(result, 0)
})

test_that(".state_abbr_to_code handles mix of valid and invalid", {
  fn <- FIAwrapper:::.state_abbr_to_code
  result <- fn(c("ME", "ZZ", "VT"))
  expect_equal(result, c(ME = 23, VT = 50))
})

test_that("fetch_fia_data errors on invalid method", {
  expect_error(
    fetch_fia_data("ME", method = "bogus"),
    "Unknown method"
  )
})

test_that("load_fia_data errors on non-existent directory", {
  expect_error(
    load_fia_data("/no/such/directory/here"),
    "Directory not found"
  )
})
