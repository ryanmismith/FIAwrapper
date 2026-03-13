test_that("ba_ft2 computes basal area in square feet", {
  # 10-inch tree: 10^2 * 0.005454154 = 0.5454154
  expect_equal(ba_ft2(10), 10^2 * 0.005454154)
  expect_equal(ba_ft2(0), 0)
  # vectorised
  expect_equal(ba_ft2(c(5, 10)), c(5^2, 10^2) * 0.005454154)
})

test_that("ba_m2 computes basal area in square meters", {
  expect_equal(ba_m2(25.4), 25.4^2 * pi / 40000)
  expect_equal(ba_m2(0), 0)
  expect_equal(ba_m2(c(10, 20)), c(10^2, 20^2) * pi / 40000)
})

test_that("qmd computes quadratic mean diameter", {
  dbh <- c(8, 10, 12)
  tpa <- c(20, 15, 10)
  expected <- sqrt(sum(dbh^2 * tpa) / sum(tpa))
  expect_equal(qmd(dbh, tpa), expected)
})

test_that("qmd returns NA for empty or zero-weight inputs", {
  expect_true(is.na(qmd(numeric(0), numeric(0))))
  expect_true(is.na(qmd(c(10, 12), c(0, 0))))
})

test_that("qmd handles NA values", {
  expect_equal(qmd(c(10, NA, 12), c(5, 5, 5)), qmd(c(10, 12), c(5, 5)))
})

test_that("inches_to_cm and cm_to_inches are inverses", {
  expect_equal(inches_to_cm(1), 2.54)
  expect_equal(cm_to_inches(2.54), 1)
  expect_equal(cm_to_inches(inches_to_cm(7.5)), 7.5)
})

test_that("feet_to_meters and meters_to_feet are inverses", {
  expect_equal(feet_to_meters(1), 0.3048)
  expect_equal(meters_to_feet(0.3048), 1)
  expect_equal(meters_to_feet(feet_to_meters(100)), 100)
})

test_that("acres_to_hectares and hectares_to_acres are inverses", {
  expect_equal(acres_to_hectares(1), 0.404686)
  expect_equal(hectares_to_acres(0.404686), 1)
  expect_equal(hectares_to_acres(acres_to_hectares(50)), 50)
})

test_that("validate_columns passes when all columns present", {
  df <- data.frame(a = 1, b = 2, c = 3)
  expect_invisible(validate_columns(df, c("a", "b"), "test_fn"))
  expect_true(validate_columns(df, c("a", "b"), "test_fn"))
})

test_that("validate_columns stops when columns are missing", {
  df <- data.frame(a = 1)
  expect_error(validate_columns(df, c("a", "z"), "my_func"), "my_func.*missing required columns.*z")
})

test_that("safe_numeric coerces to numeric", {
  expect_equal(safe_numeric(c("1", "2.5", "3")), c(1, 2.5, 3))
  expect_equal(safe_numeric(c("1", "abc", "3")), c(1, NA, 3))
  # Should not produce warnings to the caller
  expect_silent(safe_numeric(c("abc")))
})
