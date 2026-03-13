# ---- flag_outliers ----

test_that("normal trees are not flagged", {
  trees <- data.frame(
    spp_code = c("RS", "SM", "WP"),
    dbh      = c(10,   16,   20),
    height   = c(55,   70,   80)
  )

  result <- flag_outliers(trees)

  expect_false(any(result$flag_dbh))
  expect_false(any(result$flag_ht))
  expect_false(any(result$flag_hd_ratio))
  expect_true(all(is.na(result$outlier_reason)))
})

test_that("huge DBH spruce gets flagged", {
  trees <- data.frame(
    spp_code = c("RS", "RS"),
    dbh      = c(10,   50),
    height   = c(55,   80)
  )

  result <- flag_outliers(trees)

  # spruce_fir dbh_max = 25.6; 50 in exceeds it

  expect_false(result$flag_dbh[1])
  expect_true(result$flag_dbh[2])
  expect_true(grepl("DBH", result$outlier_reason[2]))
})

test_that("very short height gets flagged", {
  trees <- data.frame(
    spp_code = c("SM", "SM"),
    dbh      = c(10,   10),
    height   = c(60,   2)
  )

  result <- flag_outliers(trees)

  # n_hardwood ht_min = 4.5; 2 ft is below it
  expect_false(result$flag_ht[1])
  expect_true(result$flag_ht[2])
  expect_true(grepl("HT", result$outlier_reason[2]))
})

test_that("output has all expected columns", {
  trees <- data.frame(
    spp_code = c("BF"),
    dbh      = c(8),
    height   = c(45)
  )

  result <- flag_outliers(trees)

  expected_cols <- c("warn_dbh", "warn_ht", "warn_hd_ratio",
                     "flag_dbh", "flag_ht", "flag_hd_ratio",
                     "outlier_reason")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing column:", col))
  }
})

test_that("warn columns are logical and reason is character", {
  trees <- data.frame(
    spp_code = c("RS", "SM"),
    dbh      = c(10,   16),
    height   = c(55,   70)
  )

  result <- flag_outliers(trees)

  expect_type(result$warn_dbh, "logical")
  expect_type(result$warn_ht, "logical")
  expect_type(result$warn_hd_ratio, "logical")
  expect_type(result$flag_dbh, "logical")
  expect_type(result$flag_ht, "logical")
  expect_type(result$flag_hd_ratio, "logical")
  expect_type(result$outlier_reason, "character")
})

test_that("missing column produces an error", {
  trees <- data.frame(species = "RS", dbh = 10, height = 55)
  expect_error(flag_outliers(trees), "not found")
})
