test_that("fia_to_forestry_names renames known FIA columns", {
  df <- data.frame(DIA = 10, HT = 65, STATUSCD = 1)
  result <- fia_to_forestry_names(df)
  expect_equal(names(result), c("dbh", "height", "status_code"))
  expect_equal(result$dbh, 10)
  expect_equal(result$height, 65)
  expect_equal(result$status_code, 1)
})

test_that("fia_to_forestry_names leaves unknown columns unchanged", {
  df <- data.frame(DIA = 10, MY_COL = 99)
  result <- fia_to_forestry_names(df)
  expect_true("MY_COL" %in% names(result))
  expect_true("dbh" %in% names(result))
})

test_that("forestry_to_fia_names reverses the mapping", {
  df <- data.frame(dbh = 10, height = 65, status_code = 1)
  result <- forestry_to_fia_names(df)
  expect_equal(names(result), c("DIA", "HT", "STATUSCD"))
})

test_that("round-trip preserves data", {
  df_orig <- data.frame(DIA = 12.5, HT = 80, STATUSCD = 1, CUSTOM = "x")
  df_rt <- forestry_to_fia_names(fia_to_forestry_names(df_orig))
  expect_equal(df_rt, df_orig)
})

test_that("fia_to_forestry_names handles data frame with no FIA columns", {
  df <- data.frame(x = 1, y = 2)
  result <- fia_to_forestry_names(df)
  expect_equal(names(result), c("x", "y"))
})

test_that("forestry_to_fia_names handles data frame with no forestry columns", {
  df <- data.frame(x = 1, y = 2)
  result <- forestry_to_fia_names(df)
  expect_equal(names(result), c("x", "y"))
})
