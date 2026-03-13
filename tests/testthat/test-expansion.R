# ---- compute_tpa ----

test_that("subplot trees (>= 5 in) get subplot TPA", {
  dbh  <- c(10, 12, 16, 5)
  subp <- c(1, 2, 3, 4)

  tpa <- compute_tpa(dbh, subp)

  subplot_area_ac <- pi * 24.0^2 / 43560
  expected_tpa    <- 1 / (subplot_area_ac * 4)

  expect_equal(tpa, rep(expected_tpa, 4))
  expect_equal(round(tpa[1], 3), 6.018)
})

test_that("microplot trees (< 5 in) get microplot TPA", {
  dbh  <- c(2, 3, 4.9, 1)
  subp <- c(1, 2, 3, 4)

  tpa <- compute_tpa(dbh, subp)

  microplot_area_ac <- pi * 6.8^2 / 43560
  expected_tpa      <- 1 / (microplot_area_ac * 4)

  expect_equal(tpa, rep(expected_tpa, 4))
  expect_equal(round(tpa[1], 3), round(expected_tpa, 3))
})

test_that("mixed DBH trees get correct subplot/microplot assignment", {
  dbh  <- c(10, 3, 16, 2)
  subp <- c(1, 2, 3, 4)

  tpa <- compute_tpa(dbh, subp)

  subplot_area_ac   <- pi * 24.0^2 / 43560
  microplot_area_ac <- pi * 6.8^2 / 43560
  tpa_sub  <- 1 / (subplot_area_ac * 4)
  tpa_mic  <- 1 / (microplot_area_ac * 4)

  expect_equal(tpa, c(tpa_sub, tpa_mic, tpa_sub, tpa_mic))
})

test_that("condition proportion adjustment divides TPA by cond_prop", {
  dbh       <- c(10, 8)
  subp      <- c(1, 2)
  cond_prop <- c(0.5, 0.75)

  tpa <- compute_tpa(dbh, subp, cond_prop = cond_prop)

  subplot_area_ac <- pi * 24.0^2 / 43560
  base_tpa        <- 1 / (subplot_area_ac * 2)  # 2 unique subplots

  expect_equal(tpa[1], base_tpa / 0.5)
  expect_equal(tpa[2], base_tpa / 0.75)
})

test_that("auto-detects number of subplots from unique subplot values", {
  dbh  <- c(10, 12)
  subp <- c(1, 2)

  tpa <- compute_tpa(dbh, subp)

  subplot_area_ac <- pi * 24.0^2 / 43560
  expected_tpa    <- 1 / (subplot_area_ac * 2)

  expect_equal(tpa, rep(expected_tpa, 2))

  # With 4 subplots the TPA should differ
  tpa4 <- compute_tpa(c(10, 12, 8, 9), c(1, 2, 3, 4))
  expect_true(tpa4[1] < tpa[1])
})

test_that("mismatched lengths produce an error", {
  expect_error(compute_tpa(c(10, 12), c(1, 2, 3)),
               "same length")
  expect_error(compute_tpa(c(10, 12), c(1, 2), cond_prop = c(1)),
               "same length")
})
