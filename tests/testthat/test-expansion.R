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

test_that("condition proportion adjustment multiplies TPA by cond_prop", {
  dbh       <- c(10, 8)
  subp      <- c(1, 2, 3, 4)
  cond_prop <- c(0.5, 0.75, 1.0, 1.0)

  tpa <- compute_tpa(c(10, 8, 12, 9), subp, cond_prop = cond_prop)

  subplot_area_ac <- pi * 24.0^2 / 43560
  base_tpa        <- 1 / (subplot_area_ac * 4)  # 4 subplots (FIA standard)

  # EVALIDator: TPA_EXP = TPA_UNADJ * CONDPROP_UNADJ
  expect_equal(tpa[1], base_tpa * 0.5)
  expect_equal(tpa[2], base_tpa * 0.75)
  expect_equal(tpa[3], base_tpa * 1.0)
  expect_equal(tpa[4], base_tpa * 1.0)
})

test_that("n_subplots parameter controls expansion factor", {
  dbh  <- c(10, 12)
  subp <- c(1, 2)

  # Default is 4 subplots (FIA standard)
  tpa4 <- compute_tpa(dbh, subp)
  subplot_area_ac <- pi * 24.0^2 / 43560
  expected_tpa_4  <- 1 / (subplot_area_ac * 4)
  expect_equal(tpa4, rep(expected_tpa_4, 2))

  # Explicit 2-subplot design
  tpa2 <- compute_tpa(dbh, subp, n_subplots = 2)
  expected_tpa_2 <- 1 / (subplot_area_ac * 2)
  expect_equal(tpa2, rep(expected_tpa_2, 2))

  # 2 subplots gives higher TPA than 4
  expect_true(tpa2[1] > tpa4[1])
})

test_that("mismatched lengths produce an error", {
  expect_error(compute_tpa(c(10, 12), c(1, 2, 3)),
               "same length")
  expect_error(compute_tpa(c(10, 12), c(1, 2), cond_prop = c(1)),
               "same length")
})
