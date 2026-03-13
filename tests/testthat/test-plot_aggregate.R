test_that("summarize_plot computes correct TPA, BA/acre, and QMD", {
  dbh <- c(8, 10, 12, 6, 14)
  tpa <- rep(6.018, 5)

  result <- summarize_plot(
    stand = rep("A", 5),
    plot  = rep(1, 5),
    dbh   = dbh,
    tpa   = tpa
  )

  # TPA should be sum of expansion factors
  expected_tpa <- sum(tpa)
  expect_equal(result$tpa, expected_tpa)

  # BA per acre: sum(ba_ft2(dbh) * tpa)
  expected_ba <- sum(ba_ft2(dbh) * tpa)
  expect_equal(result$ba_per_acre, expected_ba, tolerance = 1e-6)

  # QMD: sqrt(sum(dbh^2 * tpa) / sum(tpa))
  expected_qmd <- sqrt(sum(dbh^2 * tpa) / sum(tpa))
  expect_equal(result$qmd, expected_qmd, tolerance = 1e-6)
})
