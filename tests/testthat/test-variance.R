test_that("estimate_variance SRS computes correct mean, SE, and CI", {
  y <- c(120, 95, 140, 110, 88, 130, 105, 115, 125, 100)

  result <- estimate_variance(
    stand = rep(1, 10),
    plot  = 1:10,
    y     = y
  )

  # Check mean
  expect_equal(result$mean, mean(y))

  # Check SE = sqrt(var(y) / n)
  expected_se <- sqrt(var(y) / length(y))
  expect_equal(result$se, expected_se, tolerance = 1e-8)

  # CI should bracket the mean
  expect_lt(result$ci_lower, result$mean)
  expect_gt(result$ci_upper, result$mean)

  # CI bounds should be symmetric around mean
  expect_equal(result$mean - result$ci_lower, result$ci_upper - result$mean,
               tolerance = 1e-10)

  # Method should be SRS

  expect_equal(result$method, "SRS")
  expect_equal(result$n_plots, 10L)
})
