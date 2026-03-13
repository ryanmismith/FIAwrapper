test_that("summarize_regen filters trees and computes stocking correctly", {
  # Mix of trees: some below 5 in DBH (regen), some at/above 5 in (not regen)
  stand   <- rep(1, 6)
  plot    <- rep(1, 6)
  subplot <- c(1, 1, 2, 3, 3, 4)
  dbh     <- c(1.5, 3.2, 2.0, 0.8, 6.0, 2.5)
  # Trees with dbh < 5: indices 1,2,3,4,6 (dbh = 1.5, 3.2, 2.0, 0.8, 2.5)
  # Tree at index 5 (dbh = 6.0) should be excluded

  result <- summarize_regen(
    stand   = stand,
    plot    = plot,
    subplot = subplot,
    dbh     = dbh
  )

  # Only 5 regen trees (dbh < 5.0)
  expect_equal(result$n_regen_trees, 5L)

  # 4 total subplots
  expect_equal(result$n_subplots, 4L)

  # Stocking: subplots with at least 1 regen tree

  # Subplot 1: has regen (1.5, 3.2) -> stocked
  # Subplot 2: has regen (2.0) -> stocked
  # Subplot 3: has regen (0.8) -> stocked (6.0 excluded but 0.8 remains)
  # Subplot 4: has regen (2.5) -> stocked
  # All 4 subplots stocked out of 4 total -> 100%
  expect_equal(result$stocking_pct, 100.0)
})
