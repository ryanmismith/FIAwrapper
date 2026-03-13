test_that("compute_competition calculates bal correctly", {
  tree_list <- data.frame(
    plot_cn      = rep("P1", 5),
    dbh          = c(6, 8, 10, 12, 14),
    tpa          = c(10, 10, 10, 10, 10),
    ba_per_acre  = ba_ft2(c(6, 8, 10, 12, 14)) * 10,
    status       = rep("live", 5),
    stringsAsFactors = FALSE
  )

  result <- compute_competition(tree_list)

  # BAL for the smallest tree (dbh=6) should equal sum of ba_per_acre of all larger trees
  ba_vals <- ba_ft2(c(6, 8, 10, 12, 14)) * 10
  expected_bal_smallest <- sum(ba_vals[c(2, 3, 4, 5)])  # all trees with dbh > 6
  idx_smallest <- which(result$dbh == 6)
  expect_equal(result$bal[idx_smallest], expected_bal_smallest)

  # BAL for the largest tree (dbh=14) should be 0
  idx_largest <- which(result$dbh == 14)
  expect_equal(result$bal[idx_largest], 0)
})

test_that("compute_competition calculates plot_ba correctly", {
  tree_list <- data.frame(
    plot_cn      = rep("P1", 5),
    dbh          = c(6, 8, 10, 12, 14),
    tpa          = c(10, 10, 10, 10, 10),
    ba_per_acre  = ba_ft2(c(6, 8, 10, 12, 14)) * 10,
    status       = rep("live", 5),
    stringsAsFactors = FALSE
  )

  result <- compute_competition(tree_list)

  expected_plot_ba <- sum(ba_ft2(c(6, 8, 10, 12, 14)) * 10)
  expect_true(all(result$plot_ba == expected_plot_ba))
})

test_that("compute_competition calculates plot_qmd", {
  tree_list <- data.frame(
    plot_cn      = rep("P1", 5),
    dbh          = c(6, 8, 10, 12, 14),
    tpa          = c(10, 10, 10, 10, 10),
    ba_per_acre  = ba_ft2(c(6, 8, 10, 12, 14)) * 10,
    status       = rep("live", 5),
    stringsAsFactors = FALSE
  )

  result <- compute_competition(tree_list)

  expected_qmd <- qmd(c(6, 8, 10, 12, 14), c(10, 10, 10, 10, 10))
  expect_true(all(!is.na(result$plot_qmd)))
  expect_equal(result$plot_qmd[1], expected_qmd)
})

test_that("compute_competition calculates relative_dbh as dbh / plot_qmd", {
  tree_list <- data.frame(
    plot_cn      = rep("P1", 5),
    dbh          = c(6, 8, 10, 12, 14),
    tpa          = c(10, 10, 10, 10, 10),
    ba_per_acre  = ba_ft2(c(6, 8, 10, 12, 14)) * 10,
    status       = rep("live", 5),
    stringsAsFactors = FALSE
  )

  result <- compute_competition(tree_list)

  expected_qmd <- qmd(c(6, 8, 10, 12, 14), c(10, 10, 10, 10, 10))
  for (i in seq_len(nrow(result))) {
    expect_equal(result$relative_dbh[i], result$dbh[i] / expected_qmd)
  }
})
