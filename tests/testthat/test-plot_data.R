# ---- Helpers: synthetic tree list ----

make_synthetic_tree_list <- function() {
  tibble::tibble(
    plot_cn   = rep(c(1001, 1002), each = 4),
    plot_id   = rep(c("44_7_1001", "44_7_1002"), each = 4),
    dbh       = c(10, 12, 8, 14, 6, 9, 11, 7),
    height    = c(55, 62, 48, 70, 38, 50, 58, 42),
    tpa       = c(6, 6, 6, 6, 6, 6, 6, 6),
    spp_code  = c("BF", "RO", "BF", "WP", "RO", "BF", "WP", "RO"),
    species   = c("balsam fir", "northern red oak", "balsam fir", "eastern white pine",
                  "northern red oak", "balsam fir", "eastern white pine", "northern red oak"),
    ba_tree   = ba_ft2(c(10, 12, 8, 14, 6, 9, 11, 7)),
    status    = c("live", "live", "live", "dead", "live", "live", "live", "live"),
    ba_per_acre = ba_ft2(c(10, 12, 8, 14, 6, 9, 11, 7)) * 6,
    inventory_year = rep(2019L, 8)
  )
}


# ---- Tests for get_plot_summary ----

test_that("get_plot_summary returns one row per plot", {
  tl     <- make_synthetic_tree_list()
  result <- get_plot_summary(tl)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
})

test_that("get_plot_summary excludes dead trees by default", {
  tl     <- make_synthetic_tree_list()
  result <- get_plot_summary(tl, include_dead = FALSE)

  # Plot 1001 has a dead tree (dbh=14); live TPA = 3 trees * 6 = 18
  row1 <- result[result$plot_id == "44_7_1001", ]
  expect_equal(row1$tpa, 18)
  expect_equal(row1$n_trees, 3L)
})

test_that("get_plot_summary includes dead trees when asked", {
  tl     <- make_synthetic_tree_list()
  result <- get_plot_summary(tl, include_dead = TRUE)

  row1 <- result[result$plot_id == "44_7_1001", ]
  expect_equal(row1$tpa, 24)
  expect_equal(row1$n_trees, 4L)
})

test_that("get_plot_summary computes expected metrics", {
  tl     <- make_synthetic_tree_list()
  result <- get_plot_summary(tl)

  # Check that expected columns exist
  expected_cols <- c("plot_id", "tpa", "ba_per_acre", "qmd",
                     "mean_dbh", "species_count", "dominant_species")
  for (col in expected_cols) {
    expect_true(col %in% names(result), info = paste("Missing:", col))
  }

  # All numeric metrics should be positive
  expect_true(all(result$tpa > 0))
  expect_true(all(result$ba_per_acre > 0))
  expect_true(all(result$qmd > 0))
})

test_that("get_plot_summary computes qmd correctly", {
  tl     <- make_synthetic_tree_list()
  # Filter to live trees on plot 1001 manually
  live1  <- tl[tl$plot_id == "44_7_1001" & tl$status == "live", ]
  expected_qmd <- sqrt(sum(live1$dbh^2 * live1$tpa) / sum(live1$tpa))

  result <- get_plot_summary(tl)
  row1   <- result[result$plot_id == "44_7_1001", ]
  expect_equal(row1$qmd, expected_qmd)
})


# ---- Tests for get_site_data ----

test_that("get_site_data returns plot-level site attributes", {
  fia <- list(
    PLOT = data.frame(
      CN       = c(1001, 1002),
      INVYR    = c(2019L, 2019L),
      STATECD  = c(44L, 44L),
      COUNTYCD = c(7L, 7L),
      LAT      = c(41.5, 41.6),
      LON      = c(-71.5, -71.4),
      ELEV     = c(200L, 350L),
      SLOPE    = c(15, 25),
      ASPECT   = c(180, 90),
      stringsAsFactors = FALSE
    ),
    COND = data.frame(
      PLT_CN         = c(1001, 1002),
      CONDID         = c(1L, 1L),
      FORTYPCD       = c(505L, 505L),
      STDAGE         = c(65L, 80L),
      SICOND         = c(60, 70),
      SITECLCD       = c(3L, 2L),
      CONDPROP_UNADJ = c(1.0, 1.0),
      OWNCD          = c(46L, 46L),
      stringsAsFactors = FALSE
    )
  )

  result <- get_site_data(fia)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2L)
  expect_true("plot_cn" %in% names(result))
  expect_true("elevation" %in% names(result))
  expect_true("site_index" %in% names(result))
  expect_true("forest_type_code" %in% names(result))
  expect_equal(result$elevation, c(200L, 350L))
})

test_that("get_site_data errors without PLOT table", {
  expect_error(get_site_data(list(COND = data.frame())), "PLOT table")
})

test_that("get_site_data works without COND table", {
  fia <- list(
    PLOT = data.frame(
      CN       = 1001,
      INVYR    = 2019L,
      STATECD  = 44L,
      LAT      = 41.5,
      LON      = -71.5,
      stringsAsFactors = FALSE
    )
  )
  result <- get_site_data(fia)
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 1L)
})


# ---- Tests for get_species_summary ----

test_that("get_species_summary returns per-species per-plot rows", {
  tl     <- make_synthetic_tree_list()
  result <- get_species_summary(tl)

  expect_s3_class(result, "tbl_df")
  # Should have multiple rows (species x plot combos, live trees only)
  expect_true(nrow(result) > 2)
  expect_true("spp_code" %in% names(result))
  expect_true("tpa" %in% names(result))
  expect_true("ba_per_acre" %in% names(result))
  expect_true("pct_ba" %in% names(result))
})

test_that("get_species_summary filters to live trees", {
  tl     <- make_synthetic_tree_list()
  result <- get_species_summary(tl)

  # Plot 1001 dead tree (WP, dbh=14) should be excluded
  plot1_wp <- result[result$plot_id == "44_7_1001" & result$spp_code == "WP", ]
  expect_equal(nrow(plot1_wp), 0L)
})

test_that("get_species_summary pct_ba sums to 100 per plot", {
  tl     <- make_synthetic_tree_list()
  result <- get_species_summary(tl)

  pct_sums <- tapply(result$pct_ba, result$plot_id, sum)
  for (s in pct_sums) {
    expect_equal(s, 100, tolerance = 0.5)
  }
})

test_that("get_species_summary computes qmd per species", {
  tl     <- make_synthetic_tree_list()
  result <- get_species_summary(tl)

  # Check BF on plot 1001 (live trees: dbh 10, 8; tpa 6, 6)
  bf_1001 <- result[result$plot_id == "44_7_1001" & result$spp_code == "BF", ]
  expected_qmd <- sqrt(sum(c(10, 8)^2 * c(6, 6)) / sum(c(6, 6)))
  expect_equal(bf_1001$qmd, expected_qmd)
})
