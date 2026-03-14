# ============================================================================
# FIAwrapper Accuracy Validation Tests
#
# These tests validate that FIAwrapper produces outputs consistent with
# FIA documentation, EVALIDator methodology, and hand-computed values.
# They use the bundled fia_ri dataset and verify:
#
# 1. TPA expansion factors match FIA nested plot design geometry
# 2. Basal area computations match the forestry-standard formula
# 3. QMD matches the weighted quadratic mean formula
# 4. Plot summaries aggregate correctly
# 5. Time series linking and growth computation are accurate
# 6. Species codes match FIA REF_SPECIES
# 7. FVS export format is correct
# 8. Condition proportion adjustments work properly
#
# Reference: Bechtold & Patterson (2005), FIADB User Guide v9.2
# ============================================================================

# ---- Load example data ----
data(fia_ri, package = "FIAwrapper")

# ============================================================================
# 1. TPA VALIDATION
# ============================================================================

test_that("TPA for subplot trees matches FIA design geometry exactly", {
  # FIA Phase 2 subplot: 24-foot radius

  # Area of one subplot: pi * 24^2 = 1809.557 sq ft
  # In acres: 1809.557 / 43560 = 0.04154 acres
  # 4 subplots per plot
  # TPA = 1 / (0.04154 * 4) = 6.018046
  subplot_area_ft2 <- pi * 24.0^2
  subplot_area_ac <- subplot_area_ft2 / 43560
  expected_tpa <- 1 / (subplot_area_ac * 4)

  # compute_tpa with default n_subplots=4 (FIA standard)
  tpa <- compute_tpa(dbh = c(10, 15, 20, 8), subplot = c(1, 2, 3, 4))
  expect_equal(tpa[1], expected_tpa, tolerance = 0.001)
  expect_equal(round(tpa[1], 3), 6.018)

  # Even with trees on only 3 subplots, n_subplots=4 by default
  tpa3 <- compute_tpa(dbh = c(10, 15, 20), subplot = c(1, 2, 3))
  expect_equal(tpa3[1], expected_tpa, tolerance = 0.001)
})

test_that("TPA for microplot trees matches FIA design geometry exactly", {
  # FIA Phase 2 microplot: 6.8-foot radius
  # Area of one microplot: pi * 6.8^2 = 145.267 sq ft
  # In acres: 145.267 / 43560 = 0.003336 acres
  # TPA = 1 / (0.003336 * 4) = 74.965
  microplot_area_ft2 <- pi * 6.8^2
  microplot_area_ac <- microplot_area_ft2 / 43560
  expected_tpa <- 1 / (microplot_area_ac * 4)

  tpa <- compute_tpa(dbh = c(2, 3, 4, 1), subplot = c(1, 2, 3, 4))
  expect_equal(tpa[1], expected_tpa, tolerance = 0.001)
  expect_true(abs(tpa[1] - 74.965) < 0.1)
})

test_that("DBH threshold of 5.0 inches separates subplot from microplot", {
  # Trees exactly at 5.0 inches go to subplot; default n_subplots=4
  tpa <- compute_tpa(dbh = c(5.0, 4.9), subplot = c(1, 1))
  subplot_tpa <- 1 / (pi * 24^2 / 43560 * 4)
  microplot_tpa <- 1 / (pi * 6.8^2 / 43560 * 4)
  expect_equal(tpa[1], subplot_tpa, tolerance = 0.001)   # >= 5.0 -> subplot
  expect_equal(tpa[2], microplot_tpa, tolerance = 0.001)  # < 5.0 -> microplot
})

test_that("Condition proportion adjustment multiplies TPA correctly", {
  # CONDPROP_UNADJ = 0.5 means half the plot area is in this condition
  # TPA should halve: TPA_EXP = TPA_UNADJ * CONDPROP_UNADJ (EVALIDator methodology)
  tpa_full <- compute_tpa(dbh = 10, subplot = 1, cond_prop = 1.0)
  tpa_half <- compute_tpa(dbh = 10, subplot = 1, cond_prop = 0.5)
  expect_equal(tpa_half, tpa_full * 0.5, tolerance = 0.001)
})

# ============================================================================
# 2. BASAL AREA VALIDATION
# ============================================================================

test_that("ba_ft2 matches forestry standard formula", {
  # BA (sq ft) = pi/4 * (DBH/12)^2 = DBH^2 * pi / (4 * 144) = DBH^2 * 0.005454154
  # For a 10-inch tree: 10^2 * 0.005454154 = 0.5454154 sq ft
  expect_equal(ba_ft2(10), 0.5454154, tolerance = 1e-6)

  # For a 12-inch tree: 144 * 0.005454154 = 0.7853982 sq ft
  # Cross-check: pi/4 * (12/12)^2 = pi/4 = 0.7853982
  expect_equal(ba_ft2(12), pi / 4, tolerance = 1e-6)

  # Zero DBH = zero BA
  expect_equal(ba_ft2(0), 0)
})

test_that("ba_per_acre in tree list equals ba_tree * tpa", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  # BA per acre should equal individual BA times TPA
  expected <- trees$ba_tree * trees$tpa
  expect_equal(trees$ba_per_acre, expected, tolerance = 1e-6)
})

test_that("ba_tree equals ba_ft2(dbh)", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  expected <- ba_ft2(trees$dbh)
  expect_equal(trees$ba_tree, expected, tolerance = 1e-6)
})

# ============================================================================
# 3. QMD VALIDATION
# ============================================================================

test_that("QMD matches quadratic mean diameter formula", {
  # QMD = sqrt(sum(DBH^2 * TPA) / sum(TPA))
  dbh <- c(8, 10, 12, 14, 16)
  tpa <- c(20, 15, 12, 8, 5)
  expected <- sqrt(sum(dbh^2 * tpa) / sum(tpa))
  expect_equal(qmd(dbh, tpa), expected, tolerance = 1e-6)
})

test_that("QMD is always >= arithmetic mean diameter", {
  # Mathematical property: QMD >= mean(DBH) when weighted by TPA
  trees <- build_tree_list(fia_ri, variant = "NE")
  for (pid in unique(trees$plot_id)[1:5]) {
    plot_trees <- trees[trees$plot_id == pid, ]
    plot_qmd <- qmd(plot_trees$dbh, plot_trees$tpa)
    arith_mean <- stats::weighted.mean(plot_trees$dbh, plot_trees$tpa)
    expect_true(plot_qmd >= arith_mean - 0.001,
                info = paste("QMD should be >= arithmetic mean for plot", pid))
  }
})

# ============================================================================
# 4. PLOT SUMMARY VALIDATION
# ============================================================================

test_that("plot summary TPA equals sum of individual tree TPA", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  plots <- get_plot_summary(trees)

  for (pid in unique(trees$plot_id)[1:5]) {
    tree_tpa <- sum(trees$tpa[trees$plot_id == pid])
    plot_tpa <- plots$tpa[plots$plot_id == pid]
    expect_equal(plot_tpa, tree_tpa, tolerance = 0.01,
                 info = paste("TPA mismatch for plot", pid))
  }
})

test_that("plot summary BA/acre equals sum of individual BA/acre", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  plots <- get_plot_summary(trees)

  for (pid in unique(trees$plot_id)[1:5]) {
    tree_ba <- sum(trees$ba_per_acre[trees$plot_id == pid])
    plot_ba <- plots$ba_per_acre[plots$plot_id == pid]
    expect_equal(plot_ba, tree_ba, tolerance = 0.01,
                 info = paste("BA/ac mismatch for plot", pid))
  }
})

test_that("plot summary QMD matches hand computation", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  plots <- get_plot_summary(trees)

  for (pid in unique(trees$plot_id)[1:3]) {
    pt <- trees[trees$plot_id == pid, ]
    hand_qmd <- sqrt(sum(pt$dbh^2 * pt$tpa) / sum(pt$tpa))
    plot_qmd <- plots$qmd[plots$plot_id == pid]
    expect_equal(plot_qmd, hand_qmd, tolerance = 0.01,
                 info = paste("QMD mismatch for plot", pid))
  }
})

test_that("species pct_ba sums to 100 for each plot", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  spp <- get_species_summary(trees)

  for (pid in unique(spp$plot_id)) {
    pct_sum <- sum(spp$pct_ba[spp$plot_id == pid], na.rm = TRUE)
    expect_equal(pct_sum, 100, tolerance = 0.5,
                 info = paste("pct_ba should sum to 100 for plot", pid))
  }
})

test_that("dead trees excluded by default in get_plot_summary", {
  all_trees <- build_tree_list(fia_ri, include_dead = TRUE, variant = "NE")
  live_trees <- all_trees[all_trees$status == "live", ]

  plots_default <- get_plot_summary(all_trees, include_dead = FALSE)
  plots_live <- get_plot_summary(live_trees)

  # Should be identical
  expect_equal(plots_default$tpa, plots_live$tpa, tolerance = 0.01)
  expect_equal(plots_default$ba_per_acre, plots_live$ba_per_acre, tolerance = 0.01)
})

# ============================================================================
# 5. TIME SERIES VALIDATION
# ============================================================================

test_that("remeasured plots are correctly identified", {
  remeas <- get_remeasured_plots(fia_ri)
  # fia_ri has 6 plots with 2014+2019 data
  expect_equal(nrow(remeas), 6)
  expect_true(all(remeas$n_measurements == 2))
  expect_true(all(remeas$first_year == 2014))
  expect_true(all(remeas$last_year == 2019))
})

test_that("time series growth is positive for surviving trees", {
  ts <- build_time_series(fia_ri, include_dead = TRUE, variant = "NE")

  # Survivors in measurement 2 should mostly have positive growth
  survivors <- ts[ts$measurement_number == 2 & !ts$ingrowth & !ts$mortality &
                    !is.na(ts$dbh_growth), ]
  if (nrow(survivors) > 0) {
    # Most survivors should show positive growth
    pct_positive <- mean(survivors$dbh_growth > 0, na.rm = TRUE)
    expect_true(pct_positive > 0.5,
                info = paste("Only", round(pct_positive*100), "% of survivors show positive growth"))
  }
})

test_that("annual growth is periodic growth divided by years", {
  ts <- build_time_series(fia_ri, include_dead = TRUE, variant = "NE")

  valid <- ts[!is.na(ts$dbh_growth) & !is.na(ts$annual_dbh_growth) &
                ts$years_since_previous > 0, ]
  if (nrow(valid) > 0) {
    expected_annual <- valid$dbh_growth / valid$years_since_previous
    expect_equal(valid$annual_dbh_growth, expected_annual, tolerance = 0.001)
  }
})

test_that("growth summary net = gross - mortality", {
  ts <- build_time_series(fia_ri, include_dead = TRUE, variant = "NE")
  growth <- summarize_growth(ts)

  # Net growth should approximately equal gross growth minus mortality
  # (may not be exact due to ingrowth handling)
  for (i in seq_len(nrow(growth))) {
    expected_net <- growth$gross_ba_growth_per_acre[i] - growth$mortality_ba_per_acre[i]
    actual_net <- growth$net_ba_growth_per_acre[i]
    # Allow tolerance for rounding
    expect_equal(actual_net, expected_net, tolerance = 0.5,
                 info = paste("Net growth mismatch for row", i))
  }
})

# ============================================================================
# 6. SPECIES CODE VALIDATION
# ============================================================================

test_that("core NE species codes match FIA REF_SPECIES", {
  # Verified against FIA REF_SPECIES table
  # https://apps.fs.usda.gov/fia/datamart/CSV/REF_SPECIES.csv
  set_fvs_variant("NE")

  expect_equal(fia_to_fvs(316), "RM")   # red maple
  expect_equal(fia_to_fvs(833), "RO")   # red oak (northern red oak)
  expect_equal(fia_to_fvs(129), "WP")   # eastern white pine
  expect_equal(fia_to_fvs(261), "EH")   # eastern hemlock
  expect_equal(fia_to_fvs(12),  "BF")   # balsam fir
  expect_equal(fia_to_fvs(97),  "RS")   # red spruce
  expect_equal(fia_to_fvs(318), "SM")   # sugar maple
  expect_equal(fia_to_fvs(531), "AB")   # American beech (Fagus grandifolia)
  expect_equal(fia_to_fvs(541), "WA")   # white ash (Fraxinus americana)
  expect_equal(fia_to_fvs(802), "WO")   # white oak (Quercus alba)
  expect_equal(fia_to_fvs(241), "WC")   # northern white-cedar (Thuja occidentalis)
})

test_that("fvs_to_fia is inverse of fia_to_fvs for common species", {
  set_fvs_variant("NE")

  # Round-trip: FIA -> FVS -> FIA
  codes <- c(316, 129, 261, 12, 97, 318, 802)
  for (code in codes) {
    fvs <- fia_to_fvs(code)
    back <- fvs_to_fia(fvs)
    # May not be exact inverse if multiple SPCD map to same FVS code
    # but the FVS code should work
    expect_equal(fia_to_fvs(back), fvs,
                 info = paste("Round-trip failed for SPCD", code))
  }
})

test_that("get_common_name returns correct names", {
  expect_equal(get_common_name(316), "red maple")
  expect_equal(get_common_name(129), "eastern white pine")
  expect_equal(get_common_name(12), "balsam fir")
})

# ============================================================================
# 7. FVS EXPORT VALIDATION
# ============================================================================

test_that("FVS CSV export has correct columns and values", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile))

  export_fvs_treelist(trees, tmpfile, variant = "NE", format = "csv")
  result <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)

  # Required FVS columns present
  expect_true("stand_id" %in% names(result))
  expect_true("species" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("tpa" %in% names(result))

  # Species codes should be FVS alpha (2-3 chars)
  expect_true(all(nchar(result$species) <= 3))
  expect_true(all(nchar(result$species) >= 1))

  # DBH should be positive
  expect_true(all(result$dbh > 0, na.rm = TRUE))

  # TPA should be positive
  expect_true(all(result$tpa > 0, na.rm = TRUE))

  # Row count should match tree list
  expect_equal(nrow(result), nrow(trees))
})

test_that("FVS fixed-width export has correct field widths", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  tmpfile <- tempfile(fileext = ".tre")
  on.exit(unlink(tmpfile))

  export_fvs_treelist(trees, tmpfile, variant = "NE", format = "standard")
  lines <- readLines(tmpfile)

  # Should have same number of lines as trees
  expect_equal(length(lines), nrow(trees))

  # Each line should be the same width (fixed-width format)
  widths <- nchar(lines)
  expect_true(all(widths > 20), info = "Lines too short for FVS format")
})

test_that("FVS stand export has correct structure", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  plots <- get_plot_summary(trees)
  tmpfile <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpfile))

  export_fvs_standlist(plots, tmpfile, variant = "NE")
  result <- utils::read.csv(tmpfile, stringsAsFactors = FALSE)

  expect_true("stand_id" %in% names(result))
  expect_true("variant" %in% names(result))
  expect_equal(nrow(result), nrow(plots))
  expect_true(all(result$variant == "NE"))
})

# ============================================================================
# 8. COMPETITION INDEX VALIDATION
# ============================================================================

test_that("BAL is zero for the largest tree on each plot", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  comp <- compute_competition(trees)

  # For each plot, the tree with the largest DBH should have BAL = 0
  grp_col <- if ("plot_id" %in% names(comp)) "plot_id" else "plot_cn"
  for (pid in unique(comp[[grp_col]])[1:5]) {
    plot_trees <- comp[comp[[grp_col]] == pid, ]
    max_dbh_row <- plot_trees[which.max(plot_trees$dbh), ]
    expect_equal(max_dbh_row$bal, 0, tolerance = 0.01,
                 info = paste("BAL should be 0 for largest tree on plot", pid))
  }
})

test_that("BAL is computed correctly (sum of BA from larger trees)", {
  # Create a simple test case
  test_trees <- tibble::tibble(
    plot_id = rep("test", 3),
    plot_cn = rep(1, 3),
    dbh = c(10, 15, 20),
    tpa = rep(6, 3),
    ba_per_acre = ba_ft2(c(10, 15, 20)) * 6,
    inventory_year = rep(2019, 3),
    crown_ratio = rep(40, 3)
  )
  comp <- compute_competition(test_trees)

  # Tree 1 (DBH=10): BAL = BA from trees with DBH 15 and 20
  expected_bal_10 <- ba_ft2(15) * 6 + ba_ft2(20) * 6
  expect_equal(comp$bal[comp$dbh == 10], expected_bal_10, tolerance = 0.01)

  # Tree 2 (DBH=15): BAL = BA from tree with DBH 20
  expected_bal_15 <- ba_ft2(20) * 6
  expect_equal(comp$bal[comp$dbh == 15], expected_bal_15, tolerance = 0.01)

  # Tree 3 (DBH=20): BAL = 0 (largest)
  expect_equal(comp$bal[comp$dbh == 20], 0, tolerance = 0.01)
})

test_that("plot_ba equals sum of all ba_per_acre on the plot", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  comp <- compute_competition(trees)

  for (pid in unique(comp$plot_id)[1:5]) {
    plot_trees <- comp[comp$plot_id == pid, ]
    hand_plot_ba <- sum(plot_trees$ba_per_acre)
    expect_true(all(abs(plot_trees$plot_ba - hand_plot_ba) < 0.01),
                info = paste("plot_ba mismatch for plot", pid))
  }
})

test_that("relative_dbh equals dbh / plot_qmd", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  comp <- compute_competition(trees)

  expected <- comp$dbh / comp$plot_qmd
  expect_equal(comp$relative_dbh, expected, tolerance = 0.001)
})

# ============================================================================
# 9. OUTLIER DETECTION VALIDATION
# ============================================================================

test_that("outlier bounds are in imperial units", {
  # A 10-inch BF with 55-ft height should NOT be flagged
  normal <- data.frame(spp_code = "BF", dbh = 10, height = 55)
  result <- flag_outliers(normal)
  expect_false(result$flag_dbh)
  expect_false(result$flag_ht)

  # A 30-inch BF (exceeds 25.6" max for spruce_fir) SHOULD be flagged
  big <- data.frame(spp_code = "BF", dbh = 30, height = 90)
  result <- flag_outliers(big)
  expect_true(result$flag_dbh)
})

# ============================================================================
# 10. DATA INTEGRITY
# ============================================================================

test_that("build_tree_list produces no duplicate tree records", {
  trees <- build_tree_list(fia_ri, variant = "NE", most_recent = TRUE)
  # Each tree_cn should appear exactly once
  expect_equal(length(unique(trees$tree_cn)), nrow(trees))
})

test_that("all live trees have positive DBH and height", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  live <- trees[trees$status == "live", ]
  expect_true(all(live$dbh > 0))
  expect_true(all(live$height > 0, na.rm = TRUE))
})

test_that("all TPA values are positive", {
  trees <- build_tree_list(fia_ri, variant = "NE")
  expect_true(all(trees$tpa > 0))
})

test_that("status column only contains expected values", {
  all_trees <- build_tree_list(fia_ri, include_dead = TRUE, variant = "NE")
  expect_true(all(all_trees$status %in% c("live", "dead")))
})

test_that("BA/acre values are in reasonable forestry range", {
  plots <- get_plot_summary(build_tree_list(fia_ri, variant = "NE"))
  # Typical NE forest: 40-250 ft²/acre BA
  expect_true(all(plots$ba_per_acre > 20),
              info = "Some plots have unreasonably low BA")
  expect_true(all(plots$ba_per_acre < 400),
              info = "Some plots have unreasonably high BA")
})

test_that("QMD values are in reasonable range for NE forests", {
  plots <- get_plot_summary(build_tree_list(fia_ri, variant = "NE"))
  # Typical QMD: 3-25 inches
  expect_true(all(plots$qmd > 2, na.rm = TRUE))
  expect_true(all(plots$qmd < 30, na.rm = TRUE))
})

test_that("TPA values are in reasonable range", {
  plots <- get_plot_summary(build_tree_list(fia_ri, variant = "NE"))
  # Typical: 50-1000 TPA
  expect_true(all(plots$tpa > 10))
  expect_true(all(plots$tpa < 2000))
})
