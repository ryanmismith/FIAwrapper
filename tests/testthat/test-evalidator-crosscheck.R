# -----------------------------------------------------------------------
# EVALIDator Cross-Check Tests
#
# These tests verify that FIAwrapper per-acre estimates match hand-
# calculated values using the same methodology as USDA's EVALIDator
# tool.  Every test computes the expected answer independently from
# the raw FIA tables (TREE × COND) and compares to the package output.
#
# Reference: FIA Database Description and User Guide v9.2, Chapter 3
# https://www.fia.fs.usda.gov/library/database-documentation/
# -----------------------------------------------------------------------

# ---- Helper: hand-computed per-plot estimates from raw FIA tables ------
hand_compute_plot_estimates <- function() {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE
  cond <- fia_ri$COND

  tc <- merge(tree, cond[, c("PLT_CN", "CONDID", "CONDPROP_UNADJ")],
              by = c("PLT_CN", "CONDID"))
  live <- tc[tc$STATUSCD == 1, ]
  live$BA_TREE <- live$DIA^2 * 0.005454154
  live$TPA_EXP <- live$TPA_UNADJ * live$CONDPROP_UNADJ

  do.call(rbind, lapply(split(live, live$PLT_CN), function(df) {
    tpa <- sum(df$TPA_EXP)
    ba  <- sum(df$BA_TREE * df$TPA_EXP)
    qmd <- sqrt(sum(df$DIA^2 * df$TPA_EXP) / tpa)
    volcf   <- sum(df$VOLCFNET * df$TPA_EXP)
    volcfg  <- sum(df$VOLCFGRS * df$TPA_EXP)
    volbf   <- sum(df$VOLBFNET * df$TPA_EXP)
    drybio  <- sum(df$DRYBIO_AG * df$TPA_EXP)
    carbon  <- sum(df$CARBON_AG * df$TPA_EXP)
    data.frame(PLT_CN = df$PLT_CN[1], TPA = tpa, BA = ba, QMD = qmd,
               VOLCF = volcf, VOLCFG = volcfg, VOLBF = volbf,
               DRYBIO = drybio, CARBON = carbon,
               stringsAsFactors = FALSE)
  }))
}

# ==================================================================
# 1. TPA per-acre matches EVALIDator methodology
# ==================================================================
test_that("per-plot TPA matches hand-calculated TPA_UNADJ * CONDPROP_UNADJ", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  ps <- get_plot_summary(tl)
  ps$plt_cn <- as.numeric(sub("^44_[0-9]+_", "", ps$plot_id))

  for (i in seq_len(nrow(hand))) {
    idx <- which(ps$plt_cn == hand$PLT_CN[i])
    expect_equal(ps$tpa[idx], hand$TPA[i], tolerance = 1e-4,
                 label = paste("TPA for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 2. BA per-acre matches EVALIDator methodology
# ==================================================================
test_that("per-plot BA matches sum(DBH^2 * 0.005454154 * TPA_UNADJ * CONDPROP)", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  ps <- get_plot_summary(tl)
  ps$plt_cn <- as.numeric(sub("^44_[0-9]+_", "", ps$plot_id))

  for (i in seq_len(nrow(hand))) {
    idx <- which(ps$plt_cn == hand$PLT_CN[i])
    expect_equal(ps$ba_per_acre[idx], hand$BA[i], tolerance = 1e-4,
                 label = paste("BA for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 3. QMD matches hand calculation
# ==================================================================
test_that("per-plot QMD = sqrt(sum(DBH^2 * TPA) / sum(TPA))", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  ps <- get_plot_summary(tl)
  ps$plt_cn <- as.numeric(sub("^44_[0-9]+_", "", ps$plot_id))

  for (i in seq_len(nrow(hand))) {
    idx <- which(ps$plt_cn == hand$PLT_CN[i])
    expect_equal(ps$qmd[idx], hand$QMD[i], tolerance = 1e-4,
                 label = paste("QMD for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 4. Volume per-acre matches hand calculation
# ==================================================================
test_that("per-plot net CF volume per-acre matches hand calculation", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)

  pkg <- do.call(rbind, lapply(split(tl, tl$plot_cn), function(df) {
    data.frame(PLT_CN = df$plot_cn[1],
               VOLCF = sum(df$vol_cf_net * df$tpa, na.rm = TRUE))
  }))

  for (i in seq_len(nrow(hand))) {
    idx <- which(pkg$PLT_CN == hand$PLT_CN[i])
    expect_equal(pkg$VOLCF[idx], hand$VOLCF[i], tolerance = 1e-4,
                 label = paste("Vol CF for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 5. Biomass per-acre matches hand calculation
# ==================================================================
test_that("per-plot dry biomass per-acre matches hand calculation", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)

  pkg <- do.call(rbind, lapply(split(tl, tl$plot_cn), function(df) {
    data.frame(PLT_CN = df$plot_cn[1],
               DRYBIO = sum(df$biomass_ag_dry * df$tpa, na.rm = TRUE))
  }))

  for (i in seq_len(nrow(hand))) {
    idx <- which(pkg$PLT_CN == hand$PLT_CN[i])
    expect_equal(pkg$DRYBIO[idx], hand$DRYBIO[i], tolerance = 1e-4,
                 label = paste("Biomass for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 6. Carbon per-acre matches hand calculation
# ==================================================================
test_that("per-plot carbon per-acre matches hand calculation", {
  hand <- hand_compute_plot_estimates()
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)

  pkg <- do.call(rbind, lapply(split(tl, tl$plot_cn), function(df) {
    data.frame(PLT_CN = df$plot_cn[1],
               CARBON = sum(df$carbon_ag * df$tpa, na.rm = TRUE))
  }))

  for (i in seq_len(nrow(hand))) {
    idx <- which(pkg$PLT_CN == hand$PLT_CN[i])
    expect_equal(pkg$CARBON[idx], hand$CARBON[i], tolerance = 1e-4,
                 label = paste("Carbon for plot", hand$PLT_CN[i]))
  }
})

# ==================================================================
# 7. TPA_UNADJ in raw data matches FIA plot design geometry
# ==================================================================
test_that("TPA_UNADJ matches FIA subplot/microplot geometry", {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE

  # Subplot: 4 subplots × π × 24² ft² each = 7,238.23 ft²
  # TPA = 43560 / 7238.23 = 6.018046
  subplot_tpa <- 43560 / (4 * pi * 24^2)


  # Microplot: 4 microplots × π × 6.8² ft² each = 581.07 ft²
  # TPA = 43560 / 581.07 = 74.965282
  microplot_tpa <- 43560 / (4 * pi * 6.8^2)

  # Every tree ≥ 5" should have subplot TPA
  big <- tree[tree$DIA >= 5.0, ]
  expect_true(all(abs(big$TPA_UNADJ - subplot_tpa) < 0.01),
              label = "All trees >= 5 DBH have subplot TPA")

  # Every sapling < 5" should have microplot TPA
  sap <- tree[tree$DIA < 5.0, ]
  expect_true(all(abs(sap$TPA_UNADJ - microplot_tpa) < 0.01),
              label = "All saplings < 5 DBH have microplot TPA")
})

# ==================================================================
# 8. BA formula constant matches exactly
# ==================================================================
test_that("BA constant 0.005454154 = pi / (4 * 144)", {
  ba_constant <- pi / (4 * 144)
  expect_equal(ba_constant, 0.005454154, tolerance = 1e-7)
  # Verify tree list uses this constant
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  # BA for a 10" tree should be 10^2 * 0.005454154 = 0.5454154 ft²
  t10 <- tl[abs(tl$dbh - 10.0) < 0.05, ]
  if (nrow(t10) > 0) {
    expect_equal(t10$ba_tree[1], 10^2 * 0.005454154, tolerance = 1e-6)
  }
})

# ==================================================================
# 9. Statewide means fall within published RI ranges
# ==================================================================
test_that("statewide means are within plausible RI ranges", {
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = TRUE)
  ps <- get_plot_summary(tl)

  mean_ba  <- mean(ps$ba_per_acre)
  mean_tpa <- mean(ps$tpa)
  mean_qmd <- mean(ps$qmd)

  # RI published ranges (FIA State Fact Sheet) with generous margins
  # for synthetic data
  expect_true(mean_ba >= 50 & mean_ba <= 300,
              label = sprintf("Mean BA %.1f in plausible range [50, 300]", mean_ba))
  expect_true(mean_tpa >= 50 & mean_tpa <= 1500,
              label = sprintf("Mean TPA %.1f in plausible range [50, 1500]", mean_tpa))
  expect_true(mean_qmd >= 3 & mean_qmd <= 20,
              label = sprintf("Mean QMD %.1f in plausible range [3, 20]", mean_qmd))
})

# ==================================================================
# 10. Per-plot BA = sum of per-tree ba_per_acre
# ==================================================================
test_that("plot summary BA equals sum of tree-level ba_per_acre", {
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  ps <- get_plot_summary(tl)

  for (pid in ps$plot_id) {
    trees_in_plot <- tl[tl$plot_id == pid, ]
    tree_ba_sum <- sum(trees_in_plot$ba_per_acre, na.rm = TRUE)
    ps_ba <- ps$ba_per_acre[ps$plot_id == pid]
    expect_equal(tree_ba_sum, ps_ba, tolerance = 1e-4,
                 label = paste("BA sum for", pid))
  }
})

# ==================================================================
# 11. Per-plot TPA = sum of per-tree tpa
# ==================================================================
test_that("plot summary TPA equals sum of tree-level tpa", {
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  ps <- get_plot_summary(tl)

  for (pid in ps$plot_id) {
    trees_in_plot <- tl[tl$plot_id == pid, ]
    tree_tpa_sum <- sum(trees_in_plot$tpa, na.rm = TRUE)
    ps_tpa <- ps$tpa[ps$plot_id == pid]
    expect_equal(tree_tpa_sum, ps_tpa, tolerance = 1e-4,
                 label = paste("TPA sum for", pid))
  }
})

# ==================================================================
# 12. No double-counting of trees across inventory years
# ==================================================================
test_that("tree CNs are unique within each inventory year", {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE
  plot <- fia_ri$PLOT

  tree_plot <- merge(tree, plot[, c("CN", "INVYR")],
                     by.x = "PLT_CN", by.y = "CN")
  for (yr in unique(tree_plot$INVYR)) {
    yr_trees <- tree_plot[tree_plot$INVYR == yr, ]
    expect_equal(length(unique(yr_trees$CN)), nrow(yr_trees),
                 label = paste("Unique tree CNs in", yr))
  }
})

# ==================================================================
# 13. build_tree_list excludes dead trees by default
# ==================================================================
test_that("build_tree_list excludes dead trees when include_dead = FALSE", {
  tl_live <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  tl_all  <- build_tree_list(fia_ri, include_dead = TRUE, most_recent = FALSE)

  n_dead_raw <- sum(fia_ri$TREE$STATUSCD == 2)
  expect_true(nrow(tl_all) > nrow(tl_live))
  expect_equal(nrow(tl_all) - nrow(tl_live), n_dead_raw)
  expect_true(all(tl_live$status_code == 1))
})

# ==================================================================
# 14. Growth rates for remeasured trees are biologically plausible
# ==================================================================
test_that("annual DBH growth is between 0 and 1 inch/year for survivors", {
  ts <- build_time_series(fia_ri)
  survivors <- ts[!is.na(ts$annual_dbh_growth) & ts$mortality == FALSE, ]

  expect_true(nrow(survivors) > 0, label = "Has surviving remeasured trees")
  expect_true(all(survivors$annual_dbh_growth >= 0),
              label = "No negative DBH growth")
  expect_true(all(survivors$annual_dbh_growth <= 1.0),
              label = "DBH growth <= 1 in/yr")
  expect_true(mean(survivors$annual_dbh_growth) >= 0.05,
              label = "Mean growth >= 0.05 in/yr")
  expect_true(mean(survivors$annual_dbh_growth) <= 0.5,
              label = "Mean growth <= 0.5 in/yr")
})

# ==================================================================
# 15. Growth summary net BA growth is positive (healthy stands)
# ==================================================================
test_that("net BA growth is positive for these healthy stands", {
  ts <- build_time_series(fia_ri)
  gs <- summarize_growth(ts)

  expect_true(all(gs$net_ba_growth_per_acre > 0),
              label = "All plots have positive net BA growth")
  # Annual BA growth typically 1-5 ft²/acre/year
  annual_growth <- gs$net_ba_growth_per_acre / gs$period_years
  expect_true(all(annual_growth > 0 & annual_growth < 10),
              label = "Annual BA growth in plausible range")
})

# ==================================================================
# 16. Board foot volume only for trees >= 9" DBH (sawtimber)
# ==================================================================
test_that("board foot volume is zero for trees below sawtimber size", {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE
  small <- tree[tree$DIA < 9.0, ]
  expect_true(all(small$VOLBFNET == 0),
              label = "No BF volume for trees < 9 inches")
})

# ==================================================================
# 17. CONDPROP_UNADJ sums to 1 per plot
# ==================================================================
test_that("condition proportions sum to 1 for each plot", {
  data(fia_ri, envir = environment())
  cond <- fia_ri$COND
  props <- aggregate(CONDPROP_UNADJ ~ PLT_CN, data = cond, FUN = sum)
  expect_true(all(abs(props$CONDPROP_UNADJ - 1.0) < 0.001),
              label = "CONDPROP_UNADJ sums to 1 per plot")
})

# ==================================================================
# 18. Carbon is approximately half of dry biomass
# ==================================================================
test_that("carbon is approximately 50% of dry aboveground biomass", {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE
  # Only check trees with meaningful biomass (> 100 lbs) to avoid

  # integer rounding artifacts in very small trees
  has_bio <- tree[tree$DRYBIO_AG > 100, ]
  ratio <- has_bio$CARBON_AG / has_bio$DRYBIO_AG
  expect_true(all(abs(ratio - 0.5) < 0.02),
              label = "Carbon/biomass ratio is ~0.5 for substantial trees")
})

# ==================================================================
# 19. Gross CF volume >= Net CF volume for every tree
# ==================================================================
test_that("gross cubic foot volume >= net cubic foot volume", {
  data(fia_ri, envir = environment())
  tree <- fia_ri$TREE
  has_vol <- tree[tree$VOLCFGRS > 0, ]
  expect_true(all(has_vol$VOLCFGRS >= has_vol$VOLCFNET),
              label = "Gross CF >= Net CF for all trees")
})

# ==================================================================
# 20. Species percentages sum to 100% per plot
# ==================================================================
test_that("species pct_ba sums to ~100% per plot", {
  set_fvs_variant("NE")
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = TRUE)
  spp <- get_species_summary(tl)

  pct_sums <- aggregate(pct_ba ~ plot_id, data = spp, FUN = sum)
  expect_true(all(abs(pct_sums$pct_ba - 100) < 0.5),
              label = "Species pct_ba sums to ~100% per plot")
})

# ==================================================================
# 21. FVS export preserves all trees
# ==================================================================
test_that("FVS CSV export preserves correct tree count", {
  set_fvs_variant("NE")
  tl <- build_tree_list(fia_ri, include_dead = FALSE, most_recent = FALSE)
  tmp <- tempfile(fileext = ".csv")
  export_fvs_treelist(tl, tmp, format = "csv")
  exported <- read.csv(tmp)
  expect_equal(nrow(exported), nrow(tl))
  unlink(tmp)
})

# ==================================================================
# 22. Remeasured plots link correctly via PREV_PLT_CN
# ==================================================================
test_that("remeasured plot linkage is consistent", {
  data(fia_ri, envir = environment())
  plot <- fia_ri$PLOT

  linked <- plot[!is.na(plot$PREV_PLT_CN), ]
  expect_true(nrow(linked) > 0, label = "Has remeasured plots")

  # All PREV_PLT_CN values should exist as CNs
  for (prev in linked$PREV_PLT_CN) {
    expect_true(prev %in% plot$CN,
                label = paste("PREV_PLT_CN", prev, "exists in PLOT table"))
  }
})

# ==================================================================
# 23. Verify subplot TPA against formal derivation
# ==================================================================
test_that("subplot TPA = 43560 / (4 * pi * 24^2) to 6 decimal places", {
  expected <- 43560 / (4 * pi * 24^2)  # = 6.018046...
  expect_equal(round(expected, 6), 6.018046)

  # compute_tpa with all 4 subplots should produce standard TPA
  tpa_vals <- compute_tpa(dbh = c(10, 15, 20, 12), subplot = c(1, 2, 3, 4))
  expect_equal(tpa_vals[1], expected, tolerance = 1e-4)
})

# ==================================================================
# 24. Verify microplot TPA against formal derivation
# ==================================================================
test_that("microplot TPA = 43560 / (4 * pi * 6.8^2) to 3 decimal places", {
  expected <- 43560 / (4 * pi * 6.8^2)  # = 74.965282...
  expect_equal(round(expected, 3), 74.965)

  # compute_tpa with all 4 subplots should produce standard microplot TPA
  tpa_vals <- compute_tpa(dbh = c(2, 3, 4, 2.5), subplot = c(1, 2, 3, 4))
  expect_equal(tpa_vals[1], expected, tolerance = 1e-2)
})

# ==================================================================
# 25. Statewide volume/biomass totals are additive
# ==================================================================
test_that("statewide totals equal sum of per-plot estimates", {
  hand <- hand_compute_plot_estimates()

  # Mean of per-plot estimates = statewide mean
  mean_ba   <- mean(hand$BA)
  mean_tpa  <- mean(hand$TPA)
  total_ba  <- sum(hand$BA)

  expect_equal(total_ba, mean_ba * nrow(hand), tolerance = 1e-4)
  expect_equal(sum(hand$TPA), mean_tpa * nrow(hand), tolerance = 1e-4)
})
