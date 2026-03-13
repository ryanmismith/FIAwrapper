# Build example FIA dataset for FIAwrapper package
#
# This script creates a realistic FIA dataset modeled on Rhode Island inventory
# data. All values follow FIA Database (FIADB) v9.2 specifications and use
# real STATECD (44 = Rhode Island), COUNTYCD values, and FIA numeric species
# codes (SPCD) from the REF_SPECIES table.
#
# The dataset includes:
# - 12 plots across 3 Rhode Island counties
# - 4 subplots per plot (standard FIA design)
# - Mix of live trees, standing dead, and regeneration
# - 2 inventory cycles (2014 and 2019) for 6 plots to enable remeasurement
# - Species composition typical of southern New England mixed forest
#
# FIA reference: https://www.fia.fs.usda.gov/library/database-documentation/

set.seed(42)

# ---- Rhode Island geography ----
# STATECD = 44 (Rhode Island)
# Counties: Providence (7), Kent (3), Washington (9)
ri_counties <- data.frame(
  COUNTYCD = c(7, 3, 9),
  county_name = c("Providence", "Kent", "Washington"),
  stringsAsFactors = FALSE
)

# ---- Species pool (common southern New England species) ----
# SPCD from FIADB REF_SPECIES table
species_pool <- data.frame(
  SPCD = c(12, 68, 71, 97, 129, 241, 261, 316, 318, 531, 541, 621, 802, 833, 951),
  common = c("balsam fir", "eastern redcedar", "Atlantic white-cedar",
             "red spruce", "eastern white pine", "northern white-cedar",
             "eastern hemlock", "red maple", "sugar maple",
             "American beech", "white ash", "sweetgum",
             "white oak", "northern red oak", "American elm"),
  # Approximate range of DBH (inches) and HT (feet) in RI
  dbh_mean = c(8, 7, 6, 9, 14, 6, 12, 10, 11, 14, 13, 10, 11, 12, 10),
  dbh_sd   = c(4, 3, 3, 4, 7, 3, 6, 5, 5, 6, 6, 5, 5, 6, 5),
  ht_per_dbh = c(5.5, 4.5, 5.0, 5.8, 5.0, 5.2, 5.5, 5.2, 5.5, 4.8, 5.0, 5.0, 5.2, 5.0, 4.8),
  stringsAsFactors = FALSE
)

# ---- Plot generation ----
n_plots_per_cycle <- 12
plot_base <- data.frame(
  plot_num   = 1:n_plots_per_cycle,
  STATECD    = 44L,
  COUNTYCD   = rep(c(7L, 3L, 9L), each = 4),
  PLOT       = 1001:1012,
  LAT        = c(41.82, 41.85, 41.78, 41.81,   # Providence
                 41.68, 41.72, 41.70, 41.66,    # Kent
                 41.45, 41.50, 41.48, 41.52),   # Washington
  LON        = c(-71.42, -71.38, -71.45, -71.40,
                 -71.52, -71.48, -71.55, -71.50,
                 -71.62, -71.58, -71.65, -71.60),
  ELEV       = c(420, 380, 510, 350, 280, 340, 390, 260, 120, 180, 220, 160),
  ECOSUBCD   = rep(c("221Ab", "221Ac", "221Ad"), each = 4),
  stringsAsFactors = FALSE
)

# ---- Build PLOT table ----
# Cycle 1 (2019): all 12 plots
# Cycle 2 (2014): first 6 plots (remeasured)
plots_2019 <- plot_base
plots_2019$INVYR <- 2019L
plots_2019$CN <- 10001:10012
plots_2019$PREV_PLT_CN <- NA_integer_
plots_2019$DESIGNCD <- 1L
plots_2019$MEESSION <- 2019L

plots_2014 <- plot_base[1:6, ]
plots_2014$INVYR <- 2014L
plots_2014$CN <- 10101:10106
plots_2014$PREV_PLT_CN <- NA_integer_
plots_2014$DESIGNCD <- 1L
plots_2014$MEESSION <- 2014L

# Link 2019 plots to 2014 predecessors
plots_2019$PREV_PLT_CN[1:6] <- 10101:10106

PLOT <- rbind(plots_2019, plots_2014)
PLOT$MEESSION <- NULL  # cleanup

# ---- Build COND table ----
make_cond <- function(plt_cn, fortypcd, stdage, sicond, siteclcd, slope, aspect,
                      balive, owngrpcd, dstrbcd1 = 0L) {
  data.frame(
    CN = plt_cn + 20000L,
    PLT_CN = plt_cn,
    CONDID = 1L,
    CONDPROP_UNADJ = 1.0,
    FORTYPCD = fortypcd,
    STDAGE = stdage,
    SICOND = sicond,
    SITECLCD = siteclcd,
    SLOPE = slope,
    ASPECT = aspect,
    STDSZCD = 2L,
    BALIVE = balive,
    DSTRBCD1 = dstrbcd1,
    OWNGRPCD = owngrpcd,
    PHYSCLCD = 21L,
    SDI = as.integer(round(balive * 1.8)),
    stringsAsFactors = FALSE
  )
}

# Forest type codes (FORTYPCD):
# 102 = red spruce/balsam fir
# 108 = red pine
# 501 = white oak/red oak/hickory
# 503 = white oak
# 505 = northern red oak
# 509 = red maple/oak
# 520 = mixed hardwoods
# 701 = eastern white pine

cond_list <- list(
  # 2019 plots
  make_cond(10001, 505L, 78L, 62, 3L, 15L, 225L, 125.4, 40L),
  make_cond(10002, 509L, 65L, 55, 3L, 8L,  180L, 98.2, 40L),
  make_cond(10003, 701L, 85L, 68, 2L, 22L, 270L, 142.8, 40L),
  make_cond(10004, 505L, 72L, 58, 3L, 12L, 135L, 115.6, 46L),
  make_cond(10005, 520L, 60L, 52, 3L, 18L, 200L, 88.4, 40L),
  make_cond(10006, 509L, 55L, 48, 4L, 5L,  90L,  72.6, 40L),
  make_cond(10007, 505L, 90L, 65, 2L, 25L, 315L, 155.2, 46L),
  make_cond(10008, 701L, 70L, 60, 3L, 10L, 160L, 108.8, 40L),
  make_cond(10009, 520L, 50L, 45, 4L, 3L,  0L,   68.2, 40L),
  make_cond(10010, 509L, 75L, 58, 3L, 14L, 250L, 118.4, 46L),
  make_cond(10011, 505L, 82L, 62, 3L, 20L, 290L, 135.8, 40L),
  make_cond(10012, 520L, 58L, 50, 3L, 8L,  170L, 82.4, 40L),
  # 2014 plots (same plots, 5 years earlier)
  make_cond(10101, 505L, 73L, 62, 3L, 15L, 225L, 112.2, 40L),
  make_cond(10102, 509L, 60L, 55, 3L, 8L,  180L, 86.8, 40L),
  make_cond(10103, 701L, 80L, 68, 2L, 22L, 270L, 130.4, 40L),
  make_cond(10104, 505L, 67L, 58, 3L, 12L, 135L, 104.2, 46L),
  make_cond(10105, 520L, 55L, 52, 3L, 18L, 200L, 78.0, 40L),
  make_cond(10106, 509L, 50L, 48, 4L, 5L,  90L,  64.4, 40L)
)
COND <- do.call(rbind, cond_list)

# ---- Build TREE table ----
# Generate trees for each plot-subplot combination
# FIA design: 4 subplots per plot
# Subplot radius: 24 ft (trees >= 5.0" DBH)
# Microplot radius: 6.8 ft (saplings 1.0-4.9" DBH)

tree_cn <- 100000L
tree_rows <- list()

generate_trees_for_plot <- function(plt_cn, invyr, species_weights, n_trees_per_subplot,
                                    prev_tree_cns = NULL) {
  rows <- list()
  tree_counter <- 0L

  for (subp in 1:4) {
    n_trees <- n_trees_per_subplot[subp]
    for (j in seq_len(n_trees)) {
      tree_counter <- tree_counter + 1L
      tree_cn <<- tree_cn + 1L

      # Pick species
      spp_idx <- sample(nrow(species_pool), 1, prob = species_weights)
      spp <- species_pool[spp_idx, ]

      # Generate DBH
      dbh <- max(1.0, round(rnorm(1, spp$dbh_mean, spp$dbh_sd), 1))

      # Generate height based on DBH
      ht <- max(8, round(dbh * spp$ht_per_dbh + rnorm(1, 0, 5)))

      # Crown ratio (higher for smaller trees, lower for bigger)
      cr <- min(95, max(5, round(65 - dbh * 1.5 + rnorm(1, 0, 10))))

      # Status: mostly live, ~5% dead
      statuscd <- if (runif(1) < 0.05) 2L else 1L

      # TPA_UNADJ: depends on whether subplot or microplot tree
      if (dbh >= 5.0) {
        tpa_unadj <- 6.018046  # 1 / (pi * 24^2 / 43560 * 4)
      } else {
        tpa_unadj <- 74.965282  # 1 / (pi * 6.8^2 / 43560 * 4)
      }

      # Compute volumes (simplified cubic foot / board foot)
      ba_tree <- dbh^2 * 0.005454154
      volcfnet <- round(ba_tree * ht * 0.35, 1)  # rough cf volume
      volcfgrs <- round(volcfnet * 1.12, 1)
      volbfnet <- if (dbh >= 9.0) round(volcfnet * 5.5, 0) else 0
      drybio <- round(volcfgrs * 28, 0)
      carbon <- round(drybio * 0.5, 0)

      prev_cn <- NA_integer_
      if (!is.null(prev_tree_cns) && tree_counter <= length(prev_tree_cns)) {
        prev_cn <- prev_tree_cns[tree_counter]
      }

      rows[[length(rows) + 1]] <- data.frame(
        CN = tree_cn,
        PLT_CN = plt_cn,
        CONDID = 1L,
        SUBP = subp,
        TREE = j,
        STATUSCD = statuscd,
        SPCD = spp$SPCD,
        DIA = dbh,
        HT = ht,
        ACTUALHT = if (statuscd == 1L) ht else NA_integer_,
        CR = cr,
        TPA_UNADJ = tpa_unadj,
        DRYBIO_AG = drybio,
        VOLCFNET = volcfnet,
        VOLCFGRS = volcfgrs,
        VOLBFNET = volbfnet,
        CARBON_AG = carbon,
        PREV_TRE_CN = prev_cn,
        DSTRBCD1 = 0L,
        DAMAGE_AGENT_CD1 = 0L,
        DECAYCD = if (statuscd == 2L) sample(1:5, 1) else NA_integer_,
        CCLCD = if (statuscd == 1L) sample(c(1,2,3,4), 1, prob = c(0.15, 0.35, 0.35, 0.15)) else NA_integer_,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}

# Species composition weights by forest type
oak_weights   <- c(0.02, 0.02, 0.01, 0.02, 0.08, 0.02, 0.08, 0.25, 0.10, 0.12, 0.15, 0.01, 0.05, 0.05, 0.02)
pine_weights  <- c(0.05, 0.03, 0.01, 0.03, 0.30, 0.02, 0.10, 0.15, 0.05, 0.05, 0.08, 0.01, 0.05, 0.05, 0.02)
mixed_weights <- c(0.03, 0.03, 0.01, 0.03, 0.12, 0.03, 0.08, 0.22, 0.08, 0.08, 0.12, 0.02, 0.06, 0.06, 0.03)

# Trees per subplot (5-8 per subplot is typical)
set.seed(42)

# Generate 2014 trees first (for the 6 remeasured plots)
tree_cns_by_plot_2014 <- list()

for (i in 1:6) {
  plt_cn <- 10100L + i
  fortypcd <- COND$FORTYPCD[COND$PLT_CN == plt_cn]
  wts <- if (fortypcd %in% c(505, 509, 501)) oak_weights
         else if (fortypcd == 701) pine_weights
         else mixed_weights
  n_per_sub <- sample(5:8, 4, replace = TRUE)
  trees <- generate_trees_for_plot(plt_cn, 2014L, wts, n_per_sub)
  tree_rows[[length(tree_rows) + 1]] <- trees
  tree_cns_by_plot_2014[[i]] <- trees$CN
}

# Generate 2019 trees
for (i in 1:12) {
  plt_cn <- 10000L + i
  fortypcd <- COND$FORTYPCD[COND$PLT_CN == plt_cn]
  wts <- if (fortypcd %in% c(505, 509, 501)) oak_weights
         else if (fortypcd == 701) pine_weights
         else mixed_weights
  n_per_sub <- sample(5:8, 4, replace = TRUE)

  # Link to 2014 trees for remeasured plots
  prev_cns <- if (i <= 6) tree_cns_by_plot_2014[[i]] else NULL

  trees <- generate_trees_for_plot(plt_cn, 2019L, wts, n_per_sub, prev_cns)

  # For remeasured trees, add realistic growth
  if (i <= 6 && !is.null(prev_cns)) {
    n_match <- min(nrow(trees), length(prev_cns))
    for (j in seq_len(n_match)) {
      prev_row <- tree_rows[[i]]  # 2014 data
      if (j <= nrow(prev_row)) {
        # Add 5 years of growth
        dbh_growth <- round(runif(1, 0.4, 1.5), 1)  # 0.08-0.30 in/yr
        trees$DIA[j] <- round(prev_row$DIA[j] + dbh_growth, 1)
        ht_growth <- round(dbh_growth * 3.5 + rnorm(1, 0, 2))
        trees$HT[j] <- prev_row$HT[j] + max(0L, ht_growth)
        trees$ACTUALHT[j] <- trees$HT[j]
        # Recalculate volumes
        ba <- trees$DIA[j]^2 * 0.005454154
        trees$VOLCFNET[j] <- round(ba * trees$HT[j] * 0.35, 1)
        trees$VOLCFGRS[j] <- round(trees$VOLCFNET[j] * 1.12, 1)
        trees$VOLBFNET[j] <- if (trees$DIA[j] >= 9.0) round(trees$VOLCFNET[j] * 5.5, 0) else 0
        trees$DRYBIO_AG[j] <- round(trees$VOLCFGRS[j] * 28, 0)
        trees$CARBON_AG[j] <- round(trees$DRYBIO_AG[j] * 0.5, 0)
        # Update TPA based on final DBH
        if (trees$DIA[j] >= 5.0) {
          trees$TPA_UNADJ[j] <- 6.018046
        } else {
          trees$TPA_UNADJ[j] <- 74.965282
        }
      }
    }
  }

  tree_rows[[length(tree_rows) + 1]] <- trees
}

TREE <- do.call(rbind, tree_rows)

# ---- Assemble and save ----
fia_ri <- list(
  TREE = TREE,
  PLOT = PLOT,
  COND = COND
)

# Save as package data
usethis::use_data(fia_ri, overwrite = TRUE)

cat("Dataset created:\n")
cat("  TREE:", nrow(TREE), "rows\n")
cat("  PLOT:", nrow(PLOT), "rows\n")
cat("  COND:", nrow(COND), "rows\n")
cat("  Plots: 12 (2019) + 6 (2014) = 18 plot-cycles\n")
cat("  Remeasured plots: 6\n")
