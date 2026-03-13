# ---- Helpers: synthetic FIA data ----

make_synthetic_fia <- function(n_trees = 6, include_dead = FALSE) {
  statuses <- rep(1L, n_trees)
  if (include_dead && n_trees >= 4) {
    statuses[4] <- 2L
  }

  tree_df <- data.frame(
    CN       = seq_len(n_trees),
    PLT_CN   = rep(1001, n_trees),
    SUBP     = rep(c(1L, 2L, 3L), length.out = n_trees),
    TREE     = seq_len(n_trees),
    CONDID   = rep(1L, n_trees),
    STATUSCD = statuses,
    DIA      = c(10.2, 6.5, 8.0, 12.4, 5.1, 14.0)[seq_len(n_trees)],
    HT       = c(55, 40, 48, 62, 35, 70)[seq_len(n_trees)],
    SPCD     = rep(c(12L, 97L, 261L), length.out = n_trees),
    CR       = c(45, 55, 50, 40, 60, 35)[seq_len(n_trees)],
    TPA_UNADJ = rep(6.018, n_trees),
    INVYR    = rep(2019L, n_trees),
    stringsAsFactors = FALSE
  )

  plot_df <- data.frame(
    CN       = 1001,
    INVYR    = 2019L,
    STATECD  = 44L,
    COUNTYCD = 7L,
    LAT      = 41.5,
    LON      = -71.5,
    ELEV     = 200L,
    PLOT     = 1L,
    stringsAsFactors = FALSE
  )

  cond_df <- data.frame(
    PLT_CN         = 1001,
    CONDID         = 1L,
    FORTYPCD       = 505L,
    STDAGE         = 65L,
    SICOND         = 60,
    SITECLCD       = 3L,
    SLOPE          = 15,
    ASPECT         = 180,
    CONDPROP_UNADJ = 1.0,
    OWNCD          = 46L,
    stringsAsFactors = FALSE
  )

  list(TREE = tree_df, PLOT = plot_df, COND = cond_df)
}


# ---- Tests for build_tree_list ----

test_that("build_tree_list returns a tibble with expected columns", {
  fia <- make_synthetic_fia()
  tl  <- build_tree_list(fia)

  expect_s3_class(tl, "tbl_df")
  expected_cols <- c("plot_cn", "dbh", "height", "tpa", "spp_code",
                     "status", "ba_tree", "ba_per_acre", "subplot")
  for (col in expected_cols) {
    expect_true(col %in% names(tl), info = paste("Missing column:", col))
  }
})

test_that("build_tree_list excludes dead trees by default", {
  fia <- make_synthetic_fia(n_trees = 6, include_dead = TRUE)
  tl  <- build_tree_list(fia, include_dead = FALSE)

  expect_true(all(tl$status == "live"))
})

test_that("build_tree_list includes dead trees when include_dead = TRUE", {
  fia <- make_synthetic_fia(n_trees = 6, include_dead = TRUE)
  tl  <- build_tree_list(fia, include_dead = TRUE)

  expect_true("dead" %in% tl$status)
  expect_true("live" %in% tl$status)
})

test_that("build_tree_list computes ba_tree correctly", {
  fia <- make_synthetic_fia(n_trees = 4)
  tl  <- build_tree_list(fia)

  # ba_tree should equal dbh^2 * 0.005454154
  expected_ba <- tl$dbh^2 * 0.005454154
  expect_equal(tl$ba_tree, expected_ba)
})

test_that("build_tree_list computes tpa when compute_expansion = TRUE", {
  fia <- make_synthetic_fia()
  tl  <- build_tree_list(fia, compute_expansion = TRUE)

  # All trees have dbh >= 5, so TPA should be subplot-based

  expect_true(all(!is.na(tl$tpa)))
  expect_true(all(tl$tpa > 0))
})

test_that("build_tree_list uses tpa_unadj when compute_expansion = FALSE", {
  fia <- make_synthetic_fia()
  tl  <- build_tree_list(fia, compute_expansion = FALSE)

  # Should fall back to tpa_unadj = 6.018
  expect_true(all(!is.na(tl$tpa)))
  expect_equal(unique(tl$tpa), 6.018)
})

test_that("build_tree_list errors without TREE table", {
  fia <- list(PLOT = data.frame(CN = 1), COND = data.frame(PLT_CN = 1))
  expect_error(build_tree_list(fia), "TREE table")
})

test_that("build_tree_list creates plot_id column", {
  fia <- make_synthetic_fia()
  tl  <- build_tree_list(fia)

  expect_true("plot_id" %in% names(tl))
  expect_true(all(!is.na(tl$plot_id)))
})

test_that("build_tree_list filters to most recent year by default", {
  fia <- make_synthetic_fia(n_trees = 4)
  # Add a second set of trees for an earlier inventory year
  old_trees <- fia$TREE
  old_trees$CN <- old_trees$CN + 100L
  old_trees$INVYR <- 2014L
  fia$TREE <- rbind(fia$TREE, old_trees)

  tl <- build_tree_list(fia, most_recent = TRUE)
  expect_true(all(tl$inventory_year == 2019L))
})
