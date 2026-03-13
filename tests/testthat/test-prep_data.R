# Tests for prep_data.R

# Helper: minimal TREE table
make_tree <- function(n = 4) {
  data.frame(
    PLT_CN   = rep(1001, n),
    SUBP     = seq_len(n),
    DIA      = c(10.2, 6.5, 8.0, 12.4)[seq_len(n)],
    HT       = c(55, 40, 48, 62)[seq_len(n)],
    STATUSCD = rep(1L, n),
    SPCD     = c(12L, 97L, 261L, 833L)[seq_len(n)],
    stringsAsFactors = FALSE
  )
}

# Helper: minimal PLOT table
make_plot <- function() {
  data.frame(
    CN       = 1001,
    INVYR    = 2019L,
    STATECD  = 23L,
    COUNTYCD = 5L,
    LAT      = 44.5,
    LON      = -68.2,
    stringsAsFactors = FALSE
  )
}

# Helper: minimal COND table
make_cond <- function() {
  data.frame(
    PLT_CN   = 1001,
    CONDID   = 1L,
    FORTYPCD = 101L,
    STDAGE   = 80L,
    SLOPE    = 15,
    ASPECT   = 180,
    BALIVE   = 120.0,
    stringsAsFactors = FALSE
  )
}

test_that("prep_fia_data errors when required columns are missing", {
  bad <- data.frame(PLT_CN = 1, SUBP = 1, stringsAsFactors = FALSE)
  expect_error(prep_fia_data(bad), "missing required columns")
})

test_that("prep_fia_data returns a tibble with core columns", {
  tree <- make_tree()
  out <- prep_fia_data(tree)

  expect_s3_class(out, "tbl_df")
  expect_true(all(c("plot_cn", "subplot", "dbh", "height",
                     "status_code", "status") %in% names(out)))
})

test_that("prep_fia_data renames DIA -> dbh, HT -> height", {
  tree <- make_tree(1)
  out <- prep_fia_data(tree)

  expect_equal(out$dbh[1], 10.2)
  expect_equal(out$height[1], 55)
})

test_that("prep_fia_data prefers ACTUALHT over HT", {
  tree <- make_tree(1)
  tree$ACTUALHT <- 99
  out <- prep_fia_data(tree)
  expect_equal(out$height[1], 99)
})

test_that("prep_fia_data filters dead trees by default", {
  tree <- make_tree(2)
  tree$STATUSCD <- c(1L, 2L)  # 1 = live, 2 = dead
  out <- prep_fia_data(tree)
  expect_equal(nrow(out), 1)
  expect_equal(out$status[1], "live")
})

test_that("prep_fia_data keeps dead trees when include_dead = TRUE", {
  tree <- make_tree(2)
  tree$STATUSCD <- c(1L, 2L)
  out <- prep_fia_data(tree, include_dead = TRUE)
  expect_equal(nrow(out), 2)
  expect_true("dead" %in% out$status)
})

test_that("prep_fia_data removes rows with NA or zero DBH", {
  tree <- make_tree(3)
  tree$DIA <- c(NA, 0, 8.0)
  out <- prep_fia_data(tree)
  expect_equal(nrow(out), 1)
  expect_equal(out$dbh[1], 8.0)
})

test_that("prep_fia_data joins PLOT table correctly", {
  tree <- make_tree(1)
  plt  <- make_plot()
  out  <- prep_fia_data(tree, fia_plots = plt)

  expect_true("state_code" %in% names(out))
  expect_equal(out$state_code[1], 23L)
  expect_true("lat" %in% names(out))
  expect_equal(out$lat[1], 44.5)
})

test_that("prep_fia_data joins COND table correctly", {
  tree <- make_tree(1)
  tree$CONDID <- 1L
  cnd <- make_cond()
  out <- prep_fia_data(tree, fia_cond = cnd)

  expect_true("stand_age" %in% names(out))
  expect_equal(out$stand_age[1], 80L)
  expect_true("slope" %in% names(out))
  expect_equal(out$slope[1], 15)
})

test_that("prep_fia_data errors if fia_plots lacks CN", {
  tree <- make_tree(1)
  bad_plot <- data.frame(INVYR = 2019L, stringsAsFactors = FALSE)
  expect_error(prep_fia_data(tree, fia_plots = bad_plot), "CN")
})

test_that("prep_fia_data errors if fia_cond lacks PLT_CN", {
  tree <- make_tree(1)
  bad_cond <- data.frame(CONDID = 1L, stringsAsFactors = FALSE)
  expect_error(prep_fia_data(tree, fia_cond = bad_cond), "PLT_CN")
})

test_that("prep_fia_data includes extra_tree_cols when present", {
  tree <- make_tree(1)
  tree$CUSTOM_COL <- "hello"
  out <- prep_fia_data(tree, extra_tree_cols = "CUSTOM_COL")
  expect_true("CUSTOM_COL" %in% names(out))
  expect_equal(out$CUSTOM_COL[1], "hello")
})

test_that("prep_fia_data maps volume columns", {
  tree <- make_tree(1)
  tree$VOLCFNET <- 25.5
  tree$VOLBFNET <- 150.0
  out <- prep_fia_data(tree)
  expect_true("vol_cf_net" %in% names(out))
  expect_equal(out$vol_cf_net[1], 25.5)
  expect_true("vol_bf_net" %in% names(out))
  expect_equal(out$vol_bf_net[1], 150.0)
})

test_that("prep_fia_data maps TPA_UNADJ", {
  tree <- make_tree(1)
  tree$TPA_UNADJ <- 6.018
  out <- prep_fia_data(tree)
  expect_true("tpa_unadj" %in% names(out))
  expect_equal(out$tpa_unadj[1], 6.018)
})
