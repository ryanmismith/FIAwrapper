test_that("set_fvs_variant sets the option correctly", {
  old_val <- getOption("FIAwrapper.variant")
  on.exit(options(FIAwrapper.variant = old_val), add = TRUE)

  expect_message(set_fvs_variant("NE"), "FVS variant set to: NE")
  expect_equal(getOption("FIAwrapper.variant"), "NE")
})

test_that("export_fvs_treelist writes CSV with correct columns and values", {
  old_val <- getOption("FIAwrapper.variant")
  on.exit(options(FIAwrapper.variant = old_val), add = TRUE)

  tree_list <- data.frame(
    plot_id      = c("S1", "S1", "S1"),
    spp_code     = c("RO", "BF", "WP"),
    dbh          = c(10.2, 8.5, 12.0),
    height       = c(65, 55, 70),
    crown_ratio  = c(40, 50, 35),
    tpa          = c(15.5, 20.3, 10.1),
    status       = c("live", "live", "live"),
    spcd         = c(833L, 12L, 129L),
    tree_num     = c(1L, 2L, 3L),
    stringsAsFactors = FALSE
  )

  outfile <- file.path(tempdir(), "test_fvs_export.csv")
  on.exit(unlink(outfile), add = TRUE)

  # Set variant so fia_to_fvs can work
  suppressMessages(set_fvs_variant("NE"))

  expect_message(
    export_fvs_treelist(tree_list, outfile, format = "csv"),
    "Exported"
  )

  result <- utils::read.csv(outfile, stringsAsFactors = FALSE)

  # Verify expected columns exist
  expect_true("stand_id" %in% names(result))
  expect_true("species" %in% names(result))
  expect_true("dbh" %in% names(result))
  expect_true("tpa" %in% names(result))

  # Verify values round-trip correctly
  expect_equal(nrow(result), 3)
  expect_equal(result$dbh, c(10.2, 8.5, 12.0))
  expect_equal(result$stand_id, c("S1", "S1", "S1"))
})
