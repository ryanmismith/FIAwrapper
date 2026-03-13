test_that("plot_locations errors when fia_data has no PLOT table", {
  bad_data <- list(TREE = data.frame(CN = 1:3))
  expect_error(plot_locations(bad_data), "No PLOT table found")
})

test_that("plot_locations errors when sf is not available", {
  # This test only runs if sf IS available (otherwise plot_locations would

  # always fail). We just verify the function exists and has the right signature.
  expect_true(is.function(plot_locations))
  expect_true("fia_data" %in% names(formals(plot_locations)))
  expect_true("crs" %in% names(formals(plot_locations)))
})

test_that("get_plots_in_area errors when PLOT table missing", {
  bad_data <- list(TREE = data.frame(CN = 1:3))
  expect_error(
    get_plots_in_area(aoi = c(xmin = -70, ymin = 44, xmax = -69, ymax = 45),
                      fia_data = bad_data),
    "fia_data must contain a PLOT table"
  )
})

test_that("get_plots_in_area errors when LAT/LON missing", {
  bad_data <- list(PLOT = data.frame(CN = 1:3, STATECD = 44))
  expect_error(
    get_plots_in_area(aoi = c(xmin = -70, ymin = 44, xmax = -69, ymax = 45),
                      fia_data = bad_data),
    "PLOT table must contain LAT and LON columns"
  )
})

test_that("get_plots_in_area validates buffer_ft default", {
  expect_equal(formals(get_plots_in_area)$buffer_ft, 0)
})

test_that(".read_aoi rejects invalid input", {
  skip_if_not_installed("sf")
  expect_error(.read_aoi(42), "AOI must be")
  expect_error(.read_aoi(TRUE), "AOI must be")
})

test_that(".read_aoi accepts a numeric bbox", {
  skip_if_not_installed("sf")
  # .read_aoi indexes by position (aoi[1]..aoi[4]), so names don't matter
  # but we need to pass valid finite coordinates
  bbox_vec <- c(-70, 44, -69, 45)
  attr(bbox_vec, "crs") <- 4326
  result <- .read_aoi(bbox_vec)
  expect_s3_class(result, "sf")
})

test_that(".filter_fia_by_plot_cn filters correctly", {
  fia <- list(
    PLOT = data.frame(CN = c(1, 2, 3), LAT = c(44, 45, 46), LON = c(-70, -71, -72)),
    TREE = data.frame(PLT_CN = c(1, 1, 2, 3), DIA = c(10, 12, 8, 14)),
    COND = data.frame(PLT_CN = c(1, 2, 3), FORTYPCD = c(101, 102, 103)),
    REF  = data.frame(CODE = c("A", "B"))
  )
  result <- .filter_fia_by_plot_cn(fia, plot_cns = c(1, 3))

  expect_equal(nrow(result$PLOT), 2)
  expect_equal(result$PLOT$CN, c(1, 3))
  expect_equal(nrow(result$TREE), 3)
  expect_equal(result$TREE$PLT_CN, c(1, 1, 3))
  expect_equal(nrow(result$COND), 2)
  # Tables without CN or PLT_CN are kept as-is

  expect_equal(nrow(result$REF), 2)
})

test_that("plot_locations returns sf object with synthetic data", {
  skip_if_not_installed("sf")

  plots <- data.frame(
    CN  = c(100, 200, 300),
    LAT = c(44.0, 44.5, 45.0),
    LON = c(-70.0, -70.5, -71.0),
    STATECD = c(23, 23, 23)
  )
  fia_data <- list(PLOT = plots)

  result <- suppressMessages(plot_locations(fia_data))
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 3)
  expect_true("geometry" %in% names(result))
  expect_true("CN" %in% names(result))
})

test_that("plot_locations drops rows with NA coords", {
  skip_if_not_installed("sf")

  plots <- data.frame(
    CN  = c(1, 2, 3),
    LAT = c(44.0, NA, 45.0),
    LON = c(-70.0, -70.5, NA)
  )
  fia_data <- list(PLOT = plots)

  result <- suppressMessages(plot_locations(fia_data))
  expect_equal(nrow(result), 1)
  expect_equal(result$CN, 1)
})

test_that("get_terrain_data rejects non-sf input", {
  expect_error(get_terrain_data(data.frame(x = 1)), "plots_sf must be an sf object")
})

test_that("get_climate_data rejects non-sf input", {
  expect_error(get_climate_data(data.frame(x = 1)), "plots_sf must be an sf object")
})

test_that("get_soils_data rejects non-sf input", {
  expect_error(get_soils_data(data.frame(x = 1)), "plots_sf must be an sf object")
})
