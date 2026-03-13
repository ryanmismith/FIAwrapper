#' Find FIA Plots Within an Area of Interest
#'
#' Selects all FIA plot locations that fall within a given shapefile,
#' sf object, or bounding box. Uses Python geopandas/shapely for spatial
#' operations (auto-configured on first use).
#'
#' @param aoi One of:
#'   - File path to shapefile, GeoJSON, or GeoPackage
#'   - An `sf` object
#'   - A named numeric vector `c(xmin=, ymin=, xmax=, ymax=)` with a `crs` attribute
#' @param fia_data Either a list of FIA data frames (from [fetch_fia_data()])
#'   or a character vector of state abbreviations to auto-fetch.
#' @param buffer_ft Buffer distance in feet around the AOI. Default 0.
#' @param data_dir Directory for cached data (used when `fia_data` is state names).
#'
#' @return A list of FIA data frames filtered to only plots within the AOI.
#'   Same structure as [fetch_fia_data()] output.
#'
#' @examples
#' \dontrun{
#' # From a shapefile
#' fia_local <- get_plots_in_area("my_property.shp", fia_data = c("ME", "NH"))
#'
#' # From an sf object
#' library(sf)
#' bbox <- st_as_sf(st_as_sfc(st_bbox(c(xmin=-70, ymin=44, xmax=-69, ymax=45),
#'                  crs = 4326)))
#' fia_local <- get_plots_in_area(bbox, fia_data = fia)
#' }
#'
#' @seealso [plot_locations()], [map_plots()], [fetch_fia_data()]
#' @export
get_plots_in_area <- function(aoi, fia_data, buffer_ft = 0, data_dir = NULL) {

  # If fia_data is state abbreviations, fetch first

  if (is.character(fia_data) && all(nchar(fia_data) == 2)) {
    fia_data <- fetch_fia_data(fia_data, data_dir = data_dir)
  }

  if (!"PLOT" %in% names(fia_data)) {
    stop("fia_data must contain a PLOT table.", call. = FALSE)
  }

  # Get plot coordinates
  plots <- fia_data$PLOT
  if (!all(c("LAT", "LON") %in% names(plots))) {
    stop("PLOT table must contain LAT and LON columns.", call. = FALSE)
  }

  # Remove plots without coordinates
  plots <- plots[!is.na(plots$LAT) & !is.na(plots$LON), ]

  message("Note: FIA coordinates are 'fuzzed' (~0.5-1 mile offset) for privacy.")

  # Try sf-based spatial filter first (pure R, no Python needed)
  if (requireNamespace("sf", quietly = TRUE)) {
    result <- .spatial_filter_sf(aoi, plots, fia_data, buffer_ft)
  } else {
    # Fall back to Python
    result <- .spatial_filter_python(aoi, plots, fia_data, buffer_ft)
  }

  n_total <- nrow(fia_data$PLOT)
  n_selected <- nrow(result$PLOT)
  message("Selected ", n_selected, " of ", n_total, " plots within AOI.")

  result
}

#' Extract FIA Plot Locations as sf Object
#'
#' Creates an sf POINT data frame from FIA plot coordinates.
#'
#' @param fia_data List of FIA data frames (must contain PLOT table with
#'   LAT and LON columns).
#' @param crs Coordinate reference system. Default 4326 (WGS84).
#'
#' @return An sf data frame with POINT geometry and plot attributes.
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' plots_sf <- plot_locations(fia)
#' plot(plots_sf["STATECD"])
#' }
#'
#' @seealso [get_plots_in_area()], [map_plots()]
#' @export
plot_locations <- function(fia_data, crs = 4326) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required. Install with: install.packages('sf')", call. = FALSE)
  }

  plots <- if (is.data.frame(fia_data)) fia_data else fia_data$PLOT
  if (is.null(plots)) stop("No PLOT table found.", call. = FALSE)

  plots <- plots[!is.na(plots$LAT) & !is.na(plots$LON), ]

  message("Note: FIA coordinates are 'fuzzed' (~0.5-1 mile offset) for privacy.")

  sf::st_as_sf(plots, coords = c("LON", "LAT"), crs = crs)
}

#' Quick Map of FIA Plot Locations
#'
#' Creates an interactive or static map of FIA plot locations,
#' optionally overlaid on an area of interest boundary.
#'
#' @param fia_data List of FIA data frames or an sf object of plot locations.
#' @param aoi Optional AOI to overlay (file path, sf object, or bbox).
#'
#' @return A plot object (leaflet map if available, otherwise base R plot).
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' map_plots(fia)
#' map_plots(fia, aoi = "property_boundary.shp")
#' }
#'
#' @seealso [plot_locations()], [get_plots_in_area()]
#' @export
map_plots <- function(fia_data, aoi = NULL) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("sf package required for mapping.", call. = FALSE)
  }

  # Get plot sf object
  if (inherits(fia_data, "sf")) {
    plots_sf <- fia_data
  } else {
    plots_sf <- plot_locations(fia_data)
  }

  # Load AOI if provided
  aoi_sf <- NULL
  if (!is.null(aoi)) {
    aoi_sf <- .read_aoi(aoi)
  }

  # Try interactive map first
  if (requireNamespace("leaflet", quietly = TRUE)) {
    m <- leaflet::leaflet(plots_sf) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(radius = 3, color = "darkgreen",
                                 fillOpacity = 0.7,
                                 popup = ~paste("Plot:", CN))
    if (!is.null(aoi_sf)) {
      aoi_4326 <- sf::st_transform(aoi_sf, 4326)
      m <- m |> leaflet::addPolygons(data = aoi_4326,
                                      color = "red", weight = 2,
                                      fillOpacity = 0.1)
    }
    return(m)
  }

  # Fall back to base R plot
  plot(sf::st_geometry(plots_sf), pch = 16, cex = 0.5, col = "darkgreen",
       main = paste(nrow(plots_sf), "FIA Plots"))
  if (!is.null(aoi_sf)) {
    aoi_proj <- sf::st_transform(aoi_sf, sf::st_crs(plots_sf))
    plot(sf::st_geometry(aoi_proj), add = TRUE, border = "red", lwd = 2)
  }
  invisible(NULL)
}

# ---- Internal spatial helpers ----

#' Read AOI from various formats
#' @keywords internal
.read_aoi <- function(aoi) {
  if (inherits(aoi, "sf") || inherits(aoi, "sfc")) {
    return(aoi)
  }
  if (is.character(aoi) && length(aoi) == 1 && file.exists(aoi)) {
    return(sf::st_read(aoi, quiet = TRUE))
  }
  if (is.numeric(aoi) && length(aoi) == 4) {
    crs_val <- attr(aoi, "crs") %||% 4326
    bbox <- sf::st_bbox(c(xmin = aoi[1], ymin = aoi[2],
                           xmax = aoi[3], ymax = aoi[4]),
                         crs = sf::st_crs(crs_val))
    return(sf::st_as_sf(sf::st_as_sfc(bbox)))
  }
  stop("AOI must be a file path, sf object, or named numeric bbox.",
       call. = FALSE)
}

#' sf-based spatial filter
#' @keywords internal
.spatial_filter_sf <- function(aoi, plots, fia_data, buffer_ft) {
  aoi_sf <- .read_aoi(aoi)

  # Create sf from plot coords
  plots_sf <- sf::st_as_sf(plots, coords = c("LON", "LAT"), crs = 4326)

  # Reproject AOI to match plots
  aoi_sf <- sf::st_transform(aoi_sf, 4326)

  # Apply buffer if requested (convert feet to degrees approximately)
  if (buffer_ft > 0) {
    # Project to a meter-based CRS for accurate buffering
    aoi_proj <- sf::st_transform(aoi_sf, 5070)  # CONUS Albers
    buffer_m <- buffer_ft * 0.3048
    aoi_proj <- sf::st_buffer(aoi_proj, buffer_m)
    aoi_sf <- sf::st_transform(aoi_proj, 4326)
  }

  # Spatial intersection
  in_aoi <- sf::st_intersects(plots_sf, sf::st_union(aoi_sf), sparse = FALSE)[, 1]
  selected_cns <- plots$CN[in_aoi]

  # Filter all tables
  .filter_fia_by_plot_cn(fia_data, selected_cns)
}

#' Python-based spatial filter (fallback)
#' @keywords internal
.spatial_filter_python <- function(aoi, plots, fia_data, buffer_ft) {
  ensure_python_env()
  gpd <- reticulate::import("geopandas")
  shapely <- reticulate::import("shapely")

  # Read AOI
  if (is.character(aoi) && file.exists(aoi)) {
    aoi_gdf <- gpd$read_file(aoi)
  } else {
    stop("Python spatial filter requires a file path for AOI.", call. = FALSE)
  }

  # Create GeoDataFrame from plots
  py_plots <- reticulate::r_to_py(plots[, c("CN", "LAT", "LON")])
  geometry <- lapply(seq_len(nrow(plots)), function(i) {
    shapely$geometry$Point(plots$LON[i], plots$LAT[i])
  })
  plots_gdf <- gpd$GeoDataFrame(py_plots, geometry = geometry,
                                  crs = "EPSG:4326")

  # Reproject
  aoi_gdf <- aoi_gdf$to_crs(epsg = 4326L)

  if (buffer_ft > 0) {
    aoi_proj <- aoi_gdf$to_crs(epsg = 5070L)
    aoi_proj$geometry <- aoi_proj$buffer(buffer_ft * 0.3048)
    aoi_gdf <- aoi_proj$to_crs(epsg = 4326L)
  }

  # Spatial join
  result <- gpd$sjoin(plots_gdf, aoi_gdf, how = "inner", predicate = "within")
  selected_cns <- reticulate::py_to_r(result$CN)

  .filter_fia_by_plot_cn(fia_data, selected_cns)
}

#' Filter all FIA tables to selected plot CNs
#' @keywords internal
.filter_fia_by_plot_cn <- function(fia_data, plot_cns) {
  result <- list()
  for (nm in names(fia_data)) {
    tbl <- fia_data[[nm]]
    if ("CN" %in% names(tbl) && nm == "PLOT") {
      result[[nm]] <- tbl[tbl$CN %in% plot_cns, ]
    } else if ("PLT_CN" %in% names(tbl)) {
      result[[nm]] <- tbl[tbl$PLT_CN %in% plot_cns, ]
    } else {
      result[[nm]] <- tbl
    }
  }
  result
}

# ================================================================
# Spatial API Functions (terrain, climate, soils)
# ================================================================

#' Get Terrain Data for Plot Locations
#'
#' Fetches high-resolution elevation from the USGS 3D Elevation Program
#' (3DEP) via the Elevation Point Query Service. Supplements FIA's coarse
#' terrain data with DEM-derived values.
#'
#' @param plots_sf An sf POINT object (from [plot_locations()]).
#' @param source Data source: "USGS" (default, 3DEP point query).
#'
#' @return The input sf object with `dem_elevation` (feet) column added.
#'
#' @examples
#' \dontrun{
#' plots_sf <- plot_locations(fia)
#' plots_sf <- get_terrain_data(plots_sf)
#' }
#'
#' @seealso [get_climate_data()], [get_soils_data()]
#' @export
get_terrain_data <- function(plots_sf, source = "USGS") {
  if (!inherits(plots_sf, "sf")) {
    stop("plots_sf must be an sf object. Use plot_locations() first.", call. = FALSE)
  }

  coords <- sf::st_coordinates(sf::st_transform(plots_sf, 4326))
  n <- nrow(coords)
  message("Fetching terrain data for ", n, " locations...")

  elevations <- numeric(n)
  base_url <- "https://epqs.nationalmap.gov/v1/json"

  for (i in seq_len(n)) {
    tryCatch({
      url <- paste0(base_url, "?x=", coords[i, 1], "&y=", coords[i, 2],
                     "&wkid=4326&units=Feet&includeDate=false")
      resp <- httr2::request(url) |> httr2::req_timeout(15) |> httr2::req_perform()
      json <- httr2::resp_body_json(resp)
      elevations[i] <- as.numeric(json$value)
    }, error = function(e) {
      elevations[i] <<- NA_real_
    })
  }

  plots_sf$dem_elevation <- elevations
  message("Terrain data complete. ", sum(!is.na(elevations)), " of ", n, " succeeded.")
  plots_sf
}

#' Get Climate Data for Plot Locations
#'
#' Fetches annual climate data from Daymet (ORNL) for plot locations.
#' Returns temperature and precipitation for growth modeling.
#'
#' @param plots_sf An sf POINT object.
#' @param years Integer vector of years. Default 2020.
#' @param source "daymet" (default).
#'
#' @return The sf object with `mean_annual_temp` (F) and `annual_precip` (in).
#'
#' @examples
#' \dontrun{
#' plots_sf <- get_climate_data(plots_sf, years = 2019)
#' }
#'
#' @seealso [get_terrain_data()], [get_soils_data()]
#' @export
get_climate_data <- function(plots_sf, years = 2020, source = "daymet") {
  if (!inherits(plots_sf, "sf")) {
    stop("plots_sf must be an sf object.", call. = FALSE)
  }

  coords <- sf::st_coordinates(sf::st_transform(plots_sf, 4326))
  n <- nrow(coords)
  yr <- years[1]
  message("Fetching climate data for ", n, " locations (year ", yr, ")...")

  plots_sf$mean_annual_temp <- NA_real_
  plots_sf$annual_precip    <- NA_real_

  for (i in seq_len(n)) {
    tryCatch({
      url <- paste0(
        "https://daymet.ornl.gov/single-pixel/api/data?",
        "lat=", coords[i, 2], "&lon=", coords[i, 1],
        "&vars=tmax,tmin,prcp&years=", yr
      )
      resp <- httr2::request(url) |>
        httr2::req_timeout(30) |>
        httr2::req_perform()
      body <- httr2::resp_body_string(resp)
      lines <- strsplit(body, "\n")[[1]]
      data_lines <- lines[!grepl("^#|^year", lines, ignore.case = TRUE) & nchar(lines) > 0]
      if (length(data_lines) > 0) {
        hdr_line <- lines[grepl("^year", lines, ignore.case = TRUE)]
        full_text <- paste(c(hdr_line, data_lines), collapse = "\n")
        day_data <- tryCatch(
          utils::read.csv(textConnection(full_text)),
          error = function(e) NULL
        )
        if (!is.null(day_data) && nrow(day_data) > 0) {
          tmax_col <- grep("tmax", names(day_data), value = TRUE)[1]
          tmin_col <- grep("tmin", names(day_data), value = TRUE)[1]
          prcp_col <- grep("prcp", names(day_data), value = TRUE)[1]
          if (!is.na(tmax_col) && !is.na(tmin_col)) {
            mean_temp_c <- mean((day_data[[tmax_col]] + day_data[[tmin_col]]) / 2, na.rm = TRUE)
            plots_sf$mean_annual_temp[i] <- mean_temp_c * 9/5 + 32
          }
          if (!is.na(prcp_col)) {
            plots_sf$annual_precip[i] <- sum(day_data[[prcp_col]], na.rm = TRUE) / 25.4
          }
        }
      }
    }, error = function(e) {
      # Skip failed requests
    })
  }

  message("Climate data complete.")
  plots_sf
}

#' Get Soils Data for Plot Locations
#'
#' Queries SSURGO soil data via the USDA Soil Data Access (SDA) API.
#' Returns soil properties relevant to forest site productivity.
#'
#' @param plots_sf An sf POINT object.
#'
#' @return The sf object with soil columns: `drainage_class`, `awc`,
#'   `soil_depth`, `soil_texture`.
#'
#' @examples
#' \dontrun{
#' plots_sf <- get_soils_data(plots_sf)
#' }
#'
#' @seealso [get_terrain_data()], [get_climate_data()]
#' @export
get_soils_data <- function(plots_sf) {
  if (!inherits(plots_sf, "sf")) {
    stop("plots_sf must be an sf object.", call. = FALSE)
  }

  coords <- sf::st_coordinates(sf::st_transform(plots_sf, 4326))
  n <- nrow(coords)
  message("Fetching soils data for ", n, " locations...")

  plots_sf$soil_texture   <- NA_character_
  plots_sf$drainage_class <- NA_character_
  plots_sf$awc            <- NA_real_
  plots_sf$soil_depth     <- NA_real_

  sda_url <- "https://sdmdataaccess.sc.egov.usda.gov/Tabular/post.rest"

  for (i in seq_len(n)) {
    tryCatch({
      lon <- coords[i, 1]
      lat <- coords[i, 2]

      sql <- paste0(
        "SELECT TOP 1 co.drainagecl, ch.texdesc, ch.awc_r, ch.hzdepb_r ",
        "FROM mapunit AS mu ",
        "INNER JOIN component AS co ON mu.mukey = co.mukey ",
        "INNER JOIN chorizon AS ch ON co.cokey = ch.cokey ",
        "WHERE mu.mukey IN (",
        "SELECT * FROM SDA_Get_Mukey_from_intersection_with_WktWgs84('",
        "POINT(", lon, " ", lat, ")')",
        ") ORDER BY co.comppct_r DESC, ch.hzdept_r ASC"
      )

      resp <- httr2::request(sda_url) |>
        httr2::req_body_json(list(query = sql, format = "JSON")) |>
        httr2::req_timeout(30) |>
        httr2::req_perform()

      result <- httr2::resp_body_json(resp)
      if (!is.null(result$Table) && length(result$Table) > 0) {
        row <- result$Table[[1]]
        plots_sf$soil_texture[i]   <- as.character(row$texdesc %||% NA)
        plots_sf$drainage_class[i] <- as.character(row$drainagecl %||% NA)
        plots_sf$awc[i]            <- as.numeric(row$awc_r %||% NA)
        plots_sf$soil_depth[i]     <- as.numeric(row$hzdepb_r %||% NA)
      }
    }, error = function(e) {
      # Skip failed queries
    })
  }

  message("Soils data complete.")
  plots_sf
}
