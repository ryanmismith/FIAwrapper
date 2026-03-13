#' Build a Clean Tree List from FIA Data
#'
#' The main workhorse function. Takes raw FIA data (TREE + PLOT + COND tables)
#' and produces a clean, imperial-unit tree list with per-acre expansion
#' factors, forestry-friendly column names, and species common names.
#'
#' @param fia_data A named list of FIA data frames containing at minimum a
#'   TREE table (and optionally PLOT, COND). As returned by [fetch_fia_data()]
#'   or [get_plots_in_area()].
#' @param include_dead Logical. If FALSE (default), returns only live trees.
#'   If TRUE, includes standing dead trees with a `status` column.
#' @param variant FVS variant code for species translation. If NULL, uses
#'   the session variant from [set_fvs_variant()].
#' @param most_recent Logical. If TRUE (default), keeps only the most recent
#'   inventory year per plot. Set FALSE to get all measurement years.
#' @param compute_expansion Logical. If TRUE (default), calculates TPA from
#'   the FIA nested plot design. If FALSE, uses TPA_UNADJ from the database.
#'
#' @return A tibble with one row per tree, columns:
#' \describe{
#'   \item{plot_id}{Unique plot identifier (state_code + county_code + plot).}
#'   \item{plot_cn}{FIA plot control number (CN).}
#'   \item{subplot}{Subplot number (1-4).}
#'   \item{tree_num}{Tree number within subplot.}
#'   \item{inventory_year}{Year of measurement.}
#'   \item{species}{Common name (e.g., "northern red oak").}
#'   \item{spp_code}{FVS alpha species code (e.g., "RO").}
#'   \item{spcd}{FIA numeric species code.}
#'   \item{dbh}{Diameter at breast height in inches.}
#'   \item{height}{Total height in feet.}
#'   \item{crown_ratio}{Crown ratio percentage.}
#'   \item{tpa}{Trees per acre this tree represents.}
#'   \item{ba_tree}{Basal area of this tree in square feet.}
#'   \item{ba_per_acre}{BA contribution per acre (ba_tree * tpa) in ft^2/ac.}
#'   \item{status}{"live" or "dead".}
#'   \item{state_code, county_code, lat, lon}{Location info.}
#'   \item{site_index, slope, aspect, elevation, forest_type_code}{Site data.}
#' }
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' trees <- build_tree_list(fia)
#' head(trees)
#'
#' # Include dead trees
#' all_trees <- build_tree_list(fia, include_dead = TRUE)
#' table(all_trees$status)
#' }
#'
#' @seealso [get_tree_list()], [prep_fia_data()], [compute_tpa()]
#' @export
build_tree_list <- function(fia_data,
                            include_dead = FALSE,
                            variant = NULL,
                            most_recent = TRUE,
                            compute_expansion = TRUE) {

  # Extract tables
  trees <- fia_data$TREE
  plots <- fia_data$PLOT
  cond  <- fia_data$COND

  if (is.null(trees)) {
    stop("fia_data must contain a TREE table.", call. = FALSE)
  }

  # Prep data (joins tables, renames columns, keeps imperial)
  tl <- prep_fia_data(
    fia_trees = trees,
    fia_plots = plots,
    fia_cond  = cond,
    include_dead = include_dead,
    variant = variant
  )

  # Filter to most recent inventory year per plot
  if (most_recent && "inventory_year" %in% names(tl)) {
    tl <- tl |>
      dplyr::group_by(.data$plot_cn) |>
      dplyr::filter(.data$inventory_year == max(.data$inventory_year, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  # Compute TPA
  if (compute_expansion && "subplot" %in% names(tl) && "dbh" %in% names(tl)) {
    # Group by plot to compute TPA per plot
    tl <- tl |>
      dplyr::group_by(.data$plot_cn) |>
      dplyr::mutate(
        tpa = compute_tpa(
          dbh = .data$dbh,
          subplot = .data$subplot,
          cond_prop = if ("cond_proportion" %in% names(tl)) .data$cond_proportion else NULL
        )
      ) |>
      dplyr::ungroup()
  } else if ("tpa_unadj" %in% names(tl)) {
    tl$tpa <- tl$tpa_unadj
  }

  # Compute tree-level BA and per-acre BA
  if ("dbh" %in% names(tl)) {
    tl$ba_tree <- ba_ft2(tl$dbh)
    if ("tpa" %in% names(tl)) {
      tl$ba_per_acre <- tl$ba_tree * tl$tpa
    }
  }

  # Build readable plot ID
  if (all(c("state_code", "county_code") %in% names(tl)) && "plot_cn" %in% names(tl)) {
    tl$plot_id <- paste(tl$state_code, tl$county_code, sep = "_")
    # Add plot number if available
    if ("plot_id" %in% names(fia_data$PLOT) || "PLOT" %in% names(fia_data$PLOT)) {
      # Use plot_cn as unique ID
      tl$plot_id <- paste0(tl$state_code, "_", tl$county_code, "_", tl$plot_cn)
    }
  } else {
    tl$plot_id <- as.character(tl$plot_cn)
  }

  # Select and order output columns
  col_order <- c(
    "plot_id", "plot_cn", "subplot", "tree_num", "tree_cn", "inventory_year",
    "species", "spp_code", "spcd",
    "dbh", "height", "crown_ratio", "crown_class",
    "tpa", "ba_tree", "ba_per_acre",
    "status", "status_code", "decay_class",
    "damage_code1", "damage_code2", "damage_code3",
    "vol_cf_net", "vol_cf_gross", "vol_bf_net", "vol_bf_gross",
    "biomass_ag_dry", "carbon_ag",
    "state_code", "county_code", "lat", "lon",
    "elevation", "slope", "aspect",
    "site_index", "site_class", "stand_age",
    "forest_type_code", "forest_type_group_code",
    "ownership_code", "stand_density_index",
    "cond_proportion", "condition_id",
    "prev_tree_cn", "cycle"
  )

  available_cols <- intersect(col_order, names(tl))
  extra_cols <- setdiff(names(tl), col_order)
  # Keep extra columns at the end
  tl <- tl[, c(available_cols, extra_cols), drop = FALSE]

  tibble::as_tibble(tl)
}

#' Get Tree List from States or AOI (All-in-One)
#'
#' Convenience function that fetches FIA data, optionally filters by spatial
#' area, and returns a clean tree list. One-line solution for most use cases.
#'
#' @param states Character vector of state abbreviations. Required if
#'   `aoi` is not provided.
#' @param aoi Area of interest (file path, sf object, or bbox). If provided,
#'   filters plots to this area.
#' @param data_dir Directory for cached FIA data downloads.
#' @param include_dead Logical. Include standing dead trees? Default FALSE.
#' @param variant FVS variant code for species names.
#' @param most_recent Logical. Only the latest inventory cycle? Default TRUE.
#'
#' @return A tibble tree list. See [build_tree_list()] for column details.
#'
#' @examples
#' \dontrun{
#' # From states
#' trees <- get_tree_list(states = "RI")
#'
#' # From shapefile (auto-detects states from AOI extent)
#' trees <- get_tree_list(aoi = "my_property.shp", states = "ME")
#'
#' # With dead trees
#' trees <- get_tree_list(states = "VT", include_dead = TRUE)
#' }
#'
#' @seealso [build_tree_list()], [fetch_fia_data()], [get_plots_in_area()]
#' @export
get_tree_list <- function(states = NULL,
                          aoi = NULL,
                          data_dir = NULL,
                          include_dead = FALSE,
                          variant = NULL,
                          most_recent = TRUE) {

  if (is.null(states) && is.null(aoi)) {
    stop("Provide at least one of 'states' or 'aoi'.", call. = FALSE)
  }

  # Fetch data
  if (!is.null(states)) {
    fia_data <- fetch_fia_data(states = states, data_dir = data_dir)
  } else {
    stop("When using aoi without states, provide state abbreviations to know ",
         "which states' data to download.", call. = FALSE)
  }

  # Spatial filter
  if (!is.null(aoi)) {
    fia_data <- get_plots_in_area(aoi = aoi, fia_data = fia_data)
  }

  # Build tree list
  build_tree_list(
    fia_data = fia_data,
    include_dead = include_dead,
    variant = variant,
    most_recent = most_recent
  )
}
