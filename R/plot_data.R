#' Summarize Tree List to Plot-Level Metrics
#'
#' Aggregates a tree list to one row per plot with standard forestry
#' metrics: TPA, basal area per acre, QMD, heights, and species composition.
#'
#' @param tree_list A tibble from [build_tree_list()] or any data frame
#'   with columns: plot_id (or plot_cn), dbh, tpa, and optionally height,
#'   spp_code.
#' @param include_dead Logical. Include dead trees in summaries? Default FALSE.
#'   Only relevant if tree_list contains dead trees.
#'
#' @return A tibble with one row per plot:
#' \describe{
#'   \item{plot_id}{Plot identifier.}
#'   \item{inventory_year}{Year of measurement.}
#'   \item{tpa}{Total trees per acre.}
#'   \item{ba_per_acre}{Total basal area per acre (ft^2/ac).}
#'   \item{qmd}{Quadratic mean diameter (inches).}
#'   \item{mean_dbh}{Arithmetic mean DBH (inches).}
#'   \item{top_height}{Mean height of tallest 40 TPA (feet).}
#'   \item{mean_height}{Mean height (feet).}
#'   \item{species_count}{Number of unique species on the plot.}
#'   \item{dominant_species}{Species with highest basal area.}
#'   \item{vol_per_acre}{Net cubic foot volume per acre (if available).}
#'   \item{state_code, county_code, lat, lon}{Plot location.}
#' }
#'
#' @examples
#' \dontrun{
#' trees <- build_tree_list(fia)
#' plots <- get_plot_summary(trees)
#' }
#'
#' @seealso [build_tree_list()], [get_site_data()], [get_species_summary()]
#' @export
get_plot_summary <- function(tree_list, include_dead = FALSE) {
  if (!include_dead && "status" %in% names(tree_list)) {
    tree_list <- tree_list[tree_list$status == "live", ]
  }

  # Determine grouping column
  grp_col <- if ("plot_id" %in% names(tree_list)) "plot_id" else "plot_cn"

  result <- tree_list |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_col))) |>
    dplyr::summarize(
      inventory_year  = if ("inventory_year" %in% names(tree_list))
                          dplyr::first(.data$inventory_year) else NA_integer_,
      qmd             = qmd(.data$dbh, .data$tpa),
      mean_dbh        = stats::weighted.mean(.data$dbh, .data$tpa, na.rm = TRUE),
      top_height      = .compute_top_height(.data$height, .data$tpa, .data$dbh),
      mean_height     = if ("height" %in% names(tree_list))
                          stats::weighted.mean(.data$height, .data$tpa, na.rm = TRUE)
                        else NA_real_,
      species_count   = if ("spp_code" %in% names(tree_list))
                          dplyr::n_distinct(.data$spp_code) else NA_integer_,
      dominant_species = if ("spp_code" %in% names(tree_list))
                           .get_dominant_spp(.data$spp_code, .data$ba_per_acre)
                         else NA_character_,
      vol_per_acre    = if ("vol_cf_net" %in% names(tree_list))
                          sum(.data$vol_cf_net * .data$tpa, na.rm = TRUE)
                        else NA_real_,
      n_trees         = dplyr::n(),
      tpa             = sum(.data$tpa, na.rm = TRUE),
      ba_per_acre     = sum(.data$ba_per_acre, na.rm = TRUE),
      .groups = "drop"
    )

  # Add location columns if available
  loc_cols <- c("state_code", "county_code", "lat", "lon")
  avail_loc <- intersect(loc_cols, names(tree_list))
  if (length(avail_loc) > 0) {
    loc_data <- tree_list |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_col))) |>
      dplyr::summarize(dplyr::across(dplyr::all_of(avail_loc), dplyr::first),
                        .groups = "drop")
    result <- dplyr::left_join(result, loc_data, by = grp_col)
  }

  result
}

#' Extract Site and Stand Data
#'
#' Pulls plot-level site attributes from FIA PLOT and COND tables.
#' Returns one row per plot with terrain, ownership, and stand
#' characteristics using forestry terminology.
#'
#' @param fia_data List of FIA data frames (from [fetch_fia_data()]).
#'
#' @return A tibble with one row per plot:
#' \describe{
#'   \item{plot_cn}{Plot control number.}
#'   \item{inventory_year}{Year of measurement.}
#'   \item{state_code, county_code}{FIPS codes.}
#'   \item{lat, lon}{Fuzzed coordinates.}
#'   \item{elevation}{Feet.}
#'   \item{slope}{Percent.}
#'   \item{aspect}{Degrees.}
#'   \item{forest_type_code}{FIA forest type code.}
#'   \item{stand_age}{Years.}
#'   \item{site_index}{Site index value.}
#'   \item{site_class}{Site productivity class.}
#'   \item{ownership_code}{Ownership class.}
#'   \item{stand_density_index}{SDI.}
#'   \item{disturbance_code1}{Primary disturbance.}
#' }
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' site <- get_site_data(fia)
#' }
#'
#' @seealso [get_plot_summary()], [build_tree_list()]
#' @export
get_site_data <- function(fia_data) {
  plots <- fia_data$PLOT
  cond  <- fia_data$COND

  if (is.null(plots)) stop("fia_data must contain a PLOT table.", call. = FALSE)

  # Start with plot-level data
  out <- data.frame(plot_cn = plots$CN, stringsAsFactors = FALSE)

  plot_map <- c(
    "INVYR" = "inventory_year", "STATECD" = "state_code",
    "COUNTYCD" = "county_code", "LAT" = "lat", "LON" = "lon",
    "ELEV" = "elevation", "SLOPE" = "slope", "ASPECT" = "aspect",
    "ECOSUBCD" = "ecological_subsection", "CYCLE" = "cycle"
  )
  for (fia_col in names(plot_map)) {
    if (fia_col %in% names(plots)) {
      out[[plot_map[fia_col]]] <- plots[[fia_col]]
    }
  }

  # Join condition data (use condition 1 = primary condition)
  if (!is.null(cond)) {
    # Take primary condition (CONDID == 1) or the one with largest proportion
    cond_primary <- cond |>
      dplyr::group_by(.data$PLT_CN) |>
      dplyr::arrange(dplyr::desc(
        if ("CONDPROP_UNADJ" %in% names(cond)) .data$CONDPROP_UNADJ else 1
      )) |>
      dplyr::slice(1) |>
      dplyr::ungroup()

    cond_map <- c(
      "FORTYPCD" = "forest_type_code", "FORTYPGRPCD" = "forest_type_group_code",
      "STDAGE" = "stand_age", "STDORGCD" = "stand_origin",
      "SICOND" = "site_index", "SISP" = "site_index_species",
      "SITECLCD" = "site_class", "PHYSCLCD" = "physiographic_class",
      "OWNCD" = "ownership_code", "OWNGRPCD" = "ownership_group",
      "DSTRBCD1" = "disturbance_code1", "TRTCD1" = "treatment_code1",
      "BALIVE" = "ba_live", "SDI" = "stand_density_index",
      "GSSTK" = "growing_stock_stocking", "ALSTK" = "all_stocking",
      "CONDPROP_UNADJ" = "cond_proportion"
    )

    cond_out <- data.frame(PLT_CN = cond_primary$PLT_CN, stringsAsFactors = FALSE)
    for (fia_col in names(cond_map)) {
      if (fia_col %in% names(cond_primary)) {
        cond_out[[cond_map[fia_col]]] <- cond_primary[[fia_col]]
      }
    }

    out <- merge(out, cond_out, by.x = "plot_cn", by.y = "PLT_CN",
                 all.x = TRUE, sort = FALSE)
  }

  tibble::as_tibble(out)
}

#' Summarize Tree List by Species
#'
#' Creates per-species summaries for each plot including TPA, BA/acre,
#' QMD, and percent of total basal area.
#'
#' @param tree_list A tibble from [build_tree_list()].
#'
#' @return A tibble with one row per species per plot.
#'
#' @examples
#' \dontrun{
#' trees <- build_tree_list(fia)
#' spp <- get_species_summary(trees)
#' }
#'
#' @seealso [get_plot_summary()], [build_tree_list()]
#' @export
get_species_summary <- function(tree_list) {
  grp_col <- if ("plot_id" %in% names(tree_list)) "plot_id" else "plot_cn"

  # Filter to live trees
  if ("status" %in% names(tree_list)) {
    tree_list <- tree_list[tree_list$status == "live", ]
  }

  # Plot-level totals for percent calculation
  plot_totals <- tree_list |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_col))) |>
    dplyr::summarize(total_ba = sum(.data$ba_per_acre, na.rm = TRUE),
                      .groups = "drop")

  spp_col <- if ("spp_code" %in% names(tree_list)) "spp_code" else "species"

  result <- tree_list |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(grp_col, spp_col)))) |>
    dplyr::summarize(
      species_name = if ("species" %in% names(tree_list)) dplyr::first(.data$species) else NA_character_,
      qmd          = qmd(.data$dbh, .data$tpa),
      mean_dbh     = stats::weighted.mean(.data$dbh, .data$tpa, na.rm = TRUE),
      n_trees      = dplyr::n(),
      tpa          = sum(.data$tpa, na.rm = TRUE),
      ba_per_acre  = sum(.data$ba_per_acre, na.rm = TRUE),
      .groups      = "drop"
    ) |>
    dplyr::left_join(plot_totals, by = grp_col) |>
    dplyr::mutate(
      pct_ba = round(100 * .data$ba_per_acre / .data$total_ba, 1)
    ) |>
    dplyr::select(-"total_ba") |>
    dplyr::arrange(dplyr::across(dplyr::all_of(grp_col)),
                    dplyr::desc(.data$ba_per_acre))

  result
}

# ---- Internal helpers ----

#' Compute top height (mean height of 40 largest TPA trees)
#' @keywords internal
.compute_top_height <- function(height, tpa, dbh) {
  if (all(is.na(height))) return(NA_real_)
  df <- data.frame(ht = height, tpa = tpa, dbh = dbh)
  df <- df[!is.na(df$ht), ]
  df <- df[order(-df$dbh), ]
  cum_tpa <- cumsum(df$tpa)
  # Top 40 TPA (dominant/codominant trees)
  top <- df[cum_tpa <= 40 | seq_along(cum_tpa) == 1, ]
  if (nrow(top) == 0) return(NA_real_)
  stats::weighted.mean(top$ht, top$tpa, na.rm = TRUE)
}

#' Get species with highest BA
#' @keywords internal
.get_dominant_spp <- function(spp, ba) {
  if (length(spp) == 0) return(NA_character_)
  spp_ba <- tapply(ba, spp, sum, na.rm = TRUE)
  names(which.max(spp_ba))
}
