#' Set FVS Variant for the Session
#'
#' Configures which FVS variant to use for species code translation
#' throughout the session. This affects all functions that use species codes.
#'
#' @param variant FVS variant code. Common variants:
#'   - "NE" (Northeast)
#'   - "SN" (Southern)
#'   - "LS" (Lake States)
#'   - "CS" (Central States)
#'   - "PN" (Pacific Northwest)
#'   - "WC" (West Cascades)
#'   - "BM" (Blue Mountains)
#'   - "CR" (Central Rockies)
#'
#' @return Invisible. Sets the `FIAwrapper.variant` option.
#'
#' @examples
#' set_fvs_variant("NE")
#' getOption("FIAwrapper.variant")
#'
#' @seealso [fia_to_fvs()], [export_fvs_treelist()]
#' @export
set_fvs_variant <- function(variant) {
  variant <- toupper(variant)
  valid_variants <- c("NE", "SN", "LS", "CS", "PN", "WC", "EC", "SO",
                       "BM", "CI", "CR", "EM", "IE", "KT", "NC", "NI",
                       "TT", "UT", "WS", "AK", "SE", "OC")

  if (!variant %in% valid_variants) {
    warning("Variant '", variant, "' not in standard list. ",
            "Species lookups may fall back to default table.")
  }

  options(FIAwrapper.variant = variant)

  # Clear cached species table for this variant to force reload
  .pkg_env$species_tables[[tolower(variant)]] <- NULL

  message("FVS variant set to: ", variant)
  invisible(variant)
}

#' Export Tree List in FVS Format
#'
#' Writes a tree list to FVS-formatted input file. Supports the standard
#' fixed-width format used by FVS, a Suppose-compatible format, and CSV.
#'
#' @param tree_list A tibble from [build_tree_list()] or similar, with
#'   columns: plot_id/plot_cn, spp_code, dbh, height, crown_ratio, tpa.
#' @param file Output file path (e.g., "fvs_input.tre").
#' @param variant FVS variant code. If NULL, uses session variant.
#' @param format Output format: "standard" (fixed-width .tre/.fvs),
#'   "suppose" (Suppose-compatible), or "csv".
#' @param stand_id_col Column to use as FVS stand ID. Default "plot_id".
#'
#' @return Invisible file path. File is written to disk.
#'
#' @details
#' ## FVS Tree Record Format (Standard)
#' Fixed-width columns per FVS documentation:
#' \itemize{
#'   \item Cols 1-4: Plot/Stand ID
#'   \item Cols 5-7: Tree ID
#'   \item Cols 8-10: Tree count (trees represented)
#'   \item Cols 11-13: Tree history (1=live)
#'   \item Cols 14-16: Species code (FVS alpha)
#'   \item Cols 17-20: DBH (inches, 0.1)
#'   \item Cols 21-23: DBH increment (not usually available from FIA)
#'   \item Cols 24-26: Height (feet, integer)
#'   \item Cols 27-29: Height to live crown (feet)
#'   \item Cols 30-32: Crown ratio code
#'   \item Cols 33-35: Damage code 1
#'   \item Cols 36-38: Damage severity 1
#'   \item Cols 39-41: Damage code 2
#'   \item Cols 42-44: Damage severity 2
#'   \item Cols 45-47: Damage code 3
#'   \item Cols 48-50: Damage severity 3
#'   \item Cols 51-54: Tree value class
#'   \item Cols 55-57: Prescription
#'   \item Cols 58-61: Slope (percent)
#'   \item Cols 62-64: Aspect (degrees / 10)
#'   \item Cols 65-67: Habitat type
#'   \item Cols 68-70: Top kill height
#'   \item Cols 71-73: Sight height
#' }
#'
#' @examples
#' \dontrun{
#' trees <- build_tree_list(fia)
#' export_fvs_treelist(trees, "my_stand.tre", variant = "NE")
#' export_fvs_treelist(trees, "my_stand.csv", format = "csv")
#' }
#'
#' @seealso [set_fvs_variant()], [export_fvs_standlist()], [build_tree_list()]
#' @export
export_fvs_treelist <- function(tree_list,
                                 file,
                                 variant = NULL,
                                 format = "standard",
                                 stand_id_col = "plot_id") {

  variant <- variant %||% getOption("FIAwrapper.variant")

  # Ensure species codes match the variant
  if (!is.null(variant) && "spcd" %in% names(tree_list)) {
    tree_list$spp_code <- fia_to_fvs(tree_list$spcd, variant = variant)
  }

  if (format == "csv") {
    .export_fvs_csv(tree_list, file, stand_id_col)
  } else if (format == "standard") {
    .export_fvs_fixedwidth(tree_list, file, stand_id_col)
  } else if (format == "suppose") {
    .export_fvs_suppose(tree_list, file, stand_id_col)
  } else {
    stop("Unknown format '", format, "'. Use 'standard', 'suppose', or 'csv'.",
         call. = FALSE)
  }

  n_plots <- length(unique(tree_list[[stand_id_col]]))
  n_trees <- nrow(tree_list)
  message("Exported ", n_trees, " trees in ", n_plots, " stands to: ", file)

  invisible(file)
}

#' Export Stand-Level Data for FVS
#'
#' Writes stand initialization data for FVS, including site attributes
#' like latitude, elevation, slope, aspect, and site index.
#'
#' @param plot_summary A tibble from [get_plot_summary()] or [get_site_data()].
#' @param file Output file path.
#' @param variant FVS variant code.
#'
#' @return Invisible file path.
#'
#' @examples
#' \dontrun{
#' plots <- get_plot_summary(trees)
#' site <- get_site_data(fia)
#' combined <- merge(plots, site, by = "plot_cn")
#' export_fvs_standlist(combined, "stands.csv")
#' }
#'
#' @seealso [export_fvs_treelist()], [get_site_data()]
#' @export
export_fvs_standlist <- function(plot_summary, file, variant = NULL) {
  variant <- variant %||% getOption("FIAwrapper.variant")

  out <- data.frame(
    stand_id  = if ("plot_id" %in% names(plot_summary)) plot_summary$plot_id
                else plot_summary$plot_cn,
    variant   = variant %||% "",
    latitude  = if ("lat" %in% names(plot_summary)) plot_summary$lat else NA,
    longitude = if ("lon" %in% names(plot_summary)) plot_summary$lon else NA,
    elevation = if ("elevation" %in% names(plot_summary))
                  as.integer(plot_summary$elevation) else NA_integer_,
    slope     = if ("slope" %in% names(plot_summary))
                  as.integer(plot_summary$slope) else NA_integer_,
    aspect    = if ("aspect" %in% names(plot_summary))
                  as.integer(plot_summary$aspect) else NA_integer_,
    site_index = if ("site_index" %in% names(plot_summary))
                   plot_summary$site_index else NA_real_,
    ba_per_acre = if ("ba_per_acre" %in% names(plot_summary))
                    round(plot_summary$ba_per_acre, 1) else NA_real_,
    tpa       = if ("tpa" %in% names(plot_summary))
                  round(plot_summary$tpa, 1) else NA_real_,
    stringsAsFactors = FALSE
  )

  utils::write.csv(out, file, row.names = FALSE)
  message("Exported ", nrow(out), " stand records to: ", file)
  invisible(file)
}

# ---- Internal FVS format writers ----

#' @keywords internal
.export_fvs_csv <- function(tree_list, file, stand_id_col) {
  out <- data.frame(
    stand_id     = tree_list[[stand_id_col]],
    tree_id      = if ("tree_num" %in% names(tree_list)) tree_list$tree_num else seq_len(nrow(tree_list)),
    species      = tree_list$spp_code,
    dbh          = round(tree_list$dbh, 1),
    height       = if ("height" %in% names(tree_list)) round(tree_list$height, 0) else NA,
    crown_ratio  = if ("crown_ratio" %in% names(tree_list)) tree_list$crown_ratio else NA,
    tpa          = round(tree_list$tpa, 2),
    status       = if ("status" %in% names(tree_list))
                     ifelse(tree_list$status == "live", 1, 6) else 1,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, file, row.names = FALSE)
}

#' @keywords internal
.export_fvs_fixedwidth <- function(tree_list, file, stand_id_col) {
  lines <- character(nrow(tree_list))

  for (i in seq_len(nrow(tree_list))) {
    row <- tree_list[i, ]
    stand_id <- substr(as.character(row[[stand_id_col]]), 1, 4)
    tree_id  <- if ("tree_num" %in% names(row)) row$tree_num else i
    spp      <- substr(as.character(row$spp_code), 1, 3)
    dbh_val  <- round(row$dbh, 1)
    ht_val   <- if ("height" %in% names(row) && !is.na(row$height)) round(row$height) else 0
    cr_val   <- if ("crown_ratio" %in% names(row) && !is.na(row$crown_ratio)) {
                  # FVS uses crown ratio codes 1-9 (in tenths)
                  min(9, max(1, round(row$crown_ratio / 10)))
                } else 0
    tpa_val  <- round(row$tpa, 0)
    status   <- if ("status" %in% names(row)) {
                  ifelse(row$status == "live", 1, 6)
                } else 1

    # Fixed-width format
    lines[i] <- sprintf("%-4s%3d%3d%3d%-3s%4.1f   %3d   %1d",
                          stand_id, tree_id, tpa_val, status,
                          spp, dbh_val, ht_val, cr_val)
  }

  writeLines(lines, file)
}

#' @keywords internal
.export_fvs_suppose <- function(tree_list, file, stand_id_col) {
  # Suppose uses a slightly different CSV-like format
  out <- data.frame(
    StandID      = tree_list[[stand_id_col]],
    TreeID       = if ("tree_num" %in% names(tree_list)) tree_list$tree_num else seq_len(nrow(tree_list)),
    TreeCount    = round(tree_list$tpa, 2),
    History      = if ("status" %in% names(tree_list))
                     ifelse(tree_list$status == "live", 1, 6) else 1,
    Species      = tree_list$spp_code,
    DBH          = round(tree_list$dbh, 1),
    DG           = 0,  # DBH growth (not from FIA)
    Ht           = if ("height" %in% names(tree_list)) round(tree_list$height, 0) else 0,
    HtTopK       = 0,
    CrRatio      = if ("crown_ratio" %in% names(tree_list)) {
                     ifelse(is.na(tree_list$crown_ratio), 0,
                            min(9, max(1, round(tree_list$crown_ratio / 10))))
                   } else 0,
    Damage1      = 0, Severity1 = 0,
    Damage2      = 0, Severity2 = 0,
    Damage3      = 0, Severity3 = 0,
    TreeValue    = 0,
    Prescription = 0,
    stringsAsFactors = FALSE
  )
  utils::write.csv(out, file, row.names = FALSE)
}
