#' FIA to Forestry Column Name Translation
#'
#' Renames FIA database column names to standard forestry terminology.
#' Only renames columns that are present in the data frame; others are
#' left unchanged.
#'
#' @param df A data frame with FIA column names.
#' @return The data frame with forestry-friendly column names.
#'
#' @examples
#' df <- data.frame(DIA = 10, HT = 65, STATUSCD = 1)
#' fia_to_forestry_names(df)
#' # Returns: data.frame(dbh = 10, height = 65, status_code = 1)
#'
#' @export
fia_to_forestry_names <- function(df) {
  mapping <- .fia_to_forestry_map()
  current_names <- names(df)
  matched <- current_names %in% names(mapping)
  names(df)[matched] <- mapping[current_names[matched]]
  df
}

#' Forestry to FIA Column Name Translation
#'
#' Renames forestry-friendly column names back to FIA database conventions.
#' Inverse of [fia_to_forestry_names()].
#'
#' @param df A data frame with forestry column names.
#' @return The data frame with FIA column names.
#'
#' @examples
#' df <- data.frame(dbh = 10, height = 65, status_code = 1)
#' forestry_to_fia_names(df)
#'
#' @export
forestry_to_fia_names <- function(df) {
  fwd <- .fia_to_forestry_map()
  rev_mapping <- stats::setNames(names(fwd), fwd)
  current_names <- names(df)
  matched <- current_names %in% names(rev_mapping)
  names(df)[matched] <- rev_mapping[current_names[matched]]
  df
}

#' FIA → Forestry Column Name Mapping
#' @return Named character vector: names = FIA names, values = forestry names.
#' @keywords internal
.fia_to_forestry_map <- function() {
  c(
    # Tree measurements
    "DIA"              = "dbh",
    "DIAHTCD"          = "dbh_height_code",
    "HT"               = "height",
    "ACTUALHT"         = "actual_height",
    "HTCD"             = "height_code",
    "CR"               = "crown_ratio",
    "CCLCD"            = "crown_class",
    "TREECLCD"         = "tree_class",
    "CULL"             = "cull_pct",
    "DATEFALLCD"       = "date_fall_code",

    # Tree identification
    "CN"               = "tree_cn",
    "PLT_CN"           = "plot_cn",
    "PREV_TRE_CN"      = "prev_tree_cn",
    "INVYR"            = "inventory_year",
    "STATECD"          = "state_code",
    "COUNTYCD"         = "county_code",
    "PLOT"             = "plot_id",
    "SUBP"             = "subplot",
    "TREE"             = "tree_num",
    "CONDID"           = "condition_id",
    "SPCD"             = "spcd",
    "SPGRPCD"          = "species_group",
    "STATUSCD"         = "status_code",
    "STANDING_DEAD_CD" = "standing_dead_code",
    "DECAYCD"          = "decay_class",
    "DAMAGE_AGENT_CD1" = "damage_code1",
    "DAMAGE_AGENT_CD2" = "damage_code2",
    "DAMAGE_AGENT_CD3" = "damage_code3",

    # Expansion factors
    "TPA_UNADJ"        = "tpa_unadj",
    "TPAGROW_UNADJ"    = "tpa_growth_unadj",
    "TPAREMV_UNADJ"    = "tpa_removals_unadj",
    "TPAMORT_UNADJ"    = "tpa_mortality_unadj",
    "CONDPROP_UNADJ"   = "cond_proportion",

    # Plot attributes
    "LAT"              = "lat",
    "LON"              = "lon",
    "ELEV"             = "elevation",
    "SLOPE"            = "slope",
    "ASPECT"           = "aspect",
    "MEESSION"         = "measurement_session",
    "ECOSUBCD"         = "ecological_subsection",

    # Condition / stand attributes
    "FORTYPCD"         = "forest_type_code",
    "FORTYPGRPCD"      = "forest_type_group_code",
    "FLDTYPCD"         = "field_type_code",
    "STDAGE"           = "stand_age",
    "STDORGCD"         = "stand_origin",
    "SICOND"           = "site_index",
    "SISP"             = "site_index_species",
    "SITECLCD"         = "site_class",
    "PHYSCLCD"         = "physiographic_class",
    "OWNCD"            = "ownership_code",
    "OWNGRPCD"         = "ownership_group",
    "ADFORCD"          = "admin_forest_code",
    "DSTRBCD1"         = "disturbance_code1",
    "DSTRBCD2"         = "disturbance_code2",
    "DSTRBCD3"         = "disturbance_code3",
    "TRTCD1"           = "treatment_code1",
    "TRTCD2"           = "treatment_code2",
    "TRTCD3"           = "treatment_code3",
    "BALIVE"           = "ba_live",
    "GSSTK"            = "growing_stock_stocking",
    "ALSTK"            = "all_stocking",
    "SDI"              = "stand_density_index",

    # Volume
    "VOLCFNET"         = "vol_cf_net",
    "VOLCFGRS"         = "vol_cf_gross",
    "VOLBFNET"         = "vol_bf_net",
    "VOLBFGRS"         = "vol_bf_gross",
    "VOLCFSND"         = "vol_cf_sound",
    "DRYBIO_AG"        = "biomass_ag_dry",
    "DRYBIO_BG"        = "biomass_bg_dry",
    "CARBON_AG"        = "carbon_ag",
    "CARBON_BG"        = "carbon_bg",

    # Cycle / measurement
    "CYCLE"            = "cycle",
    "SUBCYCLE"         = "subcycle",
    "MEESSION"         = "measurement_session",
    "PREV_STATUS_CD"   = "prev_status_code"
  )
}
