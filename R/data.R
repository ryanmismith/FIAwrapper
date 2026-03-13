#' Example FIA Dataset: Rhode Island
#'
#' A realistic FIA dataset modeled on Rhode Island forest inventory data.
#' Contains TREE, PLOT, and COND tables following FIADB v9.2 specifications,
#' suitable for demonstrating all FIAwrapper functions.
#'
#' The dataset includes 12 inventory plots across 3 Rhode Island counties
#' (Providence, Kent, Washington) with 2 measurement cycles (2014 and 2019).
#' Six plots have remeasurement data, enabling time series and growth analysis.
#' Species composition reflects typical southern New England mixed forests
#' dominated by red maple, oaks, white pine, and hemlock.
#'
#' @format A named list with 3 data frames:
#' \describe{
#'   \item{TREE}{467 tree records with standard FIA columns:
#'     CN, PLT_CN, CONDID, SUBP, TREE, STATUSCD, SPCD, DIA, HT, ACTUALHT,
#'     CR, TPA_UNADJ, DRYBIO_AG, VOLCFNET, VOLCFGRS, VOLBFNET, CARBON_AG,
#'     PREV_TRE_CN, DSTRBCD1, DAMAGE_AGENT_CD1, DECAYCD, CCLCD}
#'   \item{PLOT}{18 plot records (12 for 2019, 6 for 2014) with:
#'     CN, STATECD, COUNTYCD, PLOT, INVYR, LAT, LON, ELEV, PREV_PLT_CN,
#'     ECOSUBCD, DESIGNCD}
#'   \item{COND}{18 condition records with:
#'     CN, PLT_CN, CONDID, CONDPROP_UNADJ, FORTYPCD, STDAGE, SICOND,
#'     SITECLCD, SLOPE, ASPECT, STDSZCD, BALIVE, DSTRBCD1, OWNGRPCD,
#'     PHYSCLCD, SDI}
#' }
#'
#' @details
#' ## FIA Plot Design
#' Each plot has 4 subplots arranged in a cluster:
#' - **Subplot**: 24-foot radius (1/24 acre), trees >= 5.0 inches DBH
#' - **Microplot**: 6.8-foot radius (1/300 acre), saplings 1.0-4.9 inches DBH
#'
#' ## Species Codes (SPCD)
#' Common species in this dataset:
#' - 316: red maple
#' - 541: northern red oak
#' - 531: white oak
#' - 129: eastern white pine
#' - 261: eastern hemlock
#' - 318: sugar maple
#' - 833: red oak
#' - 802: white ash
#' - 97: red spruce
#' - 12: balsam fir
#'
#' ## Inventory Cycles
#' - **2019**: All 12 plots (PLT_CN 10001-10012)
#' - **2014**: 6 plots (PLT_CN 10101-10106), linked via PREV_PLT_CN
#'
#' @references
#' - FIADB User Guide: \url{https://www.fia.fs.usda.gov/library/database-documentation/}
#' - FIA DataMart: \url{https://apps.fs.usda.gov/fia/datamart/}
#' - Bechtold & Patterson (2005). The Enhanced FIA Program. Gen. Tech. Rep. SRS-80.
#'
#' @source Synthetic data generated to match FIA specifications. See
#'   \code{data-raw/build_example_data.R} for the generation script.
#'
#' @examples
#' data(fia_ri)
#' names(fia_ri)
#' nrow(fia_ri$TREE)
#' table(fia_ri$TREE$STATUSCD)
#' table(fia_ri$PLOT$INVYR)
"fia_ri"
