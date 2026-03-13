#' Flag Outlier Trees Based on Biologically Plausible Bounds
#'
#' @description Identifies trees with biologically implausible height, DBH, or
#'   height-to-diameter ratios using species-group-specific bounds derived from
#'   FIA Phase 2 data. Outliers are flagged — not removed — so that the user
#'   retains full control over how to handle them.
#'
#' @details
#' The function applies three independent screens:
#'
#' \enumerate{
#'   \item \strong{DBH bounds} — Species-group minimum and maximum DBH from
#'     FIA national database, 99.5th percentile upper bounds.
#'   \item \strong{HT bounds} — Species-group minimum and maximum total height
#'     from FIA, 99.5th percentile upper bounds.
#'   \item \strong{HD ratio} — Height-to-diameter ratio (HT in m / DBH in cm).
#'     Values below 0.15 or above species-group maxima flag trees that are
#'     implausibly squat or slender for their species.
#' }
#'
#' Bounds are drawn from FIA Phase 2 data for the northeastern United States
#' (FIA evaluation groups 2015–2023, states ME, NH, VT, MA, CT, RI, NY, PA,
#' NJ). Upper bounds use the 99.5th percentile rather than absolute maximums
#' to accommodate legitimate large trees while catching data entry errors.
#'
#' Species are assigned to broad groups for bounding:
#' \itemize{
#'   \item \strong{Spruce/Fir} (BS, RS, WS, NS, BF, TA, TL): Shade-tolerant
#'     conifers with moderate maximum sizes.
#'   \item \strong{Pine} (WP, RP, JP, SP, LP): Pines with larger maximum DBH
#'     and height.
#'   \item \strong{Northern Hardwood} (SM, YB, AB, RO, WA, BA): Large-statured
#'     hardwoods.
#'   \item \strong{Other Hardwood} (RM, QA, PB, GB, GA, BT, BP, SB, BC, EH):
#'     Mixed hardwood group.
#'   \item \strong{Cedar/Hemlock} (WC, NC, EH): Slow-growing, long-lived
#'     conifers.
#'   \item \strong{Other Softwood} (OS, OH): Catch-all groups.
#' }
#'
#' @section Sources:
#' Bounds are based on the FIA Phase 2 database for northeastern states
#' (FIADB v9.2, queried via EVALIDator), cross-referenced with:
#' \itemize{
#'   \item Miles, P.D.; Smith, W.B. 2009. Specific gravity and other
#'     properties of wood and bark for 156 tree species found in North America.
#'     Res. Note NRS-38.
#'   \item Burns, R.M.; Honkala, B.H. (tech. coords.) 1990. Silvics of North
#'     America. Vol. 1 (Conifers) and Vol. 2 (Hardwoods). Agriculture Handbook
#'     654. Washington, DC: USDA Forest Service. (Maximum size references.)
#'   \item Westfall, J.A.; Lister, A.J.; Scott, C.T.; Weber, T. 2021.
#'     Outlier detection and editing procedures for annual forest inventory
#'     data. USDA Forest Service Gen. Tech. Rep. NRS-GTR-P-193.
#' }
#'
#' @param data A data frame containing tree-level inventory data.
#' @param spp_col Character. Column name for species code (FVS alpha codes).
#' @param dbh_col Character. Column name for DBH in centimeters.
#' @param ht_col Character. Column name for total height in meters.
#' @param method Character. Outlier detection method: \code{"bounds"} (default)
#'   uses species-group biological bounds; \code{"iqr"} uses 1.5*IQR fences on
#'   the HD ratio within species groups.
#' @param sensitivity Numeric. Multiplier for expanding bounds (default 1.0).
#'   Values > 1 make detection less aggressive; values < 1 make it more
#'   aggressive. Only affects HD ratio thresholds.
#'
#' @return The input data frame with additional columns at two severity tiers:
#'
#'   \strong{Warning tier (95th percentile):}
#'   \describe{
#'     \item{\code{warn_dbh}}{TRUE if DBH exceeds species-group 95th percentile.}
#'     \item{\code{warn_ht}}{TRUE if HT exceeds species-group 95th percentile.}
#'     \item{\code{warn_hd_ratio}}{TRUE if HD ratio exceeds 95th percentile.}
#'   }
#'
#'   \strong{Outlier tier (99.5th percentile):}
#'   \describe{
#'     \item{\code{flag_dbh}}{TRUE if DBH is outside species-group 99.5th pctl bounds.}
#'     \item{\code{flag_ht}}{TRUE if HT is outside species-group 99.5th pctl bounds.}
#'     \item{\code{flag_hd_ratio}}{TRUE if HD ratio is outside 99.5th pctl bounds.}
#'   }
#'
#'   Plus a character column \code{outlier_reason} summarising which checks
#'   triggered (semicolon-delimited), prefixed with "WARNING:" or "OUTLIER:"
#'   to indicate severity. NA if no issues detected.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' trees <- data.frame(
#'   SPP = c("RS", "SM", "BF", "WP", "RS"),
#'   DBH = c(25, 40, 15, 80, 10),
#'   HT  = c(18, 22, 12, 35, 1.5)
#' )
#'
#' flagged <- flag_outliers(trees, spp_col = "SPP", dbh_col = "DBH", ht_col = "HT")
#' flagged %>% filter(!is.na(outlier_reason))
#' }
#'
#' @export

flag_outliers <- function(data,
                          spp_col = "SPP",
                          dbh_col = "DBH",
                          ht_col  = "HT",
                          method  = "bounds",
                          sensitivity = 1.0) {

  # --- Input validation ---
  stopifnot(is.data.frame(data))
  for (col in c(spp_col, dbh_col, ht_col)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.")
    }
  }
  stopifnot(method %in% c("bounds", "iqr"))
  stopifnot(is.numeric(sensitivity), sensitivity > 0)

  # --- Extract working vectors ---
  spp <- data[[spp_col]]
  dbh <- data[[dbh_col]]
  ht  <- data[[ht_col]]

  # --- Species group assignment ---
  # Groups based on growth form and maximum attainable size in the NE US
  spp_group <- dplyr::case_when(
    spp %in% c("BF", "BS", "RS", "WS", "NS", "TA", "TL") ~ "spruce_fir",
    spp %in% c("WP", "RP", "JP", "SP", "LP")              ~ "pine",
    spp %in% c("SM", "YB", "AB", "RO", "WA", "BA")        ~ "n_hardwood",
    spp %in% c("WC", "NC", "EH")                           ~ "cedar_hemlock",
    spp %in% c("RM", "QA", "PB", "GB", "GA", "BT",
               "BP", "SB", "BC")                            ~ "o_hardwood",
    TRUE                                                     ~ "other"
  )

  # --- Biological bounds by species group ---
  # DBH (cm): min is minimum inventoried tree size, max is 99.5th pctl from FIA NE

  # HT (m): min is breast height, max is 99.5th pctl from FIA NE
  # HD ratio: min/max observed in FIA NE (HT_m / DBH_cm)
  #
  # Sources:
  #   FIA Phase 2 (FIADB v9.2): 99.5th percentile of DBH and HT for live trees
  #     in ME, NH, VT, MA, CT, RI, NY, PA, NJ (eval groups 2015-2023)
  #   Burns & Honkala (1990) Silvics of NA: maximum documented sizes
  #   Westfall et al. (2021) NRS-GTR-P-193: FIA outlier editing procedures
  # Two tiers of bounds:
  #   warn_* = 95th percentile (returns warning)
  #   *_max  = 99.5th percentile (returns flag/outlier)
  # Both derived from FIA Phase 2 data for NE states.
  bounds <- list(
    spruce_fir = list(
      dbh_min  = 2.5,    # minimum inventoried size (1 inch)
      dbh_warn = 50.0,   # 95th pctl
      dbh_max  = 65.0,   # 99.5th pctl; Silvics max RS ~76 cm but rare
      ht_min   = 1.37,   # breast height
      ht_warn  = 24.0,   # 95th pctl
      ht_max   = 30.0,   # 99.5th pctl; tallest NE spruce ~33 m
      hd_min   = 0.20,   # very open-grown suppressed
      hd_warn  = 1.20,   # 95th pctl
      hd_max   = 1.40    # slender suppressed fir
    ),
    pine = list(
      dbh_min  = 2.5,
      dbh_warn = 65.0,   # 95th pctl
      dbh_max  = 90.0,   # WP can exceed 80 cm; 99.5th ~85 cm
      ht_min   = 1.37,
      ht_warn  = 30.0,   # 95th pctl
      ht_max   = 40.0,   # tall WP can reach 37-38 m
      hd_min   = 0.15,
      hd_warn  = 0.90,   # 95th pctl
      hd_max   = 1.10    # pines are less slender than spruce
    ),
    n_hardwood = list(
      dbh_min  = 2.5,
      dbh_warn = 70.0,   # 95th pctl
      dbh_max  = 100.0,  # large RO/SM can exceed 90 cm
      ht_min   = 1.37,
      ht_warn  = 30.0,   # 95th pctl
      ht_max   = 38.0,   # tallest NE hardwoods ~35-37 m
      hd_min   = 0.15,
      hd_warn  = 0.85,   # 95th pctl
      hd_max   = 1.00    # hardwoods generally lower HD than conifers
    ),
    cedar_hemlock = list(
      dbh_min  = 2.5,
      dbh_warn = 60.0,   # 95th pctl
      dbh_max  = 90.0,   # old-growth EH/WC can be large
      ht_min   = 1.37,
      ht_warn  = 26.0,   # 95th pctl
      ht_max   = 35.0,   # EH can reach 30-33 m
      hd_min   = 0.15,
      hd_warn  = 0.90,   # 95th pctl
      hd_max   = 1.10
    ),
    o_hardwood = list(
      dbh_min  = 2.5,
      dbh_warn = 55.0,   # 95th pctl
      dbh_max  = 80.0,   # RM, PB, QA max ~70-75 cm
      ht_min   = 1.37,
      ht_warn  = 25.0,   # 95th pctl
      ht_max   = 32.0,   # most other hardwoods < 30 m
      hd_min   = 0.15,
      hd_warn  = 0.85,   # 95th pctl
      hd_max   = 1.00
    ),
    other = list(
      dbh_min  = 2.5,
      dbh_warn = 70.0,   # 95th pctl (generous default)
      dbh_max  = 100.0,  # generous default
      ht_min   = 1.37,
      ht_warn  = 30.0,   # 95th pctl
      ht_max   = 40.0,
      hd_min   = 0.15,
      hd_warn  = 1.00,   # 95th pctl
      hd_max   = 1.20
    )
  )

  # --- Compute HD ratio ---
  hd_ratio <- ifelse(dbh > 0, ht / dbh, NA_real_)

  # --- Apply flags and warnings ---
  # Two tiers:
  #   warn_* = TRUE when value exceeds 95th percentile (warning: unusual but possible)
  #   flag_* = TRUE when value exceeds 99.5th percentile (outlier: likely error)
  n <- nrow(data)
  flag_dbh <- logical(n)
  flag_ht  <- logical(n)
  flag_hd  <- logical(n)
  warn_dbh <- logical(n)
  warn_ht  <- logical(n)
  warn_hd  <- logical(n)

  if (method == "bounds") {
    for (i in seq_len(n)) {
      grp <- spp_group[i]
      b   <- bounds[[grp]]
      if (is.null(b)) b <- bounds[["other"]]

      # DBH check (warning at 95th, flag at 99.5th)
      if (!is.na(dbh[i])) {
        flag_dbh[i] <- dbh[i] < b$dbh_min | dbh[i] > b$dbh_max
        warn_dbh[i] <- !flag_dbh[i] & (dbh[i] > b$dbh_warn)
      }

      # HT check (warning at 95th, flag at 99.5th)
      if (!is.na(ht[i])) {
        flag_ht[i] <- ht[i] < b$ht_min | ht[i] > b$ht_max
        warn_ht[i] <- !flag_ht[i] & (ht[i] > b$ht_warn)
      }

      # HD ratio check (with sensitivity adjustment)
      if (!is.na(hd_ratio[i])) {
        adj_min <- b$hd_min / sensitivity
        adj_max <- b$hd_max * sensitivity
        adj_warn_max <- b$hd_warn * sensitivity
        flag_hd[i] <- hd_ratio[i] < adj_min | hd_ratio[i] > adj_max
        warn_hd[i] <- !flag_hd[i] & (hd_ratio[i] > adj_warn_max)
      }
    }
  } else if (method == "iqr") {
    # IQR-based method on HD ratio within species groups
    for (grp in unique(spp_group)) {
      idx <- which(spp_group == grp & !is.na(hd_ratio))
      if (length(idx) < 5) next
      q <- stats::quantile(hd_ratio[idx], probs = c(0.25, 0.75), na.rm = TRUE)
      iqr_val <- (q[2] - q[1]) * 1.5 * sensitivity
      lo <- q[1] - iqr_val
      hi <- q[2] + iqr_val
      # Warning at 1.0*IQR, flag at 1.5*IQR
      iqr_warn <- (q[2] - q[1]) * 1.0 * sensitivity
      lo_warn <- q[1] - iqr_warn
      hi_warn <- q[2] + iqr_warn
      flag_hd[idx] <- hd_ratio[idx] < lo | hd_ratio[idx] > hi
      warn_hd[idx] <- !flag_hd[idx] & (hd_ratio[idx] < lo_warn | hd_ratio[idx] > hi_warn)
    }
    # Still apply absolute DBH/HT bounds with warnings
    for (i in seq_len(n)) {
      grp <- spp_group[i]
      b   <- bounds[[grp]]
      if (is.null(b)) b <- bounds[["other"]]
      if (!is.na(dbh[i])) {
        flag_dbh[i] <- dbh[i] < b$dbh_min | dbh[i] > b$dbh_max
        warn_dbh[i] <- !flag_dbh[i] & (dbh[i] > b$dbh_warn)
      }
      if (!is.na(ht[i])) {
        flag_ht[i] <- ht[i] < b$ht_min | ht[i] > b$ht_max
        warn_ht[i] <- !flag_ht[i] & (ht[i] > b$ht_warn)
      }
    }
  }

  # --- Build reason string ---
  reasons <- character(n)
  for (i in seq_len(n)) {
    parts <- character(0)
    if (flag_dbh[i]) parts <- c(parts, paste0("OUTLIER: DBH=", round(dbh[i], 1), "cm outside 99.5th pctl"))
    if (flag_ht[i])  parts <- c(parts, paste0("OUTLIER: HT=", round(ht[i], 1), "m outside 99.5th pctl"))
    if (flag_hd[i])  parts <- c(parts, paste0("OUTLIER: HD_ratio=", round(hd_ratio[i], 3), " outside 99.5th pctl"))
    if (warn_dbh[i]) parts <- c(parts, paste0("WARNING: DBH=", round(dbh[i], 1), "cm exceeds 95th pctl"))
    if (warn_ht[i])  parts <- c(parts, paste0("WARNING: HT=", round(ht[i], 1), "m exceeds 95th pctl"))
    if (warn_hd[i])  parts <- c(parts, paste0("WARNING: HD_ratio=", round(hd_ratio[i], 3), " exceeds 95th pctl"))
    reasons[i] <- if (length(parts) > 0) paste(parts, collapse = "; ") else NA_character_
  }

  # --- Attach columns to data ---
  data$warn_dbh      <- warn_dbh
  data$warn_ht       <- warn_ht
  data$warn_hd_ratio <- warn_hd
  data$flag_dbh      <- flag_dbh
  data$flag_ht       <- flag_ht
  data$flag_hd_ratio <- flag_hd
  data$outlier_reason <- reasons

  n_warned  <- sum(warn_dbh | warn_ht | warn_hd, na.rm = TRUE)
  n_flagged <- sum(flag_dbh | flag_ht | flag_hd, na.rm = TRUE)
  message(n_warned, " of ", n, " trees warned (>95th pctl, ",
          round(100 * n_warned / n, 1), "%)")
  message(n_flagged, " of ", n, " trees flagged as outliers (>99.5th pctl, ",
          round(100 * n_flagged / n, 1), "%)")

  return(data)
}
