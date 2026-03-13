#' Flag Outlier Trees Based on Biologically Plausible Bounds
#'
#' Identifies trees with biologically implausible height, DBH, or
#' height-to-diameter ratios using species-group-specific bounds derived from
#' FIA Phase 2 data. Outliers are flagged -- not removed -- so the user
#' retains full control over how to handle them.
#'
#' All inputs are expected in **imperial units** (DBH in inches, height in
#' feet). Bounds are derived from FIA Phase 2 data for northeastern states
#' (FIADB v9.2, eval groups 2015-2023).
#'
#' @param data A data frame containing tree-level inventory data.
#' @param spp_col Character. Column name for species code (FVS alpha codes).
#'   Default "spp_code".
#' @param dbh_col Character. Column name for DBH in **inches**. Default "dbh".
#' @param ht_col Character. Column name for total height in **feet**.
#'   Default "height".
#' @param method Character. Detection method: "bounds" (default) uses
#'   species-group biological bounds; "iqr" uses 1.5*IQR fences on the
#'   HD ratio within species groups.
#' @param sensitivity Numeric. Multiplier for expanding bounds (default 1.0).
#'   Values > 1 make detection less aggressive.
#'
#' @return The input data frame with additional columns:
#' \describe{
#'   \item{warn_dbh}{TRUE if DBH exceeds 95th percentile for species group.}
#'   \item{warn_ht}{TRUE if height exceeds 95th percentile.}
#'   \item{warn_hd_ratio}{TRUE if HD ratio exceeds 95th percentile.}
#'   \item{flag_dbh}{TRUE if DBH is outside 99.5th percentile bounds.}
#'   \item{flag_ht}{TRUE if height is outside 99.5th percentile bounds.}
#'   \item{flag_hd_ratio}{TRUE if HD ratio is outside 99.5th percentile.}
#'   \item{outlier_reason}{Character summary of flagged issues.}
#' }
#'
#' @examples
#' trees <- data.frame(
#'   spp_code = c("RS", "SM", "BF", "WP", "RS"),
#'   dbh = c(10, 16, 6, 32, 4),
#'   height = c(60, 72, 40, 115, 5)
#' )
#' flag_outliers(trees)
#'
#' @export
flag_outliers <- function(data,
                          spp_col = "spp_code",
                          dbh_col = "dbh",
                          ht_col  = "height",
                          method  = "bounds",
                          sensitivity = 1.0) {

  stopifnot(is.data.frame(data))
  for (col in c(spp_col, dbh_col, ht_col)) {
    if (!col %in% names(data)) {
      stop("Column '", col, "' not found in data.", call. = FALSE)
    }
  }
  stopifnot(method %in% c("bounds", "iqr"))
  stopifnot(is.numeric(sensitivity), sensitivity > 0)

  spp <- data[[spp_col]]
  dbh <- data[[dbh_col]]
  ht  <- data[[ht_col]]

  # Species group assignment
  spp_group <- dplyr::case_when(
    spp %in% c("BF", "BS", "RS", "WS", "NS", "TA", "TL") ~ "spruce_fir",
    spp %in% c("WP", "RP", "JP", "SP", "LP")              ~ "pine",
    spp %in% c("SM", "YB", "AB", "RO", "WA", "BA")        ~ "n_hardwood",
    spp %in% c("WC", "NC", "EH")                           ~ "cedar_hemlock",
    spp %in% c("RM", "QA", "PB", "GB", "GA", "BT",
               "BP", "SB", "BC")                            ~ "o_hardwood",
    TRUE                                                     ~ "other"
  )

  # Biological bounds in IMPERIAL units (inches / feet)
  # Converted from original metric bounds:
  #   DBH: cm → inches (÷ 2.54)
  #   HT:  m  → feet   (× 3.281)
  #   HD ratio: ft/in (not m/cm, so recalculated)
  bounds <- list(
    spruce_fir = list(
      dbh_min = 1.0, dbh_warn = 19.7, dbh_max = 25.6,
      ht_min = 4.5, ht_warn = 78.7, ht_max = 98.4,
      hd_min = 1.5, hd_warn = 9.3, hd_max = 10.8
    ),
    pine = list(
      dbh_min = 1.0, dbh_warn = 25.6, dbh_max = 35.4,
      ht_min = 4.5, ht_warn = 98.4, ht_max = 131.2,
      hd_min = 1.2, hd_warn = 7.0, hd_max = 8.5
    ),
    n_hardwood = list(
      dbh_min = 1.0, dbh_warn = 27.6, dbh_max = 39.4,
      ht_min = 4.5, ht_warn = 98.4, ht_max = 124.7,
      hd_min = 1.2, hd_warn = 6.6, hd_max = 7.7
    ),
    cedar_hemlock = list(
      dbh_min = 1.0, dbh_warn = 23.6, dbh_max = 35.4,
      ht_min = 4.5, ht_warn = 85.3, ht_max = 114.8,
      hd_min = 1.2, hd_warn = 7.0, hd_max = 8.5
    ),
    o_hardwood = list(
      dbh_min = 1.0, dbh_warn = 21.7, dbh_max = 31.5,
      ht_min = 4.5, ht_warn = 82.0, ht_max = 105.0,
      hd_min = 1.2, hd_warn = 6.6, hd_max = 7.7
    ),
    other = list(
      dbh_min = 1.0, dbh_warn = 27.6, dbh_max = 39.4,
      ht_min = 4.5, ht_warn = 98.4, ht_max = 131.2,
      hd_min = 1.2, hd_warn = 7.7, hd_max = 9.3
    )
  )

  # HD ratio in ft/in
  hd_ratio <- ifelse(dbh > 0, ht / dbh, NA_real_)

  n_rows <- nrow(data)
  flag_dbh <- logical(n_rows)
  flag_ht  <- logical(n_rows)
  flag_hd  <- logical(n_rows)
  warn_dbh <- logical(n_rows)
  warn_ht  <- logical(n_rows)
  warn_hd  <- logical(n_rows)

  if (method == "bounds") {
    for (i in seq_len(n_rows)) {
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
      if (!is.na(hd_ratio[i])) {
        adj_min <- b$hd_min / sensitivity
        adj_max <- b$hd_max * sensitivity
        adj_warn_max <- b$hd_warn * sensitivity
        flag_hd[i] <- hd_ratio[i] < adj_min | hd_ratio[i] > adj_max
        warn_hd[i] <- !flag_hd[i] & (hd_ratio[i] > adj_warn_max)
      }
    }
  } else if (method == "iqr") {
    for (grp in unique(spp_group)) {
      idx <- which(spp_group == grp & !is.na(hd_ratio))
      if (length(idx) < 5) next
      q <- stats::quantile(hd_ratio[idx], probs = c(0.25, 0.75), na.rm = TRUE)
      iqr_val <- (q[2] - q[1]) * 1.5 * sensitivity
      lo <- q[1] - iqr_val
      hi <- q[2] + iqr_val
      iqr_warn <- (q[2] - q[1]) * 1.0 * sensitivity
      lo_warn <- q[1] - iqr_warn
      hi_warn <- q[2] + iqr_warn
      flag_hd[idx] <- hd_ratio[idx] < lo | hd_ratio[idx] > hi
      warn_hd[idx] <- !flag_hd[idx] & (hd_ratio[idx] < lo_warn | hd_ratio[idx] > hi_warn)
    }
    for (i in seq_len(n_rows)) {
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

  # Build reason string
  reasons <- character(n_rows)
  for (i in seq_len(n_rows)) {
    parts <- character(0)
    if (flag_dbh[i]) parts <- c(parts, paste0("OUTLIER: DBH=", round(dbh[i], 1), "in outside 99.5th pctl"))
    if (flag_ht[i])  parts <- c(parts, paste0("OUTLIER: HT=", round(ht[i], 1), "ft outside 99.5th pctl"))
    if (flag_hd[i])  parts <- c(parts, paste0("OUTLIER: HD_ratio=", round(hd_ratio[i], 2), " outside 99.5th pctl"))
    if (warn_dbh[i]) parts <- c(parts, paste0("WARNING: DBH=", round(dbh[i], 1), "in exceeds 95th pctl"))
    if (warn_ht[i])  parts <- c(parts, paste0("WARNING: HT=", round(ht[i], 1), "ft exceeds 95th pctl"))
    if (warn_hd[i])  parts <- c(parts, paste0("WARNING: HD_ratio=", round(hd_ratio[i], 2), " exceeds 95th pctl"))
    reasons[i] <- if (length(parts) > 0) paste(parts, collapse = "; ") else NA_character_
  }

  data$warn_dbh      <- warn_dbh
  data$warn_ht       <- warn_ht
  data$warn_hd_ratio <- warn_hd
  data$flag_dbh      <- flag_dbh
  data$flag_ht       <- flag_ht
  data$flag_hd_ratio <- flag_hd
  data$outlier_reason <- reasons

  n_warned  <- sum(warn_dbh | warn_ht | warn_hd, na.rm = TRUE)
  n_flagged <- sum(flag_dbh | flag_ht | flag_hd, na.rm = TRUE)
  message(n_warned, " of ", n_rows, " trees warned (>95th pctl, ",
          round(100 * n_warned / n_rows, 1), "%)")
  message(n_flagged, " of ", n_rows, " trees flagged as outliers (>99.5th pctl, ",
          round(100 * n_flagged / n_rows, 1), "%)")

  data
}
