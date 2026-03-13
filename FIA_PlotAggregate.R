#' Aggregate FIA Subplot Trees to Plot-Level Estimates
#'
#' Rolls up individual tree records from a 4-subplot FIA cluster into
#' single plot-level summaries (TPH, BAPH, QMD, etc.). Flexible
#' \code{sum_cols} and \code{mean_cols} parameters allow arbitrary
#' additional attributes to be aggregated without modifying this function.
#'
#' @param Stand Vector of stand identifiers (character or numeric).
#' @param Plot Vector of plot identifiers (character or numeric).
#' @param DBH Numeric vector of diameters at breast height in \strong{cm}.
#' @param HT Optional numeric vector of total heights in \strong{m}.
#' @param EXPF Numeric vector of expansion factors (trees per hectare),
#'   typically from \code{\link{fia_expansion}}.
#' @param Species Optional character vector of FVS species codes.
#' @param BA Optional numeric vector of per-tree basal areas in m\eqn{^2}.
#'   If \code{NULL}, computed from \code{DBH} using \code{\link{BA}}.
#' @param SUBP Optional vector of subplot IDs. When provided, the number
#'   of unique subplots per plot is included in the output.
#' @param sum_cols Optional named list of numeric vectors to aggregate as
#'   \code{sum(col * EXPF)} (extensive attributes scaled to per-hectare).
#'   Names become output column names.
#'   Example: \code{list(biomass_ha = biomass_vec, vol_ha = vol_vec)}.
#' @param mean_cols Optional named list of numeric vectors to aggregate as
#'   \code{weighted.mean(col, EXPF)} (intensive attributes).
#'   Names become output column names.
#'   Example: \code{list(mean_cr = crown_ratio_vec)}.
#'
#' @details
#' ## Core Metrics
#' \describe{
#'   \item{TPH}{Trees per hectare: \code{sum(EXPF)}.}
#'   \item{BAPH}{Basal area per hectare (m\eqn{^2}/ha): \code{sum(BA * EXPF)}.}
#'   \item{QMD}{Quadratic mean diameter (cm):
#'     \eqn{\sqrt{\sum(DBH^2 \times EXPF) / \sum(EXPF)}}.}
#'   \item{mean_ht}{EXPF-weighted mean height (m), when \code{HT} provided.}
#'   \item{n_trees}{Count of measured trees.}
#'   \item{n_subplots}{Count of unique subplots, when \code{SUBP} provided.}
#' }
#'
#' ## Flexible Extra Columns
#' \code{sum_cols} and \code{mean_cols} accept named lists of numeric vectors
#' the same length as \code{DBH}. This lets you aggregate biomass, carbon,
#' volume, or any other tree-level attribute without modifying the function.
#'
#' @return A \code{tibble} with one row per unique Stand \eqn{\times} Plot
#'   combination, containing the core metrics plus any extra columns from
#'   \code{sum_cols} and \code{mean_cols}.
#'
#' @seealso \code{\link{fia_expansion}}, \code{\link{fia_variance}},
#'   \code{\link{BAPH}}, \code{\link{BA}}
#' @family FIA Functions
#'
#' @examples
#' # Basic aggregation
#' stand <- c(1,1,1,1,1)
#' plot  <- c(1,1,1,1,1)
#' dbh   <- c(25, 30, 5, 8, 40)
#' subp  <- c(1, 2, 3, 4, 1)
#' expf  <- fia_expansion(dbh, subp)
#'
#' fia_plot_aggregate(stand, plot, dbh, EXPF = expf, SUBP = subp)
#'
#' # With extra columns — biomass and volume per hectare
#' biomass <- c(120, 250, 5, 10, 400)   # kg per tree
#' vol     <- c(0.3, 0.8, 0.01, 0.02, 1.2)  # m3 per tree
#' fia_plot_aggregate(stand, plot, dbh, EXPF = expf,
#'                    sum_cols = list(biomass_ha = biomass, vol_ha = vol))
#'
#' @export

fia_plot_aggregate <- function(Stand, Plot, DBH, HT = NULL, EXPF,
                                Species = NULL, BA = NULL, SUBP = NULL,
                                sum_cols = NULL, mean_cols = NULL) {

  # --- Input validation ---
  n <- length(DBH)
  if (length(Stand) != n || length(Plot) != n || length(EXPF) != n) {
    stop("'Stand', 'Plot', 'DBH', and 'EXPF' must all be the same length.")
  }

  # Compute BA if not provided
  if (is.null(BA)) {
    BA <- inventoryfunctions::BA(DBH)
  }

  # --- Build working data frame ---
  df <- data.frame(
    Stand = Stand,
    Plot  = Plot,
    DBH   = as.numeric(DBH),
    EXPF  = as.numeric(EXPF),
    BA    = as.numeric(BA),
    stringsAsFactors = FALSE
  )

  if (!is.null(HT)) {
    df$HT <- as.numeric(HT)
  }
  if (!is.null(SUBP)) {
    df$SUBP <- SUBP
  }
  if (!is.null(Species)) {
    df$Species <- Species
  }

  # Add extra columns
  if (!is.null(sum_cols)) {
    for (nm in names(sum_cols)) {
      vals <- as.numeric(sum_cols[[nm]])
      if (length(vals) != n) stop("sum_cols$", nm, " must have length ", n)
      df[[paste0(".sum_", nm)]] <- vals
    }
  }
  if (!is.null(mean_cols)) {
    for (nm in names(mean_cols)) {
      vals <- as.numeric(mean_cols[[nm]])
      if (length(vals) != n) stop("mean_cols$", nm, " must have length ", n)
      df[[paste0(".mean_", nm)]] <- vals
    }
  }

  # --- Aggregate by Stand x Plot ---
  groups <- split(df, interaction(df$Stand, df$Plot, drop = TRUE))

  results <- lapply(groups, function(g) {
    tph  <- sum(g$EXPF, na.rm = TRUE)
    baph <- sum(g$BA * g$EXPF, na.rm = TRUE)
    qmd  <- sqrt(sum(g$DBH^2 * g$EXPF, na.rm = TRUE) / tph)

    out <- data.frame(
      Stand    = g$Stand[1],
      Plot     = g$Plot[1],
      TPH      = round(tph, 2),
      BAPH     = round(baph, 4),
      QMD      = round(qmd, 2),
      n_trees  = nrow(g),
      stringsAsFactors = FALSE
    )

    # Optional: mean height
    if (!is.null(HT)) {
      ht_valid <- !is.na(g$HT)
      out$mean_ht <- if (any(ht_valid)) {
        round(stats::weighted.mean(g$HT[ht_valid], g$EXPF[ht_valid]), 2)
      } else {
        NA_real_
      }
    }

    # Optional: n_subplots
    if (!is.null(SUBP)) {
      out$n_subplots <- length(unique(g$SUBP))
    }

    # Flexible sum columns (extensive: sum(col * EXPF))
    if (!is.null(sum_cols)) {
      for (nm in names(sum_cols)) {
        col_name <- paste0(".sum_", nm)
        out[[nm]] <- round(sum(g[[col_name]] * g$EXPF, na.rm = TRUE), 4)
      }
    }

    # Flexible mean columns (intensive: weighted.mean(col, EXPF))
    if (!is.null(mean_cols)) {
      for (nm in names(mean_cols)) {
        col_name <- paste0(".mean_", nm)
        valid <- !is.na(g[[col_name]])
        out[[nm]] <- if (any(valid)) {
          round(stats::weighted.mean(g[[col_name]][valid], g$EXPF[valid]), 4)
        } else {
          NA_real_
        }
      }
    }

    out
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}
