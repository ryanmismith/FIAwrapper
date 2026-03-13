#' FIA Microplot Regeneration Stocking
#'
#' Computes regeneration (sapling) metrics from FIA microplot data, including
#' trees per hectare, basal area per hectare, and stocking percentage based
#' on the proportion of microplots containing qualifying stems. Optionally
#' integrates with \code{\link{ducey_knapp_rd}} for VM0045 regeneration
#' stocking calculations.
#'
#' @param Stand Vector of stand identifiers.
#' @param Plot Vector of plot identifiers.
#' @param SUBP Vector of subplot identifiers (typically 1–4).
#' @param DBH Numeric vector of diameters at breast height in \strong{cm}.
#'   Trees below the microplot threshold (12.7 cm) are included as
#'   regeneration; larger trees are excluded.
#' @param Species Optional character vector of FVS species codes.
#' @param commercial_spp Optional character vector of FVS species codes
#'   considered commercial. When provided, \code{commercial_regen_tph}
#'   is computed using only these species.
#' @param microplot_radius_m Numeric. Microplot radius in meters.
#'   Default \code{2.07} (6.8 ft) per FIA national design.
#'
#' @details
#' ## Regeneration Filtering
#' Only trees with \code{DBH < 12.7 cm} (the FIA microplot threshold) are
#' included. These are saplings measured on microplots in the standard FIA
#' design. Seedlings (typically counted rather than measured) should be
#' handled separately.
#'
#' ## Stocking Percentage
#' \code{stocking_pct} is the proportion of microplots (subplots) that
#' contain at least one qualifying sapling stem, multiplied by 100:
#' \deqn{stocking\% = \frac{n_{occupied}}{n_{subplots}} \times 100}
#'
#' ## VM0045 Integration
#' For VM0045 IFM projects, regeneration stocking can also be quantified
#' using relative density via \code{\link{ducey_knapp_rd}} with
#' \code{tier = "regen"}. The \code{regen_tph} output from this function
#' provides the tree count complement to the RD-based stocking measure.
#'
#' @return A \code{tibble} with one row per Stand \eqn{\times} Plot:
#' \describe{
#'   \item{Stand}{Stand identifier.}
#'   \item{Plot}{Plot identifier.}
#'   \item{regen_tph}{Regeneration trees per hectare.}
#'   \item{regen_baph}{Regeneration basal area per hectare (m\eqn{^2}/ha).}
#'   \item{stocking_pct}{Percentage of microplots with \eqn{\ge 1} sapling.}
#'   \item{n_regen_trees}{Count of regeneration trees measured.}
#'   \item{n_subplots}{Number of subplots assessed.}
#'   \item{commercial_regen_tph}{TPH of commercial species only
#'     (when \code{commercial_spp} provided).}
#' }
#'
#' @seealso \code{\link{ducey_knapp_rd}}, \code{\link{fia_expansion}},
#'   \code{\link{fia_plot_aggregate}}
#' @family FIA Functions
#'
#' @examples
#' # Regeneration on a 4-subplot plot
#' stand <- rep(1, 6)
#' plot  <- rep(1, 6)
#' subp  <- c(1, 1, 2, 3, 3, 4)
#' dbh   <- c(3.5, 8.2, 5.1, 2.0, 15.0, 6.3)  # 15 cm tree excluded
#' spp   <- c("BF", "RM", "RS", "BF", "WP", "YB")
#'
#' fia_regen_stocking(stand, plot, subp, dbh, Species = spp,
#'                    commercial_spp = c("BF", "RS", "WP", "YB"))
#'
#' @export

fia_regen_stocking <- function(Stand, Plot, SUBP, DBH,
                                Species = NULL,
                                commercial_spp = NULL,
                                microplot_radius_m = 2.07) {

  # --- Input validation ---
  n <- length(DBH)
  if (length(Stand) != n || length(Plot) != n || length(SUBP) != n) {
    stop("'Stand', 'Plot', 'SUBP', and 'DBH' must all be the same length.")
  }

  DBH  <- as.numeric(DBH)
  SUBP <- as.character(SUBP)

  # --- Build working data frame ---
  df <- data.frame(
    Stand = Stand,
    Plot  = Plot,
    SUBP  = SUBP,
    DBH   = DBH,
    stringsAsFactors = FALSE
  )
  if (!is.null(Species)) {
    df$Species <- as.character(Species)
  }

  # Filter to microplot trees only (saplings)
  df <- df[df$DBH < 12.7 & !is.na(df$DBH) & df$DBH > 0, ]

  if (nrow(df) == 0) {
    # Return empty result with correct structure
    out <- data.frame(
      Stand = character(0), Plot = character(0),
      regen_tph = numeric(0), regen_baph = numeric(0),
      stocking_pct = numeric(0), n_regen_trees = integer(0),
      n_subplots = integer(0),
      stringsAsFactors = FALSE
    )
    if (!is.null(commercial_spp)) out$commercial_regen_tph <- numeric(0)
    return(tibble::as_tibble(out))
  }

  # Compute per-tree BA (m^2)
  df$BA <- BA(df$DBH)

  # --- Aggregate by Stand x Plot ---
  plot_groups <- split(df, interaction(df$Stand, df$Plot, drop = TRUE))

  # Also need total subplot count per plot (from original data, not filtered)
  orig_df <- data.frame(Stand = Stand, Plot = Plot, SUBP = as.character(SUBP),
                         stringsAsFactors = FALSE)
  total_subplots <- tapply(
    orig_df$SUBP,
    interaction(orig_df$Stand, orig_df$Plot, drop = TRUE),
    function(x) length(unique(x))
  )

  microplot_area_m2 <- pi * microplot_radius_m^2

  results <- lapply(plot_groups, function(g) {
    plot_key <- interaction(g$Stand[1], g$Plot[1], drop = TRUE)
    n_subplots <- as.integer(total_subplots[as.character(plot_key)])

    # Microplot EXPF for this plot
    micro_expf <- 10000 / (microplot_area_m2 * n_subplots)

    # Regen TPH and BAPH
    regen_tph  <- nrow(g) * micro_expf
    regen_baph <- sum(g$BA, na.rm = TRUE) * micro_expf

    # Stocking: proportion of subplots with >= 1 regen tree
    occupied <- length(unique(g$SUBP))
    stocking_pct <- (occupied / n_subplots) * 100

    out <- data.frame(
      Stand         = g$Stand[1],
      Plot          = g$Plot[1],
      regen_tph     = round(regen_tph, 2),
      regen_baph    = round(regen_baph, 4),
      stocking_pct  = round(stocking_pct, 1),
      n_regen_trees = nrow(g),
      n_subplots    = n_subplots,
      stringsAsFactors = FALSE
    )

    # Commercial species filter
    if (!is.null(commercial_spp) && !is.null(Species)) {
      comm_trees <- g[g$Species %in% commercial_spp, ]
      out$commercial_regen_tph <- round(nrow(comm_trees) * micro_expf, 2)
    }

    out
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}
