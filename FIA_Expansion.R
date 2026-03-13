#' FIA Expansion Factors from Nested Plot Design
#'
#' Computes per-tree expansion factors (trees per hectare) based on the FIA
#' national plot design. Trees \eqn{\ge} 12.7 cm (5 in.) DBH are measured on
#' subplots (7.32 m radius); smaller trees on microplots (2.07 m radius).
#' The number of subplots is auto-detected from unique \code{SUBP} values.
#'
#' @param DBH Numeric vector of diameters at breast height in \strong{cm}.
#' @param SUBP Integer or character vector identifying the subplot each tree
#'   was measured on (1–4 for standard FIA design).
#' @param CONDID Optional integer vector of condition class IDs. Not used
#'   for EXPF calculation directly, but retained for condition-level reporting.
#' @param CONDPROP_UNADJ Optional numeric vector of unadjusted condition
#'   proportions (0–1). When provided, each tree's EXPF is divided by its
#'   condition proportion to account for partial conditions on subplot
#'   boundaries (Bechtold & Patterson 2005, Eq. 4.1).
#' @param subplot_radius_m Numeric. Subplot radius in meters. Default
#'   \code{7.32} (24 ft) per FIA national design.
#' @param microplot_radius_m Numeric. Microplot radius in meters. Default
#'   \code{2.07} (6.8 ft) per FIA national design.
#' @param dbh_threshold_cm Numeric. DBH threshold separating subplot trees
#'   from microplot trees. Default \code{12.7} cm (5 in.).
#'
#' @details
#' ## FIA Plot Geometry (Bechtold & Patterson 2005)
#' Standard FIA plots have 4 subplots arranged at 120-degree azimuths,
#' 36.58 m (120 ft) apart. Each subplot is a fixed-radius circle:
#' \itemize{
#'   \item \strong{Subplot}: radius = 7.32 m, area = \eqn{\pi \times 7.32^2 = 168.27\;m^2}
#'   \item \strong{Microplot}: radius = 2.07 m, area = \eqn{\pi \times 2.07^2 = 13.47\;m^2}
#' }
#'
#' The per-tree expansion factor is:
#' \deqn{EXPF = \frac{10000}{A_{plot} \times n_{subplots}}}
#' where \eqn{A_{plot}} is the subplot or microplot area in m\eqn{^2} and
#' \eqn{n_{subplots}} is the number of measured subplots.
#'
#' For standard 4-subplot FIA clusters:
#' \itemize{
#'   \item Subplot EXPF = \eqn{10000 / (168.27 \times 4) = 14.86} TPH
#'   \item Microplot EXPF = \eqn{10000 / (13.47 \times 4) = 185.45} TPH
#' }
#'
#' ## Condition Proportion Adjustment
#' When a subplot straddles two or more condition classes (e.g., forest vs.
#' non-forest), the tree's EXPF is divided by the condition proportion to
#' correctly weight trees in partial-condition subplots:
#' \deqn{EXPF_{adj} = \frac{EXPF}{CONDPROP\_UNADJ}}
#'
#' @return Numeric vector of expansion factors (trees per hectare), same
#'   length as \code{DBH}.
#'
#' @references
#' Bechtold, W.A.; Patterson, P.L. (eds.) 2005. \emph{The Enhanced Forest
#' Inventory and Analysis Program — National Sampling Design and Estimation
#' Procedures.} Gen. Tech. Rep. SRS-80. Asheville, NC: USDA Forest Service,
#' Southern Research Station. 85 p.
#'
#' @seealso \code{\link{EXP.F}}, \code{\link{fia_plot_aggregate}},
#'   \code{\link{fia_prep_data}}
#' @family FIA Functions
#'
#' @examples
#' # Mix of subplot and microplot trees on a standard 4-subplot plot
#' dbh <- c(25, 30, 5, 8, 40)
#' subp <- c(1, 2, 3, 4, 1)
#' fia_expansion(dbh, subp)
#' # Trees >= 12.7 cm: ~14.86 TPH; Trees < 12.7 cm: ~185.45 TPH
#'
#' # Plot with only 3 measured subplots (subplot 2 hazardous)
#' fia_expansion(c(20, 15), SUBP = c(1, 3))
#' # EXPF = 10000 / (168.27 * 2) = ~29.72 TPH (only 2 unique subplots)
#'
#' # With condition proportion adjustment
#' fia_expansion(c(20, 15, 3), SUBP = c(1, 2, 1),
#'               CONDPROP_UNADJ = c(0.75, 1.0, 0.75))
#'
#' @export

fia_expansion <- function(DBH, SUBP,
                           CONDID = NULL,
                           CONDPROP_UNADJ = NULL,
                           subplot_radius_m = 7.32,
                           microplot_radius_m = 2.07,
                           dbh_threshold_cm = 12.7) {

  # --- Input validation ---
  DBH  <- as.numeric(DBH)
  SUBP <- as.character(SUBP)
  n    <- length(DBH)

  if (length(SUBP) != n) {
    stop("'DBH' and 'SUBP' must be the same length.")
  }

  if (!is.null(CONDPROP_UNADJ)) {
    CONDPROP_UNADJ <- as.numeric(CONDPROP_UNADJ)
    if (length(CONDPROP_UNADJ) != n) {
      stop("'CONDPROP_UNADJ' must be the same length as 'DBH'.")
    }
  }

  # --- Compute plot areas ---
  subplot_area_m2  <- pi * subplot_radius_m^2
  microplot_area_m2 <- pi * microplot_radius_m^2

  # Auto-detect number of subplots from unique SUBP values
  n_subplots <- length(unique(SUBP))

  # --- Assign EXPF based on DBH threshold ---
  is_subplot <- DBH >= dbh_threshold_cm

  expf <- ifelse(
    is_subplot,
    10000 / (subplot_area_m2 * n_subplots),
    10000 / (microplot_area_m2 * n_subplots)
  )

  # --- Condition proportion adjustment ---
  if (!is.null(CONDPROP_UNADJ)) {
    valid_cond <- !is.na(CONDPROP_UNADJ) & CONDPROP_UNADJ > 0
    expf <- ifelse(valid_cond, expf / CONDPROP_UNADJ, expf)
  }

  expf
}
