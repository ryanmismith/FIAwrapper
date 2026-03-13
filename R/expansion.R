#' Compute Trees Per Acre Expansion Factors
#'
#' Calculates per-tree expansion factors (trees per acre) based on the FIA
#' national nested plot design. Trees >= 5.0 inches DBH are measured on
#' subplots (24.0 ft radius); smaller trees on microplots (6.8 ft radius).
#' The number of subplots is auto-detected from unique subplot values.
#'
#' @param dbh Numeric vector of diameters at breast height in **inches**.
#' @param subplot Integer or character vector identifying the subplot each tree
#'   was measured on (1-4 for standard FIA design).
#' @param cond_prop Optional numeric vector of unadjusted condition proportions
#'   (0-1). When provided, each tree's TPA is divided by its condition
#'   proportion to account for partial conditions on subplot boundaries
#'   (Bechtold & Patterson 2005, Eq. 4.1).
#' @param subplot_radius_ft Numeric. Subplot radius in feet. Default 24.0
#'   (7.32 m) per FIA national design.
#' @param microplot_radius_ft Numeric. Microplot radius in feet. Default 6.8
#'   (2.07 m) per FIA national design.
#' @param dbh_threshold Numeric. DBH threshold in inches separating subplot
#'   trees from microplot trees. Default 5.0 (12.7 cm).
#'
#' @details
#' ## FIA Plot Geometry
#' Standard FIA plots have 4 subplots arranged at 120-degree azimuths,
#' 120 ft (36.58 m) apart. Each subplot is a fixed-radius circle:
#' \itemize{
#'   \item **Subplot**: radius = 24.0 ft, area = 1,809.56 ft^2 (0.04154 ac)
#'   \item **Microplot**: radius = 6.8 ft, area = 145.27 ft^2 (0.003335 ac)
#' }
#'
#' The per-tree expansion factor is:
#' \deqn{TPA = \frac{1}{A_{plot\_acres} \times n_{subplots}}}
#'
#' For standard 4-subplot FIA clusters:
#' \itemize{
#'   \item Subplot TPA = 1 / (0.04154 * 4) = 6.018 TPA
#'   \item Microplot TPA = 1 / (0.003335 * 4) = 74.963 TPA
#' }
#'
#' @return Numeric vector of trees per acre expansion factors, same
#'   length as `dbh`.
#'
#' @references
#' Bechtold, W.A.; Patterson, P.L. (eds.) 2005. *The Enhanced Forest
#' Inventory and Analysis Program -- National Sampling Design and Estimation
#' Procedures.* Gen. Tech. Rep. SRS-80.
#'
#' @examples
#' # Mix of subplot and microplot trees on a standard 4-subplot plot
#' dbh <- c(10, 12, 2, 3, 16)
#' subp <- c(1, 2, 3, 4, 1)
#' compute_tpa(dbh, subp)
#'
#' # With condition proportion adjustment
#' compute_tpa(c(8, 6, 1.5), subplot = c(1, 2, 1),
#'             cond_prop = c(0.75, 1.0, 0.75))
#'
#' @seealso [build_tree_list()], [summarize_plot()]
#' @export
compute_tpa <- function(dbh, subplot,
                        cond_prop = NULL,
                        subplot_radius_ft = 24.0,
                        microplot_radius_ft = 6.8,
                        dbh_threshold = 5.0) {

  dbh     <- as.numeric(dbh)
  subplot <- as.character(subplot)
  n       <- length(dbh)

  if (length(subplot) != n) {
    stop("'dbh' and 'subplot' must be the same length.", call. = FALSE)
  }

  if (!is.null(cond_prop)) {
    cond_prop <- as.numeric(cond_prop)
    if (length(cond_prop) != n) {
      stop("'cond_prop' must be the same length as 'dbh'.", call. = FALSE)
    }
  }

  # Plot areas in acres (ft^2 / 43560)
  subplot_area_ac   <- (pi * subplot_radius_ft^2) / 43560
  microplot_area_ac <- (pi * microplot_radius_ft^2) / 43560

  n_subplots <- length(unique(subplot))

  # Assign TPA based on DBH threshold
  is_subplot <- dbh >= dbh_threshold
  tpa <- ifelse(
    is_subplot,
    1 / (subplot_area_ac * n_subplots),
    1 / (microplot_area_ac * n_subplots)
  )

  # Condition proportion adjustment
  if (!is.null(cond_prop)) {
    valid_cond <- !is.na(cond_prop) & cond_prop > 0
    tpa <- ifelse(valid_cond, tpa / cond_prop, tpa)
  }

  tpa
}
