#' Estimate Variance for FIA Plot-Level Attributes
#'
#' Computes variance, standard error, and confidence intervals for
#' plot-level FIA estimates. Supports simple random sampling (SRS) and
#' post-stratified estimation with Satterthwaite degrees of freedom.
#'
#' @param stand Vector of stand identifiers.
#' @param plot Vector of plot identifiers.
#' @param y Numeric vector of the attribute to estimate (e.g., BA/acre, TPA).
#' @param stratum Optional vector of stratum identifiers for post-stratified
#'   estimation.
#' @param stratum_weight Optional numeric vector of stratum weights (must
#'   sum to 1). Required when `stratum` is provided.
#' @param conf_level Confidence level for intervals. Default 0.90.
#'
#' @return A list with:
#' \describe{
#'   \item{mean}{Estimated population mean.}
#'   \item{se}{Standard error of the mean.}
#'   \item{ci_lower, ci_upper}{Confidence interval bounds.}
#'   \item{ci_hw}{Half-width of the confidence interval.}
#'   \item{precision_pct}{Precision as percentage of mean (ci_hw / mean * 100).}
#'   \item{n_plots}{Number of plots used.}
#'   \item{conf_level}{Confidence level used.}
#'   \item{method}{"SRS" or "post-stratified".}
#' }
#'
#' @examples
#' # Simple random sampling
#' estimate_variance(
#'   stand = rep(1, 10), plot = 1:10,
#'   y = c(120, 95, 140, 110, 88, 130, 105, 115, 125, 100)
#' )
#'
#' @seealso [get_plot_summary()]
#' @export
estimate_variance <- function(stand, plot, y,
                              stratum = NULL,
                              stratum_weight = NULL,
                              conf_level = 0.90) {

  n <- length(y)
  if (length(stand) != n || length(plot) != n) {
    stop("stand, plot, and y must be the same length.", call. = FALSE)
  }

  y <- as.numeric(y)
  valid <- !is.na(y)
  y <- y[valid]
  n_plots <- length(y)

  if (n_plots < 2) {
    stop("At least 2 plots required for variance estimation.", call. = FALSE)
  }

  if (is.null(stratum)) {
    # --- Simple Random Sampling ---
    y_bar <- mean(y)
    s2 <- stats::var(y)
    se <- sqrt(s2 / n_plots)
    df <- n_plots - 1
    method <- "SRS"
  } else {
    # --- Post-stratified estimation ---
    stratum <- stratum[valid]
    if (is.null(stratum_weight)) {
      stop("stratum_weight required for post-stratified estimation.", call. = FALSE)
    }

    strata <- unique(stratum)
    if (length(stratum_weight) != length(strata)) {
      stop("stratum_weight must have one value per stratum.", call. = FALSE)
    }
    names(stratum_weight) <- strata

    # Stratum means and variances
    stratum_stats <- lapply(strata, function(s) {
      ys <- y[stratum == s]
      list(
        mean = mean(ys),
        var  = if (length(ys) > 1) stats::var(ys) else 0,
        n    = length(ys),
        w    = stratum_weight[s]
      )
    })
    names(stratum_stats) <- strata

    # Weighted mean
    y_bar <- sum(sapply(stratum_stats, function(s) s$w * s$mean))

    # Combined variance (Cochran 1977)
    se2 <- sum(sapply(stratum_stats, function(s) {
      (s$w^2 * s$var) / s$n
    }))
    se <- sqrt(se2)

    # Satterthwaite df
    num <- se2^2
    denom <- sum(sapply(stratum_stats, function(s) {
      if (s$n > 1) {
        ((s$w^2 * s$var / s$n)^2) / (s$n - 1)
      } else 0
    }))
    df <- if (denom > 0) num / denom else n_plots - length(strata)
    method <- "post-stratified"
  }

  # Confidence interval
  alpha <- 1 - conf_level
  t_val <- stats::qt(1 - alpha / 2, df = df)
  ci_hw <- t_val * se

  list(
    mean         = y_bar,
    se           = se,
    ci_lower     = y_bar - ci_hw,
    ci_upper     = y_bar + ci_hw,
    ci_hw        = ci_hw,
    precision_pct = if (abs(y_bar) > 0) round(100 * ci_hw / abs(y_bar), 2) else NA_real_,
    n_plots      = n_plots,
    conf_level   = conf_level,
    method       = method
  )
}
