#' FIA Post-Stratified Variance Estimator
#'
#' Computes variance, standard error, and confidence intervals for
#' plot-level FIA estimates. Supports simple random sampling (SRS) when
#' no strata are provided, and post-stratified estimation when stratum
#' assignments and weights are given.
#'
#' @param Stand Vector of stand identifiers. Used for grouping when
#'   multiple stands are present.
#' @param Plot Vector of plot identifiers. Each unique Stand \eqn{\times}
#'   Plot combination represents one sampling unit.
#' @param y Numeric vector of plot-level attribute values (e.g., BAPH,
#'   carbon_ha from \code{\link{fia_plot_aggregate}} output). Must be
#'   one value per plot.
#' @param stratum Optional character or integer vector of stratum
#'   assignments (one per plot). When provided with \code{stratum_weight},
#'   post-stratified estimation is used.
#' @param stratum_weight Optional named numeric vector of stratum area
#'   weights (proportions summing to 1, or areas that will be normalized).
#'   Names must match the unique values in \code{stratum}.
#' @param conf_level Confidence level for intervals. Default \code{0.90}
#'   (90\% CI, consistent with VM0045 requirements).
#'
#' @details
#' ## Simple Random Sampling (SRS)
#' When \code{stratum} is \code{NULL}:
#' \deqn{\bar{y} = \frac{1}{n} \sum y_i}
#' \deqn{SE = \frac{s}{\sqrt{n}}}
#' with \eqn{t}-distribution CIs at the specified confidence level.
#'
#' ## Post-Stratified Estimation
#' When \code{stratum} and \code{stratum_weight} are provided:
#' \deqn{\bar{y}_{ps} = \sum_h w_h \bar{y}_h}
#' \deqn{V(\bar{y}_{ps}) = \sum_h w_h^2 \frac{s_h^2}{n_h}}
#' where \eqn{w_h} is the proportion of total area in stratum \eqn{h}.
#' Degrees of freedom use the Satterthwaite approximation, and CIs use
#' the \eqn{t}-distribution.
#'
#' ## VM0045 Precision Test
#' The returned \code{precision_pct} is:
#' \deqn{Precision\% = \frac{CI_{hw}}{|\bar{y}|} \times 100}
#' A value \eqn{\le 10} indicates the sampling meets VM0045 Eq. 32
#' precision requirements.
#'
#' @return A named list with elements:
#' \describe{
#'   \item{mean}{Estimated population mean.}
#'   \item{se}{Standard error of the mean.}
#'   \item{ci_lower}{Lower confidence limit.}
#'   \item{ci_upper}{Upper confidence limit.}
#'   \item{ci_hw}{Half-width of the confidence interval.}
#'   \item{precision_pct}{CI half-width as percentage of mean
#'     (VM0045 precision metric).}
#'   \item{n_plots}{Number of plots used.}
#'   \item{conf_level}{Confidence level used.}
#'   \item{method}{Estimation method (\code{"SRS"} or \code{"post-stratified"}).}
#' }
#'
#' @references
#' Bechtold, W.A.; Patterson, P.L. (eds.) 2005. \emph{The Enhanced Forest
#' Inventory and Analysis Program — National Sampling Design and Estimation
#' Procedures.} Gen. Tech. Rep. SRS-80.
#'
#' @seealso \code{\link{carbon_variance}}, \code{\link{fia_plot_aggregate}},
#'   \code{\link{SampleSize}}
#' @family FIA Functions
#'
#' @examples
#' # SRS example
#' set.seed(42)
#' baph <- rnorm(20, mean = 25, sd = 8)
#' fia_variance(Stand = rep(1, 20), Plot = 1:20, y = baph)
#'
#' # Post-stratified example
#' strata <- c(rep("upland", 12), rep("wetland", 8))
#' weights <- c(upland = 0.65, wetland = 0.35)
#' fia_variance(Stand = rep(1, 20), Plot = 1:20, y = baph,
#'              stratum = strata, stratum_weight = weights)
#'
#' @export

fia_variance <- function(Stand, Plot, y,
                          stratum = NULL,
                          stratum_weight = NULL,
                          conf_level = 0.90) {

  # --- Input validation ---
  y <- as.numeric(y)
  n <- length(y)

  if (n < 2) stop("At least 2 plot-level values are required.")
  if (conf_level <= 0 || conf_level >= 1) {
    stop("'conf_level' must be between 0 and 1.")
  }

  alpha <- 1 - conf_level

  # --- SRS ---
  if (is.null(stratum) || is.null(stratum_weight)) {

    y_clean <- y[!is.na(y)]
    n_clean <- length(y_clean)
    if (n_clean < 2) stop("At least 2 non-NA values are required.")

    mn     <- mean(y_clean)
    se     <- stats::sd(y_clean) / sqrt(n_clean)
    t_crit <- stats::qt(1 - alpha / 2, df = n_clean - 1)
    hw     <- t_crit * se

    return(list(
      mean          = mn,
      se            = se,
      ci_lower      = mn - hw,
      ci_upper      = mn + hw,
      ci_hw         = hw,
      precision_pct = 100 * hw / abs(mn),
      n_plots       = n_clean,
      conf_level    = conf_level,
      method        = "SRS"
    ))
  }

  # --- Post-stratified estimation ---
  stratum <- as.character(stratum)
  if (length(stratum) != n) {
    stop("'stratum' must be the same length as 'y'.")
  }

  # Normalize weights to proportions
  stratum_ids <- unique(stratum[!is.na(y)])
  missing_strata <- setdiff(stratum_ids, names(stratum_weight))
  if (length(missing_strata) > 0) {
    stop("Missing stratum_weight for strata: ",
         paste(missing_strata, collapse = ", "))
  }

  w_raw <- stratum_weight[stratum_ids]
  w_h   <- w_raw / sum(w_raw)

  # Per-stratum statistics
  ybar_h <- numeric(length(stratum_ids))
  var_h  <- numeric(length(stratum_ids))
  n_h    <- integer(length(stratum_ids))

  for (j in seq_along(stratum_ids)) {
    h <- stratum_ids[j]
    vals <- y[stratum == h & !is.na(y)]
    n_h[j] <- length(vals)

    if (n_h[j] < 1) {
      warning("Stratum '", h, "' has no valid observations.")
      ybar_h[j] <- 0
      var_h[j]  <- 0
      next
    }
    if (n_h[j] < 2) {
      warning("Stratum '", h, "' has only 1 plot. Variance set to 0.")
    }

    ybar_h[j] <- mean(vals)
    var_h[j]  <- if (n_h[j] >= 2) stats::var(vals) else 0
  }

  # Post-stratified mean and variance
  mn_ps <- sum(w_h * ybar_h)
  V_ps  <- sum(w_h^2 * var_h / pmax(n_h, 1))
  se_ps <- sqrt(V_ps)

  # Satterthwaite df
  df_components <- w_h^2 * var_h / pmax(n_h, 1)
  df_satt <- if (sum(df_components) > 0) {
    sum(df_components)^2 / sum(df_components^2 / pmax(n_h - 1, 1))
  } else {
    sum(n_h) - length(stratum_ids)
  }
  df_satt <- max(df_satt, 1)

  t_crit <- stats::qt(1 - alpha / 2, df = df_satt)
  hw     <- t_crit * se_ps

  list(
    mean          = mn_ps,
    se            = se_ps,
    ci_lower      = mn_ps - hw,
    ci_upper      = mn_ps + hw,
    ci_hw         = hw,
    precision_pct = 100 * hw / abs(mn_ps),
    n_plots       = sum(n_h),
    conf_level    = conf_level,
    method        = "post-stratified"
  )
}
