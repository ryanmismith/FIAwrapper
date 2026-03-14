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

#' Design-Based Population Estimates Using FIA POP Tables
#'
#' Computes design-based per-acre estimates (ratio-of-means estimator)
#' using the FIA stratified sampling design and POP tables. This produces
#' estimates comparable to EVALIDator and published FIA state fact sheets.
#'
#' Uses the Bechtold & Patterson (2005) estimation framework:
#' \deqn{\hat{R} = \frac{\sum_h w_h \bar{y}_h}{\sum_h w_h \bar{a}_h}}
#' where \eqn{\bar{y}_h} is the mean attribute per plot in stratum h,
#' \eqn{\bar{a}_h} is the mean forest area per plot, and
#' \eqn{w_h = n_h \times EXPNS_h}.
#'
#' @param fia_data Named list of FIA tables. Must include TREE, PLOT, COND,
#'   and POP tables (POP_STRATUM, POP_PLOT_STRATUM_ASSGN, POP_EVAL,
#'   POP_EVAL_TYP).
#' @param attribute Character. Per-acre attribute to estimate. One of:
#'   \code{"ba"} (basal area), \code{"tpa"} (trees per acre),
#'   \code{"volcfnet"} (net cubic-foot volume), \code{"volcfgrs"} (gross
#'   cubic-foot volume), \code{"volbfnet"} (net board-foot volume),
#'   \code{"drybio"} (dry aboveground biomass), \code{"carbon"}
#'   (aboveground carbon).
#' @param eval_type Character. Evaluation type. Default \code{"EXPCURR"}
#'   (current area/volume). Other options: \code{"EXPVOL"}, \code{"EXPALL"}.
#' @param conf_level Confidence level for intervals. Default 0.90.
#'
#' @return A list with:
#' \describe{
#'   \item{estimate}{Per-acre estimate (ratio estimator).}
#'   \item{se}{Approximate standard error.}
#'   \item{ci_lower, ci_upper}{Confidence interval bounds.}
#'   \item{n_plots}{Number of plots in evaluation.}
#'   \item{n_forest_plots}{Plots with forest conditions.}
#'   \item{total_forest_acres}{Estimated total forest area (acres).}
#'   \item{eval_descr}{Description of the evaluation used.}
#'   \item{method}{"design-based ratio estimator".}
#' }
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' estimate_population(fia, attribute = "ba")
#' estimate_population(fia, attribute = "tpa")
#' }
#'
#' @references
#' Bechtold, W.A.; Patterson, P.L. (eds.) 2005. *The Enhanced Forest
#' Inventory and Analysis Program -- National Sampling Design and Estimation
#' Procedures.* Gen. Tech. Rep. SRS-80.
#'
#' @seealso [estimate_variance()], [get_plot_summary()]
#' @export
estimate_population <- function(fia_data,
                                attribute = c("ba", "tpa", "volcfnet",
                                              "volcfgrs", "volbfnet",
                                              "drybio", "carbon"),
                                eval_type = "EXPCURR",
                                conf_level = 0.90) {

  attribute <- match.arg(attribute)

  # Validate POP tables exist
  required_pop <- c("POP_STRATUM", "POP_PLOT_STRATUM_ASSGN",
                     "POP_EVAL", "POP_EVAL_TYP")
  missing_pop <- setdiff(required_pop, names(fia_data))
  if (length(missing_pop) > 0) {
    stop("fia_data missing POP tables: ",
         paste(missing_pop, collapse = ", "),
         ". Design-based estimation requires POP tables from FIA DataMart.",
         call. = FALSE)
  }

  tree <- fia_data$TREE
  cond <- fia_data$COND
  pe   <- fia_data$POP_EVAL
  pet  <- fia_data$POP_EVAL_TYP
  ppsa <- fia_data$POP_PLOT_STRATUM_ASSGN
  pstr <- fia_data$POP_STRATUM

  # Find most recent evaluation of requested type
  eval_type <- toupper(eval_type)
  evals_of_type <- pet[pet$EVAL_TYP == eval_type, ]
  if (nrow(evals_of_type) == 0) {
    stop("No evaluations of type '", eval_type, "' found.", call. = FALSE)
  }
  eval_info <- merge(evals_of_type, pe[, c("CN", "EVALID", "EVAL_DESCR")],
                     by.x = "EVAL_CN", by.y = "CN")
  best_eval <- eval_info[which.max(eval_info$EVALID), ]

  # Get plots and strata for this evaluation
  ppsa_e <- ppsa[ppsa$EVALID == best_eval$EVALID, ]
  pstr_e <- pstr[pstr$EVALID == best_eval$EVALID, ]

  # Link plots to stratum weights
  ppsa_e <- merge(ppsa_e,
    pstr_e[, c("CN", "EXPNS", "ADJ_FACTOR_SUBP", "ADJ_FACTOR_MICR",
               "STRATUMCD", "P2POINTCNT")],
    by.x = "STRATUM_CN", by.y = "CN", suffixes = c("", ".strat"))

  # Forest conditions only
  cond_forest <- cond[!is.na(cond$COND_STATUS_CD) & cond$COND_STATUS_CD == 1, ]

  # Attribute column mapping
  attr_col <- switch(attribute,
    ba       = NULL,  # computed from DIA
    tpa      = NULL,  # computed from TPA_UNADJ
    volcfnet = "VOLCFNET",
    volcfgrs = "VOLCFGRS",
    volbfnet = "VOLBFNET",
    drybio   = "DRYBIO_AG",
    carbon   = "CARBON_AG"
  )

  # Compute per-plot values
  plot_vals <- lapply(seq_len(nrow(ppsa_e)), function(i) {
    plt_cn <- ppsa_e$PLT_CN[i]
    adj_sub <- ppsa_e$ADJ_FACTOR_SUBP[i]
    adj_mic <- ppsa_e$ADJ_FACTOR_MICR[i]
    expns   <- ppsa_e$EXPNS[i]
    strat   <- ppsa_e$STRATUMCD.strat[i]
    if (is.na(strat)) strat <- ppsa_e$STRATUMCD[i]

    # Forest conditions for this plot
    fc <- cond_forest[cond_forest$PLT_CN == plt_cn, ]
    forest_area <- if (nrow(fc) > 0) sum(fc$CONDPROP_UNADJ, na.rm = TRUE) else 0

    # Live trees on forest conditions
    pt <- tree[tree$PLT_CN == plt_cn & tree$STATUSCD == 1, ]
    if (nrow(pt) > 0 && nrow(fc) > 0) {
      pt <- merge(pt, fc[, c("PLT_CN", "CONDID", "CONDPROP_UNADJ")],
                  by = c("PLT_CN", "CONDID"))
    } else {
      pt <- pt[0, ]
    }

    y_val <- 0
    a_val <- forest_area  # adjusted forest area for denominator

    if (nrow(pt) > 0) {
      pt$ADJ <- ifelse(pt$DIA >= 5.0, adj_sub, adj_mic)

      if (attribute == "ba") {
        pt$BA_TREE <- pt$DIA^2 * 0.005454154
        y_val <- sum(pt$BA_TREE * pt$TPA_UNADJ * pt$ADJ * pt$CONDPROP_UNADJ,
                     na.rm = TRUE)
      } else if (attribute == "tpa") {
        y_val <- sum(pt$TPA_UNADJ * pt$ADJ * pt$CONDPROP_UNADJ, na.rm = TRUE)
      } else {
        tree_attr <- pt[[attr_col]]
        if (!is.null(tree_attr)) {
          y_val <- sum(tree_attr * pt$TPA_UNADJ * pt$ADJ * pt$CONDPROP_UNADJ,
                       na.rm = TRUE)
        }
      }
    }

    data.frame(stratum = strat, expns = expns,
               y = y_val, a = a_val, stringsAsFactors = FALSE)
  })

  plot_df <- do.call(rbind, plot_vals)

  # Ratio-of-means estimator by stratum
  strata <- unique(plot_df$stratum)
  strat_results <- lapply(strata, function(s) {
    d <- plot_df[plot_df$stratum == s, ]
    n_h <- nrow(d)
    w_h <- d$expns[1] * n_h  # stratum weight = EXPNS * n_plots
    y_bar <- mean(d$y)
    a_bar <- mean(d$a)
    list(w = w_h, y_bar = y_bar, a_bar = a_bar, n = n_h,
         y_var = if (n_h > 1) stats::var(d$y) else 0,
         a_var = if (n_h > 1) stats::var(d$a) else 0,
         ya_cov = if (n_h > 1) stats::cov(d$y, d$a) else 0,
         expns = d$expns[1])
  })

  # Combined ratio estimate
  total_y <- sum(sapply(strat_results, function(s) s$w * s$y_bar))
  total_a <- sum(sapply(strat_results, function(s) s$w * s$a_bar))
  ratio_est <- total_y / total_a

  # Variance of ratio estimator (delta method)
  var_num <- sum(sapply(strat_results, function(s) {
    (s$w^2 / s$n) * s$y_var
  }))
  var_den <- sum(sapply(strat_results, function(s) {
    (s$w^2 / s$n) * s$a_var
  }))
  cov_nd <- sum(sapply(strat_results, function(s) {
    (s$w^2 / s$n) * s$ya_cov
  }))

  se_ratio <- sqrt((var_num - 2 * ratio_est * cov_nd +
                      ratio_est^2 * var_den) / total_a^2)

  # Satterthwaite df (simplified)
  df <- max(sum(sapply(strat_results, function(s) s$n)) - length(strata), 1)

  alpha <- 1 - conf_level
  t_val <- stats::qt(1 - alpha / 2, df = df)
  ci_hw <- t_val * se_ratio

  # Total forest area in acres
  total_forest_acres <- total_a

  list(
    estimate         = ratio_est,
    se               = se_ratio,
    ci_lower         = ratio_est - ci_hw,
    ci_upper         = ratio_est + ci_hw,
    precision_pct    = if (abs(ratio_est) > 0) round(100 * ci_hw / abs(ratio_est), 2) else NA_real_,
    n_plots          = nrow(ppsa_e),
    n_forest_plots   = sum(plot_df$a > 0),
    total_forest_acres = total_forest_acres,
    eval_descr       = as.character(best_eval$EVAL_DESCR),
    conf_level       = conf_level,
    method           = "design-based ratio estimator"
  )
}
