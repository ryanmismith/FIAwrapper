#' Summarize Plot from Individual Tree Data
#'
#' Aggregates individual tree records to plot-level summaries. Imperial units:
#' TPA, BA/acre (ft^2/ac), QMD (inches). This is an alternative to
#' [get_plot_summary()] for when you have raw vectors instead of a tree list
#' data frame.
#'
#' @param stand Vector of stand identifiers.
#' @param plot Vector of plot identifiers.
#' @param dbh Numeric vector of DBH in **inches**.
#' @param height Optional numeric vector of total height in **feet**.
#' @param tpa Numeric vector of trees per acre expansion factors.
#' @param species Optional character vector of species codes.
#' @param ba Optional numeric vector of per-tree basal area in ft^2.
#'   If NULL, computed from `dbh` via [ba_ft2()].
#' @param subplot Optional vector of subplot IDs. If provided, the number
#'   of unique subplots per plot is included in output.
#' @param sum_cols Optional named list of numeric vectors to aggregate as
#'   sum(col * tpa) (per-acre extensive attributes).
#' @param mean_cols Optional named list of numeric vectors to aggregate as
#'   weighted.mean(col, tpa) (intensive attributes).
#'
#' @return A tibble with one row per stand x plot:
#' \describe{
#'   \item{stand}{Stand identifier.}
#'   \item{plot}{Plot identifier.}
#'   \item{tpa}{Total trees per acre.}
#'   \item{ba_per_acre}{Basal area per acre (ft^2/ac).}
#'   \item{qmd}{Quadratic mean diameter (inches).}
#'   \item{mean_height}{Mean height in feet (if provided).}
#'   \item{n_subplots}{Number of subplots (if subplot provided).}
#' }
#'
#' @examples
#' summarize_plot(
#'   stand = rep("A", 5),
#'   plot = rep(1, 5),
#'   dbh = c(8, 10, 12, 6, 14),
#'   tpa = rep(6.018, 5)
#' )
#'
#' @seealso [get_plot_summary()], [compute_tpa()]
#' @export
summarize_plot <- function(stand, plot, dbh, height = NULL, tpa,
                           species = NULL, ba = NULL, subplot = NULL,
                           sum_cols = NULL, mean_cols = NULL) {

  n <- length(dbh)
  if (length(stand) != n || length(plot) != n || length(tpa) != n) {
    stop("stand, plot, dbh, and tpa must all be the same length.", call. = FALSE)
  }

  dbh <- as.numeric(dbh)
  tpa <- as.numeric(tpa)

  if (is.null(ba)) {
    ba <- ba_ft2(dbh)
  }

  df <- data.frame(
    stand = stand, plot = plot, dbh = dbh, tpa = tpa, ba = ba,
    stringsAsFactors = FALSE
  )
  if (!is.null(height)) df$height <- as.numeric(height)
  if (!is.null(species)) df$species <- as.character(species)
  if (!is.null(subplot)) df$subplot <- as.character(subplot)

  # Add custom columns
  if (!is.null(sum_cols)) {
    for (nm in names(sum_cols)) df[[nm]] <- as.numeric(sum_cols[[nm]])
  }
  if (!is.null(mean_cols)) {
    for (nm in names(mean_cols)) df[[nm]] <- as.numeric(mean_cols[[nm]])
  }

  # Aggregate by stand x plot
  plot_groups <- split(df, interaction(df$stand, df$plot, drop = TRUE))

  results <- lapply(plot_groups, function(g) {
    out <- data.frame(
      stand       = g$stand[1],
      plot        = g$plot[1],
      tpa         = sum(g$tpa, na.rm = TRUE),
      ba_per_acre = sum(g$ba * g$tpa, na.rm = TRUE),
      qmd         = qmd(g$dbh, g$tpa),
      stringsAsFactors = FALSE
    )

    if ("height" %in% names(g)) {
      valid <- !is.na(g$height) & !is.na(g$tpa) & g$tpa > 0
      out$mean_height <- if (any(valid)) {
        stats::weighted.mean(g$height[valid], g$tpa[valid])
      } else NA_real_
    }

    if ("subplot" %in% names(g)) {
      out$n_subplots <- length(unique(g$subplot))
    }

    # Custom sum columns
    if (!is.null(sum_cols)) {
      for (nm in names(sum_cols)) {
        out[[nm]] <- sum(g[[nm]] * g$tpa, na.rm = TRUE)
      }
    }

    # Custom mean columns
    if (!is.null(mean_cols)) {
      for (nm in names(mean_cols)) {
        valid <- !is.na(g[[nm]]) & !is.na(g$tpa) & g$tpa > 0
        out[[nm]] <- if (any(valid)) {
          stats::weighted.mean(g[[nm]][valid], g$tpa[valid])
        } else NA_real_
      }
    }

    out
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}
