#' Summarize Regeneration from Microplot Data
#'
#' Computes regeneration (sapling) metrics from FIA microplot data: trees
#' per acre, basal area per acre, and stocking percentage. Filters to trees
#' < 5.0 inches DBH (the FIA microplot/subplot threshold).
#'
#' All inputs and outputs in **imperial units** (inches, feet, acres).
#'
#' @param stand Vector of stand identifiers.
#' @param plot Vector of plot identifiers.
#' @param subplot Vector of subplot identifiers (typically 1-4).
#' @param dbh Numeric vector of DBH in **inches**. Trees below 5.0 inches
#'   are included as regeneration; larger trees are excluded.
#' @param species Optional character vector of species codes.
#' @param commercial_species Optional character vector of commercial species
#'   codes. When provided, commercial regen TPA is computed separately.
#' @param microplot_radius_ft Numeric. Microplot radius in feet. Default
#'   6.8 (2.07 m) per FIA national design.
#'
#' @return A tibble with one row per stand x plot:
#' \describe{
#'   \item{stand}{Stand identifier.}
#'   \item{plot}{Plot identifier.}
#'   \item{regen_tpa}{Regeneration trees per acre.}
#'   \item{regen_ba_per_acre}{Regeneration BA per acre (ft^2/ac).}
#'   \item{stocking_pct}{Percent of subplots with >= 1 sapling.}
#'   \item{n_regen_trees}{Count of regen trees measured.}
#'   \item{n_subplots}{Number of subplots assessed.}
#'   \item{commercial_regen_tpa}{TPA of commercial species only (if provided).}
#' }
#'
#' @examples
#' summarize_regen(
#'   stand = rep(1, 6), plot = rep(1, 6),
#'   subplot = c(1, 1, 2, 3, 3, 4),
#'   dbh = c(1.5, 3.2, 2.0, 0.8, 6.0, 2.5),
#'   species = c("BF", "RM", "RS", "BF", "WP", "YB")
#' )
#'
#' @seealso [compute_tpa()], [get_plot_summary()]
#' @export
summarize_regen <- function(stand, plot, subplot, dbh,
                            species = NULL,
                            commercial_species = NULL,
                            microplot_radius_ft = 6.8) {

  n <- length(dbh)
  if (length(stand) != n || length(plot) != n || length(subplot) != n) {
    stop("stand, plot, subplot, and dbh must all be the same length.",
         call. = FALSE)
  }

  dbh     <- as.numeric(dbh)
  subplot <- as.character(subplot)

  df <- data.frame(
    stand = stand, plot = plot, subplot = subplot, dbh = dbh,
    stringsAsFactors = FALSE
  )
  if (!is.null(species)) df$species <- as.character(species)

  # Filter to microplot trees only (saplings < 5.0 inches)
  df_regen <- df[df$dbh < 5.0 & !is.na(df$dbh) & df$dbh > 0, ]

  if (nrow(df_regen) == 0) {
    out <- data.frame(
      stand = character(0), plot = character(0),
      regen_tpa = numeric(0), regen_ba_per_acre = numeric(0),
      stocking_pct = numeric(0), n_regen_trees = integer(0),
      n_subplots = integer(0), stringsAsFactors = FALSE
    )
    if (!is.null(commercial_species)) out$commercial_regen_tpa <- numeric(0)
    return(tibble::as_tibble(out))
  }

  # Compute per-tree BA in ft^2
  df_regen$ba <- ba_ft2(df_regen$dbh)

  # Aggregate by stand x plot
  plot_groups <- split(df_regen, interaction(df_regen$stand, df_regen$plot, drop = TRUE))

  # Total subplot count from original data (before filtering)
  orig_df <- data.frame(stand = stand, plot = plot, subplot = as.character(subplot),
                         stringsAsFactors = FALSE)
  total_subplots <- tapply(
    orig_df$subplot,
    interaction(orig_df$stand, orig_df$plot, drop = TRUE),
    function(x) length(unique(x))
  )

  microplot_area_ac <- (pi * microplot_radius_ft^2) / 43560

  results <- lapply(plot_groups, function(g) {
    plot_key <- interaction(g$stand[1], g$plot[1], drop = TRUE)
    n_subplots <- as.integer(total_subplots[as.character(plot_key)])

    micro_tpa <- 1 / (microplot_area_ac * n_subplots)

    regen_tpa      <- nrow(g) * micro_tpa
    regen_ba_ac    <- sum(g$ba, na.rm = TRUE) * micro_tpa
    occupied       <- length(unique(g$subplot))
    stocking_pct   <- (occupied / n_subplots) * 100

    out <- data.frame(
      stand          = g$stand[1],
      plot           = g$plot[1],
      regen_tpa      = round(regen_tpa, 2),
      regen_ba_per_acre = round(regen_ba_ac, 4),
      stocking_pct   = round(stocking_pct, 1),
      n_regen_trees  = nrow(g),
      n_subplots     = n_subplots,
      stringsAsFactors = FALSE
    )

    if (!is.null(commercial_species) && "species" %in% names(g)) {
      comm_trees <- g[g$species %in% commercial_species, ]
      out$commercial_regen_tpa <- round(nrow(comm_trees) * micro_tpa, 2)
    }

    out
  })

  result <- do.call(rbind, results)
  rownames(result) <- NULL
  tibble::as_tibble(result)
}
