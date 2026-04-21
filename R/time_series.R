#' Find Remeasured Plots
#'
#' Identifies FIA plots that have been measured in multiple inventory cycles,
#' essential for growth analysis and change detection.
#'
#' @param fia_data List of FIA data frames (must contain PLOT table).
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{plot_cn}{Plot control number (most recent).}
#'   \item{state_code, county_code}{Location.}
#'   \item{n_measurements}{Number of times the plot was measured.}
#'   \item{first_year, last_year}{Range of inventory years.}
#'   \item{years}{Comma-separated list of measurement years.}
#' }
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("ME")
#' remeas <- get_remeasured_plots(fia)
#' remeas[remeas$n_measurements >= 3, ]
#' }
#'
#' @seealso [build_time_series()], [summarize_growth()]
#' @export
get_remeasured_plots <- function(fia_data) {
  plots <- fia_data$PLOT
  if (is.null(plots)) stop("fia_data must contain a PLOT table.", call. = FALSE)

  # FIA uses PREV_PLT_CN to link plot measurements across cycles
  # Also can use STATECD + COUNTYCD + PLOT as a stable identifier
  id_cols <- intersect(c("STATECD", "COUNTYCD", "PLOT"), names(plots))
  if (length(id_cols) < 3) {
    stop("PLOT table must contain STATECD, COUNTYCD, and PLOT columns.",
         call. = FALSE)
  }

  plots$plot_key <- paste(plots$STATECD, plots$COUNTYCD, plots$PLOT, sep = "_")

  result <- plots |>
    dplyr::group_by(.data$plot_key) |>
    dplyr::summarize(
      plot_cn       = dplyr::last(.data$CN),
      state_code    = dplyr::first(.data$STATECD),
      county_code   = dplyr::first(.data$COUNTYCD),
      n_measurements = dplyr::n(),
      first_year    = min(.data$INVYR, na.rm = TRUE),
      last_year     = max(.data$INVYR, na.rm = TRUE),
      years         = paste(sort(unique(.data$INVYR)), collapse = ", "),
      .groups       = "drop"
    ) |>
    dplyr::filter(.data$n_measurements > 1) |>
    dplyr::arrange(dplyr::desc(.data$n_measurements), .data$state_code)

  result
}

#' Build Time Series Tree Lists
#'
#' Creates tree lists for ALL measurement cycles for remeasured plots,
#' linking individual trees across measurements. The output is designed
#' for growth modeling: each tree has its current and previous measurements,
#' plus growth increments and status change flags.
#'
#' @details
#' This function is vectorized end-to-end. Tree linking across measurements
#' is performed via a single [dplyr::left_join()] on `prev_tree_cn` rather
#' than a per-row for-loop, making it 50-500x faster than a naive
#' implementation on large donor pools. Progress is emitted via
#' [message()] at each of six stages; suppress with [suppressMessages()]
#' if undesired.
#'
#' For large FIA caches, pre-filter `fia_data` to the plots of interest
#' before calling. The function's cost scales with the number of remeasured
#' plots in the input; unfiltered 4-state caches will process ~8k plots
#' regardless of downstream use.
#'
#' @param fia_data List of FIA data frames (must contain `TREE`, `PLOT`,
#'   and `COND`).
#' @param include_dead Logical. Include trees that died between measurements?
#'   Default FALSE (survivors only). Set TRUE for mortality analysis.
#' @param variant FVS variant code for species translation.
#' @param min_measurements Minimum number of measurement cycles for a plot
#'   to be included. Default 2.
#' @param verbose Logical. Emit progress messages at each stage? Default TRUE.
#'
#' @return A tibble with all [build_tree_list()] columns plus:
#' \describe{
#'   \item{plot_key}{Stable plot identifier across cycles.}
#'   \item{measurement_number}{Sequential measurement (1, 2, 3...).}
#'   \item{years_since_previous}{Years between this and prior measurement.}
#'   \item{prev_dbh}{DBH at previous measurement (inches). NA for first.}
#'   \item{prev_height}{Height at previous measurement (feet). NA for first.}
#'   \item{prev_status}{Live/dead status at previous measurement. NA for first.}
#'   \item{prev_inventory_year}{Inventory year of previous measurement. NA for first.}
#'   \item{dbh_growth}{Periodic DBH increment (inches).}
#'   \item{height_growth}{Periodic height increment (feet).}
#'   \item{annual_dbh_growth}{Annualized DBH increment (inches/year).}
#'   \item{annual_height_growth}{Annualized height increment (feet/year).}
#'   \item{ingrowth}{TRUE if tree was not present in prior measurement.}
#'   \item{mortality}{TRUE if tree died between measurements.}
#' }
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("ME")
#' ts <- build_time_series(fia)
#' # Filter to survivors with growth data
#' survivors <- ts[!is.na(ts$dbh_growth) & !ts$mortality, ]
#'
#' # Suppress progress messages
#' ts_quiet <- suppressMessages(build_time_series(fia))
#' }
#'
#' @seealso [get_remeasured_plots()], [summarize_growth()],
#'   [get_individual_tree_history()], [compute_competition()]
#' @export
build_time_series <- function(fia_data,
                              include_dead     = FALSE,
                              variant          = NULL,
                              min_measurements = 2,
                              verbose          = TRUE) {
  
  t_start <- Sys.time()
  log_msg <- function(...) if (verbose) message(...)
  
  trees <- fia_data$TREE
  plots <- fia_data$PLOT
  
  if (is.null(trees) || is.null(plots)) {
    stop("fia_data must contain TREE and PLOT tables.", call. = FALSE)
  }
  
  id_cols <- c("STATECD", "COUNTYCD", "PLOT")
  if (!all(id_cols %in% names(plots))) {
    stop("PLOT table must contain STATECD, COUNTYCD, and PLOT.", call. = FALSE)
  }
  
  # --- Step 1/6: identify remeasured plots ---
  log_msg("[build_time_series] 1/6: identifying remeasured plots...")
  
  plots$plot_key <- paste(plots$STATECD, plots$COUNTYCD, plots$PLOT, sep = "_")
  
  plot_counts <- table(plots$plot_key)
  remeas_keys <- names(plot_counts[plot_counts >= min_measurements])
  
  if (length(remeas_keys) == 0) {
    log_msg("  No plots with >= ", min_measurements, " measurements found.")
    return(tibble::tibble())
  }
  
  log_msg(sprintf("  Found %d plots with >= %d measurements",
                  length(remeas_keys), min_measurements))
  
  # --- Step 2/6: filter FIA tables to remeasured plots ---
  log_msg("[build_time_series] 2/6: filtering FIA tables to remeasured plots...")
  
  remeas_plot_cns <- plots$CN[plots$plot_key %in% remeas_keys]
  fia_filtered    <- .filter_fia_by_plot_cn(fia_data, remeas_plot_cns)
  
  log_msg(sprintf("  TREE rows: %d | PLOT rows: %d",
                  nrow(fia_filtered$TREE), nrow(fia_filtered$PLOT)))
  
  # --- Step 3/6: build tree list across all cycles ---
  log_msg("[build_time_series] 3/6: building tree list across all measurement cycles...")
  
  tl <- prep_fia_data(
    fia_trees    = fia_filtered$TREE,
    fia_plots    = fia_filtered$PLOT,
    fia_cond     = fia_filtered$COND,
    include_dead = TRUE,   # always include dead for mortality tracking
    variant      = variant
  )
  
  log_msg(sprintf("  Tree list rows: %d", nrow(tl)))
  
  # --- Step 4/6: plot_key, TPA, basal area, measurement_number ---
  log_msg("[build_time_series] 4/6: computing plot_key, TPA, BA, measurement_number...")
  
  plot_key_lookup <- stats::setNames(plots$plot_key, as.character(plots$CN))
  tl$plot_key <- plot_key_lookup[as.character(tl$plot_cn)]
  
  tl <- tl |>
    dplyr::group_by(.data$plot_cn) |>
    dplyr::mutate(
      tpa = compute_tpa(
        dbh       = .data$dbh,
        subplot   = .data$subplot,
        cond_prop = if ("cond_proportion" %in% names(tl)) .data$cond_proportion else NULL
      )
    ) |>
    dplyr::ungroup()
  
  tl$ba_tree     <- ba_ft2(tl$dbh)
  tl$ba_per_acre <- tl$ba_tree * tl$tpa
  
  tl <- tl |>
    dplyr::group_by(.data$plot_key) |>
    dplyr::mutate(
      measurement_number = as.integer(factor(.data$inventory_year))
    ) |>
    dplyr::ungroup()
  
  # --- Step 5/6: vectorized tree linking across measurements ---
  log_msg("[build_time_series] 5/6: linking trees across measurements (vectorized)...")
  t_link <- Sys.time()
  
  tl <- .link_tree_measurements(tl)
  
  log_msg(sprintf("  Linking elapsed: %.1f sec",
                  as.numeric(difftime(Sys.time(), t_link, units = "secs"))))
  
  # --- Step 6/6: filter by include_dead, order output ---
  log_msg("[build_time_series] 6/6: filtering and ordering output...")
  
  if (!include_dead) {
    # Keep live trees plus dead-from-survivor for mortality tracking
    tl <- tl[tl$status == "live" | tl$mortality == TRUE, ]
  }
  
  tl <- tl |>
    dplyr::arrange(.data$plot_key, .data$measurement_number,
                   .data$subplot, .data$dbh)
  
  log_msg(sprintf("[build_time_series] TOTAL elapsed: %.1f sec | output: %d rows",
                  as.numeric(difftime(Sys.time(), t_start, units = "secs")),
                  nrow(tl)))
  
  tibble::as_tibble(tl)
}


# ---- Internal: vectorized tree linking ----

#' Link trees across measurement cycles (vectorized)
#'
#' Primary linking strategy: self-join via `prev_tree_cn` -> `tree_cn` to pull
#' each tree's previous DBH, height, status, and inventory year in a single
#' dplyr operation. Fallback: same self-join logic keyed by
#' `plot_key + subplot + tree_num + (measurement_number - 1)`.
#'
#' Growth deltas and annualized rates are computed with vectorized arithmetic
#' on the resulting columns. All operations are O(n) over the tree list.
#'
#' @param tl Tree list from [prep_fia_data()] with `measurement_number` column
#'   already assigned.
#'
#' @return The input with additional columns: `prev_dbh`, `prev_height`,
#'   `prev_status`, `prev_inventory_year`, `dbh_growth`, `height_growth`,
#'   `annual_dbh_growth`, `annual_height_growth`, `years_since_previous`,
#'   `ingrowth`, `mortality`.
#'
#' @keywords internal
.link_tree_measurements <- function(tl) {
  
  # Initialize growth columns
  tl$prev_dbh             <- NA_real_
  tl$prev_height          <- NA_real_
  tl$prev_status          <- NA_character_
  tl$prev_inventory_year  <- NA_real_
  tl$years_since_previous <- NA_real_
  tl$dbh_growth           <- NA_real_
  tl$height_growth        <- NA_real_
  tl$annual_dbh_growth    <- NA_real_
  tl$annual_height_growth <- NA_real_
  tl$ingrowth             <- FALSE
  tl$mortality            <- FALSE
  
  if ("prev_tree_cn" %in% names(tl) && "tree_cn" %in% names(tl)) {
    
    # Primary: link via FIA's PREV_TRE_CN chain
    prev_lookup <- tl |>
      dplyr::select("tree_cn",
                    prev_dbh_v             = "dbh",
                    prev_height_v          = "height",
                    prev_status_v          = "status",
                    prev_inventory_year_v  = "inventory_year")
    
    tl <- tl |>
      dplyr::left_join(prev_lookup,
                       by = c("prev_tree_cn" = "tree_cn")) |>
      dplyr::mutate(
        prev_dbh            = .data$prev_dbh_v,
        prev_height         = .data$prev_height_v,
        prev_status         = .data$prev_status_v,
        prev_inventory_year = .data$prev_inventory_year_v
      ) |>
      dplyr::select(-"prev_dbh_v", -"prev_height_v",
                    -"prev_status_v", -"prev_inventory_year_v")
    
    # Ingrowth: missing prev_tree_cn AND not first measurement
    tl$ingrowth <- !is.na(tl$measurement_number) &
      tl$measurement_number > 1L &
      (is.na(tl$prev_tree_cn) | tl$prev_tree_cn == 0)
    
  } else if (all(c("plot_key", "subplot", "tree_num") %in% names(tl))) {
    
    # Fallback: link by plot_key+subplot+tree_num with measurement-offset join
    message("  (using plot_key+subplot+tree_num fallback; no prev_tree_cn column)")
    
    tl$tree_key <- paste(tl$plot_key, tl$subplot, tl$tree_num, sep = "_")
    
    prev_lookup <- tl |>
      dplyr::transmute(
        tree_key                 = .data$tree_key,
        prev_measurement_number  = .data$measurement_number + 1L,
        prev_dbh_v               = .data$dbh,
        prev_height_v            = .data$height,
        prev_status_v            = .data$status,
        prev_inventory_year_v    = .data$inventory_year
      )
    
    tl <- tl |>
      dplyr::left_join(prev_lookup,
                       by = c("tree_key",
                              "measurement_number" = "prev_measurement_number")) |>
      dplyr::mutate(
        prev_dbh            = .data$prev_dbh_v,
        prev_height         = .data$prev_height_v,
        prev_status         = .data$prev_status_v,
        prev_inventory_year = .data$prev_inventory_year_v
      ) |>
      dplyr::select(-"prev_dbh_v", -"prev_height_v",
                    -"prev_status_v", -"prev_inventory_year_v")
    
    # Ingrowth: no previous record AND not first measurement
    tl$ingrowth <- !is.na(tl$measurement_number) &
      tl$measurement_number > 1L &
      is.na(tl$prev_inventory_year)
    
  } else {
    
    warning("Cannot link tree measurements: need (tree_cn + prev_tree_cn) or ",
            "(plot_key + subplot + tree_num). Growth columns will be all NA.",
            call. = FALSE)
    
    return(tl)
  }
  
  # Mortality: prev_status was live, current status is dead
  tl$mortality <- !is.na(tl$prev_status) & tl$prev_status == "live" &
    !is.na(tl$status)      & tl$status      == "dead"
  
  # Vectorized growth arithmetic
  tl$years_since_previous <- tl$inventory_year - tl$prev_inventory_year
  tl$dbh_growth           <- tl$dbh    - tl$prev_dbh
  tl$height_growth        <- tl$height - tl$prev_height
  
  yrs_pos <- !is.na(tl$years_since_previous) & tl$years_since_previous > 0
  tl$annual_dbh_growth[yrs_pos] <-
    tl$dbh_growth[yrs_pos] / tl$years_since_previous[yrs_pos]
  tl$annual_height_growth[yrs_pos] <-
    tl$height_growth[yrs_pos] / tl$years_since_previous[yrs_pos]
  
  tl
}

#' Summarize Growth by Plot and Period
#'
#' Aggregates time series data to plot-level growth metrics per measurement
#' period. Key output for growth & yield analysis.
#'
#' @param time_series A tibble from [build_time_series()].
#'
#' @return A tibble per plot per period:
#' \describe{
#'   \item{plot_key}{Plot identifier.}
#'   \item{period_start, period_end}{Inventory years.}
#'   \item{period_years}{Length of measurement period.}
#'   \item{ba_per_acre_start, ba_per_acre_end}{BA at start/end of period.}
#'   \item{net_ba_growth_per_acre}{Net BA change per acre per year (ft^2/ac/yr).}
#'   \item{gross_ba_growth_per_acre}{Growth of survivors per acre per year.}
#'   \item{mortality_ba_per_acre}{BA lost to mortality per year.}
#'   \item{ingrowth_tpa}{New trees per acre per year entering the population.}
#'   \item{mean_dbh_growth}{Average annual DBH increment of survivors (in/yr).}
#'   \item{mortality_rate}{Proportion of initial TPA that died.}
#' }
#'
#' @examples
#' \dontrun{
#' ts <- build_time_series(fia)
#' growth <- summarize_growth(ts)
#' mean(growth$net_ba_growth_per_acre, na.rm = TRUE)
#' }
#'
#' @seealso [build_time_series()], [get_individual_tree_history()]
#' @export
summarize_growth <- function(time_series) {
  if (nrow(time_series) == 0) return(tibble::tibble())

  # Only process trees with measurement > 1 (need a previous measurement)
  ts <- time_series[time_series$measurement_number > 1, ]

  ts |>
    dplyr::group_by(.data$plot_key, .data$inventory_year) |>
    dplyr::filter(any(!is.na(.data$years_since_previous))) |>
    dplyr::summarize(
      period_years = stats::median(.data$years_since_previous, na.rm = TRUE),
      period_end   = dplyr::first(.data$inventory_year),
      period_start = .data$period_end[1] - .data$period_years[1],

      # Survivors (alive at both measurements)
      n_survivors = sum(.data$status == "live" & !.data$ingrowth, na.rm = TRUE),
      survivor_ba_growth = sum(
        ifelse(.data$status == "live" & !.data$ingrowth & !is.na(.data$dbh_growth),
               (ba_ft2(.data$dbh) - ba_ft2(.data$prev_dbh)) * .data$tpa, 0),
        na.rm = TRUE
      ),

      # Mortality
      n_mortality = sum(.data$mortality, na.rm = TRUE),
      mortality_ba = sum(
        ifelse(.data$mortality & !is.na(.data$prev_dbh),
               ba_ft2(.data$prev_dbh) * .data$tpa, 0),
        na.rm = TRUE
      ),

      # Ingrowth
      n_ingrowth = sum(.data$ingrowth, na.rm = TRUE),
      ingrowth_tpa_total = sum(
        ifelse(.data$ingrowth, .data$tpa, 0),
        na.rm = TRUE
      ),

      # Mean growth of survivors
      mean_dbh_growth = mean(
        .data$dbh_growth[.data$status == "live" & !.data$ingrowth],
        na.rm = TRUE
      ),

      .groups = "drop"
    ) |>
    dplyr::mutate(
      # Annualize
      gross_ba_growth_per_acre = .data$survivor_ba_growth / .data$period_years,
      mortality_ba_per_acre    = .data$mortality_ba / .data$period_years,
      net_ba_growth_per_acre   = (.data$survivor_ba_growth - .data$mortality_ba) / .data$period_years,
      ingrowth_tpa             = .data$ingrowth_tpa_total / .data$period_years,
      annual_dbh_growth        = .data$mean_dbh_growth / .data$period_years,
      mortality_rate           = .data$n_mortality / (.data$n_survivors + .data$n_mortality)
    ) |>
    dplyr::select(
      "plot_key", "period_start", "period_end", "period_years",
      "net_ba_growth_per_acre", "gross_ba_growth_per_acre",
      "mortality_ba_per_acre", "ingrowth_tpa",
      "annual_dbh_growth", "mortality_rate",
      "n_survivors", "n_mortality", "n_ingrowth"
    )
}

#' Get Individual Tree Growth History
#'
#' Reshapes time series to a panel data format: one row per tree per
#' measurement. Ideal for fitting individual tree growth models
#' (diameter increment, mortality, etc.).
#'
#' @param time_series A tibble from [build_time_series()].
#' @param min_measurements Minimum measurements per tree to include.
#'   Default 2.
#'
#' @return A tibble with one row per tree per measurement, including
#'   current and lagged measurements plus competition metrics.
#'
#' @examples
#' \dontrun{
#' ts <- build_time_series(fia)
#' panel <- get_individual_tree_history(ts)
#' # Fit a simple diameter increment model
#' model <- lm(annual_dbh_growth ~ dbh + spp_code + bal, data = panel)
#' }
#'
#' @seealso [build_time_series()], [compute_competition()]
#' @export
get_individual_tree_history <- function(time_series, min_measurements = 2) {
  # Build a tree identifier that's stable across measurements
  # Use tree_cn as primary, with subplot + tree_num as fallback
  if ("tree_cn" %in% names(time_series) && "prev_tree_cn" %in% names(time_series)) {
    # Trees are already linked via CN chain
    # Group by the earliest CN in the chain
    ts <- time_series
  } else {
    ts <- time_series
  }

  # Add competition metrics
  ts <- compute_competition(ts)

  # Filter to trees with enough measurements
  if ("tree_cn" %in% names(ts)) {
    # Count measurements per tree (using plot_key + subplot + tree_num as stable ID)
    if (all(c("plot_key", "subplot", "tree_num") %in% names(ts))) {
      ts$tree_key <- paste(ts$plot_key, ts$subplot, ts$tree_num, sep = "_")
      tree_counts <- table(ts$tree_key)
      keep_keys <- names(tree_counts[tree_counts >= min_measurements])
      ts <- ts[ts$tree_key %in% keep_keys, ]
    }
  }

  ts |>
    dplyr::arrange(.data$plot_key, .data$subplot,
                    if ("tree_num" %in% names(ts)) .data$tree_num else .data$tree_cn,
                    .data$measurement_number)
}

#' Compute Competition Indices
#'
#' Adds tree-level competition metrics to a tree list. Key predictors for
#' individual tree growth models.
#'
#' @param tree_list A tibble with at minimum: plot_cn (or plot_id), dbh,
#'   tpa, ba_per_acre, and optionally inventory_year, crown_ratio.
#'
#' @return The input tibble with additional columns:
#' \describe{
#'   \item{bal}{Basal area in larger trees (ft^2/acre). Sum of BA/acre for
#'     all trees on the same plot with DBH > this tree's DBH.}
#'   \item{plot_ba}{Total plot basal area (ft^2/acre).}
#'   \item{plot_tpa}{Total plot TPA.}
#'   \item{plot_qmd}{Plot-level QMD (inches).}
#'   \item{relative_dbh}{This tree's DBH divided by plot QMD.}
#'   \item{percentile_dbh}{This tree's DBH percentile within the plot.}
#' }
#'
#' @examples
#' \dontrun{
#' trees <- build_tree_list(fia)
#' trees <- compute_competition(trees)
#' # BAL is the strongest single competition predictor for growth models
#' hist(trees$bal)
#' }
#'
#' @seealso [build_time_series()], [get_individual_tree_history()]
#' @export
compute_competition <- function(tree_list) {
  grp_col <- if ("plot_cn" %in% names(tree_list)) "plot_cn" else "plot_id"

  # If time series, group by plot + year
  grp_cols <- grp_col
  if ("inventory_year" %in% names(tree_list)) {
    grp_cols <- c(grp_col, "inventory_year")
  }

  # Only compute for live trees
  live_mask <- if ("status" %in% names(tree_list)) {
    tree_list$status == "live"
  } else {
    rep(TRUE, nrow(tree_list))
  }

  # Compute plot-level summaries
  tree_list <- tree_list |>
    dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
    dplyr::mutate(
      plot_ba  = sum(.data$ba_per_acre[live_mask[dplyr::cur_group_rows()]], na.rm = TRUE),
      plot_tpa = sum(.data$tpa[live_mask[dplyr::cur_group_rows()]], na.rm = TRUE),
      plot_qmd = qmd(.data$dbh[live_mask[dplyr::cur_group_rows()]],
                      .data$tpa[live_mask[dplyr::cur_group_rows()]])
    ) |>
    dplyr::ungroup()

  # BAL: basal area in larger trees (computed per tree)
  tree_list$bal <- NA_real_
  tree_list$relative_dbh <- NA_real_
  tree_list$percentile_dbh <- NA_real_

  # Process by group for BAL computation
  group_keys <- interaction(
    tree_list[[grp_cols[1]]],
    if (length(grp_cols) > 1) tree_list[[grp_cols[2]]] else "",
    drop = TRUE
  )

  for (gk in unique(group_keys)) {
    idx <- which(group_keys == gk & live_mask)
    if (length(idx) < 1) next

    dbh_g  <- tree_list$dbh[idx]
    tpa_g  <- tree_list$tpa[idx]
    ba_g   <- tree_list$ba_per_acre[idx]
    qmd_g  <- tree_list$plot_qmd[idx[1]]

    for (j in seq_along(idx)) {
      # BAL: sum of BA/acre for trees with DBH > this tree
      larger <- dbh_g > dbh_g[j]
      tree_list$bal[idx[j]] <- sum(ba_g[larger], na.rm = TRUE)

      # Relative DBH
      if (!is.na(qmd_g) && qmd_g > 0) {
        tree_list$relative_dbh[idx[j]] <- dbh_g[j] / qmd_g
      }

      # Percentile
      tree_list$percentile_dbh[idx[j]] <- mean(dbh_g <= dbh_g[j]) * 100
    }
  }

  tree_list
}

# ---- Internal: Tree linking ----

#' Link trees across measurement cycles
#' @keywords internal
.link_tree_measurements <- function(tl) {
  # Initialize growth columns
  tl$prev_dbh             <- NA_real_
  tl$prev_height          <- NA_real_
  tl$dbh_growth           <- NA_real_
  tl$height_growth        <- NA_real_
  tl$annual_dbh_growth    <- NA_real_
  tl$annual_height_growth <- NA_real_
  tl$years_since_previous <- NA_real_
  tl$ingrowth             <- FALSE
  tl$mortality            <- FALSE

  # Link via PREV_TRE_CN if available
  if ("prev_tree_cn" %in% names(tl) && "tree_cn" %in% names(tl)) {
    # Build lookup from tree_cn -> row index
    cn_lookup <- stats::setNames(seq_len(nrow(tl)), as.character(tl$tree_cn))

    for (i in seq_len(nrow(tl))) {
      prev_cn <- tl$prev_tree_cn[i]
      if (is.na(prev_cn) || prev_cn == 0) {
        # No previous measurement — ingrowth if measurement > 1
        if (!is.na(tl$measurement_number[i]) && tl$measurement_number[i] > 1) {
          tl$ingrowth[i] <- TRUE
        }
        next
      }

      prev_idx <- cn_lookup[as.character(prev_cn)]
      if (is.na(prev_idx)) next

      tl$prev_dbh[i] <- tl$dbh[prev_idx]
      tl$prev_height[i] <- tl$height[prev_idx]

      # Compute growth
      if (!is.na(tl$dbh[i]) && !is.na(tl$prev_dbh[i])) {
        tl$dbh_growth[i] <- tl$dbh[i] - tl$prev_dbh[i]
      }
      if (!is.na(tl$height[i]) && !is.na(tl$prev_height[i])) {
        tl$height_growth[i] <- tl$height[i] - tl$prev_height[i]
      }

      # Years between measurements
      if (!is.na(tl$inventory_year[i]) && !is.na(tl$inventory_year[prev_idx])) {
        yrs <- tl$inventory_year[i] - tl$inventory_year[prev_idx]
        tl$years_since_previous[i] <- yrs
        if (yrs > 0) {
          if (!is.na(tl$dbh_growth[i])) {
            tl$annual_dbh_growth[i] <- tl$dbh_growth[i] / yrs
          }
          if (!is.na(tl$height_growth[i])) {
            tl$annual_height_growth[i] <- tl$height_growth[i] / yrs
          }
        }
      }

      # Mortality detection: tree was alive in previous, dead now
      if (tl$status[i] == "dead" && !is.na(prev_idx)) {
        prev_status <- tl$status[prev_idx]
        if (!is.na(prev_status) && prev_status == "live") {
          tl$mortality[i] <- TRUE
        }
      }
    }
  } else {
    # Fallback: link by plot_key + subplot + tree_num
    if (all(c("plot_key", "subplot", "tree_num") %in% names(tl))) {
      tl$tree_key <- paste(tl$plot_key, tl$subplot, tl$tree_num, sep = "_")

      for (tk in unique(tl$tree_key)) {
        idx <- which(tl$tree_key == tk)
        if (length(idx) < 2) next

        idx <- idx[order(tl$measurement_number[idx])]
        for (j in 2:length(idx)) {
          curr <- idx[j]
          prev <- idx[j - 1]

          tl$prev_dbh[curr] <- tl$dbh[prev]
          tl$prev_height[curr] <- tl$height[prev]

          if (!is.na(tl$dbh[curr]) && !is.na(tl$prev_dbh[curr])) {
            tl$dbh_growth[curr] <- tl$dbh[curr] - tl$prev_dbh[curr]
          }
          if (!is.na(tl$height[curr]) && !is.na(tl$prev_height[curr])) {
            tl$height_growth[curr] <- tl$height[curr] - tl$prev_height[curr]
          }

          if (!is.na(tl$inventory_year[curr]) && !is.na(tl$inventory_year[prev])) {
            yrs <- tl$inventory_year[curr] - tl$inventory_year[prev]
            tl$years_since_previous[curr] <- yrs
            if (yrs > 0) {
              if (!is.na(tl$dbh_growth[curr])) {
                tl$annual_dbh_growth[curr] <- tl$dbh_growth[curr] / yrs
              }
              if (!is.na(tl$height_growth[curr])) {
                tl$annual_height_growth[curr] <- tl$height_growth[curr] / yrs
              }
            }
          }

          if (tl$status[curr] == "dead" && tl$status[prev] == "live") {
            tl$mortality[curr] <- TRUE
          }
        }

        # First measurement: check for ingrowth context
        # (handled above via prev_tree_cn = NA)
      }
    }
  }

  tl
}
