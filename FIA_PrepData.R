#' Prepare FIA Data for inventoryfunctions
#'
#' Maps raw FIA database column names (as returned by \pkg{rFIA}) to the
#' standard column conventions used throughout the \pkg{inventoryfunctions}
#' package. Performs unit conversions (imperial to metric) and optionally
#' joins PLOT and COND tables to attach stand-level attributes.
#'
#' @param fia_trees Data frame of FIA TREE table records (e.g., from
#'   \code{rFIA::readFIA()$TREE}). Must contain at minimum: \code{PLT_CN},
#'   \code{SUBP}, \code{DIA}, and \code{STATUSCD}.
#' @param fia_plots Optional data frame of FIA PLOT table records. When
#'   provided, stand identifiers and site attributes are joined. Must
#'   contain \code{CN} (the key matching \code{PLT_CN} in TREE).
#' @param fia_cond Optional data frame of FIA COND table records. When
#'   provided, condition-level attributes are joined. Must contain
#'   \code{PLT_CN} and \code{CONDID}.
#' @param extra_tree_cols Optional character vector of additional columns
#'   from the TREE table to carry through (e.g.,
#'   \code{c("VOLCFNET", "DRYBIO_AG", "CARBON_AG")}).
#' @param extra_plot_cols Optional character vector of additional columns
#'   from the PLOT table to carry through (e.g.,
#'   \code{c("ECOSUBCD", "ELEV")}).
#' @param extra_cond_cols Optional character vector of additional columns
#'   from the COND table to carry through (e.g.,
#'   \code{c("DSTRBCD1", "BALIVE")}).
#' @param live_only Logical. When \code{TRUE} (default), filters to live
#'   trees only (\code{STATUSCD == 1}). Set to \code{FALSE} to retain
#'   dead trees for mortality analysis.
#'
#' @details
#' ## Column Mapping
#' \tabular{lll}{
#'   \strong{FIA Column} \tab \strong{Package Column} \tab \strong{Conversion} \cr
#'   PLT_CN \tab Plot \tab — \cr
#'   SUBP \tab SUBP \tab — \cr
#'   DIA (inches) \tab DBH (cm) \tab \eqn{\times 2.54} \cr
#'   HT or ACTUALHT (feet) \tab HT (m) \tab \eqn{\times 0.3048} \cr
#'   SPCD \tab SPCD \tab — (retained for reference) \cr
#'   SPCD \tab Species \tab via \code{\link{FIAtoFVS.SPP}} \cr
#'   TREECLCD \tab TreeClass \tab — \cr
#'   SPGRPCD \tab SpeciesGroup \tab — \cr
#'   STATUSCD \tab StatusCD \tab — (used for filtering) \cr
#' }
#'
#' When \code{fia_plots} is provided, the following are also mapped:
#' \tabular{ll}{
#'   \strong{PLOT Column} \tab \strong{Mapped To} \cr
#'   ECOSUBCD or STATECD \tab Stand \cr
#'   LAT \tab Lat \cr
#'   LON \tab Lon \cr
#'   ELEV \tab Elev \cr
#' }
#'
#' When \code{fia_cond} is provided:
#' \tabular{ll}{
#'   \strong{COND Column} \tab \strong{Mapped To} \cr
#'   CONDPROP_UNADJ \tab CONDPROP_UNADJ \cr
#'   FORTYPCD \tab FORTYPCD \cr
#'   STDAGE \tab STDAGE \cr
#'   SITECLCD \tab SITECLCD \cr
#'   OWNGRPCD \tab OWNGRPCD \cr
#' }
#'
#' ## Workflow Example
#' \preformatted{
#' # Step 1: Download FIA data with rFIA
#' fia_data <- rFIA::getFIA(states = "ME", dir = "fia_data/")
#'
#' # Step 2: Prep for inventoryfunctions
#' tree_list <- fia_prep_data(
#'   fia_trees = fia_data$TREE,
#'   fia_plots = fia_data$PLOT,
#'   fia_cond  = fia_data$COND
#' )
#'
#' # Step 3: Compute expansion factors
#' tree_list$EXPF <- fia_expansion(
#'   DBH  = tree_list$DBH,
#'   SUBP = tree_list$SUBP,
#'   CONDPROP_UNADJ = tree_list$CONDPROP_UNADJ
#' )
#'
#' # Step 4: Aggregate to plot level
#' plot_summary <- fia_plot_aggregate(
#'   Stand = tree_list$Stand, Plot = tree_list$Plot,
#'   DBH = tree_list$DBH, HT = tree_list$HT,
#'   EXPF = tree_list$EXPF
#' )
#'
#' # Step 5: Estimate variance
#' fia_variance(Stand = plot_summary$Stand, Plot = plot_summary$Plot,
#'              y = plot_summary$BAPH, conf_level = 0.90)
#' }
#'
#' @return A \code{tibble} with standardized column names and metric units,
#'   ready for use with \code{\link{fia_expansion}},
#'   \code{\link{fia_plot_aggregate}}, and other package functions.
#'
#' @seealso \code{\link{fia_expansion}}, \code{\link{fia_plot_aggregate}},
#'   \code{\link{FIAtoFVS.SPP}}, \code{\link{FVStoFIA.SPP}}
#' @family FIA Functions
#'
#' @examples
#' # Create minimal mock FIA TREE data
#' mock_tree <- data.frame(
#'   PLT_CN   = rep(1001, 5),
#'   SUBP     = c(1, 1, 2, 3, 4),
#'   DIA      = c(10.2, 6.5, 8.0, 2.1, 12.4),
#'   HT       = c(55, 40, 48, 15, 62),
#'   STATUSCD = c(1, 1, 1, 1, 1),
#'   SPCD     = c(12, 97, 261, 531, 833),
#'   TREECLCD = c(2, 2, 2, 3, 2),
#'   SPGRPCD  = c(5, 7, 8, 27, 30),
#'   stringsAsFactors = FALSE
#' )
#'
#' prepped <- fia_prep_data(mock_tree)
#' print(prepped)
#'
#' @export

fia_prep_data <- function(fia_trees,
                           fia_plots = NULL,
                           fia_cond = NULL,
                           extra_tree_cols = NULL,
                           extra_plot_cols = NULL,
                           extra_cond_cols = NULL,
                           live_only = TRUE) {

  # --- Validate required columns ---
  required_tree <- c("PLT_CN", "SUBP", "DIA", "STATUSCD")
  missing <- setdiff(required_tree, names(fia_trees))
  if (length(missing) > 0) {
    stop("fia_trees is missing required columns: ",
         paste(missing, collapse = ", "))
  }

  # --- Filter to live trees ---
  if (live_only) {
    fia_trees <- fia_trees[fia_trees$STATUSCD == 1, ]
  }

  # --- Core column mapping with unit conversion ---
  out <- data.frame(
    Plot = fia_trees$PLT_CN,
    SUBP = fia_trees$SUBP,
    DBH  = fia_trees$DIA * 2.54,            # inches -> cm
    stringsAsFactors = FALSE
  )

  # Height: prefer ACTUALHT, fall back to HT
  if ("ACTUALHT" %in% names(fia_trees)) {
    out$HT <- fia_trees$ACTUALHT * 0.3048   # feet -> m
  } else if ("HT" %in% names(fia_trees)) {
    out$HT <- fia_trees$HT * 0.3048         # feet -> m
  }

  # Species code (numeric SPCD retained)
  if ("SPCD" %in% names(fia_trees)) {
    out$SPCD <- fia_trees$SPCD
    # Convert to FVS species code
    out$Species <- tryCatch(
      FIAtoFVS.SPP(fia_trees$SPCD),
      error = function(e) rep(NA_character_, nrow(fia_trees))
    )
  }

  # Optional standard columns
  if ("TREECLCD" %in% names(fia_trees)) {
    out$TreeClass <- fia_trees$TREECLCD
  }
  if ("SPGRPCD" %in% names(fia_trees)) {
    out$SpeciesGroup <- fia_trees$SPGRPCD
  }
  if ("STATUSCD" %in% names(fia_trees)) {
    out$StatusCD <- fia_trees$STATUSCD
  }
  if ("CONDID" %in% names(fia_trees)) {
    out$CONDID <- fia_trees$CONDID
  }

  # Extra tree columns
  if (!is.null(extra_tree_cols)) {
    avail <- intersect(extra_tree_cols, names(fia_trees))
    missing_extra <- setdiff(extra_tree_cols, names(fia_trees))
    if (length(missing_extra) > 0) {
      warning("extra_tree_cols not found in fia_trees: ",
              paste(missing_extra, collapse = ", "))
    }
    for (col in avail) {
      out[[col]] <- fia_trees[[col]]
    }
  }

  # --- Join PLOT table ---
  if (!is.null(fia_plots)) {
    if (!"CN" %in% names(fia_plots)) {
      stop("fia_plots must contain a 'CN' column to join with TREE.PLT_CN.")
    }

    # Build plot-level lookup
    plot_cols <- c("CN")

    # Stand identifier: prefer ECOSUBCD, fall back to STATECD
    stand_col <- if ("ECOSUBCD" %in% names(fia_plots)) "ECOSUBCD"
                 else if ("STATECD" %in% names(fia_plots)) "STATECD"
                 else NULL

    optional_plot <- c("LAT", "LON", "ELEV")
    plot_cols <- c(plot_cols,
                   stand_col,
                   intersect(optional_plot, names(fia_plots)))

    if (!is.null(extra_plot_cols)) {
      avail_p <- intersect(extra_plot_cols, names(fia_plots))
      missing_p <- setdiff(extra_plot_cols, names(fia_plots))
      if (length(missing_p) > 0) {
        warning("extra_plot_cols not found in fia_plots: ",
                paste(missing_p, collapse = ", "))
      }
      plot_cols <- c(plot_cols, avail_p)
    }

    plot_cols <- unique(plot_cols)
    plot_sub  <- fia_plots[, intersect(plot_cols, names(fia_plots)), drop = FALSE]

    # Join
    out <- merge(out, plot_sub,
                 by.x = "Plot", by.y = "CN",
                 all.x = TRUE, sort = FALSE)

    # Rename stand column
    if (!is.null(stand_col) && stand_col %in% names(out)) {
      out$Stand <- out[[stand_col]]
    }

    # Rename coordinate columns
    if ("LAT" %in% names(out)) names(out)[names(out) == "LAT"] <- "Lat"
    if ("LON" %in% names(out)) names(out)[names(out) == "LON"] <- "Lon"
    if ("ELEV" %in% names(out)) names(out)[names(out) == "ELEV"] <- "Elev"
  }

  # --- Join COND table ---
  if (!is.null(fia_cond)) {
    if (!"PLT_CN" %in% names(fia_cond)) {
      stop("fia_cond must contain a 'PLT_CN' column.")
    }

    cond_cols <- c("PLT_CN")
    optional_cond <- c("CONDPROP_UNADJ", "FORTYPCD", "FORTYPGRPCD",
                       "STDAGE", "SITECLCD", "STDORGCD", "OWNGRPCD")
    cond_cols <- c(cond_cols, intersect(optional_cond, names(fia_cond)))

    if ("CONDID" %in% names(fia_cond)) {
      cond_cols <- c(cond_cols, "CONDID")
    }

    if (!is.null(extra_cond_cols)) {
      avail_c <- intersect(extra_cond_cols, names(fia_cond))
      missing_c <- setdiff(extra_cond_cols, names(fia_cond))
      if (length(missing_c) > 0) {
        warning("extra_cond_cols not found in fia_cond: ",
                paste(missing_c, collapse = ", "))
      }
      cond_cols <- c(cond_cols, avail_c)
    }

    cond_cols <- unique(cond_cols)
    cond_sub  <- fia_cond[, intersect(cond_cols, names(fia_cond)), drop = FALSE]

    # Join by PLT_CN (and CONDID if available in both)
    if ("CONDID" %in% names(out) && "CONDID" %in% names(cond_sub)) {
      # Remove CONDID from cond_sub join columns to avoid duplication
      cond_join <- cond_sub
      out <- merge(out, cond_join,
                   by.x = c("Plot", "CONDID"),
                   by.y = c("PLT_CN", "CONDID"),
                   all.x = TRUE, sort = FALSE)
    } else {
      out <- merge(out, cond_sub,
                   by.x = "Plot", by.y = "PLT_CN",
                   all.x = TRUE, sort = FALSE)
    }
  }

  # --- Ensure Stand column exists ---
  if (!"Stand" %in% names(out)) {
    out$Stand <- 1L
  }

  # Remove NA DBH rows
  out <- out[!is.na(out$DBH) & out$DBH > 0, ]

  tibble::as_tibble(out)
}
