#' Prepare Raw FIA Data for Analysis
#'
#' Maps raw FIA database column names to forestry-friendly names, joins
#' TREE + PLOT + COND tables, converts species codes, and keeps data in
#' **imperial units** (DBH in inches, height in feet). This is a lower-level
#' function; most users will prefer [build_tree_list()] which calls this
#' internally.
#'
#' @param fia_trees Data frame of FIA TREE table records. Must contain at
#'   minimum: PLT_CN, SUBP, DIA, and STATUSCD.
#' @param fia_plots Optional data frame of FIA PLOT table records. Must
#'   contain CN (joins to TREE.PLT_CN).
#' @param fia_cond Optional data frame of FIA COND table records. Must
#'   contain PLT_CN and CONDID.
#' @param extra_tree_cols Character vector of additional TREE columns to keep.
#' @param extra_plot_cols Character vector of additional PLOT columns to keep.
#' @param extra_cond_cols Character vector of additional COND columns to keep.
#' @param include_dead Logical. When FALSE (default), filters to live trees
#'   only (STATUSCD == 1). Set to TRUE to retain dead trees.
#' @param variant FVS variant code for species translation. If NULL, uses
#'   the session variant set by [set_fvs_variant()].
#'
#' @return A tibble with standardized forestry column names and imperial units.
#'
#' @examples
#' mock_tree <- data.frame(
#'   PLT_CN = rep(1001, 4), SUBP = c(1, 1, 2, 3),
#'   DIA = c(10.2, 6.5, 8.0, 12.4), HT = c(55, 40, 48, 62),
#'   STATUSCD = c(1, 1, 1, 1), SPCD = c(12, 97, 261, 833)
#' )
#' prep_fia_data(mock_tree)
#'
#' @seealso [build_tree_list()], [fia_to_forestry_names()]
#' @export
prep_fia_data <- function(fia_trees,
                          fia_plots = NULL,
                          fia_cond = NULL,
                          extra_tree_cols = NULL,
                          extra_plot_cols = NULL,
                          extra_cond_cols = NULL,
                          include_dead = FALSE,
                          variant = NULL) {

  # Validate required columns
  required_tree <- c("PLT_CN", "SUBP", "DIA", "STATUSCD")
  missing <- setdiff(required_tree, names(fia_trees))
  if (length(missing) > 0) {
    stop("fia_trees missing required columns: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }

  # Filter by status
  if (!include_dead) {
    fia_trees <- fia_trees[fia_trees$STATUSCD == 1, ]
  }

  # Core columns -- keep imperial (DIA already in inches, HT in feet)
  out <- data.frame(
    plot_cn   = fia_trees$PLT_CN,
    subplot   = fia_trees$SUBP,
    dbh       = as.numeric(fia_trees$DIA),
    stringsAsFactors = FALSE
  )

  # Height: prefer ACTUALHT, fall back to HT (both already in feet)
  if ("ACTUALHT" %in% names(fia_trees)) {
    out$height <- as.numeric(fia_trees$ACTUALHT)
  } else if ("HT" %in% names(fia_trees)) {
    out$height <- as.numeric(fia_trees$HT)
  }

  # Species
  if ("SPCD" %in% names(fia_trees)) {
    out$spcd     <- as.integer(fia_trees$SPCD)
    out$spp_code <- fia_to_fvs(fia_trees$SPCD, variant = variant)
    out$species  <- get_common_name(fia_trees$SPCD)
  }

  # Tree status
  out$status_code <- as.integer(fia_trees$STATUSCD)
  out$status <- ifelse(fia_trees$STATUSCD == 1, "live", "dead")

  # Crown ratio
  if ("CR" %in% names(fia_trees)) {
    out$crown_ratio <- as.numeric(fia_trees$CR)
  }

  # Crown class
  if ("CCLCD" %in% names(fia_trees)) {
    out$crown_class <- as.integer(fia_trees$CCLCD)
  }

  # Tree class
  if ("TREECLCD" %in% names(fia_trees)) {
    out$tree_class <- as.integer(fia_trees$TREECLCD)
  }

  # Decay class (dead trees)
  if ("DECAYCD" %in% names(fia_trees)) {
    out$decay_class <- as.integer(fia_trees$DECAYCD)
  }

  # Damage codes
  for (d in paste0("DAMAGE_AGENT_CD", 1:3)) {
    if (d %in% names(fia_trees)) {
      out[[gsub("DAMAGE_AGENT_CD", "damage_code", d)]] <- fia_trees[[d]]
    }
  }

  # Tree CN for time series linking
  if ("CN" %in% names(fia_trees)) {
    out$tree_cn <- fia_trees$CN
  }
  if ("PREV_TRE_CN" %in% names(fia_trees)) {
    out$prev_tree_cn <- fia_trees$PREV_TRE_CN
  }
  if ("TREE" %in% names(fia_trees)) {
    out$tree_num <- as.integer(fia_trees$TREE)
  }
  if ("CONDID" %in% names(fia_trees)) {
    out$condition_id <- as.integer(fia_trees$CONDID)
  }
  if ("INVYR" %in% names(fia_trees)) {
    out$inventory_year <- as.integer(fia_trees$INVYR)
  }

  # Volume columns (already in ft^3 / board feet)
  vol_cols <- c("VOLCFNET", "VOLCFGRS", "VOLBFNET", "VOLBFGRS", "VOLCFSND")
  vol_names <- c("vol_cf_net", "vol_cf_gross", "vol_bf_net", "vol_bf_gross", "vol_cf_sound")
  for (i in seq_along(vol_cols)) {
    if (vol_cols[i] %in% names(fia_trees)) {
      out[[vol_names[i]]] <- as.numeric(fia_trees[[vol_cols[i]]])
    }
  }

  # Biomass / carbon
  bio_cols <- c("DRYBIO_AG", "DRYBIO_BG", "CARBON_AG", "CARBON_BG")
  bio_names <- c("biomass_ag_dry", "biomass_bg_dry", "carbon_ag", "carbon_bg")
  for (i in seq_along(bio_cols)) {
    if (bio_cols[i] %in% names(fia_trees)) {
      out[[bio_names[i]]] <- as.numeric(fia_trees[[bio_cols[i]]])
    }
  }

  # TPA adjustment columns
  if ("TPA_UNADJ" %in% names(fia_trees)) {
    out$tpa_unadj <- as.numeric(fia_trees$TPA_UNADJ)
  }

  # Extra tree columns
  if (!is.null(extra_tree_cols)) {
    avail <- intersect(extra_tree_cols, names(fia_trees))
    for (col in avail) out[[col]] <- fia_trees[[col]]
  }

  # --- Join PLOT table ---
  if (!is.null(fia_plots)) {
    if (!"CN" %in% names(fia_plots)) {
      stop("fia_plots must contain 'CN' column.", call. = FALSE)
    }

    # Filter to sampled forest plots (PLOT_STATUS_CD == 1)
    if ("PLOT_STATUS_CD" %in% names(fia_plots)) {
      fia_plots <- fia_plots[!is.na(fia_plots$PLOT_STATUS_CD) &
                               fia_plots$PLOT_STATUS_CD == 1, ]
    }

    plot_sub <- data.frame(CN = fia_plots$CN, stringsAsFactors = FALSE)

    # Standard plot columns
    plot_map <- c(
      "INVYR" = "inventory_year", "STATECD" = "state_code",
      "COUNTYCD" = "county_code", "LAT" = "lat", "LON" = "lon",
      "ELEV" = "elevation", "ECOSUBCD" = "ecological_subsection",
      "CYCLE" = "cycle", "SUBCYCLE" = "subcycle"
    )
    for (fia_col in names(plot_map)) {
      if (fia_col %in% names(fia_plots)) {
        plot_sub[[plot_map[fia_col]]] <- fia_plots[[fia_col]]
      }
    }

    if (!is.null(extra_plot_cols)) {
      for (col in intersect(extra_plot_cols, names(fia_plots))) {
        plot_sub[[col]] <- fia_plots[[col]]
      }
    }

    out <- merge(out, plot_sub, by.x = "plot_cn", by.y = "CN",
                 all.x = TRUE, sort = FALSE, suffixes = c("", ".plot"))

    # Use PLOT INVYR if tree didn't have it
    if ("inventory_year.plot" %in% names(out) && "inventory_year" %in% names(out)) {
      out$inventory_year <- ifelse(is.na(out$inventory_year),
                                    out$inventory_year.plot,
                                    out$inventory_year)
      out$inventory_year.plot <- NULL
    }
  }

  # --- Join COND table ---
  if (!is.null(fia_cond)) {
    if (!"PLT_CN" %in% names(fia_cond)) {
      stop("fia_cond must contain 'PLT_CN' column.", call. = FALSE)
    }

    # Filter to forest conditions (COND_STATUS_CD == 1)
    if ("COND_STATUS_CD" %in% names(fia_cond)) {
      fia_cond <- fia_cond[!is.na(fia_cond$COND_STATUS_CD) &
                             fia_cond$COND_STATUS_CD == 1, ]
    }

    cond_sub <- data.frame(PLT_CN = fia_cond$PLT_CN, stringsAsFactors = FALSE)

    cond_map <- c(
      "CONDID" = "condition_id", "CONDPROP_UNADJ" = "cond_proportion",
      "FORTYPCD" = "forest_type_code", "FORTYPGRPCD" = "forest_type_group_code",
      "STDAGE" = "stand_age", "STDORGCD" = "stand_origin",
      "SICOND" = "site_index", "SISP" = "site_index_species",
      "SITECLCD" = "site_class", "SLOPE" = "slope", "ASPECT" = "aspect",
      "PHYSCLCD" = "physiographic_class", "OWNCD" = "ownership_code",
      "OWNGRPCD" = "ownership_group", "DSTRBCD1" = "disturbance_code1",
      "BALIVE" = "ba_live", "SDI" = "stand_density_index",
      "GSSTK" = "growing_stock_stocking"
    )
    for (fia_col in names(cond_map)) {
      if (fia_col %in% names(fia_cond)) {
        cond_sub[[cond_map[fia_col]]] <- fia_cond[[fia_col]]
      }
    }

    if (!is.null(extra_cond_cols)) {
      for (col in intersect(extra_cond_cols, names(fia_cond))) {
        cond_sub[[col]] <- fia_cond[[col]]
      }
    }

    # Join by PLT_CN and CONDID if available
    if ("condition_id" %in% names(out) && "condition_id" %in% names(cond_sub)) {
      out <- merge(out, cond_sub,
                   by.x = c("plot_cn", "condition_id"),
                   by.y = c("PLT_CN", "condition_id"),
                   all.x = TRUE, sort = FALSE)
    } else {
      out <- merge(out, cond_sub,
                   by.x = "plot_cn", by.y = "PLT_CN",
                   all.x = TRUE, sort = FALSE)
    }
  }

  # Remove invalid DBH rows
  out <- out[!is.na(out$dbh) & out$dbh > 0, ]

  tibble::as_tibble(out)
}
