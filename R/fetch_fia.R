#' Download FIA Data for One or More States
#'
#' Downloads FIA inventory data from either the rFIA package or the
#' FIA DataMart bulk download service. Data is cached locally for reuse.
#'
#' @param states Character vector of state abbreviations (e.g., `c("ME", "NH")`).
#' @param tables Character vector of FIA tables to download. Default includes
#'   the core tables needed for tree lists: TREE, PLOT, COND, SUBPLOT.
#' @param data_dir Directory path to cache downloaded data. Default uses
#'   `getOption("FIAwrapper.data_dir")` or a temp directory.
#' @param method Download method: "rfia" (default, uses rFIA::getFIA),
#'   "datamart" (direct CSV from FIA DataMart), or "auto" (tries rFIA first).
#'
#' @return A named list of data frames, one per table (e.g.,
#'   `list(TREE = ..., PLOT = ..., COND = ...)`).
#'
#' @examples
#' \dontrun{
#' fia <- fetch_fia_data("RI")
#' names(fia)  # "TREE", "PLOT", "COND", ...
#' head(fia$TREE)
#' }
#'
#' @seealso [load_fia_data()], [build_tree_list()]
#' @export
fetch_fia_data <- function(states,
                           tables = c("TREE", "PLOT", "COND", "SUBPLOT"),
                           data_dir = NULL,
                           method = "auto") {

  states <- toupper(states)
  data_dir <- data_dir %||% getOption("FIAwrapper.data_dir") %||% tempdir()

  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE)
  }

  if (method == "auto") {
    method <- if (requireNamespace("rFIA", quietly = TRUE)) "rfia" else "datamart"
  }

  result <- list()

  if (method == "rfia") {
    result <- .fetch_via_rfia(states, tables, data_dir)
  } else if (method == "datamart") {
    result <- .fetch_via_datamart(states, tables, data_dir)
  } else {
    stop("Unknown method '", method, "'. Use 'rfia', 'datamart', or 'auto'.",
         call. = FALSE)
  }

  message("Loaded ", length(result), " tables for ",
          paste(states, collapse = ", "))
  result
}

#' Load Previously Downloaded FIA Data
#'
#' Loads FIA data from a local directory, auto-detecting whether it was
#' downloaded via rFIA or as raw DataMart CSVs.
#'
#' @param data_dir Directory containing downloaded FIA data.
#' @param states Optional state filter. If provided, only loads data for
#'   these states.
#'
#' @return A named list of data frames.
#'
#' @examples
#' \dontrun{
#' fia <- load_fia_data("~/fia_data")
#' }
#'
#' @seealso [fetch_fia_data()]
#' @export
load_fia_data <- function(data_dir, states = NULL) {
  if (!dir.exists(data_dir)) {
    stop("Directory not found: ", data_dir, call. = FALSE)
  }

  # Try rFIA format first (RDS files)
  rds_files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE,
                          ignore.case = TRUE)
  if (length(rds_files) > 0) {
    return(.load_rfia_format(data_dir, states))
  }

  # Try CSV format
  csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE,
                          ignore.case = TRUE)
  if (length(csv_files) > 0) {
    return(.load_csv_format(data_dir, states))
  }

  stop("No FIA data files found in: ", data_dir, call. = FALSE)
}

# ---- Internal: rFIA download ----

#' @keywords internal
.fetch_via_rfia <- function(states, tables, data_dir) {
  if (!requireNamespace("rFIA", quietly = TRUE)) {
    stop("rFIA package required. Install with: install.packages('rFIA')",
         call. = FALSE)
  }

  message("Downloading FIA data via rFIA for: ", paste(states, collapse = ", "))

  all_data <- list()
  for (state in states) {
    fia_db <- tryCatch(
      rFIA::getFIA(states = state, dir = data_dir, tables = tables),
      error = function(e) {
        warning("rFIA download failed for ", state, ": ", e$message)
        NULL
      }
    )
    if (!is.null(fia_db)) {
      for (tbl_name in names(fia_db)) {
        if (tbl_name %in% names(all_data)) {
          all_data[[tbl_name]] <- rbind(all_data[[tbl_name]], fia_db[[tbl_name]])
        } else {
          all_data[[tbl_name]] <- fia_db[[tbl_name]]
        }
      }
    }
  }

  if (length(all_data) == 0) {
    stop("No data downloaded. Check state abbreviations and network.", call. = FALSE)
  }
  all_data
}

# ---- Internal: DataMart CSV download ----

#' @keywords internal
.fetch_via_datamart <- function(states, tables, data_dir) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("httr2 package required for DataMart downloads. ",
         "Install with: install.packages('httr2')", call. = FALSE)
  }

  base_url <- "https://apps.fs.usda.gov/fia/datamart/CSV"

  all_data <- list()
  for (state in states) {
    for (tbl in tables) {
      fname <- paste0(state, "_", tbl, ".csv")
      local_path <- file.path(data_dir, fname)

      # Use cached file if it exists and is recent (< 30 days)
      if (file.exists(local_path)) {
        age_days <- difftime(Sys.time(), file.mtime(local_path), units = "days")
        if (age_days < 30) {
          message("Using cached: ", fname)
          df <- utils::read.csv(local_path, stringsAsFactors = FALSE)
          all_data[[tbl]] <- if (tbl %in% names(all_data)) {
            rbind(all_data[[tbl]], df)
          } else {
            df
          }
          next
        }
      }

      url <- paste0(base_url, "/", fname)
      message("Downloading: ", fname, " ...")

      tryCatch({
        req <- httr2::request(url)
        resp <- httr2::req_perform(req, path = local_path)
        df <- utils::read.csv(local_path, stringsAsFactors = FALSE)
        all_data[[tbl]] <- if (tbl %in% names(all_data)) {
          rbind(all_data[[tbl]], df)
        } else {
          df
        }
      }, error = function(e) {
        warning("Failed to download ", fname, ": ", e$message)
      })
    }
  }

  if (length(all_data) == 0) {
    stop("No data downloaded from DataMart.", call. = FALSE)
  }
  all_data
}

# ---- Internal: Load from disk ----

#' @keywords internal
.load_rfia_format <- function(data_dir, states) {
  rds_files <- list.files(data_dir, pattern = "\\.rds$", full.names = TRUE,
                          ignore.case = TRUE)
  all_data <- list()
  for (f in rds_files) {
    obj <- readRDS(f)
    if (is.list(obj) && !is.data.frame(obj)) {
      for (nm in names(obj)) {
        if (nm %in% names(all_data)) {
          all_data[[nm]] <- rbind(all_data[[nm]], obj[[nm]])
        } else {
          all_data[[nm]] <- obj[[nm]]
        }
      }
    }
  }

  if (!is.null(states) && "PLOT" %in% names(all_data)) {
    state_codes <- .state_abbr_to_code(states)
    if (!is.null(state_codes) && "STATECD" %in% names(all_data$PLOT)) {
      plot_cns <- all_data$PLOT$CN[all_data$PLOT$STATECD %in% state_codes]
      for (nm in names(all_data)) {
        if ("PLT_CN" %in% names(all_data[[nm]])) {
          all_data[[nm]] <- all_data[[nm]][all_data[[nm]]$PLT_CN %in% plot_cns, ]
        }
      }
      all_data$PLOT <- all_data$PLOT[all_data$PLOT$STATECD %in% state_codes, ]
    }
  }

  all_data
}

#' @keywords internal
.load_csv_format <- function(data_dir, states) {
  csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE,
                          ignore.case = TRUE)
  all_data <- list()

  for (f in csv_files) {
    fname <- basename(f)
    # Try to parse STATE_TABLE.csv pattern
    parts <- strsplit(tools::file_path_sans_ext(fname), "_")[[1]]
    if (length(parts) >= 2) {
      state <- parts[1]
      tbl   <- paste(parts[-1], collapse = "_")

      if (!is.null(states) && !toupper(state) %in% toupper(states)) next

      df <- utils::read.csv(f, stringsAsFactors = FALSE)
      if (tbl %in% names(all_data)) {
        all_data[[tbl]] <- rbind(all_data[[tbl]], df)
      } else {
        all_data[[tbl]] <- df
      }
    }
  }
  all_data
}

#' State Abbreviation to FIPS Code
#' @keywords internal
.state_abbr_to_code <- function(abbr) {
  lookup <- c(
    AL=1, AK=2, AZ=4, AR=5, CA=6, CO=8, CT=9, DE=10, FL=12, GA=13,
    HI=15, ID=16, IL=17, IN=18, IA=19, KS=20, KY=21, LA=22, ME=23,
    MD=24, MA=25, MI=26, MN=27, MS=28, MO=29, MT=30, NE=31, NV=32,
    NH=33, NJ=34, NM=35, NY=36, NC=37, ND=38, OH=39, OK=40, OR=41,
    PA=42, RI=44, SC=45, SD=46, TN=47, TX=48, UT=49, VT=50, VA=51,
    WA=53, WV=54, WI=55, WY=56
  )
  result <- lookup[toupper(abbr)]
  result[!is.na(result)]
}
