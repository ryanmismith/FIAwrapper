# Package-level environment for storing state
.pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Set default options
  op <- options()
  op_fiawrapper <- list(
    FIAwrapper.variant = NULL,
    FIAwrapper.units = "imperial",
    FIAwrapper.data_dir = NULL
  )
  toset <- !(names(op_fiawrapper) %in% names(op))
  if (any(toset)) options(op_fiawrapper[toset])

  # Initialize package state
  .pkg_env$python_checked <- FALSE
  .pkg_env$species_tables <- list()

  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "FIAwrapper v", utils::packageVersion("FIAwrapper"),
    " -- Clean FIA tree lists in forestry language"
  )
  variant <- getOption("FIAwrapper.variant")
  if (is.null(variant)) {
    packageStartupMessage(
      "No FVS variant set. Use set_fvs_variant() to configure species codes."
    )
  }
}
