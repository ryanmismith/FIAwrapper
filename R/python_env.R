#' Check Python Dependencies
#'
#' Verifies that the Python environment required for spatial functions
#' is properly configured. Returns TRUE if all dependencies are available.
#'
#' @return Logical. TRUE if Python environment is ready, FALSE otherwise.
#'   Prints informative messages about the environment status.
#'
#' @examples
#' \dontrun{
#' check_python_deps()
#' }
#'
#' @export
check_python_deps <- function() {
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    message("reticulate package is not installed. Install with: install.packages('reticulate')")
    return(invisible(FALSE))
  }

  env_name <- "r-fiawrapper"
  envs <- tryCatch(reticulate::conda_list(), error = function(e) NULL)

  if (is.null(envs)) {
    message("Conda is not available. Install Miniconda with: reticulate::install_miniconda()")
    return(invisible(FALSE))
  }

  if (!env_name %in% envs$name) {
    message("Python environment '", env_name, "' not found.")
    message("It will be created automatically on first spatial function call,")
    message("or you can create it now with: FIAwrapper:::ensure_python_env()")
    return(invisible(FALSE))
  }

  # Try importing key packages
  reticulate::use_condaenv(env_name, required = FALSE)
  pkgs <- c("geopandas", "shapely", "pyproj")
  all_ok <- TRUE
  for (pkg in pkgs) {
    available <- reticulate::py_module_available(pkg)
    status <- if (available) "OK" else "MISSING"
    message(sprintf("  %-12s: %s", pkg, status))
    if (!available) all_ok <- FALSE
  }

  if (all_ok) {
    message("Python environment is ready.")
  } else {
    message("Some dependencies are missing. Run FIAwrapper:::ensure_python_env() to install.")
  }

  invisible(all_ok)
}

#' Ensure Python Environment Exists
#'
#' Creates the conda environment and installs required packages if they
#' don't exist. Called internally by spatial functions on first use.
#' Caches result so it only runs once per session.
#'
#' @return Invisible TRUE on success.
#' @keywords internal
ensure_python_env <- function() {
  # Only check once per session
  if (isTRUE(.pkg_env$python_checked)) {
    return(invisible(TRUE))
  }

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop(
      "The 'reticulate' package is required for spatial functions.\n",
      "Install it with: install.packages('reticulate')",
      call. = FALSE
    )
  }

  env_name <- "r-fiawrapper"

  # Ensure conda is available
  tryCatch({
    envs <- reticulate::conda_list()
  }, error = function(e) {
    message("Conda not found. Installing Miniconda...")
    reticulate::install_miniconda()
    envs <<- reticulate::conda_list()
  })

  # Create env if it doesn't exist
  if (!env_name %in% reticulate::conda_list()$name) {
    message("Creating Python environment '", env_name, "'...")
    message("This is a one-time setup and may take a few minutes.")
    reticulate::conda_create(envname = env_name, python_version = "3.11")

    # Install packages
    packages <- c("geopandas", "shapely", "pyproj", "fiona", "requests")
    message("Installing Python packages: ", paste(packages, collapse = ", "))
    reticulate::conda_install(
      envname = env_name,
      packages = packages,
      pip = TRUE
    )
    message("Python environment setup complete.")
  }

  # Activate the environment
  reticulate::use_condaenv(env_name, required = TRUE)
  .pkg_env$python_checked <- TRUE
  invisible(TRUE)
}

#' Import a Python Module (with env setup)
#'
#' Ensures the Python environment is ready, then imports the requested module.
#'
#' @param module_name Name of the Python module to import.
#' @return The imported Python module object.
#' @keywords internal
import_python <- function(module_name) {
  ensure_python_env()
  reticulate::import(module_name)
}
