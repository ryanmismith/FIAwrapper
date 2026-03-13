#' Basal Area in Square Feet
#'
#' Computes individual tree basal area from DBH in inches.
#'
#' @param dbh_inches Numeric vector of diameters at breast height in inches.
#' @return Numeric vector of basal area in square feet.
#'
#' @examples
#' ba_ft2(10)   # 0.5454 ft^2
#' ba_ft2(c(5, 10, 15))
#'
#' @export
ba_ft2 <- function(dbh_inches) {
  dbh_inches^2 * 0.005454154
}

#' Basal Area in Square Meters
#'
#' Computes individual tree basal area from DBH in centimeters.
#'
#' @param dbh_cm Numeric vector of diameters at breast height in centimeters.
#' @return Numeric vector of basal area in square meters.
#'
#' @examples
#' ba_m2(25.4)
#'
#' @export
ba_m2 <- function(dbh_cm) {
  dbh_cm^2 * pi / 40000
}

#' Quadratic Mean Diameter
#'
#' Computes QMD from tree diameters and per-acre expansion factors.
#'
#' @param dbh Numeric vector of diameters (inches or cm depending on context).
#' @param tpa Numeric vector of trees per acre (or per hectare) for each tree.
#' @return Numeric scalar: the quadratic mean diameter.
#'
#' @examples
#' qmd(c(8, 10, 12), c(20, 15, 10))
#'
#' @export
qmd <- function(dbh, tpa) {
  if (length(dbh) == 0 || sum(tpa, na.rm = TRUE) == 0) return(NA_real_)
  valid <- !is.na(dbh) & !is.na(tpa) & tpa > 0
  sqrt(sum(dbh[valid]^2 * tpa[valid]) / sum(tpa[valid]))
}

#' Convert Inches to Centimeters
#' @param x Numeric vector in inches.
#' @return Numeric vector in centimeters.
#' @keywords internal
inches_to_cm <- function(x) x * 2.54

#' Convert Centimeters to Inches
#' @param x Numeric vector in centimeters.
#' @return Numeric vector in inches.
#' @keywords internal
cm_to_inches <- function(x) x / 2.54

#' Convert Feet to Meters
#' @param x Numeric vector in feet.
#' @return Numeric vector in meters.
#' @keywords internal
feet_to_meters <- function(x) x * 0.3048

#' Convert Meters to Feet
#' @param x Numeric vector in meters.
#' @return Numeric vector in feet.
#' @keywords internal
meters_to_feet <- function(x) x / 0.3048

#' Convert Acres to Hectares
#' @param x Numeric vector in acres.
#' @return Numeric vector in hectares.
#' @keywords internal
acres_to_hectares <- function(x) x * 0.404686

#' Convert Hectares to Acres
#' @param x Numeric vector in hectares.
#' @return Numeric vector in acres.
#' @keywords internal
hectares_to_acres <- function(x) x / 0.404686

#' Convert Square Feet per Acre to Square Meters per Hectare
#' @param x Numeric vector in ft^2/acre.
#' @return Numeric vector in m^2/ha.
#' @keywords internal
sqft_ac_to_sqm_ha <- function(x) x * 0.229568

#' Convert Square Meters per Hectare to Square Feet per Acre
#' @param x Numeric vector in m^2/ha.
#' @return Numeric vector in ft^2/acre.
#' @keywords internal
sqm_ha_to_sqft_ac <- function(x) x / 0.229568

#' Validate Required Columns Exist in a Data Frame
#' @param df A data frame.
#' @param required Character vector of required column names.
#' @param caller String identifying the calling function for error messages.
#' @keywords internal
validate_columns <- function(df, required, caller = "function") {
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(
      caller, ": missing required columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Safely Coerce to Numeric
#' @param x Vector to coerce.
#' @return Numeric vector; non-numeric values become NA with a warning.
#' @keywords internal
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}
