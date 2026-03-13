#' Convert FIA Species Code to FVS Species Code
#'
#' Translates FIA numeric species codes (SPCD) to FVS alpha codes.
#' When a variant is specified (or set via [set_fvs_variant()]), uses
#' variant-specific mappings. Falls back to the default (NE) mapping
#' for unrecognized species.
#'
#' @param spcd Integer vector of FIA SPCD values.
#' @param variant Optional FVS variant code (e.g., "NE", "SN", "LS").
#'   If NULL, uses the variant set by [set_fvs_variant()], or the default table.
#' @return Character vector of FVS species codes. Unrecognized SPCDs
#'   return "OH" (other hardwood).
#'
#' @examples
#' fia_to_fvs(c(833, 12, 129, 375, 318))
#' fia_to_fvs(97)
#'
#' @seealso [fvs_to_fia()], [get_common_name()], [set_fvs_variant()]
#' @family Species Lookups
#' @export
fia_to_fvs <- function(spcd, variant = NULL) {
  variant <- variant %||% getOption("FIAwrapper.variant")
  tbl <- .get_species_lookup(variant)

  result <- tbl$fvs_code[match(as.integer(spcd), tbl$spcd)]
  result[is.na(result)] <- "OH"
  as.character(result)
}

#' Convert FVS Species Code to FIA Species Code
#'
#' Translates FVS alpha species codes to FIA numeric SPCD values.
#' Variant-aware when a variant is set.
#'
#' @param fvs_code Character vector of FVS alpha species codes.
#' @param variant Optional FVS variant code. If NULL, uses session variant.
#' @return Integer vector of FIA SPCD values. Unrecognized codes return 999L.
#'
#' @examples
#' fvs_to_fia(c("RO", "BF", "WP"))
#'
#' @seealso [fia_to_fvs()], [set_fvs_variant()]
#' @family Species Lookups
#' @export
fvs_to_fia <- function(fvs_code, variant = NULL) {
  variant <- variant %||% getOption("FIAwrapper.variant")
  tbl <- .get_species_lookup(variant)

  result <- tbl$spcd[match(toupper(fvs_code), toupper(tbl$fvs_code))]
  result[is.na(result)] <- 999L
  as.integer(result)
}

#' Get Common Name from FIA SPCD
#'
#' Returns common tree names for FIA species codes.
#'
#' @param spcd Integer vector of FIA SPCD values.
#' @return Character vector of common names. Unknown species return
#'   "unknown species".
#'
#' @examples
#' get_common_name(c(833, 12, 129))
#' # "northern red oak", "balsam fir", "eastern white pine"
#'
#' @family Species Lookups
#' @export
get_common_name <- function(spcd) {
  tbl <- .get_species_lookup(NULL)
  result <- tbl$common_name[match(as.integer(spcd), tbl$spcd)]
  result[is.na(result)] <- "unknown species"
  as.character(result)
}

#' Get Full Species Table
#'
#' Returns the complete species mapping table for a given FVS variant.
#'
#' @param variant FVS variant code. If NULL, returns the default table.
#' @return A data frame with columns: spcd, fvs_code, common_name,
#'   scientific_name, wood_type.
#'
#' @examples
#' head(get_species_table("NE"))
#'
#' @family Species Lookups
#' @export
get_species_table <- function(variant = NULL) {
  variant <- variant %||% getOption("FIAwrapper.variant")
  .get_species_lookup(variant)
}

#' Convert PEF Species Code to FVS Species Code
#'
#' Translates Penobscot Experimental Forest (PEF) numeric species codes
#' to FVS alpha codes. Regional lookup for Maine PEF inventory data.
#'
#' @param pef_code Integer vector of PEF species codes.
#' @return Character vector of FVS species codes. Unknown codes return "OH".
#'
#' @examples
#' pef_to_fvs(c(1, 4, 15, 35))
#'
#' @family Species Lookups
#' @export
pef_to_fvs <- function(pef_code) {
  lookup <- c(
    `1`  = "BF", `4`  = "RS", `5`  = "WS", `6`  = "EH",
    `7`  = "EC", `10` = "WP", `11` = "TA", `15` = "RM",
    `16` = "PB", `18` = "QA", `20` = "GB", `31` = "WA",
    `32` = "AB", `33` = "YB", `35` = "SM", `36` = "RO"
  )
  result <- lookup[as.character(as.integer(pef_code))]
  result[is.na(result)] <- "OH"
  as.character(result)
}

# ---- Internal Species Table Loading ----

#' Load or Retrieve Species Lookup Table
#' @param variant FVS variant code or NULL for default.
#' @return Data frame with spcd, fvs_code, common_name, scientific_name, wood_type.
#' @keywords internal
.get_species_lookup <- function(variant = NULL) {
  key <- if (is.null(variant)) "default" else tolower(variant)

  # Check cache

if (!is.null(.pkg_env$species_tables[[key]])) {
    return(.pkg_env$species_tables[[key]])
  }

  # Try loading from CSV
  tbl <- .load_variant_csv(variant)
  if (is.null(tbl)) {
    tbl <- .default_species_table()
  }

  .pkg_env$species_tables[[key]] <- tbl
  tbl
}

#' Load Variant-Specific CSV
#' @param variant FVS variant code.
#' @return Data frame or NULL if file not found.
#' @keywords internal
.load_variant_csv <- function(variant) {
  if (is.null(variant)) return(NULL)
  fname <- paste0(tolower(variant), "_species.csv")
  path <- system.file("extdata", "fvs_species_variants", fname,
                       package = "FIAwrapper")
  if (path == "") {
    # Try from source directory during development
    path <- file.path("inst", "extdata", "fvs_species_variants", fname)
    if (!file.exists(path)) return(NULL)
  }
  utils::read.csv(path, stringsAsFactors = FALSE)
}

#' Default Species Table (NE + SN combined)
#'
#' Built from the original FIA_to_FVS_SPP.r lookup tables plus
#' common names and scientific names.
#'
#' @return Data frame.
#' @keywords internal
.default_species_table <- function() {
  data.frame(
    spcd = c(
      12L, 15L, 17L, 64L, 68L, 71L, 81L, 90L, 91L, 93L, 94L, 95L, 97L,
      105L, 108L, 110L, 111L, 121L, 122L, 125L, 126L, 129L, 131L, 132L,
      221L, 241L, 261L, 263L, 299L, 313L, 315L, 316L, 317L, 318L, 320L,
      341L, 371L, 372L, 375L, 379L, 391L, 400L, 407L, 521L, 531L, 541L,
      543L, 544L, 601L, 602L, 611L, 621L, 652L, 653L, 660L, 693L, 694L,
      701L, 731L, 740L, 741L, 742L, 743L, 746L, 754L, 761L, 762L, 763L,
      802L, 806L, 813L, 823L, 827L, 832L, 833L, 835L, 837L, 901L, 920L,
      922L, 935L, 951L, 970L, 972L
    ),
    fvs_code = c(
      "BF", "WF", "NP", "JM", "RC", "TA", "IC", "PI", "NS", "RA", "WS",
      "BS", "RS", "JP", "LP", "SP", "SL", "LO", "PP", "RP", "PP", "WP",
      "LL", "VP", "BD", "WC", "EH", "WH", "CY", "BE", "ST", "RM", "SV",
      "SM", "NM", "AI", "YB", "SB", "PB", "GB", "AH", "HI", "SH", "PS",
      "AB", "WA", "BA", "GA", "BN", "WN", "SG", "YP", "MG", "CT", "AP",
      "BG", "SA", "HH", "SY", "CW", "BP", "EC", "BT", "QA", "SX", "PR",
      "BC", "CC", "WO", "SO", "CK", "BR", "NQ", "CO", "RO", "PO", "BO",
      "BK", "WL", "BL", "MA", "BW", "OH", "AE"
    ),
    common_name = c(
      "balsam fir", "white fir", "noble fir", "one-seed juniper",
      "eastern redcedar", "tamarack", "incense cedar", "spruce spp.",
      "Norway spruce", "red alder", "white spruce", "black spruce",
      "red spruce", "jack pine", "lodgepole pine", "shortleaf pine",
      "slash pine", "longleaf pine", "ponderosa pine", "red pine",
      "pitch pine", "eastern white pine", "loblolly pine", "Virginia pine",
      "baldcypress", "northern white-cedar", "eastern hemlock",
      "western hemlock", "Atlantic white-cedar", "American beech",
      "striped maple", "red maple", "silver maple", "sugar maple",
      "Norway maple", "American holly", "yellow birch", "sweet birch",
      "paper birch", "gray birch", "alternate-leaf dogwood",
      "hickory spp.", "shagbark hickory", "persimmon",
      "American beech", "white ash", "black ash", "green ash",
      "butternut", "black walnut", "sweetgum", "yellow-poplar",
      "magnolia spp.", "cucumbertree", "pawpaw", "blackgum",
      "sassafras", "eastern hophornbeam", "American sycamore",
      "cottonwood spp.", "balsam poplar", "eastern cottonwood",
      "bigtooth aspen", "quaking aspen", "willow spp.", "pin cherry",
      "black cherry", "chokecherry", "white oak", "southern red oak",
      "cherrybark oak", "bear oak", "water oak", "chestnut oak",
      "northern red oak", "post oak", "bur oak", "bitternut hickory",
      "western larch", "black locust", "mountain-ash", "butternut",
      "other hardwood", "American elm"
    ),
    scientific_name = c(
      "Abies balsamea", "Abies concolor", "Abies procera",
      "Juniperus monosperma", "Juniperus virginiana", "Larix laricina",
      "Calocedrus decurrens", "Picea spp.", "Picea abies",
      "Alnus rubra", "Picea glauca", "Picea mariana", "Picea rubens",
      "Pinus banksiana", "Pinus contorta", "Pinus echinata",
      "Pinus elliottii", "Pinus palustris", "Pinus ponderosa",
      "Pinus resinosa", "Pinus rigida", "Pinus strobus", "Pinus taeda",
      "Pinus virginiana", "Taxodium distichum", "Thuja occidentalis",
      "Tsuga canadensis", "Tsuga heterophylla",
      "Chamaecyparis thyoides", "Fagus grandifolia", "Acer pensylvanicum",
      "Acer rubrum", "Acer saccharinum", "Acer saccharum",
      "Acer platanoides", "Ilex opaca", "Betula alleghaniensis",
      "Betula lenta", "Betula papyrifera", "Betula populifolia",
      "Cornus alternifolia", "Carya spp.", "Carya ovata",
      "Diospyros virginiana", "Fagus grandifolia", "Fraxinus americana",
      "Fraxinus nigra", "Fraxinus pennsylvanica", "Juglans cinerea",
      "Juglans nigra", "Liquidambar styraciflua",
      "Liriodendron tulipifera", "Magnolia spp.",
      "Magnolia acuminata", "Asimina triloba", "Nyssa sylvatica",
      "Sassafras albidum", "Ostrya virginiana",
      "Platanus occidentalis", "Populus spp.", "Populus balsamifera",
      "Populus deltoides", "Populus grandidentata", "Populus tremuloides",
      "Salix spp.", "Prunus pensylvanica", "Prunus serotina",
      "Prunus virginiana", "Quercus alba", "Quercus falcata",
      "Quercus pagoda", "Quercus ilicifolia", "Quercus nigra",
      "Quercus montana", "Quercus rubra", "Quercus stellata",
      "Quercus macrocarpa", "Carya cordiformis", "Larix occidentalis",
      "Robinia pseudoacacia", "Sorbus americana", "Juglans cinerea",
      "hardwood spp.", "Ulmus americana"
    ),
    wood_type = c(
      "softwood", "softwood", "softwood", "softwood", "softwood",
      "softwood", "softwood", "softwood", "softwood", "hardwood",
      "softwood", "softwood", "softwood", "softwood", "softwood",
      "softwood", "softwood", "softwood", "softwood", "softwood",
      "softwood", "softwood", "softwood", "softwood", "softwood",
      "softwood", "softwood", "softwood", "softwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "softwood", "hardwood",
      "hardwood", "hardwood", "hardwood", "hardwood"
    ),
    stringsAsFactors = FALSE
  )
}
