#' Convert FVS Species Code to FIA Species Code (SPCD)
#'
#' Translates FVS alpha species codes (used in the Forest Vegetation Simulator and
#' many regional CFI programs) to FIA numeric species codes (SPCD) as used in the
#' FIADB and the R \pkg{rFIA} package. Supports northeastern/Acadian and
#' southern US commercial species.
#'
#' @section Sources:
#' FIA species codes from FIADB Reference Population Table (REF_SPECIES).
#' Southern variant codes follow FVS-SN species mappings.
#'
#' @param FVS.SPP A character vector of FVS alpha species codes (e.g., \code{"RO"}).
#'   Unknown codes return \code{999}.
#'
#' @return An integer vector of FIA SPCD values the same length as \code{FVS.SPP}.
#'   Unrecognized FVS codes return \code{999}.
#'
#' @examples
#' FVStoFIA.SPP(c("RO", "BF", "WP", "PB", "SM"))
#' FVStoFIA.SPP("OH")  # catch-all other hardwood -> 970
#'
#' @seealso \code{\link{FIAtoFVS.SPP}}, \code{\link{PEFtoFVS.SPP}}
#' @family Species Lookups
#' @export

FVStoFIA.SPP <- function(FVS.SPP) {
  lookup <- c(
    'AB' = 531L,  # American beech
    'BA' = 543L,  # Black ash
    'BC' = 762L,  # Black cherry
    'BE' = 313L,  # American beech (alt FVS code)
    'BF' = 12L,   # Balsam fir
    'BG' = 693L,  # Blackgum / black tupelo (Nyssa sylvatica)
    'BK' = 901L,  # Bitternut hickory
    'BL' = 922L,  # Blacklocust
    'BN' = 970L,  # Butternut
    'BO' = 837L,  # Bur oak
    'BP' = 741L,  # Balsam poplar
    'BR' = 823L,  # Bear oak / scrub oak
    'BS' = 95L,   # Black spruce
    'BT' = 743L,  # Bigtooth aspen
    'BU' = 970L,  # Basswood (American linden)
    'BW' = 951L,  # Butternut
    'CC' = 763L,  # Choke cherry
    'CE' = 241L,  # Northern white cedar
    'CO' = 832L,  # Chestnut oak
    'CT' = 653L,  # Cucumbertree
    'CW' = 740L,  # Cottonwood
    'CY' = 299L,  # Atlantic white cedar
    'EC' = 742L,  # Eastern cottonwood
    'EH' = 261L,  # Eastern hemlock
    'EL' = 970L,  # Elm (generic)
    'GA' = 544L,  # Green ash
    'GB' = 379L,  # Gray birch
    'HH' = 701L,  # Eastern hophornbeam
    'HI' = 400L,  # Hickory (generic)
    'HM' = 261L,  # Hemlock (alt code)
    'IC' = 81L,   # Incense cedar
    'JM' = 64L,   # One-seed juniper
    'JP' = 105L,  # Jack pine
    'JV' = 68L,   # Eastern redcedar
    'LP' = 108L,  # Lodgepole pine
    'MA' = 935L,  # Mountain ash
    'MG' = 652L,  # Magnolia
    'NM' = 320L,  # Norway maple
    'NP' = 17L,   # Noble fir
    'NS' = 91L,   # Norway spruce
    'OA' = 835L,  # Scarlet oak
    'OG' = 827L,  # Gambel oak
    'OH' = 970L,  # Other hardwood (catch-all)
    'OS' = 110L,  # Other softwood (catch-all)
    'PB' = 375L,  # Paper birch
    'PC' = 761L,  # Pin cherry
    'PD' = 746L,  # Plains cottonwood
    'PI' = 90L,   # Spruce (generic)
    'PM' = 64L,   # Honey mesquite
    'PO' = 835L,  # Post oak (Quercus stellata)
    'PP' = 122L,  # Ponderosa pine
    'PR' = 761L,  # Pin cherry (alt)
    'QA' = 746L,  # Quaking aspen
    'RA' = 93L,   # Red alder
    'RB' = 372L,  # River birch
    'RM' = 316L,  # Red maple
    'RN' = 125L,  # Red pine (alt)
    'RO' = 833L,  # Red oak
    'RP' = 125L,  # Red pine
    'RS' = 97L,   # Red spruce
    'SA' = 694L,  # Sassafras
    'SB' = 372L,  # Sweet birch (alt)
    'SD' = 920L,  # Swamp Douglas-fir (alt)
    'SH' = 407L,  # Shagbark hickory
    'SM' = 318L,  # Sugar maple
    'SO' = 806L,  # Southern red oak
    'SP' = 110L,  # Shortleaf pine (Pinus echinata)
    'SS' = 920L,  # Sitka spruce
    'ST' = 315L,  # Striped maple
    'SU' = 611L,  # Sweetgum
    'SV' = 317L,  # Silver maple
    'SX' = 754L,  # Willow (generic)
    'SY' = 731L,  # Sycamore (Platanus occidentalis)
    'TA' = 71L,   # Tamarack
    'UA' = 972L,  # American elm
    'VP' = 132L,  # Virginia pine
    'WA' = 541L,  # White ash
    'WC' = 241L,  # Northern white cedar (alt)
    'WF' = 15L,   # White fir
    'WH' = 263L,  # Western hemlock
    'WL' = 920L,  # Western larch
    'WN' = 602L,  # Black walnut
    'WO' = 802L,  # White oak
    'WP' = 129L,  # Eastern white pine
    'WS' = 94L,   # White spruce
    'YB' = 371L,  # Yellow birch
    'YP' = 621L,  # Yellow poplar (tulip tree)
    # ---- SOUTHERN SPECIES (FIA SPCD from FIADB) ----
    'BD' = 221L,  # Baldcypress (Taxodium distichum)
    'CK' = 813L,  # Cherrybark oak (Quercus pagoda)
    'LL' = 131L,  # Loblolly pine (Pinus taeda)
    'LO' = 121L,  # Longleaf pine (Pinus palustris)
    'NQ' = 827L,  # Water oak (Quercus nigra)
    'PS' = 521L,  # Persimmon (Diospyros virginiana)
    'RC' = 68L,   # Eastern redcedar (Juniperus virginiana)
    'SG' = 611L,  # Sweetgum (Liquidambar styraciflua)
    'SL' = 111L   # Slash pine (Pinus elliottii)
  )

  result <- lookup[FVS.SPP]
  result[is.na(result)] <- 999L
  as.integer(result)
}


#' Convert FIA Species Code (SPCD) to FVS Species Code
#'
#' Translates FIA numeric species codes (SPCD from the FIADB / \pkg{rFIA}) to
#' FVS alpha codes used in the Forest Vegetation Simulator and regional CFI
#' inventory programs. Supports vectorized input.
#'
#' @param FIA.SPP An integer (or numeric) vector of FIA SPCD values.
#'   Unrecognized SPCDs return \code{"OH"} (other hardwood catch-all).
#'
#' @return A character vector of FVS species codes the same length as
#'   \code{FIA.SPP}.
#'
#' @examples
#' FIAtoFVS.SPP(c(833, 12, 129, 375, 318))  # RO BF WP PB SM
#' FIAtoFVS.SPP(97)   # RS — red spruce
#' FIAtoFVS.SPP(999)  # OH — unrecognized -> other hardwood
#'
#' @seealso \code{\link{FVStoFIA.SPP}}, \code{\link{PEFtoFVS.SPP}}
#' @family Species Lookups
#' @export

FIAtoFVS.SPP <- function(FIA.SPP) {
  lookup <- c(
    `531` = 'AB',  # American beech (FVS NE "AB" = beech)
    `972` = 'AE',  # American elm
    `391` = 'AH',  # Alternate-leaf dogwood
    `341` = 'AI',  # American holly
    `660` = 'AP',  # Pawpaw
    `543` = 'BA',  # Black ash
    `762` = 'BC',  # Black cherry
    `313` = 'BE',  # American beech (alt SPCD — some FIA datasets use 313)
    `12`  = 'BF',  # Balsam fir
    `922` = 'BL',  # Black locust
    `601` = 'BN',  # Butternut
    `837` = 'BO',  # Bur oak
    `741` = 'BP',  # Balsam poplar
    `823` = 'BR',  # Bear oak
    `95`  = 'BS',  # Black spruce
    `743` = 'BT',  # Bigtooth aspen
    `951` = 'BW',  # Butternut (alt)
    `763` = 'CC',  # Choke cherry
    `742` = 'EC',  # Eastern cottonwood
    `261` = 'EH',  # Eastern hemlock
    `544` = 'GA',  # Green ash
    `379` = 'GB',  # Gray birch
    `701` = 'HH',  # Eastern hophornbeam
    `400` = 'HI',  # Shagbark hickory (generic)
    `105` = 'JP',  # Jack pine
    `935` = 'MA',  # Mountain ash
    `320` = 'NM',  # Norway maple
    `91`  = 'NS',  # Norway spruce
    `375` = 'PB',  # Paper birch
    `126` = 'PP',  # Pitch pine
    `761` = 'PR',  # Pin cherry
    `746` = 'QA',  # Quaking aspen
    `316` = 'RM',  # Red maple
    `833` = 'RO',  # Red oak (northern)
    `125` = 'RP',  # Red pine
    `97`  = 'RS',  # Red spruce
    `372` = 'SB',  # Sweet birch
    `407` = 'SH',  # Shagbark hickory
    `318` = 'SM',  # Sugar maple
    `110` = 'SP',  # Shortleaf pine (Pinus echinata)
    `131` = 'LL',  # Loblolly pine (Pinus taeda)
    `315` = 'ST',  # Striped maple
    `317` = 'SV',  # Silver maple
    `71`  = 'TA',  # Tamarack
    `541` = 'WA',  # White ash
    `241` = 'WC',  # Northern white cedar
    `920` = 'WL',  # Western larch
    `802` = 'WO',  # White oak
    `129` = 'WP',  # Eastern white pine
    `94`  = 'WS',  # White spruce
    `371` = 'YB',  # Yellow birch
    # ---- SOUTHERN SPECIES (FIA SPCD -> FVS code) ----
    `111` = 'SL',  # Slash pine (Pinus elliottii)
    `121` = 'LO',  # Longleaf pine (Pinus palustris)
    `132` = 'VP',  # Virginia pine (Pinus virginiana)
    `221` = 'BD',  # Baldcypress (Taxodium distichum)
    `521` = 'PS',  # Persimmon (Diospyros virginiana)
    `602` = 'WN',  # Black walnut (Juglans nigra)
    `611` = 'SG',  # Sweetgum (Liquidambar styraciflua)
    `621` = 'YP',  # Yellow-poplar (Liriodendron tulipifera)
    `693` = 'BG',  # Blackgum / black tupelo (Nyssa sylvatica)
    `731` = 'SY',  # Sycamore (Platanus occidentalis)
    `806` = 'SO',  # Southern red oak (Quercus falcata)
    `813` = 'CK',  # Cherrybark oak (Quercus pagoda)
    `827` = 'NQ',  # Water oak (Quercus nigra)
    `835` = 'PO'   # Post oak (Quercus stellata)
  )

  result <- lookup[as.character(as.integer(FIA.SPP))]
  result[is.na(result)] <- 'OH'
  as.character(result)
}


#' Convert PEF Species Code to FVS Species Code
#'
#' Translates Penobscot Experimental Forest (PEF) numeric species codes to FVS
#' alpha codes. This is a regional lookup for Maine PEF inventory data.
#'
#' @param PEF.SPP An integer (or numeric) vector of PEF species codes.
#'   Unrecognized codes return \code{"OH"}.
#'
#' @return A character vector of FVS species codes.
#'
#' @examples
#' PEFtoFVS.SPP(c(1, 4, 15, 35))  # BF RS RM SM
#'
#' @seealso \code{\link{FIAtoFVS.SPP}}, \code{\link{FVStoFIA.SPP}}
#' @family Species Lookups
#' @export

PEFtoFVS.SPP <- function(PEF.SPP) {
  lookup <- c(
    `1`  = 'BF',  # Balsam fir
    `4`  = 'RS',  # Red spruce
    `5`  = 'WS',  # White spruce
    `6`  = 'EH',  # Eastern hemlock
    `7`  = 'EC',  # Eastern cottonwood
    `10` = 'WP',  # Eastern white pine
    `11` = 'TA',  # Tamarack
    `15` = 'RM',  # Red maple
    `16` = 'PB',  # Paper birch
    `18` = 'QA',  # Quaking aspen
    `20` = 'GB',  # Gray birch
    `31` = 'WA',  # White ash
    `32` = 'AB',  # American beech
    `33` = 'YB',  # Yellow birch
    `35` = 'SM',  # Sugar maple
    `36` = 'RO'   # Red oak
  )

  result <- lookup[as.character(as.integer(PEF.SPP))]
  result[is.na(result)] <- 'OH'
  as.character(result)
}
