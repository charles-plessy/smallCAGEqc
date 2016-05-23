#' fldgmChamberWell
#'
#' Converts C1 array capture chamber numbers to 96-well names and vice-versa
#'
#' @param ... Either chamber numbers (nummeric format) or 96-well
#'        names (character format)
#'
#' @examples
#' fldgmChamberWell("E04")
#' fldgmChamberWell(c("E04", "A01"))
#' fldgmChamberWell(3, 2, 1)
#' fldgmChamberWell(c(3,2,1))
#' fldgmChamberWell(fldgmChamberWell(c(3,2,1)))
#' 
#' @export fldgmChamberWell

fldgmChamberWell <- function(...) {

input <- list(...)

# Conversion table

W2C <- data.frame(
   Well = c( "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12"
           , "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12"
           , "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12"
           , "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12"
           , "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12"
           , "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12"
           , "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12"
           , "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12"
           )
  , Chamber = c ( 03, 02, 01, 49, 50, 51, 06, 05, 04, 52, 53, 54
                , 09, 08, 07, 55, 56, 57, 12, 11, 10, 58, 59, 60
                , 15, 14, 13, 61, 62, 63, 18, 17, 16, 64, 65, 66
                , 21, 20, 19, 67, 68, 69, 24, 23, 22, 70, 71, 72
                , 25, 26, 27, 75, 74, 73, 28, 29, 30, 78, 77, 76
                , 31, 32, 33, 81, 80, 79, 34, 35, 36, 84, 83, 82
                , 37, 38, 39, 87, 86, 85, 40, 41, 42, 90, 89, 88
                , 43, 44, 45, 93, 92, 91, 46, 47, 48, 96, 95, 94
                )
  , stringsAsFactors = FALSE
  )

well2chamber <- function(wells) {
  well2chamber_ <- Vectorize (
    function(well) W2C[W2C$Well == well, "Chamber"]
  )
  well2chamber_(wells) %>% unname
}

chamber2well <- Vectorize (
  function(chamber) W2C[W2C$Chamber == chamber, "Well"]
)

allCharacters <- function(X) rapply(X, is.character) %>% all
allNumeric    <- function(X) rapply(X, is.numeric)   %>% all

input_ <- unlist(input, recursive = TRUE)

if (allCharacters(input)) {
  
  if (input_ %in% W2C$Well %>% all) {
    well2chamber(input_)
  } else stop ("Unknown well ID(s)")
  
} else if (allNumeric(input)) {
  
  if (input_ %in% W2C$Chamber %>% all) {
    chamber2well(input_)
  } else stop ("Unknown chamber ID(s)")
  
} else stop ("Input must be either well names (characters) or chamber IDs (numbers).")

}
