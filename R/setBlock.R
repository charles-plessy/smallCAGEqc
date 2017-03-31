#' setBlock
#' 
#' Define properties for a rectangular block in a 96- or 384-well plate.
#' 
#' Update a data frame representing a multiwell plate, by setting a given value
#' for all wells in a block or a list of blocks defined by the well coordinates
#' of their upper-left and bottom-right corners.
#' 
#' @param plate A data frame representing a multiwell plate ("long format").
#' @param block Coordinates of a rectangular block, or a vector of coordinates.
#' @param what A column name in the plate data frame.
#' @param value The value to insert or update with.
#' 
#' @examples 
#' \dontrun{
#' plate <- setBlock(plate, "A01~C04", "dNTP", 0.5)
#' 
#' # Same with magrittr
#' plate %<>% setBlock("A01~C04", "dNTP", 0.5)
#' 
#' # Chained updates with magrittr
#' plate %<>%
#'   setBlock("A01~C04", "dNTP", 0.5) %>%
#'   setBlock("A01~C04", "Mg",   3.0)
#' }
#' 
#' p <- data.frame(Well = platetools::num_to_well(1:96))
#' p$Row <- sub("..$", "", p$Well) %>% factor
#' p$Col <- sub("^.",  "", p$Well) %>% as.numeric %>% factor
#' 
#' p %>% setBlock("A01~B02", "dNTP", 0.5) %>% head
#' p %>% setBlock(c("A01~B02", "D01~D02"), "dNTP", 0.5) %>% head
#' 
#' 
#' @importFrom magrittr '%>%'
#' @importFrom platetools num_to_well
#' @export setBlock

setBlock <- function(plate, block, what, value) {
  
  # First, define a function that works on a single block.
  setBlock_ <- function(plate, block) {
    startWell <- sub("~.*", "",  block)
    endWell   <- sub(".*~", "",  block)
    startRow  <- substr(startWell, 1,1)
    endRow    <- substr(endWell,   1,1)
    startCol  <- substr(startWell, 2,3) %>% as.numeric
    endCol    <- substr(endWell,   2,3) %>% as.numeric
    Rows      <- LETTERS[seq(which(LETTERS == startRow), which(LETTERS == endRow))]
    Cols      <- seq(startCol, endCol)
    plate[plate$Row %in% Rows & plate$Col %in% Cols, what] <- value
    plate
  }
  
  # Then, apply it to every blocks supplied.
  Reduce(setBlock_, block, plate)
}