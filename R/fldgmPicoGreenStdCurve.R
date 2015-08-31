#' fldgmPicoGreenStdCurve
#' 
#' Reads standard curve from fluorometric measures entered in a template Excel sheet.
#' 
#' @param FILE A standard Excel sheet containing fluorometric measurements of Fluidigm C1 cDNA yields.
#'
#' @return 
#' A data frame with standard DNA concentration in the first column, and fluorescence intensity in the second.
#' 
#' @seealso fldgmPicoGreen
#' 
#' @examples
#' ## With a file called "1772-064-102.picogreen.xlsx":
#' ##
#' ## fldgmPicoGreenStdCurve(FILE="1772-064-102.picogreen.xlsx")

fldgmPicoGreenStdCurve <- function(FILE) {
  sc <- gdata::read.xls( FILE
                       , sheet=3
                       , skip=2
                       , nrow=10
                       , header=FALSE)[,c(2,5)]
  colnames(sc) <- c("DNA", "Fluorescence")
  sc
}
