#' fldgmPicoGreen
#' 
#' Reads Fluidigm C1 cDNA yields from fluorometric measures entered in a template Excel sheet.
#'
#' After a C1 run, the PCR products are transferred to a 96-well plate and their
#' concentration can be quantified by a fluorometric (PicoGreen) assay in a
#' 384-well plate.  Fluidigm provides template Excel sheets in which to paste the
#' raw results, that calculates concentrations using the standard curve,
#' replicates and blank wells of the 384-well-plate.
#' 
#' This function locates the results in the Excel sheet and returns them as a data
#' frame. Only the templates PN 100-6260 and PN 100-6160 are supported, other
#' templates (if any) were not tested.
#' 
#' @param FILE     Reads Fluidigm C1 cDNA yields from fluorometric measures entered in a template Excel sheet.
#' @param TEMPLATE  devtools::document()A template identification number.
#' @param FORMAT   Long or wide.
#' @param RUN      A string representing the run name (for instance, the serial ID of the capture array.
#' 
#' @return 
#' Returns a data frame indicating containing cDNA concentrations in nanograms per
#' microliter for each well.
#'
#' In the "long" format, the data frame has 4 columns ("well", "row", "column" and
#' "concentration") and 96 rows, sorted by well from A01, A02, ... to H11, H12.
#' Row names are either well names, or run and well names, separated by an
#' underscore (\sQuote{_}).
#'
#' With a "wide" format, returns a data frame with 8 rows (A to H) and 12 columns
#' (01 to 12).
#'  
#' @examples
#' ## With a file called "1772-064-102.picogreen.xlsx":
#' ##
#' ## fldgmPicoGreen(FILE="1772-064-102.picogreen.xlsx"
#' ##
#' ## In command line:
#' ##
#' ## Rscript -e 'smallCAGEqc::fldgmPicoGreen(FILE="1772-064-102.picogreen.xlsx", TEMPLATE="PN 100-6260", RUN="1772-064-102")



fldgmPicoGreen <- function(FILE, TEMPLATE='PN 100-6260', FORMAT='long', RUN) {

  if      (TEMPLATE == 'PN 100-6260')
    linesToSkip = 45
  else if (TEMPLATE == 'PN 100-6160')
    linesToSkip = 41
  else
    stop ('Unknown template number.')

  if (! FORMAT %in% c("long", "wide"))
    stop ("FORMAT should be 'long' or 'wide'.")

  picogreen <- read.xls( FILE
                       , sheet =  3
                       , skip  = linesToSkip
                       , nrow  =  8
                       , head  = FALSE
                       , blank.lines.skip = FALSE )[,1:13]

  if ( !all (picogreen[[1]] == c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')))
    stop ('Could not find data. Are you indicating the right template ?')

  colnames(picogreen) <- c('row', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

  if (FORMAT == "long") {
    picogreen <- melt.data.frame(picogreen, id.vars='row')
    colnames(picogreen) <- c('row', 'column', 'concentration')
    picogreen[,"well"] <- paste(picogreen$row, picogreen$column, sep='')
    picogreen <- picogreen[ order(picogreen$well)
                          , c('well', 'row', 'column', 'concentration')]
    if (missing(RUN)) {
       rownames(picogreen) <- picogreen$well
    } else {
       picogreen$run <- RUN
       rownames(picogreen) <- paste(picogreen$run, picogreen$well, sep='_')
    }
    return(picogreen)
  } else {
    return(data.frame( picogreen
                     , row.names   = 1
                     , check.names = FALSE))
  }
}
