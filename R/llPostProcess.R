#' llPostProcess
#' 
#' Post-process the output of loadMoiraiStats
#' 
#' @seealso loadMoiraiStats
#' 
#' @param TABLE A table output by loadMoiraiStats.  Should at least contain
#'        a column called \sQuote{samplename}.
#' @param TYPE A type of post-processing.
#' 
#' @details 
#' 
#' If the TYPE is \sQuote{C1 96}, the sample names are assumed to be made
#' of a run name, followed by an underscore, followed by a coordinate in
#' 96-well plate format.  These sample names are parsed to parsed to produce
#' new columns for \sQuote{Run}, \sQuote{Well}, \sQuote{Row}, \sQuote{Column}
#' and \sQuote{Chamber} accordingly.
#' 
#' If the TYPE is \sQuote{nano-fluidigm}, the generic \sQuote{RunA} and
#' \sQuote{RunB} names are replaced with the C1 run IDs found in the
#' \code{RunA} and \code{RunB} variables, which must exist.  These new sample
#' names are then parsed like in the \sQuote{C1 96} type.
#' 
#' @family smallCAGEqc metadata functions
#' 
#' @examples
#' libs <-
#' loadMoiraiStats(
#'     summary   = system.file("extdata/summary.txt", package="smallCAGEqc")
#'   , multiplex = system.file("extdata/samplename_to_sampleid.txt", package="smallCAGEqc")
#'   , pipeline  = "OP-WORKFLOW-CAGEscan-short-reads-v2.0" )
#'   
#'  libs %>% head
#'  
#'  # Basic processing with "C1 96" mode.
#'  libs %>% llPostProcess("C1 96") %>% head
#'  
#'  # More extensive processing requires RunA and RunB to be set.
#'  RunA <- "ID-of-Run-A"
#'  RunB <- NA  # No RunB in this example
#'  libs %>% llPostProcess("nano-fluidigm") %>% head
#' 
#' @export llPostProcess

llPostProcess <- function (TABLE, TYPE='') {
  
  if(! "samplename" %in% colnames(TABLE))
    stop('Input table must have a "samplename" column.')
  
  samplesnamesToWell <- function(TABLE) {
    TABLE$Well    <- sub('.*_', '', TABLE$samplename)
    TABLE$Row     <- sub('.',   '', TABLE$Well)
    TABLE$Column  <- sub('..$', '', TABLE$Well)
    TABLE$Chamber <- fldgmChamberWell(TABLE$Well)
    TABLE
  }
  
  samplenamesToRun <- function(TABLE) {
    TABLE$Run <- sub("_.*", "", TABLE$samplename)
    TABLE$Run <- factor(TABLE$Run)
    TABLE
  }
  
  if (TYPE == "nano-fluidigm") {
    if (!exists("RunA") | !exists("RunB"))
      stop ('"RunA" and "RunB" must be defined.')
    TABLE$samplename <- sub('RunA', RunA, TABLE$samplename)
    TABLE$samplename <- sub('RunB', RunB, TABLE$samplename)
    rownames(TABLE) <- TABLE$samplename
    TABLE %<>% samplenamesToRun %>% samplesnamesToWell
  }
  
  if (TYPE == "C1 CAGE") {
    warning('Deprecated.  Use "C1 96" instead.')
    samplesnamesToWell.deprec <- function(TABLE) {
      TABLE$Well   <- sub('.*_', '', TABLE$samplename)
      TABLE$row    <- sub('.',   '', TABLE$Well)
      TABLE$column <- sub('..$', '', TABLE$Well)
      TABLE
    }
    TABLE %<>% samplenamesToRun %>% samplesnamesToWell.deprec
  }
  
  if (TYPE == "C1 96") {
    TABLE %<>% samplenamesToRun %>% samplesnamesToWell
  }
  
  TABLE
}
