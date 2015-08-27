#' llPostProcess
#' 
#' Post-process the output of loadLogs.
#' 
#' @seealso loadLogs
#' 
#' @param TABLE A table output by loadLogs
#' @param TYPE A type of post-processing
#' 
#' @details 
#' 
#' If the TYPE is "nano-fluidigm", the generic "RunA" and "RunB" names are
#' replaced with the C1 run IDs found in the "RunA" and "RunB" variables,
#' which must exist.  These new sample names are then used are row names.
#' They are parsed to produce new columns for "Well", "row" and "column"
#' IDs in the 96-well plate format.

llPostProcess <- function (TABLE, TYPE='') {
  if (TYPE == "nano-fluidigm") {
    if (!exists("RunA") | !exists("RunB"))
      stop ('"RunA" and "RunB" must be defined.')
    TABLE$samplename <- sub('RunA', RunA, TABLE$samplename)
    TABLE$samplename <- sub('RunB', RunB, TABLE$samplename)
    rownames(TABLE) <- TABLE$samplename
    TABLE$Run <- sub("_.*", "", TABLE$samplename)
    TABLE$Run <- factor(TABLE$Run)
    TABLE$Well   <- sub('.*_', '', TABLE$samplename)
    TABLE$row    <- sub('.',   '', TABLE$Well)
    TABLE$column <- sub('..$', '', TABLE$Well)
  }
  TABLE
}