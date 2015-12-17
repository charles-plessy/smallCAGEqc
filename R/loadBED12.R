#' loadBED12
#' 
#' Load CAGE data from a file in BED12 format.
#' 
#' Loads a BED12-formatted file in a \code{data.table}, and adds an extra column
#' holding the sample name (same for each row).  If no sample name is provided, 
#' it is created from the file name by removing the \code{.bed} and compression 
#' extensions.
#' 
#' We use the BED12 format to represent data related to the CAGE and CAGEscan 
#' methods.  In brief, in a file repesents \sQuote{CAGEscan pairs}, then each
#' line is one read count and the score is the sum of the mapping qualities of
#' both reads.  In files representing \sQuote{CAGEscan fragments}, each line is
#' one RNA molecule and the score is the number of CAGEscan pairs that were used
#' to build the fragment.  For \sQuote{CAGEscan clusters}, each line is one
#' transcript model and the score is the number of molecules used to build the
#' model.
#' 
#' @param file Name of the BED12 file or full path to it.
#' @param samplename Optional. Name of the sample represented by the file.
#'   
#' @return \code{loadBED12} always returns a \code{data.table}, so that it can 
#'   safely be used in a accumulator loop.  It will check if the file exists and
#'   if it is not empty, because in our current pipeline, absence of emptiness 
#'   can happen when a sample contains no properly paired reads.  In that case, 
#'   it will return an empty \code{data.table},
#'   
#' @seealso \code{\link{bedFieldNames}}, \code{\link{data.table}}
#'   
#' @examples 
#' fileA <- system.file("extdata", "BED12_A.bed", package="smallCAGEqc")
#' fileB <- system.file("extdata", "BED12_B.bed", package="smallCAGEqc")
#' fileC <- system.file("extdata", "BED12_C.bed", package="smallCAGEqc")
#' 
#' loadBED12(fileA, "A")
#' Reduce( function(X,Y) {rbind(X, loadBed(Y))}
#'       , c(fileA, fileB, fileC)
#'       , data.table::data.table())
#'   
#' @export loadBED12

loadBED12 <- function(file, samplename) {
  
  if (! file.exists(file))
    stop(paste("Could not find file:", file))
  
  if (file.info(file)$size == 0 )
    return(data.table::data.table())
  
  if (missing(samplename))
    samplename <- sub(".bed(.gz|.bz2|.xz|)", "", basename(file))
  
  DT <- data.table::fread(file, sep="\t")
  data.table::setnames(DT, bedFieldNames())
  DT$library <- samplename
  DT
  
}