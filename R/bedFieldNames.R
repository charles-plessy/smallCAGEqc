#' bedFieldNames
#' 
#' Provide standard field names of BED files
#' 
#' See \url{http://www.genome.ucsc.edu/FAQ/FAQformat.html#format1 for details}.
#' 
#' @param n Number of fields
#' 
#' @examples 
#' bedFieldNames(6)

bedFieldNames <- function(n=12)
  c( "chrom"
   , "chromStart"
   , "chromEnd"
   , "name"
   , "score"
   , "strand"
   , "thickStart"
   , "thickEnd"
   , "itemRgb"
   , "blockCount"
   , "blockSizes"
   , "blockStarts") %>%
  head(n)
