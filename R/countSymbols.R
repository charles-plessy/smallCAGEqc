#' countSymbols
#' 
#' Counts unique symbols from an expression table.
#' 
#' Takes an expression table and returns the number of detected genes for
#' each library.
#' 
#' Returns a named numeric vector where each element is the number of
#' different symbols found to be expressed in a given sample.  In the
#' expression table, columns represent samples and rows represent symbols.
#' Some rows can represent multiple samples, typically when a CAGE tag
#' overlaps with more than one reference transcript for different genes.
#' In that case, the row names are made of a comma-separated list of
#' symbols.  Thus, a symbol can be found on multiple rows.  This is
#' why this function is needed.
#' 
#' @param TABLE An expression table
#' 
#' @seealso code{\link{listSymbols}}
#' 
#' @examples 
#' exp.tbl <- data.frame(sample1=c(1,2), sample2=c(1,0), row.names=c("A", "A,B"))
#' countSymbols(exp.tbl)
#' 
#' @importFrom magrittr '%>%'
#' @export countSymbols

countSymbols <- function(TABLE) {
  countSymbolsVector <- function(x) {
    if (all (x == 0))
      return (0)
    rownames(TABLE)[x >0] %>%
    listSymbols %>%
    length
  }

  lapply(TABLE, countSymbolsVector) %>%
  unlist
}
