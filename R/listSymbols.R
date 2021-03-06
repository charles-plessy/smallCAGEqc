#' listSymbols
#' 
#' List unique gene symbols, which some can be comma-separated.
#' 
#' Takes a serie of strings containing either one gene symbol or
#' comma-separated gene symbols, character vector of unique gene symbols.
#' 
#' @param ... Vector(s) or list(s) of character strings.
#' 
#' Multiple genes can overlap at a given position on the genome.  Therefore,
#' it is hard to associate directly a single-base TSS to a single gene symbol.
#' In our workflows we prepare gene expression tables where TSS counts are pooled
#' per gene symbol.  If a position belongs to more than one gene, an artificial
#' ad-hoc symbol is created by concatenating the symbols with commas.  For
#' instance, \sQuote{7SK,ACTR5}.  As a result, one can not infer the number of
#' detected genes by simply counting the number of rows where expression is 
#' igher that zero.
#' 
#' \code{listSymbols} is the solution to that problem.  It will concatenate
#' with commas a list of row names from such gene expression tables, and then
#' expand it again and remove duplicates.  That is, \sQuote{"7SK,ACTR5",
#' "7SK,ADAM10"} becomes \sQuote{"7SK,ACTR5,7SK,ADAM10"} and then
#' \sQuote{"7SK" ,ACTR5", "ADAM10"}.  \code{listSymbols} will also search
#' and remove the \dQuote{.} gene symbol, which is a special artefact of
#' our workflows.
#' 
#' @return Returns a vector of unique character strings, or \code{NULL} if
#' the input contained no strong, the empty string alone, or the special
#' symbol \dQuote{.}.
#' 
#' @examples 
#' listSymbols("7SK,ACTR5", "7SK,ADAM10")
#' length(listSymbols("7SK,ACTR5", "7SK,ADAM10"))
#' listSymbols("")
#' length(listSymbols(""))
#' 
#' @export listSymbols

listSymbols <- function(...) {
  symbols <- c(...)
  if (length(symbols) == 0)
    return(NULL)
  if (length(symbols) == 1) if (symbols %in% c("", "."))
    return(NULL)
  symbols %>%
  subset(subset=( . != '.')) %>%
  paste(collapse=',') %>%
  read.table(text=., colClasses="character", sep=',') %>%
  unlist %>%
  unique
}
