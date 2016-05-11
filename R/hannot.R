#' hannot
#' 
#' Hierarchical annotation of the CAGE clusters
#' 
#' Parses a vector of annotations in character format an returns a vector of
#' annotations as factors, where one single keyword has been selected from a
#' provided list or the default list (see below).
#' 
#' @param annot A character vector of annotations.
#' @param hierarchy Either the character \dQuote{default}, or a data frame
#'        whith patterns in its first column and keywords in its second
#'        column.  The default data frame is picked when \dQuote{default} is
#'        passed as argument.
#' 
#' The default data frame is as follows (as of version 0.2.1).
#' 
#' \preformatted{
#' None               None
#' snoscan            other
#' antisense          antisense
#' intron,sense\\|    intron
#' exon,sense\\|      exon
#' lncRNA\\|          exon
#' miRNA\\|           miRNA
#' snRNA\\|           snRNA
#' snoRNA\\|          snoRNA
#' tRNA\\|            tRNA
#' }
#' 
#' @return A vector of annotations as factors, where the levels are sorted in
#' the hierarchical order.  For instance, with the data frame above, if an
#' annotation matches for the keywords \sQuote{miRNA} and \sQuote{snoRNA},
#' then the reported one will be \sQuote{snoRNA}.
#' 
#' @examples
#' 
#' x <- c('exon,sense|foo', 'foo', 'antisense of bar', 'intron,sense|bar'
#'       , 'antisense of baz;snoRNA|SNORA70')
#' hannot(x)
#' cbind(x, as.character(hannot(x)))

hannot <- function (annot, hierarchy="default") {

# Default hierarchy

if ( hierarchy == "default") {
  hierarchy <- data.frame(
    rbind(
    c("None",            "None"),
    c("snoscan",         "other"),
    c("antisense",       "antisense"),
    c("intron,sense\\|", "intron"),
    c("exon,sense\\|",   "exon"),
    c("lncRNA\\|",       "exon"),
    c("miRNA\\|",        "miRNA"),
    c("snRNA\\|",        "snRNA"),
    c("snoRNA\\|",       "snoRNA"),
    c("tRNA\\|",         "tRNA")),
    stringsAsFactors=FALSE
  )
}

hannot <- rep("other", length(annot))
for (i in 1:nrow(hierarchy))
  hannot[grepl(hierarchy[i,1], annot)] <- hierarchy[i,2]

factor(hannot, levels=unique(hierarchy[[2]]))
}
