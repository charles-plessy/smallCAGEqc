#' ranges2annot
#' 
#' hierarchical annotation of a Genomic Range as promoter, exon or intron.
#' 
#' @param ranges The Genomics Ranges object, for example extracted from a
#'               RangedSummarizedExperiment object with the \code{rowRanges}
#'               command.
#' 
#' @param annot A Genomics Ranges object providing enough information for
#'              inferring promoters, exons and introns.  Typically GENCODE.
#' 
#' 
#' @return A factor of same length as the GRanges object, indicating if the
#'         interval is promoter, exon, intron or unknown.
#'         
#' @family smallCAGEqc annotation functions
#' 
#' @examples
#' \dontrun{
#' rowRanges(l1)$annotation <- ranges2annot(rowRanges(l1), gff)
#' }
#' 
#' @export ranges2annot
#' @importFrom GenomicRanges findOverlaps promoters

ranges2annot <- function(ranges, annot, showClasses=NULL) {
  classes <- c("promoter", "exon", "intron", "unknown")
  typesWithPromoter <- c( "protein_coding", "processed_transcript", "lincRNA"
                          , "antisense", "processed_pseudogene"
                          , "unprocessed_pseudogene")
  
  if (! missing(showClasses) ) return(classes)
  
  findOverlapsBool <- function(A, B)
    findOverlaps(A, B) %>% as(., "List") %>% any
  
  p <- findOverlapsBool( ranges
                         , promoters( annot[ annot$type == "transcript"
                                           & annot$transcript_type %in% typesWithPromoter]
                                      , 500, 500))
  e <- findOverlapsBool(ranges, annot[annot$type == "exon"])
  t <- findOverlapsBool(ranges, annot[annot$type == "transcript"])
  
  sapply( 1:length(ranges), function(i) {
    if (p[i]) {classes[1]}
    else if (e[i]) {classes[2]}
    else if (t[i]) {classes[3]}
    else           {classes[4]}
  }) %>% factor(levels = classes)
}

#' ranges2genes
#' 
#' Assign gene symbol(s) to Genomic Ranges.
#' 
#' @param ranges The Genomics Ranges object, for example extracted from a
#'               RangedSummarizedExperiment object with the \code{rowRanges}
#'               command.
#' 
#' @param annot A Genomics Ranges object providing enough information for
#'              finding gene symbols.  Typically GENCODE.
#' 
#' 
#' @return A character vector of same length as the GRanges object, indicating
#'         one gene symbol or a comma-separated list of gene symbols for each
#'         range.
#'         
#' @family smallCAGEqc annotation functions
#' 
#' @examples
#' \dontrun{
#' rowRanges(l1)$genes  <- ranges2genes(rowRanges(l1), gff)
#' }
#' 
#' @export ranges2genes
#' @importFrom GenomicRanges findOverlaps
#' @importFrom S4Vectors List unstrsplit
#' @importFrom IRanges extractList

ranges2genes <- function(ranges, genes)
  findOverlaps(ranges, genes) %>%
    as(., "List") %>%
    extractList(genes$gene_name, .) %>%
    unique %>%
    unstrsplit(";")