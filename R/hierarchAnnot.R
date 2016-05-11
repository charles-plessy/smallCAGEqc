#' hierarchAnnot
#' 
#' Hierarchical annotation of the CAGE clusters
#' 
#' Parse an annotation data frame and retains only one annotation,
#' in the order \sQuote{promoter}, \sQuote{exon}, \sQuote{intron}
#' and \sQuote{unknown}.
#' 
#' @param annot A data frame with a \code{feature} column that contains
#'        comma-separated annotations, like for instance \sQuote{exon,intron}.
#' 
#' The annotation data frame is typically loaded from a file produces with
#' commands such as the following.
#' 
#' \preformatted{
#'  zcat /analysisdata/annotations/100712hg19/refGeneTssProximal500.bed.gz | awk '{OFS="\t"}{print $1, $2, $3, "promoter", $5, $6}' >  100712hg19.annot
#'  zcat /analysisdata/annotations/100712hg19/refGeneExon.bed.gz |           awk '{OFS="\t"}{print $1, $2, $3, "exon",     $5, $6}' >> 100712hg19.annot
#'  zcat /analysisdata/annotations/100712hg19/refGene.bed.gz |               awk '{OFS="\t"}{print $1, $2, $3, "gene",     $5, $6}' >> 100712hg19.annot
#'  
#'  zcat $LIBRARY.l1.gz | grep -v \# | sed 1d | awk '{OFS="\t"}{print $2, $3, $4, "l2", "1000", $5}' > $LIBRARY.l1.bed
#'  zcat $LIBRARY.l2.gz | grep -v \# | sed 1d | awk '{OFS="\t"}{print $2, $3, $4, "l2", "1000", $5}' > $LIBRARY.l2.bed
#'  
#'  bedtools intersect -a $LIBRARY.l1.bed -b 100712hg19.annot -s -loj |
#'    awk '{OFS="\t"}{print $1":"$2"-"$3$6,$10}' | 
#'    bedtools groupby -g 1 -c 2 -o collapse > $LIBRARY.l1.annot
#'  
#'  bedtools intersect -a $LIBRARY.l2.bed -b 100712hg19.annot -s -loj |
#'    awk '{OFS="\t"}{print $1":"$2"-"$3$6,$10}' | 
#'    bedtools groupby -g 1 -c 2 -o collapse > $LIBRARY.l2.annot
#' 
#' @return The annotation table with the hierarchical annotation in a column
#' called \code{class}.
#' 
#' @family smallCAGEqc annotation functions
#' 
#' @seealso \code{\link{loadLogs}}
#' 
#' @export hierarchAnnot

hierarchAnnot <- function (annot) {
  annot$class <- 'other'
  annot[grepl("\\.",        annot$feature), "class"] <- 'unknown'
  annot[grepl("antisense",  annot$feature), "class"] <- 'antisense'
  annot[grepl("gene",       annot$feature), "class"] <- 'intron' # Because exons are overridden at next command.
  annot[grepl("exon",       annot$feature), "class"] <- 'exon'
  annot[grepl("promoter",   annot$feature), "class"] <- 'promoter' 
  annot$class <- factor(annot$class)
  annot
}
