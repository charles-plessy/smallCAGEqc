#' chrFreqs
#' 
#' Count features per chromosome
#' 
#' @param gr A GenomicRanges object.
#' @param chrs A character vector of chromosome names.
#' 
#' @return A named character vector indicating the number of feature
#' counts per chromosome in a GenomicRanges object.  This is done by
#' running GRanges' seqnames() function, converting the output to a
#' table, and converting the table to a named character vector, according
#' to the names passed to the function.  This is useful to create a
#' matrix or a data frame from a list of genomic ranges.
#' 
#' @examples 
#' if(require(rtracklayer, quietly = TRUE)) {
#'   gr <- system.file("extdata", "BED12_A.bed", package="smallCAGEqc") %>%
#'           rtracklayer::import.bed
#'   chrFreqs(gr, c("chr1", "chr2", "chrX", "chrY", "chrZ"))
#' }
#' 
#' @importFrom GenomicRanges seqnames
#' @importFrom BiocGenerics table
#' @export chrFreqs

chrFreqs <- function(gr, chrs)
  gr %>%
    GenomicRanges::seqnames() %>%
    BiocGenerics::table() %>%
    tableToNamedVector(chrs)

tableToNamedVector <- function(tbl, nms)
  tbl %>%
  extract(nms) %>%
  as.vector %>%
  set_names(nms)
