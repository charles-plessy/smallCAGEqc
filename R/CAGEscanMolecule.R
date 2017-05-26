parseCAGEscanMolecule <- function (molecule) {
  parseBlockToGrange <- function(block) {
    chrBlock <- unlist(strsplit(block, ":"))
    chr <- chrBlock[1]
    coords <- as.integer(unlist(strsplit(chrBlock[2], "-")))
    if (coords[1] == coords[2]) stop("Blocks of width 1 not supported.")
    strand <- ifelse(coords[1] < coords[2], "+", "-")
    irange <- IRanges(min(coords), max(coords))
    GRanges(chr, irange, strand)
  }
  expandGrangeWithType <- function(grange, type) {
    start <- flank(grange, 1, start = TRUE)
    start$type <- substr(type, 1, 1)
    end   <- flank(grange, 1, start = FALSE)
    end$type <- substr(type, 2, 2)
    # F indicates first exon, G indicates full-spliced exon, E indicates exonic sequence
    grange$type <- switch(type, "S," = "F", ",," = "G", "E")
    c(start, grange, end)
  }
  parseBlocksAndSeparators <- function(molecule) {
    molecule <- gsub("([;,|])", "~\\1~", molecule)
    molecule <- strsplit(molecule, "~")
    molecule <- unlist(molecule)
    nblocks <- (length(molecule) + 1) / 2
    blocks <- molecule[(1:nblocks * 2) - 1]
    blocks <- lapply(blocks, parseBlockToGrange)
    # S indicates start site, T indicates termination site.
    # , ; | are splice, possible splice, and gap of coverage.
    seps <- molecule[(1:(nblocks -1) * 2)]          #      ,   ;   |
    seps <- c("S", seps, "T")                       #  S   ,   ;   |   T
    seps <- paste0(seps, tail(seps, -1))[1:nblocks] #     S,  ,;  ;|  |T
    Map(expandGrangeWithType, blocks, seps)
  }
  parseBlocksAndSeparators(molecule)
}

#' import.CAGEscanMolecule
#' 
#' Imports a CAGEscan \dQuote{molecule} file in a GRanges object
#' 
#' @param filepath The path to the \dQuote{molecule} file.
#' 
#' @seealso parseCAGEscanMolecule
#' 
#' @examples
#' # TODO import.CAGEscanMolecule(system.file("extdata", "example.molecule.txt", package = "CAGEr"))

import.CAGEscanMolecule <- function(filepath) {
  molecules <- unname(unlist(fread(select = 9, paste( "grep -v \\#", filepath))))
  unlist(GRangesList(sapply(molecules[1:600], function(X) parseCAGEscanMolecule(X)[[1]][1])))
    
    
  read.table( file = filepath
                         , header = F
                         , sep = "\t"
                         , col.names  = c("chr",       "pos",     "strand",    "score")
                         , colClasses = c("character", "integer", "character", "integer"))
  GRanges( seqnames = CTSS$chr
           , ranges   = IRanges(CTSS$pos, width = 1)
           , strand   = CTSS$strand
           , score    = CTSS$score)
}
