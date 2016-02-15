#' erccSpikeConcentration
#' 
#' Various data related to ERCC spikes
#' 
#' In quantitative transcriptome analysis, we often add synthetic RNA to the reaction for
#' quality control and normalisation, and the External RNA Controls Consortium (ERCC)
#' spikes are a popular choice, available commercially from Invitrogen (now Thermo Fisher).
#' In the commercial product, the spikes have different concentrations, covering six
#' orders of magnitude.  These concentrations are given in a text file on the vendor's
#' webiste, and I do not know if the file is freely redistributable, hence this function
#' to retreive the data from the Internet or a local file.
#' 
#' @param file File name or URL where to find the \sQuote{cms_095046.txt} text file.
#'             Defaults to the current URL on the Thermo Fisher website.
#' 
#' @return A data frame representing the file \sQuote{cms_095046.txt} from the vendors
#' website.
#' 
#' The original column names are \sQuote{Re-sort ID}, \sQuote{ERCC ID}, \sQuote{subgroup},
#' \sQuote{concentration in Mix 1 (attomoles/ul)},
#' \sQuote{concentration in Mix 2 (attomoles/ul)}, \sQuote{expected fold-change ratio},
#' \sQuote{log2(Mix 1/Mix 2)}, but this not fit well for a R data frame.  Therefore,
#' they are renamed as: \sQuote{sortID}, \sQuote{erccID}, \sQuote{subgroup},
#' \sQuote{concMix1}, \sQuote{concMix2}, \sQuote{FC}, \sQuote{log2FC}.
#' 
#' @seealso loadMoiraiStats
#' 
#' @export erccSpikeConcentration

erccSpikeConcentration <- function (file="http://tools.thermofisher.com/content/sfs/manuals/cms_095046.txt") {

  ercc <- read.table( file
                    , head=T
                    , sep="\t"
                    , colClasses = c( "numeric"
                                    , "character"
                                    , "factor"
                                    , rep("numeric", 4))
                    , col.names = c( "sortID"
                                   , "erccID"
                                   , "subgroup"
                                   , "concMix1"
                                   , "concMix2"
                                   , "FC"
                                   , "log2FC"))

  rownames(ercc) <- ercc$erccID
  ercc
}