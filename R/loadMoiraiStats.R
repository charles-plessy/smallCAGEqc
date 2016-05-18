#' loadMoiraiStats
#' 
#' Load mapping statistics from log files
#' 
#' Loads mapping counts and other statistics produced during processing. This
#' function deprecates \code{loadLogs("moirai")}.  It loads data from a summary file
#' and a multiplex file.  When their path is not given by \code{multiplex} and
#' \code{summary}, they will be searched at fixed locations in the
#' \code{PROCESSED_DATA} directory using the \code{LIBRARY} variable.
#' 
#' \code{loadMoiraiStats} will recognise the \sQuote{nano-fluidigm}
#' or the \sQuote{nanoCAGE2} Moirai users, or fail.  For the \sQuote{nano-fluidigm}
#' user, the samples are sorted by numbers and associated to sorted well names, from
#' A01, A02, ..., to H11 and H12.
#' 
#' @param multiplex Optional. Path to a \sQuote{multiplex} file.
#' @param summary Optional. Path to a \sQuote{summary} file.
#' @param pipeline Optional. Version string identifying the pipeline used to process the data.
#' @param ercc Optional. [Experimental] Return ERCC spike counts.
#' 
#' @return Returns a data frame with one row per sample, and the following columns (if the
#' corresponding data is available).
#' \enumerate{
#'  \item{samplename} {Sample identifier (factor)}
#'  \item{total} {Number of demultiplexed reads (if available).}
#'  \item{extracted} {Number of extracted reads}
#'  \item{cleaned} {Numbers of reads after removing spikes, rRNA, and other artefacts}
#'  \item{tagdust} {Number of reads containing oligonucleotide artefacts}
#'  \item{spikes} {Number of reads overlaping with the reference spike sequences}
#'  \item{rdna} {Number of reads overlaping with the reference ribosomal DNA sequences}
#'  \item{mapped} {Number of reads aligned to the reference genome}
#'  }
#'  
#'  Alternatively, returns ERCC spike counts when \sQuote{ercc} is set to \sQuote{TRUE}.
#'  
#' @family smallCAGEqc metadata functions
#' @seealso \code{\link{hierarchAnnot}}, code{\link{loadLogs}}, \code{\link{mapStats}}
#' 
#' @examples 
#' libs <- loadMoiraiStats(
#'     summary   = system.file("extdata/summary.txt", package="smallCAGEqc")
#'   , multiplex = system.file("extdata/samplename_to_sampleid.txt", package="smallCAGEqc")
#'   , pipeline  = "OP-WORKFLOW-CAGEscan-short-reads-v2.0" )
#'         
#' libs$group <- libs$samplename %>% sub("Run._", "", .) %>% substr(1,1) %>% factor

loadMoiraiStats <- function(multiplex, summary, pipeline, ercc = FALSE) {

  # Use heuristics if parameters are missing.
  
  if (missing(pipeline)) {
    if (! exists("PROCESSED_DATA"))
      stop("PROCESSED_DATA must exist or the 'pipeline' argument must be passed.")
    pipeline <- PROCESSED_DATA
  }
  
  if (missing(multiplex)) {
    if (! exists("PROCESSED_DATA"))
      stop("PROCESSED_DATA must exist or the 'multiplex' argument must be passed.")
    if (grepl('nanoCAGE2', PROCESSED_DATA)) {
      multiplex <- paste0( '/osc-fs_home/scratch/moirai/nanoCAGE2/input/'
                         , LIBRARY
                         , '.multiplex.txt')
    } else if (grepl('OP-WORKFLOW-CAGEscan-short-reads-v2.0', PROCESSED_DATA)) {
      multiplex <- '/osc-fs_home/scratch/moirai/nano-fluidigm/input/samplename_to_sampleid.txt'
    } else if (grepl('nano-fluidigm', PROCESSED_DATA)) {
      multiplex <- '/osc-fs_home/scratch/moirai/nano-fluidigm/input/default.multiplex.txt'
    } else {
      stop('Could not dectect a known Moirai user in PROCESSED_DATA')
    }
  }
    
  if (missing(summary)) {
    if (! exists("PROCESSED_DATA"))
      stop("PROCESSED_DATA must exist or the 'summary' argument must be passed.")
      summary <- paste0( PROCESSED_DATA, '/text/summary.txt')
  }
  
  # Load the "multiplex" file in a table called "libs".

  libs <- read.table( file   = multiplex
                    , sep    = '\t'
                    , header = T )
  rownames(libs) <- libs$samplename
  
  # Load the "summary" file in a table called "moirai".  
  
  moirai <- reshape::cast( data=read.table( summary, sep='\t', comment.char = '')
                         , value='V3'
                         , V1 ~ V2)
  moirai[is.na(moirai)] <- 0
  rownames(moirai) <- moirai$V1
  if(nrow(moirai) == 96) libs <- libs[1:96,]
  
  moiraiToLibs <- function(COL) {
    if (! COL %in% colnames(moirai))
      return(rep(0, nrow(libs)))
    values <- moirai[rownames(libs), COL]
    values[is.na(values)] <- 0
    return(values)
  }
  
  if (ercc == TRUE) {
    if (file.exists("cms_095046.txt")) {
      ercc <- erccSpikeConcentration("cms_095046.txt")
    } else {
      ercc <- erccSpikeConcentration()
    }
    colnames(moirai) %<>% sub("\\|.*", "", .)
      ercc <- sapply(rownames(ercc), moiraiToLibs) %>%
        as.data.frame
      rownames(ercc) <- rownames(libs)
      return(ercc)
  }
  
  if (grepl('OP-WORKFLOW-CAGEscan-short-reads-v2.0', pipeline)) {
      libs$total       <- moiraiToLibs('raw')
      libs$extracted   <- moiraiToLibs('extracted')
      libs$cleaned     <- moiraiToLibs('non_reference_extracted')
      libs$tagdust     <- moiraiToLibs('filtered_for_artefact')
      libs$rdna        <- moiraiToLibs('filtered_for_rrna')
      libs$spikes      <- moiraiToLibs('filtered_for_spikes')
      libs$mapped      <- moiraiToLibs('genome_mapped')
      libs$properpairs <- moiraiToLibs('properly_mapped')
      libs$counts      <- moiraiToLibs('transcript_count')
# delete the following line if confirmed that it is also un-needed with nanoCAGE2 user
# libs$extracted   <- libs$extracted - libs$spikes

# raw                = extracted               + removed_by_extraction
# extracted          = non_reference_extracted + removed_references
# removed_references = filtered_for_spikes     + filtered_for_rrna
# genome_mapped      = properly_mapped         + removed_improper_pairs
    } else {
        rownames(moirai) <- sub(paste0(LIBRARY, '.'),'', moirai$V1)
        libs$extracted <- moiraiToLibs('extracted')
        libs$mapped    <- moiraiToLibs('genome_mapped')
        libs$rdna      <- moiraiToLibs('removed_rrna')
        libs$tagdust   <- moiraiToLibs('removed_artefacts')
        libs$spikes    <- moiraiToLibs('removed_spikes')
        libs$extracted <- libs$extracted - libs$spikes
    }
    return(libs)
}
