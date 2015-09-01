#' loadLogs
#' 
#' Load mapping statistics from log files
#' 
#' Loads mapping counts and other statistics produced during processing.
#'  
#' With \code{source='logs'}, \code{loadLogs} will load data from every file
#' ending in sQuote{\code{.log}} in the work directory.  Thes files are expected
#' contain tab-separated triples, with first the name of the mapping statistics,
#' like extracted, mapped, rdna, etc., then the sample identifier, and then the
#' number of reads.  \code{loadLogs} will crash or produce incorrect output if the
#' files do not contain triples, or if the sample identifiers are not matched
#' correctly in the files, or if the first word of the triples appears in multiple
#' files.
#' 
#' With \code{source='moirai'}, \code{loadLogs} will load data from a summary file
#' and a multiplex file.  When their path is not given by \code{multiplex} and
#' \code{summary}, they will be searched at fixed locations in the
#' \code{PROCESSED_DATA} directory using the \code{LIBRARY} variable.
#' 
#' With \code{source='moirai'}, \code{loadLogs} will recognise the \sQuote{nano-fluidigm}
#' or the \sQuote{nanoCAGE2} Moirai users, or fail.  For the \sQuote{nano-fluidigm}
#' user, the samples are sorted by numbers and associated to sorted well names, from
#' A01, A02, ..., to H11 and H12.
#' 
#' @param source Indicate to load the data from log files in the current 
#'  directory, or from the summary file of Moirai.
#' @param multiplex Optional. Path to a \sQuote{multiplex} file.
#' @param summary Optional. Path to a \sQuote{summary} file.
#' 
#' @return Returns a data frame with one row per sample, and the following columns (if the
#' corresponding data is available).
#' \enumerate{
#'  \item{sample} {Sample identifier (factor)}
#'  \item{extracted} {Number of extracted reads}
#'  \item{tagdust} {Number of reads containing oligonucleotide artefacts}
#'  \item{spikes} {Number of reads overlaping with the reference spike sequences}
#'  \item{rdna} {Number of reads overlaping with the reference ribosomal DNA sequences}
#'  \item{mapped} {Number of reads aligned to the reference genome}
#'  }
#'  
#' @seealso \code{\link{hierarchAnnot}}, \code{\link{mapStats}}

setGeneric( "loadLogs"
            , signature=c("source", "multiplex", "summary")
            , function(source, multiplex, summary)
              standardGeneric("loadLogs")
)

.loadLogs <- function(source) {
    logfiles <- list.files(path='.', pattern='*\\.log')
    logs <- data.frame( variable=factor()
                      , sample=factor()
                      , value=numeric())
    for (logfile in logfiles) {
        logs <- rbind(logs, read.table(logfile, col.names=colnames(logs)))
    }
    logs <- cast(logs)
    rownames(logs) <- logs$sample
    return(logs)
}

.loadStats <- function(source, multiplex, summary) {
    if (missing(multiplex)) {
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
    if (missing(summary))
        summary <- paste0( PROCESSED_DATA, '/text/summary.txt')
    readMultiplex <- function (multiplex) {
       libs <- read.table( multiplex
                         , sep       = '\t'
                         , header    = T )
       if (grepl('OP-WORKFLOW-CAGEscan-short-reads-v2.0', PROCESSED_DATA)) {
         rownames(libs) <- libs$sampleid
       } else {
         rownames(libs) <- libs$samplename
       }
       return(libs)
    }
    libs <- readMultiplex(multiplex)
    moirai <- cast( data=read.table( summary
                                   , sep='\t')
                  , value='V3'
                  , V1 ~ V2)
    if(nrow(moirai) == 96) libs <- libs[1:96,]
    moiraiToLibs <- function(COL) {
      if (COL %in% colnames(moirai)) {
        moirai[rownames(libs), COL]
      } else 0
    }
    if (grepl('OP-WORKFLOW-CAGEscan-short-reads-v2.0', PROCESSED_DATA)) {
        libs$total       <- moiraiToLibs('raw')
        libs$extracted   <- moiraiToLibs('extracted')
        libs$tagdust     <- moiraiToLibs('filtered_for_artefact')
        libs$rdna        <- moiraiToLibs('filtered_for_rrna')
        libs$spikes      <- moiraiToLibs('filtered_for_spikes')
        libs$mapped      <- moiraiToLibs('genome_mapped')
        libs$properpairs <- moiraiToLibs('properly_mapped')
        libs$counts      <- moiraiToLibs('transcript_count')
        libs$extracted   <- libs$extracted - libs$spikes
# raw                = extracted               + removed_by_extraction
# extracted          = non_reference_extracted + removed_references
# removed_references = filtered_for_spikes     + filtered_for_rrna
# genome_mapped      = properly_mapped         + removed_improper_pairs
    } else if (grepl('nano-fluidigm', PROCESSED_DATA)) {
        # Ignore Read 2 and Undetermined
        sampleNames <- x$V1
        linesToKeep <- ! grepl('(_R2_|Undetermined)', sampleNames)
        x           <- subset(x,           linesToKeep)
        sampleNames <- subset(sampleNames, linesToKeep)
        # Reorder from sample 1 to 96.
        x <- x[order(as.numeric(sub('_.*','', sampleNames))),]
        rownames(x) <- rownames(libs)
        libs$extracted <- moiraiToLibs('raw')
        libs$mapped    <- moiraiToLibs('genome_mapped')
        libs$rdna      <- moiraiToLibs('removed_rrna')
        libs$tagdust   <- moiraiToLibs('removed_artifacts') # Note the different spelling
        libs$spikes    <- moiraiToLibs('removed_spikes')
        libs$extracted <- libs$extracted - libs$spikes
    } else {
        rownames(x) <- sub(paste0(LIBRARY, '.'),'', x$V1)
        libs$extracted <- moiraiToLibs('extracted')
        libs$mapped    <- moiraiToLibs('genome_mapped')
        libs$rdna      <- moiraiToLibs('removed_rrna')
        libs$tagdust   <- moiraiToLibs('removed_artefacts')
        libs$spikes    <- moiraiToLibs('removed_spikes')
        libs$extracted <- libs$extracted - libs$spikes
    }
    return(libs)
}

setMethod( loadLogs
         , signature=c(source="missing", multiplex="ANY", summary="ANY")
         , function(source)
             stop("Syntax: loadLogs(source='logs') or loadLogs(source='moirai')")
)

setMethod( loadLogs
         , signature=c(source="character", multiplex="ANY", summary="ANY")
         , function(source) {
               if (source=="logs")   {return (.loadLogs(source))  }
               if (source=="moirai") {return (.loadStats(source)) }
               stop("Syntax: loadLogs(source='logs') or loadLogs(source='moirai')")
           }
)

setMethod( loadLogs
         , signature=c(source="character", multiplex="character", summary="character")
         , function(source, multiplex, summary) {
               .loadStats(source, multiplex, summary)
           }
)