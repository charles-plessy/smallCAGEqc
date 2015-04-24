.loadLogs <- function(source) {
    logfiles <- list.files(path='.', pattern='*log')
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
        } else if (grepl('nano-fluidigm', PROCESSED_DATA)) {
            multiplex <- '/osc-fs_home/scratch/moirai/nano-fluidigm/input/default.multiplex.txt'
        } else {
            stop('Could not dectect a known Moirai user in PROCESSED_DATA')
        }
    }
    if (missing(summary)) {
        summary <- paste0( PROCESSED_DATA, '/text/summary.txt')
    }
    libs <- read.table( multiplex
                      , sep='\t'
                      , header=T
                      )[,c('samplename', 'group', 'barcode', 'index')]
    rownames(libs) <-libs$samplename

    x <- cast( data=read.table( summary
                              , sep='\t')
                              , value='V3'
                              , V1 ~ V2)
    if (grepl('nano-fluidigm', PROCESSED_DATA)) {
        # Ignore Read 2 and Undetermined
        sampleNames <- x$V1
        linesToKeep <- ! grepl('(_R2_|Undetermined)', sampleNames)
        x           <- subset(x,           linesToKeep)
        sampleNames <- subset(sampleNames, linesToKeep)
        # Reorder from sample 1 to 96.
        x <- x[order(as.numeric(sub('_.*','', sampleNames))),]
        rownames(x) <- rownames(libs)
        libs$extracted <- x[rownames(libs), 'raw']
        libs$mapped    <- x[rownames(libs), 'genome_mapped']
        libs$rdna      <- x[rownames(libs), 'removed_rrna']
        libs$tagdust   <- x[rownames(libs), 'removed_artifacts'] # Note the different spelling
        libs$spikes    <- x[rownames(libs), 'removed_spikes']
        libs$extracted <- libs$extracted - libs$spikes
    } else if (grepl('CAGEscan_short-reads_full-deduplication', PROCESSED_DATA)) {
        rownames(x) <- sub(paste0(LIBRARY, '.'),'', x$V1)
# remove later
        rownames(x) <- sub('_READ1', '', rownames(x))
        libs$extracted <- x[rownames(libs), 'umi_extracted']
        libs$mapped    <- x[rownames(libs), 'genome_mapped']
        libs$rdna      <- x[rownames(libs), 'removed_artifacts']
        libs$counts    <- x[rownames(libs), 'exact_transcript_count']
    } else {
        rownames(x) <- sub(paste0(LIBRARY, '.'),'', x$V1)
        libs$extracted <- x[rownames(libs), 'extracted']
        libs$mapped    <- x[rownames(libs), 'genome_mapped']
        libs$rdna      <- x[rownames(libs), 'removed_rrna']
        libs$tagdust   <- x[rownames(libs), 'removed_artefacts']
        libs$spikes    <- x[rownames(libs), 'removed_spikes']
        libs$extracted <- libs$extracted - libs$spikes
    }
    return(libs)
}

setGeneric( "loadLogs"
          , signature=c("source", "multiplex", "summary")
          , function(source, multiplex, summary)
              standardGeneric("loadLogs")
)

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
