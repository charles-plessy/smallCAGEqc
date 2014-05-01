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
        multiplex <- paste0( '/osc-fs_home/scratch/moirai/nanoCAGE2/input/'
                           , LIBRARY
                           , '.multiplex.txt')
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
    rownames(x) <- sub(paste0(LIBRARY, '.'),'', x$V1)
    libs$extracted <- x[rownames(libs), 'extracted']
    libs$mapped    <- x[rownames(libs), 'genome_mapped']
    libs$rdna      <- x[rownames(libs), 'removed_rrna']
    libs$tagdust   <- x[rownames(libs), 'removed_artefacts']
    libs$spikes    <- x[rownames(libs), 'removed_spikes']
    libs$extracted <- libs$extracted - libs$spikes
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
