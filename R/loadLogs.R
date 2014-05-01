.loadLogs <- function(source) {
    if (source=="logs") {
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
    if (source=="moirai") {
        libs <- read.table( paste0( '/osc-fs_home/scratch/moirai/nanoCAGE2/input/'
                                  , LIBRARY
                                  , '.multiplex.txt')
                          , sep='\t'
                          , header=T
                          )[,c('samplename', 'group', 'barcode', 'index')]
        rownames(libs) <-libs$samplename

        x <- cast( data=read.table( paste0( PROCESSED_DATA, '/text/summary.txt')
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
    stop("Syntax: loadLogs(source='logs') or loadLogs(source='moirai')")
}

setGeneric( "loadLogs"
          , signature="source"
          , function(source)
              standardGeneric("loadLogs")
)

setMethod( loadLogs
         , signature=c(source="missing")
         , function(source)
             stop("Syntax: loadLogs(source='logs') or loadLogs(source='moirai')")
)

setMethod( loadLogs
         , signature=c(source="character")
         , .loadLogs )
