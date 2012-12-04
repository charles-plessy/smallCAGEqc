loadLogs <- function() {

logs <- data.frame(variable=factor(), sample=factor(), value=numeric())

if ( file.exists('extracted.log') )
  logs <- rbind(logs, read.table('extracted.log', col.names=colnames(logs)))

if ( file.exists('tagdust.log') )
  logs <- rbind(logs, read.table('tagdust.log',   col.names=colnames(logs)))

if ( file.exists('spikes.log') )
  logs <- rbind(logs, read.table('spikes.log',    col.names=colnames(logs)))

if ( file.exists('rdna.log') )
  logs <- rbind(logs, read.table('rdna.log',      col.names=colnames(logs)))

if ( file.exists('mapped.log') )
  logs <- rbind(logs, read.table('mapped.log',    col.names=colnames(logs)))

logs <- reshape::cast(logs)
rownames(logs) <- logs$sample

logs
}
