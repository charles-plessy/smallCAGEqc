loadLogs <- function() {

logfiles <- list.files(path='.', pattern='*log')

logs <- data.frame(variable=factor(), sample=factor(), value=numeric())

for (logfile in logfiles) {logs <- rbind(logs, read.table(logfile, col.names=colnames(logs)))}

logs <- reshape::cast(logs)
rownames(logs) <- logs$sample

logs
}
