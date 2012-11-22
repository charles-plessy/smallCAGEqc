TPM <- function(clusters){

clusters.tpm <- data.frame(prop.table(as.matrix(clusters),2) * 1000000)

colnames(clusters.tpm) <- colnames(clusters)

return(clusters.tpm)

}
