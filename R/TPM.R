TPM <- function(clusters){
  
  if (! is.data.frame(clusters))
    stop("Input must be a data frame.")
  
  if (any(colSums(clusters) == 0))
    warning("Some samples have no counts; NaN values will be returned.")

clusters.tpm <- data.frame(prop.table(as.matrix(clusters),2) * 1000000)

colnames(clusters.tpm) <- colnames(clusters)

return(clusters.tpm)

}
