#' TPM
#' 
#' Tags Per Million
#' 
#' Takes a data frame of tag counts and return a data frame of tags per million.
#' 
#' Ensures that the column names of the returned data frame are identical to the input.
#' 
#' @param clusters A data frame of tag counts, where each row is a cluster and each
#'        column is a library.
#'
#' @return Returns a data frame expression values normalised in tags per million,
#' where each row is a cluster and each column is a library.
#' 
#' @examples
#' TPM(data.frame(A=c(1,0,3), b=c(2,5,9)))
#' 
#' # Should warn about NaN
#' # TPM(data.frame(A=c(1,0,3), b=c(2,5,9), c=c(0,0,0)))
#' 
#' @export TPM

TPM <- function(clusters){
  
  if (! is.data.frame(clusters))
    stop("Input must be a data frame.")
  
  if (any(colSums(clusters) == 0))
    warning("Some samples have no counts; NaN values will be returned.")

clusters.tpm <- data.frame(prop.table(as.matrix(clusters),2) * 1000000)

colnames(clusters.tpm) <- colnames(clusters)

return(clusters.tpm)

}
