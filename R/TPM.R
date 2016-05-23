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
#' @param margin 1 for rows and 2 for columns (default)
#'
#' @return Returns a data frame expression values normalised in tags per million,
#' where each row is a cluster and each column is a library.
#'
#' TPM uses prop.table() internally.
#' 
#' @seealso prop.table
#'  
#' @examples
#' TPM(data.frame(A=c(1,0,3), b=c(2,5,9)))
#' TPM(data.frame(A=c(1,0,3), b=c(2,5,9)), margin = 1)
#' 
#' # Should warn about NaN
#' # TPM(data.frame(A=c(1,0,3), b=c(2,5,9), c=c(0,0,0)))
#' 
#' @export TPM

TPM <- function(clusters, margin = 2){
  
  if (! is.data.frame(clusters))
    stop("Input must be a data frame.")
  
  if (any(colSums(clusters) == 0))
    warning("Some samples have no counts; NaN values will be returned.")

clusters.tpm <- clusters %>%
  as.matrix %>%
  prop.table(margin) %>%
  multiply_by(1e6) %>%
  data.frame

colnames(clusters.tpm) <- colnames(clusters)

return(clusters.tpm)

}
