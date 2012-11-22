TPM <- function(clusters){

data.frame(prop.table(as.matrix(clusters),2) * 1000000)

}
