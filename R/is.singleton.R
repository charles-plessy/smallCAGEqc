is.singleton <- function(expression_table) {

if ( ! is.matrix(expression_table) & ! is.data.frame(expression_table) )
  stop("argument must be a matrix or a data frame.")
else if ( ! all ( round(expression_table) == expression_table ) )
  stop("argument must be expression counts (integer).")

rowSums(expression_table) < 2

}
