#' plotSlimSmear
#' 
#' Slimmer plots with a bit of data loss
#' 
#' To reduce the size of SVG or PDF plots, this function rounds the data and
#' remove the duplicate points at the same coordinates.
#' 
#' @param COMP A DGELRT object from edgeR.
#' @param MAIN The title of the plot.
#' @param ROUND The decimal for the rounding.
#' @param PCH The \code{pch} arguments passed to the \code{points} function
#' @param CEX The \code{cex} arguments passed to the \code{points} function
#' 
#' 
#' Significantly over- and under-represented items are plotted in red and blue
#' respectively.
#' 
#' Using this function creates data loss.  Try rounding at different decimals and
#' pick one that do not change the visual appearance of the plot significantly.
#' 
#' @seealso 
#' \code{\link[edgeR]{plotSmear}}
#' 
#' @examples
#' \dontrun{
#' plotSlimSmear(obj.comp, 'Here is the title')
#' }
#' 
#' @export plotSlimSmear

plotSlimSmear <- function(COMP, MAIN, ROUND=2, PCH=19, CEX=1) {

  if (!requireNamespace("edgeR", quietly = TRUE))
    stop( "edgeR is needed for this function to work. Please install it."
        , call. = FALSE)
  
  dgeTable <- COMP$table[,c('logCPM', 'logFC')]

  roundUnique <- function(TABLE) TABLE %>% round(ROUND) %>% unique
  up          <- function (X)    rownames(X)[edgeR::decideTestsDGE(X) > 0]
  down        <- function (X)    rownames(X)[edgeR::decideTestsDGE(X) < 0]
  subTable    <- function(LIST)  dgeTable[LIST, ] %>% roundUnique

  upList        <-   up(COMP)
  downList      <- down(COMP)
  notSignifList <- rownames(COMP) %>% setdiff (c(upList, downList))

  if (missing(MAIN))
    MAIN <- deparse(substitute(COMP))

  plot(dgeTable, main=MAIN, type='n')
  grid()
  notSignifList %>% subTable %>% points(col='grey',  pch=PCH, cex=CEX)
  upList        %>% subTable %>% points(col='blue', pch=PCH, cex=CEX)
  downList      %>% subTable %>% points(col='red', pch=PCH, cex=CEX)
}
