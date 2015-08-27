#' hanabiPlot
#' 
#' Plot feature discovery curves
#' 
#' Plots the number of features (genes, transcripts, ...) detected for a
#' given number of counts (reads, unique molecules, ...).  Each library is
#' sub-sampled by rarefaction at various sample sizes, picked to provide
#' enough points so that the curves look smooth.  The final point is plotted
#' as an open circle, hence the name "hanabi", which means fireworks in
#' Japanese. 
#' 
#' The rarefactions take time to do, so this step is done by a separate
#' function, so that the result is easily cached.
#' 
#' @param RAR A rarefaction table.
#' @param S A vector of subsample sizes.
#' @param log The scale of the plot (by default, log on both axis).
#' @param type What to plot at the end of the curve (by default,
#'  open circles).
#' @param ... Further arguments to be passed to the first plot function,
#'  that plots the empty frame.
#' @param GROUP A vector of factors grouping the samples.
#' 
#' @seealso vegan


hanabiPlot <- function (RAR, S, log='xy', type='o', GROUP, ...) {
  
  # Accessory function to define the range of the empty frame's axis.
  minMax <- function (X) c( min(X, na.rm=TRUE)
                          , max(X, na.rm=TRUE))
  
  # Accessory function to make the lines a little transparent.
  # See https://gist.github.com/mages/5339689#file-add-alpha-r
  add.alpha <- function(col, alpha=1)
    apply( sapply(col, col2rgb)/255
           , 2
           , function(x)
             rgb(x[1], x[2], x[3], alpha=alpha))
  
  # Accessory function to prepare an empty frame.
  emptyFrame <- function ()
    plot( minMax(S)
          , minMax(RAR)
          , type='n'
          , log=log
          , ...)
  
  # Accessory function to plot the lines.
  rarLines  <- function (X)
    lines( S
        , RAR[X,]
        , col=add.alpha(as.numeric(GROUP[X]),0.5)
        , type=type)
  
  
  # Eliminate data points past a cell's sampling size.
  # Insert NAs everytime the subsampled number of genes
  # equals the total number of genes (since it means that
  # subsampling size was larger than real sampling size),
  # except for the first occurence, which is the last
  # point of the curve.
  shiftTrueLeft <- function(TABLE) {
    n <- ncol(TABLE)
    TABLE <- cbind(F, TABLE)
    TABLE[, 1:n]
  }
  
  RAR[shiftTrueLeft(RAR==apply(RAR, 1, max))] <- NA
  
  emptyFrame()
  sapply( 1:nrow(RAR), rarLines)
  points( x = apply(RAR, 1, function(X) max(s[!is.na(X)])) # sampling sizes for each cell
          , y = apply(RAR, 1, max, na.rm=T) # num. of detected feat. at max. sampl. size
          , col=as.numeric(GROUP))
}