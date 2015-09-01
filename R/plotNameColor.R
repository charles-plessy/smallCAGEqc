#' plotNameColor
#' 
#' XY plot of colored labels
#' 
#' Labels typically represent sample names, for instance 96-well-plate
#' coordinates.  Colors group the samples according to quality controls,
#' experimental design, etc.
#' 
#' Currently the plot colors are hardocded.  Dark green, cyan and yellow
#' are used because the light versions are hard to see on a screen.
#' 
#' @param X Coordinates on the horizontal axis.
#' @param Y Coordinates on the vertical axis.
#' @param GROUP Factors indicating now to group the cells.
#' @param LABELS Labels to plot.


plotNameColor <- function(X, Y, GROUP, LABELS, LegendPos="topleft") {

    plotColors <- c("black","red", "darkgreen", "blue", "darkcyan", "magenta", "orange")

  plot( X, Y
      , type='n'
      , xlab=deparse(substitute(X))
      , ylab=deparse(substitute(Y)))
  text( X, Y
      , LABELS, col=plotColors[unclass(GROUP)]) # as.numeric would transform 1,3,3,1 to 1,2,2,1...

    legend( LegendPos
          , legend=levels(GROUP)
          , col=plotColors[1:nlevels(GROUP)]
          , pch=1)
}