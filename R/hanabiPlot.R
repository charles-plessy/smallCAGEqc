#' hanabi
#' 
#' Rarefy data at multiple sample sizes, in preparation for plotting.
#' 
#' The computation can be long, so the steps of rarefaction and plotting
#' are kept separate.
#' 
#' @param expr_data An expression table where columns are samples and rows
#'        are features such as genes, TSS, etc.
#' @param npoints The maximum number of rarefactions per sample.
#' @param step Subsample sizes are calculated by taking the largest sample
#'        and multiplying it by the step "npoints" times.
#' @param from Add one sample size (typically "0") in order to extend the
#'        plot on the left-hand side.
#'
#' @return A list-based object of class "hanabi".
#'
#' @family Hanabi functions
#' @seealso `[.hanabi`, as.list.hanabi, and vegan::rarecurve.
#' 
#' @importFrom vegan rarefy
#' @export hanabi
#' 
#' @examples
#' 
#' bedFiles <- system.file(package = "smallCAGEqc", "extdata") %>%
#'               list.files("*BED", full.names = TRUE)
#' bed <- loadBED12(bedFiles)
#' rar <- tapply(bed$score, bed$library, hanabi, from = 0) %>%
#'          structure(class = "hanabi")  # tapply discards the class !
#' hanabiPlot(rar, GROUP = levels(bed$library))

hanabi <- function( expr_data
                  , npoints = 20
                  , step = 0.75
                  , from = NULL) {
  if (is.null(dim(expr_data)) & is.integer(expr_data))
    expr_data %<>% data.frame
  ns <- step ^ (0:npoints)
  ns <- round(max(colSums(expr_data)) * ns)
  if (! is.null(from))
    ns <- c(ns, from)
  nraref <- function(lib) {
    ntags <- sum(lib)
    ns <- c(ntags, ns[ns < ntags])
    rarefy(lib, ns) %>% xy.coords(x = ns)
  }
  x <- lapply(expr_data, nraref)
  structure(x, class = "hanabi")
}

as.list.hanabi <- function(h)
  unclass(h)

`[.hanabi`  <- function(h, i)
  structure(as.list(h)[i], class = "hanabi")

#' points.hanabi
#' 
#' Add a final point in hanabi plots.
#' 
#' Will only add a point for the final, non-subsampled value of each
#' sample of in a hanabi object.
#' 
#' @param h The hanabi object.
#' @param ... Other parameters passed to the generic points function
#' 
#' @family Hanabi functions
#' @seealso hanabi, plot.hanabi
#' 
#' @export plot.hanabi

points.hanabi <- function(h, ...) {
  xmax <- sapply(h, function(x) max(x$x))
  ymax <- sapply(h, function(x) max(x$y))
  points(xmax, ymax, ...)
}

lines.hanabi  <- function(h, ...) {
  Map(lines, h, ...) %>% invisible
}

#' plot.hanabi
#' 
#' Plotting Hanabi objects
#' 
#' @param h The hanabi object to plot.
#' @param alpha The alpha transparency of the plot lines.
#' @param col A vector indicating a color per sample (or a vector that
#'        can be recycled that way).
#' @param xlab Horizontal axis label.
#' @param ylab Vertical axis label.
#' @param main Plot title.
#' @param pch Plot character at the tip of the lines.
#' @param ... other arguments passed to the generic plot function.
#' 
#' @family Hanabi functions
#' @seealso hanabi
#' 
#' @export plot.hanabi

plot.hanabi <-
  function( h
          , alpha = 0.5
          , col   = "black"
          , xlab  = "Total counts"
          , ylab  = "Unique features"
          , main  = "Hanabi plot"
          , pch   = 1
          , ...) {
  xmax <- sapply(h, function(x) max(x$x))
  xmin <- sapply(h, function(x) min(x$x))
  ymax <- sapply(h, function(x) max(x$y))
  ymin <- sapply(h, function(x) min(x$y))
  # Accessory function to make the lines a little transparent.
  # See https://gist.github.com/mages/5339689#file-add-alpha-r
  add.alpha <- function(col, alpha)
    apply( sapply(col, col2rgb) / 255
           , 2
           , function(x)
             rgb(x[1], x[2], x[3], alpha=alpha))
  plot( c(min(xmin), max(xmax))
      , c(min(ymin), max(ymax))
      , type="n"
      , xlab = xlab
      , ylab = ylab
      , main = main
      , ...)
  lines( h
       , col = add.alpha(col, alpha))
  points( h
        , col = col
        , pch = pch)
}

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
#' @param RAR A rarefaction table, or a hanabi object.
#' @param S A vector of subsample sizes.
#' @param GROUP A vector of factors grouping the samples.
#' @param ... Further arguments to be passed to the first plot function,
#'  that plots the empty frame.
#' @param legend.pos Position of the legend, passed as "x" parameter to the
#'        "legend" function.
#' @param pch Plot character at the tip of the lines.
#' 
#' @seealso vegan, plot.hanabi, hanabi
#' 
#' @examples
#' \dontrun{
#' hanabi(genes, npoints = 20, step = 0.8, from = 0) %>% hanabiPlot
#' hanabi(genes, npoints = 20, step = 0.9) %>% hanabiPlot
#' }
#' bedFiles <- system.file(package = "smallCAGEqc", "extdata") %>%
#'               list.files("*BED", full.names = TRUE)
#' bed <- loadBED12(bedFiles)
#' rar <- tapply(bed$score, bed$library, hanabi, from = 0) %>%
#'          structure(class = "hanabi")  # tapply discards the class !
#' hanabiPlot(rar, GROUP = levels(bed$library))
#' 
#' @family Hanabi functions
#' 
#' @importFrom vegan rarefy
#' @export hanabiPlot

hanabiPlot <- function ( RAR, S, GROUP=NULL
                       , legend.pos = "topleft", pch = 1, ...) {
  
  # Accessory function to make the lines a little transparent.
  # See https://gist.github.com/mages/5339689#file-add-alpha-r
  add.alpha <- function(col, alpha=1)
    apply( sapply(col, col2rgb) / 255
           , 2
           , function(x)
             rgb(x[1], x[2], x[3], alpha=alpha))

  if (class(RAR) == "hanabi") {
    if (! is.null(GROUP)) {
      GROUP %<>% factor
      col <- as.numeric(GROUP)
      plot(RAR, col = col, pch = pch, ...)
      legend( x = legend.pos
            , legend = levels(GROUP)
            , col = 1:nlevels(GROUP)
            , pch = pch)
    } else {
        plot(RAR, pch = pch, ...)
    }
    return(invisible())
  }
  
  # Accessory function to prepare an empty frame.
  emptyFrame <- function ()
    plot( range(S)
          , range(RAR, na.rm = TRUE)
          , type='n'
          , ...)
  
  # Accessory function to plot the lines.
  rarLines  <- function (X)
    lines( S
        , RAR[X,]
        , col=add.alpha(as.numeric(GROUP[X]),0.5))
  
  
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
  points( x = apply(RAR, 1, function(X) max(S[!is.na(X)])) # sampling sizes for each cell
          , y = apply(RAR, 1, max, na.rm=T) # num. of detected feat. at max. sampl. size
          , col=as.numeric(GROUP))
}
