#' pcaCompOrientation
#' 
#' Returns \code{TRUE} if the principal component outputed from \code{dudi.pca} 
#' object (package \sQuote{ade4}) is mostly oriented towards positive values ; 
#' else returns \code{FALSE}.
#' 
#' Take the principal component vector and check which extremity value is the farthest from 0.
#' 
#' @param compPca The vector of variable scores for a given principal component.
#'   
#' @return A boolean object is returned. 
#'   
#' @seealso \code{\link{ade4::dudi.pca}}
#'   
#' @examples
#' library(ade4)
#' data("doubs")
#' pcaCompOrientation(dudi.pca(doubs$env, scannf = F, nf = 3)$co[, 1])
#' pcaCompOrientation(dudi.pca(doubs$env, scannf = F, nf = 3)$co[, 3])
#' 
#' @export pcaCompOrientation

pcaCompOrientation <- function(compPca){
  ifelse(abs(range(compPca)[1]) > abs(range(compPca)[2]), FALSE, TRUE)
}

#' pcaCompGenesList 
#' 
#' Return the genes sorted based on their score on a given principal component ; also returns 
#' scores on the other components.
#' 
#' Check if there are 3 principal components. If TRUE, then create a \sQuote{geneNames} column
#' based on the rownames of the \sQuote{pcaAde4co} \code{data.frame}. Then rearrange the column 
#' for \sQuote{geneNames} to be at the beginning. Then order the current \code{data.frame} based 
#' on column \code{comp}. 
#' 
#' @param pcaAde4co The \code{data.frame} of variable scores output from the \sQuote{co} 
#' element of a \code{dudi.pca} object (package \sQuote{ade4}).
#' @param comp An integer designating the principal component we want to sort .
#' the \code{data.frame} on.
#'   
#' @return Only if 3 principal components were kept, returns a sorted \code{data.frame} based 
#' on principal component number \code{comp}. First column is the gene name. Second to 
#' fourth columns are score values of the gene on each principal components.
#'   
#' @seealso \code{\link{ade4::dudi.pca}}
#'   
#' @examples 
#' library(ade4)
#' data("doubs")
#' pcaCompGenesList(dudi.pca(doubs$env, scannf = F, nf = 3)$co, 1)
#' pcaCompGenesList(dudi.pca(doubs$env, scannf = F, nf = 3)$co, 3)
#'   
#' @export pcaCompGenesList

pcaCompGenesList <- function(pcaAde4co, comp){
  stopifnot(ncol(pcaAde4co) == 3)
  
  genesCo <- pcaAde4co %>%
    mutate(geneNames = rownames(.)) %>% 
    select(geneNames, Comp1, Comp2, Comp3)
  
  ifelse(pcaCompOrientation(genesCo[comp+1]),
    genesCo %<>% setorderv(colnames(.)[comp+1], order=-1),
    genesCo %<>% setorderv(colnames(.)[comp+1], order=1))
  
  genesCo
}

#' plotHTB
#' 
#' Return a barplot of the first and last genes revealed by the principal 
#' component (from \code{dudi.pca}) object (package \sQuote{ade4}).
#' 
#' Take the \code{data.frame} outputed from \sQuote{pcaCompGenesList} and display on 
#' the same barplot, the \code{head(., nbDispGenes)} and \code{tail(., nbDispGenes)} with 
#' the gene names below each bar.
#' 
#' @param orderedCompPca A \code{data.frame} outputed from \sQuote{pcaCompGenesList}.
#' @param comp An integer designating the principal component we want to plot the barplot on.
#' @param nbDispGenes An integer designating how many genes we want to display.
#'   
#' @return Display a barplot. Invisibly returns names and values for head and tail.
#'   
#' @seealso \code{\link{ade4::dudi.pca}} \code{\link{pcaCompGenesList}}
#'   
#' @examples 
#' library(ade4)
#' data("doubs")
#' plotHTB(pcaCompGenesList(dudi.pca(doubs$env, scannf = F, nf = 3)$co, 1), 1, 10)
#' plotHTB(pcaCompGenesList(dudi.pca(doubs$env, scannf = F, nf = 3)$co, 3), 3, 10)
#'   
#' @export plotHTB

# Barplots of head and tail genes for each component
plotHTB <- function(orderedCompPca, comp, nbDispGenes = 25){
  par(mfrow=c(1, 2))
  
  bp1h <- orderedCompPca[, comp+1] %>% 
    head(nbDispGenes) %>%
    barplot(. ,
      ylim = c(min(orderedCompPca[, comp+1]), max(orderedCompPca[, comp+1])),
      axes = FALSE, axisnames = FALSE, main = paste0("comp ", comp, " head"))
  text(bp1h, par("usr")[3], labels = orderedCompPca$geneNames %>% head(nbDispGenes),
    srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=1)
  axis(2)
  
  bp1t <- orderedCompPca[, comp+1] %>%
    tail(nbDispGenes) %>%
    barplot(. ,
      ylim = c(min(orderedCompPca[, comp+1]), max(orderedCompPca[, comp+1])),
      axes = FALSE, axisnames = FALSE, main = paste0("comp ", comp, " tail"))
  text(bp1t, par("usr")[3], labels = orderedCompPca$geneNames %>% tail(nbDispGenes),
    srt = 90, adj = c(1.1,1.1), xpd = TRUE, cex=1)
  axis(4)
  
  invisible(list(headName = orderedCompPca$geneNames %>% head(nbDispGenes),
    tailName = orderedCompPca$geneNames %>% tail(nbDispGenes),
    headValues = orderedCompPca[, comp+1] %>% head(nbDispGenes),
    tailValues = orderedCompPca[, comp+1] %>% tail(nbDispGenes)))
}

#' pcaPlots
#' 
#' Take the object outputed from a \code{dudi.pca} object (package \sQuote{ade4}) and
#' returns a list of plots of the genes and cells processed by the PCA according to a
#' variety of axis combination.
#' 
#' Take the object outputed from a \code{dudi.pca} object (package \sQuote{ade4}) and
#' create a plot for genes (variables) and cells (individuals) according to differents
#' combination of axis. If axis 3,2 plot has its absciss coordinates reversed to fit
#' a more visual configuration when plotting all 3 dimensions on three side-by-side graphs. 
#' For now can only process PCA with 3 axis (\code{nf=3} as argument of \code{dudi.pca}).
#' 
#' @param pcaAde4 An object outputed from a \code{dudi.pca} object (package \sQuote{ade4}).
#' @param cellcol A vector of colors regarding the individuals (cells). If not provided, all points will be black.
#' @param size An integer designating the size of the text on the plots.
#' @param transparency A numeric (0-1) designating the transparency of the text to display on plots.
#'   
#' @return A list of plots of \sQuote{genes} (variables in PCA) or \sQuote{cells} 
#' (individuals or samples). Each element contains a list of plots of a variety of combination of axis.
#'   
#' @seealso \code{\link{ade4::dudi.pca}}
#'   
#' @examples 
#' library(ade4)
#' library(adegraphics)
#' data("doubs")
#' pcaPlots(dudi.pca(doubs$env, scannf = F, nf = 3))$genes$ax12
#' pcaPlots(dudi.pca(doubs$env, scannf = F, nf = 3), cellcol = rainbow(nrow(doubs$env)))$cells$ax12
#' 
#' @export pcaPlots

pcaPlots <- function(pcaAde4, cellcol=NULL, size = 1, transparency = 0.6, optim = T){
  stopifnot(pcaAde4$nf == 3)
  
  # Need to check if cellcol is defined
  if (is.null(cellcol))
    cellcol <- rep("black", nrow(pcaAde4$li))
  
  if (nrow(pcaAde4$li) != length(cellcol))
    paste0("Error: length(cellcol) [", length(cellcol), "] is not equal to the number of cells [", nrow(pcaAde4$li), "]")
  
  genePlot <- function(absciss, ordinate){
    if ((absciss == 3) && (ordinate == 2)) {
      # g32 seems more practical when displaying all 3 axes in the same plot
      # so we need to inverse coordinates
      g32df <- data.frame(-1*pcaAde4$co[, absciss], pcaAde4$co[, ordinate])
      rownames(g32df) <- rownames(pcaAde4$co)
      s.arrow(g32df,
        plabels = list(box = list(draw = F), optim = optim, cex = size, alpha = transparency),
        main = paste0(absciss,",",ordinate),
        xlab = paste0("comp ",absciss),
        ylab = paste0("comp ",ordinate),
        plot = F)
    }
    else {
      s.arrow(pcaAde4$co[, c(absciss, ordinate)],
        plabels = list(box = list(draw = F), optim = optim, cex = size, alpha = transparency),
        main = paste0(absciss, ",", ordinate),
        xlab = paste0("comp ", absciss),
        ylab = paste0("comp ", ordinate),
        plot = F)
    }
  }
  
  cellPlot <- function(absciss, ordinate){
    if ((absciss == 3) && (ordinate == 2)) {
      # g32 seems more practical when displaying all 3 axes in the same plot
      # so we need to inverse coordinates
      c32df <- data.frame(-1*pcaAde4$li[, absciss], pcaAde4$li[, ordinate])
      rownames(c32df) <- rownames(pcaAde4$li)
      s.label(c32df,
        plabels = list(box = list(draw = F), optim = optim, cex = size, alpha = transparency, col = cellcol),
        ppoints = list(col = cellcol, cex = size),
        main = paste0(absciss, ",", ordinate),
        xlab = paste0("comp ", absciss),
        ylab = paste0("comp ", ordinate),
        plot = F)
    }
    else {
      s.label(pcaAde4$li[, c(absciss, ordinate)], 
        plabels = list(box = list(draw = F), optim = optim, cex = size, alpha = transparency, col = cellcol),
        ppoints = list(col = cellcol, cex = size),
        main = paste0(absciss, ",", ordinate),
        xlab = paste0("comp ", absciss),
        ylab = paste0("comp ", ordinate),
        plot = F)
    }
  }
  
  return(list(
    genes = list(ax12 = genePlot(1, 2), ax32 = genePlot(3, 2), ax13 = genePlot(1, 3), ax23 = genePlot(2, 3)),
    cells = list(ax12 = cellPlot(1, 2), ax32 = cellPlot(3, 2), ax13 = cellPlot(1, 3), ax23 = cellPlot(2, 3))
  ))
}