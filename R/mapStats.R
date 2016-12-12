#' mapStats
#' 
#' Process mapping statistics
#' 
#' Using a data frame containing mapping statistics in counts, transform the data in
#' percentages that can be used for stacked barplots.
#' 
#' See http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2 about stacked barplot.
#' 
#' The \dQuote{mapped} and \dQuote{counts} scopes assume that transcript counts are available.
#' 
#' @param libs A data frame with columns named \code{promoter}, \code{exon}, \code{intron}
#'        \code{mapped}, \code{extracted}, \code{rdna}, and \code{tagdust}.
#' @param scope The value on which to normalise. \dQuote{all} and \dQuote{qc}
#'        normalise on the number of extracted tags, \dQuote{annotation} on the
#'        number of aligned tags, \dQuote{mapped} on the number of aligned tags
#'        and \dQuote{counts} on the transcript counts.
#' @param group A vector of factors defining groups in the data.  By default,
#'        the \dQuote{group} column of the \dQuote{libs} table.
#' @param customScope A function that implements a custom scope.  Use with
#'        \code{scope = "custom"}.  The function takes a data frame in input
#'        and returns a named list containing a data frame (\dQuote{libs}),
#'        a character vector of columns to be plotted (\dQuote{columns}), and
#'        a numeric vector of totals for the normalisation (\dQuote{total}).
#'
#' @return
#' Returns a data frame with mean and standard deviation of normalised mapping statistics,
#' plus absolute positions for the error bars.  The first column, \code{group}, is
#' a vector of factors sorted with the \code{gtools::mixedorder} function.
#' 
#' @details 
#' 
#' See the plotAnnot vignette for details on what the scopes are.
#' 
#' @family smallCAGEqc annotation functions
#' @seealso \code{\link{loadLogs}}, \code{\link{loadMoiraiStats}}
#' 
#' @examples
#' example(loadMoiraiStats)
#' mapStats(libs, "qc")
#' 
#' @export mapStats
#' @importFrom gtools mixedorder
#' @importFrom reshape melt

mapStats <- function( libs
                    , scope = c( "all"
                               , "annotation"
                               , "counts"
                               , "mapped"
                               , "qc"
                               , "steps"
                               , "custom")
                    , group="default"
                    , customScope = NULL)
{
  scope <- match.arg(scope)
  if (identical(group, "default")) {
      if      ("group" %in% colnames(libs)) {
      group <- libs$group
    } else if ("Group" %in% colnames(libs)) {
      group <- libs$Group
    } else
      stop(paste("Missing", dQuote("group"), "column in the data frame."))
  }
  
  totalIs <- function(what) {
    if (! what %in% colnames(libs))
      stop( paste(what, "column is missing, see the plotAnnot vignette.")
           , call. = FALSE)
    if (is.numeric(libs[, what])) {
      total <<- libs[, what]
    } else stop(paste(what, "column is not numeric."), call. = FALSE)
  }
    
  defaultToZero <- function(what)
    if (! (what %in% colnames(libs)))
      libs[, what] <<- 0
  
  defaultToZero("tagdust")
  
  if (scope == "counts") {
    totalIs("counts")
    columns <- c("Promoter", "Exon", "Intron", "Intergenic")
    libs %<>% within({
      Promoter   <- promoter
      Exon       <- exon
      Intron     <- intron
      Intergenic <- counts - promoter - intron - exon
    })
  } else if (scope == "custom") {
    stopifnot(is.function(customScope))
    custom.list <- customScope(libs)
    libs    <- custom.list$libs
    columns <- custom.list$columns
    total   <- custom.list$total
  } else if (scope == "mapped") {
    totalIs("mapped")
    columns <- c( "Promoter", "Exon", "Intron", "Intergenic"
                , "Duplicates", "Non_proper")
    libs %<>% within({
      Non_proper <- mapped - properpairs
      Duplicates <- properpairs - counts
      Intergenic <- counts - promoter - intron - exon
      Intron     <- intron
      Exon       <- exon
      Promoter   <- promoter
    })
  } else if (scope == "qc") {
    totalIs("extracted")
    columns <- c( "Tag_dust", "rDNA", "Spikes", "Unmapped"
                , "Non_proper", "Duplicates", "Counts")
    libs %<>% within({
      Tag_dust     <- extracted   - rdna - spikes - cleaned
      rDNA         <- rdna
      Spikes       <- spikes
      Unmapped     <- cleaned     - mapped
      Non_proper   <- mapped      - properpairs
      Duplicates   <- properpairs - counts
      Counts       <- counts
    })
   } else if (scope == "steps") {
    totalIs("extracted")
    columns <- c("Cleaning", "Mapping", "Deduplication", "Counts")
    libs %<>% within({
      Cleaning      <- extracted   - cleaned
      Mapping       <- cleaned     - properpairs
      Deduplication <- properpairs - counts
      Counts        <- counts
    })
    if ("total" %in% colnames(libs)) {
      totalIs("total")
      libs$Extraction <- with(libs, total - extracted)
      columns <- c("Extraction", columns)
    }
   } else {
    if (scope == "all")        totalIs("extracted")
    if (scope == "annotation") totalIs("mapped")
    columns <- c("promoter","exon","intron","mapped","rdna", "tagdust")
    libs %<>% within({
       mapped <- mapped - promoter - intron - exon
    })
  }
  
  doMean <- function (X) tapply(libs[,X] / total, group, mean)
  doSd   <- function (X) tapply(libs[,X] / total, group, sd  )
  
  # "simplify" needs to be FALSE so that conversion to data frame works even
  # when the group contains only a single level.
  mapstats          <- sapply(columns, doMean, simplify = FALSE) %>% data.frame
  mapstats$group    <- rownames(mapstats)
  mapstats[gtools::mixedorder(mapstats$group), ]
    mapstats$group    %<>% factor(mapstats$group %>% unique)
  
  mapstats.sd       <- sapply(columns, doSd, simplify = FALSE)   %>% data.frame
  mapstats.sd$group <- rownames(mapstats.sd)
  
  mapstats          <- melt(mapstats,    id.vars="group")
  mapstats$sd       <- melt(mapstats.sd, id.vars="group")$value
  
  mapstats          <- plyr::ddply( mapstats
                                  , plyr::.(group)
                                  , transform
                                  , ystart = cumsum(value)
                                  , yend   = cumsum(value) + sd)
  
  mapstats
}
