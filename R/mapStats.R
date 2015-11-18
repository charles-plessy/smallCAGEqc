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
#'
#' @return
#' Returns mean and standard deviation of normalised mapping statistics, plus absolute
#' positions for the error bars.
#' 
#' @seealso \code{\link{hierarchAnnot}}, \code{\link{loadLogs}}, \code{\link{plotAnnot}}
#' 
#' @examples
#' libs <- read.csv(system.file("extdata", "libs.csv", package = "smallCAGEqc"))
#' mapStats(libs)
#' 
#' @importFrom magrittR '%>%'

mapStats <- function(libs, scope=c("all", "annotation", "counts", "mapped", "qc"), group="default") {
    
  scope <- match.arg(scope)
  if (identical(group, "default")) {
    if (! "group" %in% colnames(libs))
      stop(paste("Missing", dQuote("group"), "column in the data frame."))
    group <- libs$group
  }
  
  if (scope == 'all') {
    if (is.numeric(libs$extracted)){
      total <- libs$extracted
    } else {
      stop("libs$extracted missing or erroneous.") }
  } else if (scope == 'annotation') {
    if (is.numeric(libs$mapped)) {
      total <- libs$mapped
    } else {
      stop("libs$mapped missing or erroneous.") }
  } else if (scope == 'counts') {
    if (is.numeric(libs$counts)) {
      total <- libs$counts
    } else {
      stop("libs$counts missing or erroneous.") }
  } else if (scope == 'mapped') {
    if (is.numeric(libs$mapped)) {
      total <- libs$mapped
    } else {
      stop("libs$mapped missing or erroneous.") }
  } else if (scope == "qc") {
    total <- libs$extracted
  }
  
  if (! ("tagdust" %in% colnames(libs)))
    libs$tagdust <- 0
  
  if (scope == "counts") {
    libs$intergenic = with(libs, counts - promoter - intron - exon)
    columns <- c("promoter","exon","intron","intergenic")
  } else if (scope == "mapped") {
    libs$intergenic = with(libs, counts - promoter - intron - exon)
    libs$duplicates = with(libs, mapped - counts)
    columns <- c("promoter","exon","intron","intergenic", "duplicates")
  } else if (scope == "qc") {
    libs$mapped %<>% subtract(libs$properpairs)
    libs$properpairs %<>% subtract(libs$counts)
    columns <- c("counts", "properpairs", "mapped", "spikes", "rdna", "tagdust")
  } else {
    libs$mapped <- with(libs, mapped  - promoter - intron - exon)
    columns <- c("promoter","exon","intron","mapped","rdna", "tagdust")
  }
  
  doMean <- function (X) tapply(libs[,X] / total, group, mean)
  doSd   <- function (X) tapply(libs[,X] / total, group, sd  )
  
  mapstats          <- sapply(columns, doMean) %>% data.frame
  mapstats$group    <- rownames(mapstats)
  
  mapstats.sd       <- sapply(columns, doSd)   %>% data.frame
  mapstats.sd$group <- rownames(mapstats.sd)
  
  mapstats          <- melt(mapstats)
  mapstats$sd       <- melt(mapstats.sd)$value
  
  mapstats          <- ddply( mapstats
                            , .(group)
                            , transform
                            , ystart = cumsum(value)
                            , yend   = cumsum(value) + sd)
  
  mapstats
}