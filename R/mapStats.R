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
#'
#' @return
#' Returns a data frame with mean and standard deviation of normalised mapping statistics,
#' plus absolute positions for the error bars.  The first column, \code{group}, is
#' a vector of factors sorted with the \code{gtools::mixedorder} function.
#' 
#' @details 
#' 
#' The following categories describe the total remaining pairs after each
#' step of the processing.
#' 
#' \describe{
#'   \item{total}{The total number of pairs before tag extraction.  In some
#'   cases this number is not available per sample, for example when
#'   demultiplexing and tag extraction are performed at the same stage.}
#'   \item{extracted}{The number of pairs where the linkers and unique
#'   molecular identifier (if present) were succesfully extracted.}
#'   \item{cleaned}{The number of pairs remaining after filtering out spike,
#'   rRNA, low-complexity, primer artefact and other unwanted sequences.}
#'   \item{mapped}{The number of pairs with at least one successful
#'   alignment.}
#'   \item{counts}{The number of unique molecules counted after alignment.}
#' }
#' 
#' The following categories describe the number of pairs removed at each
#' step of the processing.
#' 
#' \describe{
#'   \item{unextracted}{The total number of pairs where a tag could not
#'   be extracted.}
#'   \item{spikes, rRNA}{The number of pairs removed because they matched
#'   spikes or rRNA reference sequences, respectively.}
#'   \item{tagdust}{The number of pairs removed because of low-complexity
#'   or similarity to primer artefacts.}
#'   \item{unmapped}{The number of non-mapped pairs.}
#'   \item{non-proper}{The number of non-properly mapped pairs.}
#'   \item{duplicates}{The number of pairs that do not add a molecule count.}
#' }
#' 
#' @family smallCAGEqc annotation functions
#' @seealso \code{\link{loadLogs}}, \code{\link{loadMoiraiStats}}
#' 
#' @examples
#' libs <- read.csv( system.file("extdata", "libs.csv", package = "smallCAGEqc")
#'                 , row.names = 1)
#' mapStats(libs, "qc", libs$Error)
#' 
#' @importFrom magrittR '%>%' '%<>%' subtract
#' @importFrom gtools mixedorder

mapStats <- function(libs, scope=c("all", "annotation", "counts", "mapped", "qc"), group="default") {
    
  scope <- match.arg(scope)
  if (identical(group, "default")) {
    if (! "group" %in% colnames(libs))
      stop(paste("Missing", dQuote("group"), "column in the data frame."))
    group <- libs$group
  }
  
  totalIs <- function(what) {
    if (is.numeric(libs[,what])) {
      total <<- libs[,what]
    } else {
      stop(paste0("libs$", what, " missing or not numeric")) }
  }
    
  if (scope == 'all')        totalIs("extracted")
  if (scope == "qc")         totalIs("extracted")
  if (scope == 'annotation') totalIs("mapped")
  if (scope == 'mapped')     totalIs("mapped")
  if (scope == 'counts')     totalIs("counts")
  
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
    libs$unmapped <- libs$extracted - libs$tagdust - libs$rdna - libs$spikes - libs$mapped
    libs$mapped %<>% subtract(libs$properpairs)
    libs$properpairs %<>% subtract(libs$counts)
    columns <- c("counts", "properpairs", "mapped", "unmapped", "spikes", "rdna", "tagdust")
  } else {
    libs$mapped <- with(libs, mapped  - promoter - intron - exon)
    columns <- c("promoter","exon","intron","mapped","rdna", "tagdust")
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
  
  mapstats          <- reshape::melt(mapstats,    id.vars="group")
  mapstats$sd       <- reshape::melt(mapstats.sd, id.vars="group")$value
  
  mapstats          <- plyr::ddply( mapstats
                                  , plyr::.(group)
                                  , transform
                                  , ystart = cumsum(value)
                                  , yend   = cumsum(value) + sd)
  
  mapstats
}
