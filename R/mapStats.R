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
#' Here is a description of each scope.
#' 
#' \describe{
#'   \item{all}{Pairs are categorised by extraction step and genome
#'   annotation.}
#'   \item{steps}{Pairs are categorised by the extraction steps described
#'   above (Total, Extracted, Cleaned, Mapped and Counts).}
#'   \item{qc}{Pairs are categorised as tag dust, rDNA, unmapped, spikes,
#'   non-proper, duplicates and counts, and normalised by the total number
#'   of extracted pairs.  Non-extracted pairs are ignored.}
#'   \item{mapped}{"promoter","exon","intron","intergenic", "duplicates"}
#'   \item{counts}{The unique molecule counts are grouped in
#'   annotation categories ("promoter", "exon", "intron" and "intergenic").}
#'   \item{annotation}{Same as \sQuote{all} except that normalisation is
#'   relative to the number of mapped reads.}
#' }
#' 
#' @family smallCAGEqc annotation functions
#' @seealso \code{\link{loadLogs}}, \code{\link{loadMoiraiStats}}
#' 
#' @examples
#' example(loadMoiraiStats)
#' mapStats(libs, "qc")
#' 
#' @export mapStats
#' @importFrom magrittr '%>%' '%<>%' subtract
#' @importFrom gtools mixedorder
#' @importFrom reshape melt

mapStats <- function( libs
                    , scope = c( "all"
                               , "annotation"
                               , "counts"
                               , "mapped"
                               , "qc"
                               , "steps")
                    , group="default")
{
  scope <- match.arg(scope)
  if (identical(group, "default")) {
    if (! "group" %in% colnames(libs))
      stop(paste("Missing", dQuote("group"), "column in the data frame."))
    group <- libs$group
  }
  
  totalIs <- function(what)
    if (is.numeric(libs[, what])) {
      total <<- libs[, what]
    } else stop(paste0("libs$", what, " missing or not numeric"))
    
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
  } else if (scope == "mapped") {
    totalIs("mapped")
    columns <- c("promoter","exon","intron","intergenic", "duplicates")
    libs %<>% within({
      intergenic = counts - promoter - intron - exon
      duplicates = mapped - counts
    })
  } else if (scope == "qc") {
    totalIs("extracted")
    columns <- c( "Tag_dust", "rDNA", "Unmapped", "Spikes"
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
      Cleaning      <- extracted - cleaned
      Mapping       <- cleaned   - mapped
      Deduplication <- mapped    - counts
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
