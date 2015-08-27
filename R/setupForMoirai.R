#' setupForMoirai
#'
#' Exports environment variables to find Moirai files.
#' 
#' LOCAL FUNCTION WITH A LOT OF HARDOCODING.  If no timestamp is given,
#' it will look for the latest processing.
#' 
#' Using LIBRARY, MOIRAI_USER and MOIRAI_PROJECT, \code{setupForMoirai} will find
#' the directory containing the last Moirai run for that library, and export its
#' path in the environment variable PROCESSED_DATA.
#' 
#' It will also set the environment variables GROUP_SHARED, GENE_SYMBOLS and
#' ANNOTATION, to default values.
#' 
#' @param LIBRARY A name identifying a library given in Moirai.
#' @param MOIRAI_USER A Moirai user name (part of the file path).
#' @param MOIRAI_PROJECT A Moirai project name (part of the file path).
#'        Note that some Moirai project names look like user names and
#'        vice versa.
#' @param MOIRAI_STAMP A Moirai time stamp.
#' @param GENOME A genome identifier, to get default annoation and gene definition.
#' 
#' @return 
#' Prints shell commands that, would have the same effect.  This is useful when
#' cut-pasting from knitR workflows that use R to change the environment and then
#' run some shell chunks.
#' 
#' Returns PROCESSED_DATA.
#'
#' @seealso exportInEnv
#' 
#' @examples 
#' 
#' ## PROCESSED_DATA <- setupForMoirai(
#' ##   LIBRARY        = '140609_M00528_0024_000000000-A8CBW',
#' ##   MOIRAI_USER    = 'nano-fluidigm',
#' ##   MOIRAI_PROJECT = 'C1_CAGE' )

setupForMoirai <- function( LIBRARY
                          , MOIRAI_USER
                          , MOIRAI_PROJECT
                          , MOIRAI_STAMP="latest"
                          , GENOME=c("hg19", "hg38")
                          )
{
  GROUP_SHARED   <- '/osc-fs_home/scratch/gmtu'
  
  if (identical(GENOME, "hg19")) {
    GENE_SYMBOLS   <- paste0(GROUP_SHARED, '/annotation/homo_sapiens/gencode-14/gencode.v14.annotation.genes.bed')
    ANNOTATION     <- paste0(GROUP_SHARED, '/annotation/homo_sapiens/100712hg19/100712hg19', sep='/')
  } else if (identical(GENOME, "hg38")) {
    GENE_SYMBOLS   <- paste0(GROUP_SHARED, '/annotation/homo_sapiens/gencode-23/gencode.v23.annotation.genes.bed')
    ANNOTATION     <- paste0(GROUP_SHARED, '/annotation/homo_sapiens/gencode-23/annot.bed')
  } else {
    stop(paste0("Unknown genome. Supported ones: ", paste(GENOME, collapse=' '), "."))
  }
  
  latest <- function(RUNS_NAMES) {
    position <- RUNS_NAMES %>%
                  sub('.*\\.','',.) %>%
                  order(decreasing = TRUE) %>%
                  head(1)
    return(RUNS_NAMES[position])
  }

  PROCESSED_DATA <- paste ("/osc-fs_home/scratch/moirai" , MOIRAI_USER , "project" , MOIRAI_PROJECT, sep='/') %>%
                    list.files(pattern=LIBRARY, full.names=TRUE) %>%
                    latest
  
  exportInEnv( "LIBRARY", "MOIRAI_USER", "MOIRAI_PROJECT", "GENE_SYMBOLS", "ANNOTATION", "PROCESSED_DATA")

  return(PROCESSED_DATA)
}
