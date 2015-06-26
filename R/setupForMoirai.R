setupForMoirai <- function( LIBRARY
                          , MOIRAI_USER
                          , MOIRAI_PROJECT
                          )
{

  GROUP_SHARED   <- '/osc-fs_home/scratch/gmtu'
  GENE_SYMBOLS   <- paste(GROUP_SHARED, 'annotation/homo_sapiens/gencode-14/gencode.v14.annotation.genes.bed', sep='/')
  ANNOTATION     <- paste(GROUP_SHARED, 'annotation/homo_sapiens/100712hg19/100712hg19', sep='/')

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

  exportInEnv( "LIBRARY", "MOIRAI_USER", "MOIRAI_PROJECT", "GROUP_SHARED"
             , "GENE_SYMBOLS", "ANNOTATION", "PROCESSED_DATA")

  return(PROCESSED_DATA)
}
