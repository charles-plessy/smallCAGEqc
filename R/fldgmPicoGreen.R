fldgmPicoGreen <- function(FILE, TEMPLATE='PN 100-6260', FORMAT='long') {

  if      (TEMPLATE == 'PN 100-6260')
    linesToSkip = 45
  else if (TEMPLATE == 'PN 100-6160')
    linesToSkip = 41
  else
    stop ('Unknown template number.')

  if (! FORMAT %in% c("long", "wide"))
    stop ("FORMAT should be 'long' or 'wide'.")

  picogreen <- read.xls( FILE
                       , sheet =  3
                       , skip  = linesToSkip
                       , nrow  =  8
                       , head  = FALSE
                       , blank.lines.skip = FALSE )[,1:13]

  if ( !all (picogreen[[1]] == c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H')))
    stop ('Could not find data. Are you indicating the right template ?')

  colnames(picogreen) <- c('row', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

  meltConcentrationTable <- function (TABLE) {
    picogreen <- melt.data.frame(TABLE, id.vars='row')
    colnames(picogreen) <- c('row', 'column', 'concentration')
    picogreen[,"well"] <- paste(picogreen$row, picogreen$column, sep='')
    picogreen <- picogreen[ order(picogreen$well)
                          , c('well', 'row', 'column', 'concentration')]
    picogreen
  }

  if (FORMAT == "long")
    return(meltConcentrationTable(picogreen))
  else
    return(data.frame( picogreen
                     , row.names   = 1
                     , check.names = FALSE))
}
