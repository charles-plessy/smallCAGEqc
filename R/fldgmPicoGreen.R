fldgmPicoGreen <- function(FILE, TEMPLATE='PN 100-6260') {

  if      (TEMPLATE == 'PN 100-6260')
    linesToSkip = 45
  else if (TEMPLATE == 'PN 100-6160')
    linesToSkip = 41
  else
    stop ('Unknown template number.')

  picogreen <- read.xls( FILE
                       , sheet =  3
                       , skip  = linesToSkip
                       , nrow  =  8
                       , head  = FALSE
                       , blank.lines.skip = FALSE )[,1:13]

  colnames(picogreen) <- c('row', '01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12')

  tryCatch( data.frame( picogreen
                       , row.names   = 1
                       , check.names = FALSE)
          , error = function (...) stop ('Could not find data. Are you indicating the right template ?')
  )
}
