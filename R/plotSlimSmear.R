plotSlimSmear <- function(COMP, MAIN, ROUND=2, PCH=19, CEX=1) {

  dgeTable <- COMP$table[,c('logCPM', 'logFC')]

  roundUnique <- function(TABLE) TABLE %>% round(ROUND) %>% unique
  up          <- function (X)    rownames(X)[decideTestsDGE(X) > 0]
  down        <- function (X)    rownames(X)[decideTestsDGE(X) < 0]
  subTable    <- function(LIST)  dgeTable[LIST, ] %>% roundUnique

  upList        <-   up(COMP)
  downList      <- down(COMP)
  notSignifList <- rownames(COMP) %>% setdiff (c(upList, downList))

  if (missing(MAIN))
    MAIN <- deparse(substitute(COMP))

  plot(dgeTable, main=MAIN, type='n')
  grid()
  notSignifList %>% subTable %>% points(col='grey',  pch=PCH, cex=CEX)
  upList        %>% subTable %>% points(col='blue', pch=PCH, cex=CEX)
  downList      %>% subTable %>% points(col='red', pch=PCH, cex=CEX)
}
