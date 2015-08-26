#' fldgmArrayQC
#'
#' Plots various quantities as heatmaps sorted by capture chamber.
#'
#' @param LIBS A "libs" table, where rows are capture chambers and columns are various data.
#' @param title The title displayed on the plot.
#'
#' The "libs" table must contain columns for 'Error', 'Concentration', 'total', 'extracted',
#' 'spikes', 'rdna', 'properpairs', 'counts', 'mean_ch2', 'mean_ch3'.  It must be sorted by
#' well of 96-well plate.
#'
#' Each column containing numerical data is centered and reduce before plotting.
#'
#' @examples
#' fldgmArrayQCplot <- function(RUN) fldgmArrayQC(libs[libs$Run==RUN,], RUN)
#' ## fldgmArrayQCplot(RunB)

fldgmArrayQC <- function(LIBS, title='') {

# 'http://rlgsw35.gsc.riken.jp/gitlab/fucci/fucci/raw/master/fluorescence/fluorescence_QC.csv' %>% read.csv(stringsAsFactors=F) %>% head(96) %>% subset(select=c('Well', 'Cell.Number')) %>% dput(control= NULL) # plus sorting afterwards and renaming column.

W2C <- data.frame(
Well = c("A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10",
"A11", "A12", "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09",
"B10", "B11", "B12", "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08",
"C09", "C10", "C11", "C12", "D01", "D02", "D03", "D04", "D05", "D06", "D07",
"D08", "D09", "D10", "D11", "D12", "E01", "E02", "E03", "E04", "E05", "E06",
"E07", "E08", "E09", "E10", "E11", "E12", "F01", "F02", "F03", "F04", "F05",
"F06", "F07", "F08", "F09", "F10", "F11", "F12", "G01", "G02", "G03", "G04",
"G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12", "H01", "H02", "H03",
"H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12"),
Chamber.Number = c(3, 2, 1, 49, 50, 51, 6, 5, 4, 52, 53, 54, 9, 8, 7, 55, 56, 57,
12, 11, 10, 58, 59, 60, 15, 14, 13, 61, 62, 63, 18, 17, 16, 64, 65, 66, 21, 20,
19, 67, 68, 69, 24, 23, 22, 70, 71, 72, 25, 26, 27, 75, 74, 73, 28, 29, 30, 78,
77, 76, 31, 32, 33, 81, 80, 79, 34, 35, 36, 84, 83, 82, 37, 38, 39, 87, 86, 85,
40, 41, 42, 90, 89, 88, 43, 44, 45, 93, 92, 91, 46, 47, 48, 96, 95, 94)
)

if (! all(W2C$Well == LIBS$Well))
  stop('The data table is not well sorted.  See help page for details.')

LIBS$Chamber.Number <- W2C$Chamber.Number

if (nrow(LIBS) == 192) {
  LIBS[97:192, "Chamber.Number"] <- LIBS[97:192, "Chamber.Number"] + 96
} else if (nrow(LIBS) != 96) {
  stop('The data table should contain 96 or 192 rows.  See help page for details.')
}

LIBS <- LIBS[order(LIBS$Chamber.Number),]

LIBS$Error     <- as.numeric(LIBS$Error)
LIBS[is.na(LIBS)] <- 0

centerAndReduce <- function(TABLE) {
  M  <- apply(TABLE, 2, mean)
  SD <- apply(TABLE, 2, sd)
  t((t(TABLE) - M) / SD)
}

mask <- function(TABLE,LIST) {
  TABLE[,colnames(TABLE) %in% LIST] <- NA
  return(TABLE)
}

# quick hack because gplots is borken here.

redblue <- c("#FF0000", "#FF0808", "#FF1010", "#FF1919", "#FF2121", "#FF2929", 
"#FF3131", "#FF3A3A", "#FF4242", "#FF4A4A", "#FF5252", "#FF5A5A", 
"#FF6363", "#FF6B6B", "#FF7373", "#FF7B7B", "#FF8484", "#FF8C8C", 
"#FF9494", "#FF9C9C", "#FFA5A5", "#FFADAD", "#FFB5B5", "#FFBDBD", 
"#FFC5C5", "#FFCECE", "#FFD6D6", "#FFDEDE", "#FFE6E6", "#FFEFEF", 
"#FFF7F7", "#FFFFFF", "#FFFFFF", "#F7F7FF", "#EFEFFF", "#E6E6FF", 
"#DEDEFF", "#D6D6FF", "#CECEFF", "#C5C5FF", "#BDBDFF", "#B5B5FF", 
"#ADADFF", "#A5A5FF", "#9C9CFF", "#9494FF", "#8C8CFF", "#8484FF", 
"#7B7BFF", "#7373FF", "#6B6BFF", "#6363FF", "#5A5AFF", "#5252FF", 
"#4A4AFF", "#4242FF", "#3A3AFF", "#3131FF", "#2929FF", "#2121FF", 
"#1919FF", "#1010FF", "#0808FF", "#0000FF")

if (max(LIBS$Error) > 7)
  stop('Unsupported number of error codes') 

colorsForErrorCodes <- c('white', 'black', 'blue', 'grey', 'darkgreen', 'purple', 'red')[1:max(LIBS$Error)]
displayedData <- c('Concentration', 'total', 'extracted', 'spikes', 'rdna', 'properpairs', 'counts', 'mean_ch2', 'mean_ch3')

LIBS %>%
  subset(.,,displayedData) %>%
  centerAndReduce %>%
  t %>%
  as.matrix %>%
  NMF::aheatmap( breaks=c(-3,-2,-1, 1, 2, 3)
               , col="-RdYlBu2:5"
               , Rowv=NA
               , Colv=NA
               , annCol=list(Error=LIBS$Error)
               , annColors=list(Error=colorsForErrorCodes)
               , main=title
               )
}
