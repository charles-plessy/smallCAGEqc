#' imageRenameCellomicsC1
#'
#' Rename raw image files into 96-well format.
#'
#' Due to filesystem case-insensitivity, it is not possible to have 96
#' different identifiers in one Cellomics run.  Therefore, two runs
#' are made with arbitrary names, and the files are renamed with this
#' script to reflect coordinates in 96-well format.
#'
#' WARNING: This function renames files and deletes directory; use it at
#  your own risk.
#' 
#' @param C1ID Run ID, for example "1772-066-262".
#' @param directory Directory containing raw files.
#' @param fileExtension File extension for the raw files.  Usually "C01".
#' 
#' @author Michael Böttcher and Charles Plessy

imageRenameCellomicsC1 <- function(C1ID, directory, fileExtension) {

  # saves Cellomics directory names in vector
  RUN <- list.files(directory, pattern=C1ID, full.names=T)

  fileExtensionPattern <- paste("\\.", fileExtension, "$", sep="")
  
  # remove all files that do not match the extension
  removeOtherFiles <- function(path)
    unlink(grep(fileExtensionPattern, list.files(path, full.names=T), value=T, invert=T))
  removeOtherFiles(RUN[1])
  removeOtherFiles(RUN[2])
  
  newNames <- function(path) {
    pics <- list.files(path, full.names=T)
    pics <- sub("f00d0", "_BF",    pics)
    pics <- sub("f00d1", "_Green", pics)
    pics <- sub("f00d2", "_Red",   pics)
    return(pics)
  }

  run1 <- newNames(RUN[1])
  run2 <- newNames(RUN[2])

  # Example: 'file.ext' becomes 'file_01.ext'
  addNumberToNames <- function(pattern, numbers, names) {
    paste( grep(pattern, sub(fileExtensionPattern, "_", names), value=T)
         , as.vector(sapply(numbers, function(x) rep(x, 3)))
         , paste(".", fileExtension, sep="")
         , sep="")
  }

  run1 <- c( addNumberToNames("_[A-Z]01_", seq(1,24), run1)
           , addNumberToNames("_[A-Z]02_", seq(49,72), run1))

  run2 <- c( addNumberToNames("_[A-Z]01_", seq(25,48), run2)
           , addNumberToNames("_[A-Z]02_", seq(73,96), run2))

  run1 <- basename(run1)
  run2 <- basename(run2)
  
  # first renaming step 

  firstRename <- function(names, runDirectory)
    file.rename( list.files(runDirectory, pattern=fileExtensionPattern, full.names=T)
               , paste(directory, sort(names), sep="/"))

  firstRename(run1, RUN[1])
  firstRename(run2, RUN[2])

  # delete empty directories
  removeIfEmpty <- function(path)
    if (identical(list.files(path), character(0))) {
      unlink(path, recursive=T)
    } else {
      stop(paste("Problem,", path, "was not empty."))
  }

  removeIfEmpty(RUN[1])
  removeIfEmpty(RUN[2])

# Made with `dput(read.csv('nameLink.csv', stringsAsFactors=F), control=NULL)`

wellToCellomicsID <- data.frame(
  Well = c(
    "A01", "A02", "A03", "A04", "A05", "A06", "A07", "A08", "A09", "A10", "A11", "A12"
  , "B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B09", "B10", "B11", "B12"
  , "C01", "C02", "C03", "C04", "C05", "C06", "C07", "C08", "C09", "C10", "C11", "C12"
  , "D01", "D02", "D03", "D04", "D05", "D06", "D07", "D08", "D09", "D10", "D11", "D12"
  , "E01", "E02", "E03", "E04", "E05", "E06", "E07", "E08", "E09", "E10", "E11", "E12"
  , "F01", "F02", "F03", "F04", "F05", "F06", "F07", "F08", "F09", "F10", "F11", "F12"
  , "G01", "G02", "G03", "G04", "G05", "G06", "G07", "G08", "G09", "G10", "G11", "G12"
  , "H01", "H02", "H03", "H04", "H05", "H06", "H07", "H08", "H09", "H10", "H11", "H12"),
  Cellomics_ID = c(
    "C01", "B01", "A01", "A02", "B02", "C02", "F01", "E01", "D01", "D02", "E02", "F02"
  , "I01", "H01", "G01", "G02", "H02", "I02", "L01", "K01", "J01", "J02", "K02", "L02"
  , "O01", "N01", "M01", "M02", "N02", "O02", "R01", "Q01", "P01", "P02", "Q02", "R02"
  , "U01", "T01", "S01", "S02", "T02", "U02", "X01", "W01", "V01", "V02", "W02", "X02"
  , "A01", "B01", "C01", "C02", "B02", "A02", "D01", "E01", "F01", "F02", "E02", "D02"
  , "G01", "H01", "I01", "I02", "H02", "G02", "J01", "K01", "L01", "L02", "K02", "J02"
  , "M01", "N01", "O01", "O02", "N02", "M02", "P01", "Q01", "R01", "R02", "Q02", "P02"
  , "S01", "T01", "U01", "U02", "T02", "S02", "V01", "W01", "X01", "X02", "W02", "V02"),
  CellNumber = c(
    3, 2, 1, 49, 50, 51, 6, 5, 4, 52, 53, 54, 9, 8, 7, 55, 56, 57, 12, 11, 10, 58, 59
  , 60, 15, 14, 13, 61, 62, 63, 18, 17, 16, 64, 65, 66, 21, 20, 19, 67, 68, 69, 24, 23
  , 22, 70, 71, 72, 25, 26, 27, 75, 74, 73, 28, 29, 30, 78, 77, 76, 31, 32, 33, 81, 80
  , 79, 34, 35, 36, 84, 83, 82, 37, 38, 39, 87, 86, 85, 40, 41, 42, 90, 89, 88, 43, 44
  , 45, 93, 92, 91, 46, 47, 48, 96, 95, 94)
)

  wellToCellomicsID$Run <- C1ID
  wellToCellomicsID$cell_id <- paste(wellToCellomicsID$Run, wellToCellomicsID$Well, sep="_")
  
  # load renamed files and split into character vectors
  run <- list.files(directory, recursive=T)
  
  # creates final file name vector
  run   <- as.character(sapply(run, function(x) stringr::str_split(x, "_", n=5)[[1]][5]))
  Image <- as.character(sapply(run, function(x) stringr::str_split(x, "_", n=3)[[1]][2]))
  CellNumber <- as.character(sapply(run, function(x) stringr::str_split(x, "_", n=3)[[1]][3]))
  CellNumber <- gsub(fileExtensionPattern, "", CellNumber)
  index <- match(as.integer(CellNumber), wellToCellomicsID$CellNumber)
  finalName <- paste(wellToCellomicsID$cell_id[index], Image, sep="_")
  finalName <- paste(finalName, paste(".", fileExtension, sep=""), sep="")
  
  # renames selected files with names in new name vector
  file.rename(paste(directory, list.files(directory), sep="/"), paste(directory, finalName, sep="/"))
}


## args <- commandArgs(trailingOnly = TRUE)
## 
## if (length(args) != 3) stop ('Not enough command-line arguments, please read the manual\n')
## 
## renameImages(args[1], args[2], args[3])
