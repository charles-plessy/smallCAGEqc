imageRenameCellomicsC1 <- function(C1ID, directory, fileExtension, nameLinkFile) {

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
  
  # load table that links Cellomics ID to well
  wellToCellomicsID <- read.csv(nameLinkFile, stringsAsFactors=F)
  wellToCellomicsID$Run <- C1ID
  wellToCellomicsID$cell_id <- paste(wellToCellomicsID$Run, wellToCellomicsID$Well, sep="_")
  
  # load renamed files and split into character vectors
  run <- list.files(directory, recursive=T)
  
  # creates final file name vector
  library(stringr)
  run <- as.character(sapply(run, function(x) str_split(x, "_", n=5)[[1]][5]))
  Image <- as.character(sapply(run, function(x) str_split(x, "_", n=3)[[1]][2]))
  CellNumber <- as.character(sapply(run, function(x) str_split(x, "_", n=3)[[1]][3]))
  CellNumber <- gsub(fileExtensionPattern, "", CellNumber)
  index <- match(as.integer(CellNumber), wellToCellomicsID$CellNumber)
  finalName <- paste(wellToCellomicsID$cell_id[index], Image, sep="_")
  finalName <- paste(finalName, paste(".", fileExtension, sep=""), sep="")
  
  # renames selected files with names in new name vector
  file.rename(paste(directory, list.files(directory), sep="/"), paste(directory, finalName, sep="/"))
}


## args <- commandArgs(trailingOnly = TRUE)
## 
## if (length(args) != 4) stop ('Not enough command-line arguments, please read the manual\n')
## 
## renameImages(args[1], args[2], args[3], args[4])
