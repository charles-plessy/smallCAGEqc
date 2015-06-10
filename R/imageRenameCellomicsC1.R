imageRenameCellomicsC1 <- function(C1ID, directory, fileExtension, nameLinkFile) {

  # saves Cellomics directory names in vector
  RUN <- list.files(directory, pattern=C1ID, full.names=T)
  
  # remove all files that are not bitmap files
  unlink(paste(RUN[1], grep(paste("\\.", fileExtension, sep=""), list.files(RUN[1]), value=T, invert=T), sep="/"))
  unlink(paste(RUN[2], grep(paste("\\.", fileExtension, sep=""), list.files(RUN[2]), value=T, invert=T), sep="/"))
  
  pics1 <- sub("f00d0", "_BF", list.files(RUN[1], "f00d.", full.names=T))
  pics1 <- sub("f00d1", "_Green", pics1)
  run1 <- sub("f00d2", "_Red", pics1)
  
  pics2 <- sub("f00d0", "_BF", list.files(RUN[2], "f00d.", full.names=T))
  pics2 <- sub("f00d1", "_Green", pics2)
  run2 <- sub("f00d2", "_Red", pics2)
  
  run1 <- c(paste(grep("_[A-Z]01_", sub(paste("\\.", fileExtension, sep=""), "_", run1), value=T)
                  , as.vector(sapply(seq(1,24), function(x) rep(x, 3))), paste(".", fileExtension, sep=""), sep="")
            , paste(grep("_[A-Z]02_", sub(paste("\\.", fileExtension, sep=""), "_", run1), value=T)
                    , as.vector(sapply(seq(49,72), function(x) rep(x, 3))), paste(".", fileExtension, sep=""), sep=""))
  run2 <- c(paste(grep("_[A-Z]01_", sub(paste("\\.", fileExtension, sep=""), "_", run2), value=T)
                  , as.vector(sapply(seq(25,48), function(x) rep(x, 3))), paste(".", fileExtension, sep=""), sep="")
            , paste(grep("_[A-Z]02_", sub(paste("\\.", fileExtension, sep=""), "_", run2), value=T)
                    , as.vector(sapply(seq(73,96), function(x) rep(x, 3))), paste(".", fileExtension, sep=""), sep=""))
  
  # replace Cellomics directory name with C1ID
  run1 <- sapply(strsplit(run1, "/", fixed = TRUE), "[[", length(strsplit(run1, "/", fixed = TRUE)[[1]]))
  run1 <- gsub(list.files(directory)[1], C1ID, run1)
  run2 <- sapply(strsplit(run2, "/", fixed = TRUE), "[[", length(strsplit(run2, "/", fixed = TRUE)[[1]]))
  run2 <- gsub(list.files(directory)[2], C1ID, run2)
  
  # first renaming step 
  file.rename(list.files(RUN[1], pattern=paste("\\.", fileExtension, sep=""), full.names=T)
              , paste(directory, sort(run1), sep=""))
  file.rename(list.files(RUN[2], pattern=paste("\\.", fileExtension, sep=""), full.names=T)
              , paste(directory, sort(run2), sep=""))
  
  # delete empty directories
  unlink(RUN, recursive=T)
  
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
  CellNumber <- gsub(paste("\\.", fileExtension, sep=""), "", CellNumber)
  index <- match(as.integer(CellNumber), wellToCellomicsID$CellNumber)
  finalName <- paste(wellToCellomicsID$cell_id[index], Image, sep="_")
  finalName <- paste(finalName, paste(".", fileExtension, sep=""), sep="")
  
  # renames selected files with names in new name vector
  file.rename(paste(directory, list.files(directory), sep=""), paste(directory, finalName, sep=""))
}


## args <- commandArgs(trailingOnly = TRUE)
## 
## if (length(args) != 4) stop ('Not enough command-line arguments, please read the manual\n')
## 
## renameImages(args[1], args[2], args[3], args[4])
