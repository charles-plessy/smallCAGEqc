## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----libs, message = FALSE-----------------------------------------------
library(smallCAGEqc)
libs <- read.table(system.file("extdata/libs-with-all-metadata.tsv", package="smallCAGEqc"))

## ----steps---------------------------------------------------------------
plotAnnot(libs, "steps", "steps")

## ----qc------------------------------------------------------------------
plotAnnot(libs, "qc", "qc")

## ----counts--------------------------------------------------------------
plotAnnot(libs[libs$counts > 0,], "counts", "counts")

## ----mapped--------------------------------------------------------------
plotAnnot(libs, "mapped", "mapped")

## ----all-----------------------------------------------------------------
plotAnnot(libs, "all", "all")

## ----annotation----------------------------------------------------------
plotAnnot(libs, "annotation", "annotation")

## ----show-libs-----------------------------------------------------------
libs

## ----session-info--------------------------------------------------------
sessionInfo()

