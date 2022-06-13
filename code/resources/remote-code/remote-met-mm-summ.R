local({r <- getOption("repos")
r["CRAN"] <- "https://cloud.r-project.org" 
options(repos=r)
})

install.packages("pacman")
pacman::p_load(devtools, tidyverse, rstan)
#install.packages("tidyverse")
#install.packages("rstan")
devtools::install_github("appling/unitted")
devtools::install_github("USGS-R/streamMetabolizer", dependencies = TRUE)
library("streamMetabolizer")

args <- commandArgs(trailingOnly = TRUE)
fileName = args[1]

print(fileName)

nameSplit = strsplit(fileName, "/")
print(nameSplit)
siteName = gsub("_mm.rds","", sapply(nameSplit, "[", 2))

print(siteName)

workDir = getwd()
print(workDir)

outName = paste0(siteName,"_dailyFit.rds")

summ_met = function(){
  metDf = readRDS(fileName)
  
  fitDf = metDf@fit
  
  dailyFitDf = fitDf$daily
  
  print(paste0("File saved as:",outName))
  
  saveRDS( dailyFitDf, file = outName )
  
}
summ_met()

cat("The script was completed.")