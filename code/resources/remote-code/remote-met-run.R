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
siteName = gsub("_met.rds","", sapply(nameSplit, "[", 2))
print(siteName)
workDir = getwd()
print(workDir)
colNames = c("solar.time", "DO.obs", "DO.sat", "depth", "temp.water", "light", "discharge")

run_met = function(){
metDf = readRDS(fileName)

metDf = metDf[,names(metDf) %in% colNames]

# set the model specifications 
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)

bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 9000, saved_steps= 3000,
                      thin_steps = 10, n_cores=12, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = FALSE)

fileName = paste0(workDir,"/",siteName,"_mm.rds")

mm_bin <- metab(bayes_specs, data=metDf)

print(paste0("File saved as:",fileName))

saveRDS(mm_bin, file = fileName)

}
run_met()

cat("The script was completed.")
