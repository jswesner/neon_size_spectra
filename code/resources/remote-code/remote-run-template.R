
library("tidyverse")
library("streamMetabolizer")

source(paste0(getwd(),"/code/01_load-packages.R"))

args <- commandArgs(trailingOnly = TRUE)
fileName = args[1]

siteName = gsub("_met.rds","", sapply(strsplit(fileName, "/"), "[", length(.)))
workDir = getwd()
run_met = function(){
metDf = readRDS(fileName)

# set the model specifications 
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)

bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 10000, saved_steps= 5000,
                      thin_steps = 10, n_cores=8, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = FALSE)

fileName = paste0(workDir,siteName,"mm_bin.rds")

mm_bin <- metab(bayes_specs, data=metDf)

saveRDS(mm_bin, fileName)

}
