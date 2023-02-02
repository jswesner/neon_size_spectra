rm(list = ls())
here::i_am("code/resources/metModelsMLE/MAYF_metModel.R")
source(here::here("code/resources/01_load-packages.R"))
# 
rerun = FALSE
if(rerun){
tictoc::tic();run_metab_multimodel(siteCode = "MAYF", metDf = NULL, return = FALSE, save = TRUE, parallel = FALSE);tictoc::toc()
}
siteCode = 'MAYF'
load(file = here::here("ignore/metab-models/mleModLists/MAYFmlemods.Rdata"))
params = list(site = siteCode,
              modList = modList)


rmarkdown::render(here::here("code/resources/metModelsMLE/MLE-model-report-template.Rmd"),
                  output_format = "pdf_document",
                  output_dir = here::here("code/resources/metModelsMLE/reports"),
                  output_file = paste0(siteCode,"_MLE-report.pdf"),
                  knit_root_dir = here::here(),
                  params = list(site = siteCode,
                                modList = modList),
                  clean = TRUE)

# to do any data cleaning load the individual models into the environment
# comment out below before rerunning this full script.
# 
# modList_sub = unlist(lapply(modList, function(x) !is.data.frame(x)))
# modList = modList[modList_sub]
# # 
# modListNames = purrr::map(modList, ~get_metab_info(.x, 'name')) %>% unlist
# modList = setNames(modList, nm = modListNames)
# # 
# quiet(list2env(modList, environment()))
# # ### Here is the final model choice after assessing the model fits
# # 
# saveRDS(mm3_sat, "./ignore/metab-models/MAYF_full_mle.rds")



# run this "by hand", below 
# rm(list = ls())
# here::i_am("./code/resources/metModelsMLE/MAYF_metModel.R")
# source(here::here("./code/resources/01_load-packages.R"))
# MAYF_met_full = get_site_data(siteCode = "MAYF") %>%
#   dplyr::select(-DO.pctsat)
# 
# MAYF_kmod = readRDS(file = "./ignore/site-gpp-data/MAYF_kGAM.rds")
# 
# discharge.daily = MAYF_met_full %>%
#   dplyr::mutate(date = as.Date(solar.time)) %>%
#   group_by(date) %>%
#   dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE),
#                    model = 'empirical-gam') 
# 
# discharge.daily$K600.daily = exp(predict(MAYF_kmod, newdata = discharge.daily))
# # 
# ## set all mle model specs
# mle_specs <- specs(mm_name(type = "mle"))
# mle_specs_sat <- specs(mm_name(type = "mle", GPP_fun = 'satlight'))
# mle_specs_satq10 <- specs(mm_name(type = "mle", GPP_fun = 'satlight', ER_fun = 'q10temp'))
# 
# # run models on raw data
# mm1 <- metab_mle(mle_specs, data = MAYF_met_full, info = list(name = "mm1", model = "raw"))
# mm1_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, info = list(name = "mm1_sat", model = "raw"))
# mm1_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, info = list(name = "mm1_satq10", model = "raw"))
# 
# k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
# 
# km1 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# km2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# km3 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# km4 <- metab_night(data = MAYF_met_full)
# 
# 
# k600_mm2 <- get_params(km1) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   bind_rows(get_params(km4) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'night')) %>%
#   bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
#  
# mm2 <- metab_mle(mle_specs, data = MAYF_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2", model = "loess"))
# 
# mm3 <- metab_mle(mle_specs, data = MAYF_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily),info = list(name = "mm3", model = "lm"))
# 
# mm4 <- metab_mle(mle_specs, data = MAYF_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4", model = "mean"))
# 
# mm5 <- metab_mle(mle_specs, data = MAYF_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5", model = "night"))
# 
# #fit saturation models
# k600_mm1_sat <- get_params(mm1_sat, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily)) 
# 
# km1_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 1),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# 
# km2_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
# 
# km3_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
# 
# # 
# k600_mm2_sat <- get_params(km1_sat) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2_sat) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3_sat) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   bind_rows(get_params(km4) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'night')) %>%
#   bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
# 
# mm2_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2_sat", model = "loess"))
# 
# mm3_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily),info = list(name = "mm3_sat", model = "lm"))
# 
# mm4_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4_sat", model = "mean"))
# 
# mm5_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5_sat", model = "night"))
# 
# k600_mm1_satq10 <- get_params(mm1_satq10, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily)) 
# 
# # 
# km1_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# 
# km2_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
# 
# km3_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
# # 
# k600_mm2_satq10 <- get_params(km1_satq10) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2_satq10) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3_satq10) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   bind_rows(get_params(km4) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'night')) %>%
#   bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
#   left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
# 
# 
# mm2_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2_satq10", model = "loess"))
# 
# mm3_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = "mm3_satq10", model = "lm"))
# 
# mm4_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4_satq10", model = "mean"))
# 
# mm5_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5_satq10", model = "night"))
# 
# # fit all the empirical models if emprical k model exists
# mm6 <- metab_mle(mle_specs, data = MAYF_met_full, data_daily = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily),info = list(name = "mm6", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
# 
# mm6_sat <- metab_mle(mle_specs_sat, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily),info = list(name = "mm6_sat", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
# 
# mm6_satq10 <- metab_mle(mle_specs_satq10, data = MAYF_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily), info = list(name = "mm6_satq10", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
# # 
# modList = ls()[grep("^mm\\d{1}.*", ls())] %>% purrr::map(~eval(as.symbol(.x))) 
# save(modList, file = paste0("./ignore/metab-models/mleModLists/MAYFmlemods.Rdata"))

# # model assessment
# mods = data.frame(
#   modelID = c("mm1","mm2","mm3","mm4","mm1_sat","mm2_sat","mm3_sat","mm4_sat","mm1_satq10","mm2_satq10","mm3_satq10","mm4_satq10"),
#   modelType = c("raw", "loess","lm","mean","raw", "loess","lm","mean","raw", "loess","lm","mean"),
#   gppTot = c(sum(mm1@metab_daily$GPP, na.rm = TRUE),
#              sum(mm2@metab_daily$GPP, na.rm = TRUE),
#              sum(mm3@metab_daily$GPP, na.rm = TRUE),
#              sum(mm4@metab_daily$GPP, na.rm = TRUE),
#              sum(mm1_sat@metab_daily$GPP, na.rm = TRUE),
#              sum(mm2_sat@metab_daily$GPP, na.rm = TRUE),
#              sum(mm3_sat@metab_daily$GPP, na.rm = TRUE),
#              sum(mm4_sat@metab_daily$GPP, na.rm = TRUE),
#              sum(mm1_satq10@metab_daily$GPP, na.rm = TRUE),
#              sum(mm2_satq10@metab_daily$GPP, na.rm = TRUE),
#              sum(mm3_satq10@metab_daily$GPP, na.rm = TRUE),
#              sum(mm4_satq10@metab_daily$GPP, na.rm = TRUE)),
#   RSME = c(calc_mod_RSME(plot_DO_preds(mm1), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm2), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm3), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm4), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm1_sat), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm2_sat), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm3_sat), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm4_sat), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm1_satq10), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm2_satq10), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm3_satq10), relative = TRUE),
#            calc_mod_RSME(plot_DO_preds(mm4_satq10), relative = TRUE)),
#   negatives = c(count_negative_dates(mm1),
#                 count_negative_dates(mm2),
#                 count_negative_dates(mm3),
#                 count_negative_dates(mm4),
#                 count_negative_dates(mm1_sat),
#                 count_negative_dates(mm2_sat),
#                 count_negative_dates(mm3_sat),
#                 count_negative_dates(mm4_sat),
#                 count_negative_dates(mm1_satq10),
#                 count_negative_dates(mm2_satq10),
#                 count_negative_dates(mm3_satq10),
#                 count_negative_dates(mm4_satq10)),
#   meanGPP = c(calc_gpp_mean(mm1),
#               calc_gpp_mean(mm2),
#               calc_gpp_mean(mm3),
#               calc_gpp_mean(mm4),
#               calc_gpp_mean(mm1_sat),
#               calc_gpp_mean(mm2_sat),
#               calc_gpp_mean(mm3_sat),
#               calc_gpp_mean(mm4_sat),
#               calc_gpp_mean(mm1_satq10),
#               calc_gpp_mean(mm2_satq10),
#               calc_gpp_mean(mm3_satq10),
#               calc_gpp_mean(mm4_satq10))
# )
# comment(mods$meanGPP) <- "mg C m-2 d-1"
# comment(mods$gppTot) <- "mg O2 m-2"
# 
# modList = ls()[grep("^mm\\d{1}.*", ls())] %>% purrr::map(~eval(as.symbol(.x)))
# save(modList, file = "./ignore/metab-models/mleModLists/MAYFmlemods.Rdata")
# 
# knitr::kable(mods)
# 
# # topMod = pick_model(mods)
# 
# # saveRDS(mm4_sat, "./ignore/metab-models/MAYF_full_mle.rds")


###

# ## run a quick version of the 
# bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
# bayes_specs <- specs(bayes_name)
# # 
# bayes_specs <- revise(bayes_specs, day_start = 4, day_end = 28, burnin_steps= 500, saved_steps= 1000, thin_steps = 5,n_cores=3, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = TRUE)
# 
# data_daily = NULL
# info = NULL
# source("./ignore/metab-models/metab_fun.R")
# source("./ignore/metab-models/bayes_allply.R")
# # debugonce(metab_fun);
# debugonce(bayes_allply)
# metab_fun(specs = bayes_specs, data = MAYF_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = MAYF_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = MAYF_met_mod,    # named list of data
#   chains = 4,             # number of Markov chains
#   warmup = 500,          # number of warmup iterations per chain
#   iter = 1000,            # total number of iterations per chain
#   cores = 4,              # number of cores (could use one per chain)
#   refresh = 100             # no progress shown
# )
# 
# 
# # 
# debugonce(metab)
# mm_bin <- metab(bayes_specs, data=MAYF_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/MAYF_test.rds")