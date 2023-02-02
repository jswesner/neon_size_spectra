rm(list = ls())
here::i_am("code/resources/metModelsMLE/WLOU_metModel.R")
source(here::here("code/resources/01_load-packages.R"))
# 
rerun = FALSE
if(rerun){
tictoc::tic();run_metab_multimodel(siteCode = "WLOU", metDf = NULL, return = FALSE, save = TRUE, parallel = FALSE);tictoc::toc()
}
siteCode = 'WLOU'
load(file = here::here("ignore/metab-models/mleModLists/WLOUmlemods.Rdata"))
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


# # to do any data cleaning load the individual models into the environment
# # comment out below before rerunning this full script.
# 
# modList_sub = unlist(lapply(modList, function(x) !is.data.frame(x)))
# modList = modList[modList_sub]
# 
# modListNames = purrr::map(modList, ~get_metab_info(.x, 'name')) %>% unlist
# modList = setNames(modList, nm = modListNames)
# 
# quiet(list2env(modList, environment()))
# ### Here is the final model choice after assessing the model fits
# 
# saveRDS(mm3_sat, "./ignore/metab-models/WLOU_full_mle.rds")


# WLOU_met_full = get_site_data(siteCode = "WLOU") %>%
#    dplyr::select(-DO.pctsat) #%>% 
#   # dplyr::mutate(DO.obs = case_when(DO.obs < 6.5 ~ NA_real_,
#                                    # TRUE ~ DO.obs),
#                 # DO.obs = zoo::na.approx(DO.obs, maxgap = 10))
# 
# WLOU_met_clean = clean_met_data(WLOU_met_full, doPctCutoff = c(90,110))
# 
# discharge.daily = WLOU_met_full %>%
#   dplyr::mutate(date = as.Date(solar.time)) %>%
#   group_by(date) %>%
#   dplyr::summarise(discharge = mean(discharge, na.rm = TRUE))
# 
# ## 
# mle_specs <- specs(mm_name(type = "mle"))
# # debugonce(metab_mle)
# mm1 <- metab_mle(mle_specs, data = WLOU_met_full)
# 
# k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
#   select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(WLOU_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# mm1_vis = get_params(mm1, uncertainty = 'ci') %>%
#   select(date, K600.daily, K600.daily.lower, K600.daily.upper)%>%
#   left_join(WLOU_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)+0.001))
# 
# mm1_vis %>%
#   ggplot()+
#   geom_point(aes(x = discharge.daily, y = K600.daily)) +
#   coord_trans('log10')
# # 
# km1 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1)
# km2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1)
# km3 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1)
# # 
# k600_mm2 <- get_params(km1) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   left_join(WLOU_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# k600_mm2 %>%
#   ggplot()+
#   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
#   coord_trans('log10') +
#   facet_wrap(~model, scales = 'free_y')
# # 
# mm2 <- metab_mle(mle_specs, data = WLOU_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily))
# 
# mm3 <- metab_mle(mle_specs, data = WLOU_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily))
# 
# mm4 <- metab_mle(mle_specs, data = WLOU_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily))
# # model assessment
# mods = data.frame(
#   modelID = c("mm1","mm2","mm3","mm4"),
#   modelType = c("raw", "loess","lm","mean"),
#   gppTot = c(sum(mm1@fit$GPP.daily, na.rm = TRUE),
#              sum(mm2@fit$GPP.daily, na.rm = TRUE),
#              sum(mm3@fit$GPP.daily, na.rm = TRUE),
#              sum(mm4@fit$GPP.daily, na.rm = TRUE)),
#   RSME = c(calc_mod_RSME(plot_DO_preds(mm1)),
#            calc_mod_RSME(plot_DO_preds(mm2)),
#            calc_mod_RSME(plot_DO_preds(mm3)),
#            calc_mod_RSME(plot_DO_preds(mm4)))
# )
# 
# knitr::kable(mods)
# 
# topMod = pick_model(mods)
# 
# saveRDS(mm1, "./ignore/metab-models/WLOU_full_mle.rds")
# ###

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
# metab_fun(specs = bayes_specs, data = WLOU_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = WLOU_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = WLOU_met_mod,    # named list of data
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
# mm_bin <- metab(bayes_specs, data=WLOU_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/WLOU_test.rds")