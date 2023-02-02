rm(list = ls())
here::i_am("code/resources/metModelsMLE/ARIK_metModel.R")
source(here::here("code/resources/01_load-packages.R"))
rerun = FALSE
if(rerun){
tictoc::tic();run_metab_multimodel(siteCode = "ARIK", metDf = NULL, return = FALSE, save = TRUE, parallel = FALSE);tictoc::toc()
}
siteCode = 'ARIK'
load(file = here::here("ignore/metab-models/mleModLists/ARIKmlemods.Rdata"))
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

# modList_sub = unlist(lapply(modList, function(x) !is.data.frame(x)))
# modList = modList[modList_sub]

# modListNames = purrr::map(modList, ~get_metab_info(.x, 'name')) %>% unlist
# modList = setNames(modList, nm = modListNames)

# quiet(list2env(modList, .GlobalEnv))
### Here is the final model choice after assessing the model fits

# saveRDS(mm3_sat, "./ignore/metab-models/ARIK_full_mle.rds")


# source(here::here("./code/resources/01_load-packages.R"))
# ARIK_met_full = get_site_data(siteCode = "ARIK") %>%
#   dplyr::select(-DO.pctsat)
# 
# discharge.daily = ARIK_met_full %>%
#   dplyr::mutate(date = as.Date(solar.time)) %>%
#   group_by(date) %>%
#   dplyr::summarise(discharge = mean(discharge, na.rm = TRUE))
# 
# ## 
# mle_specs <- specs(mm_name(type = "mle", GPP_fun = "linlight"))
# # debugonce(metab_mle)
# mm1 <- metab_mle(mle_specs, data = ARIK_met_full, info = list(name = 'mm1', model = 'raw'))
# 
# # identify negative values
# k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) 
# 
# k600_mm1 %>%
#   ggplot()+
#   geom_point(aes(x = discharge.daily, y = K600.daily)) +
#   coord_trans('log10', ylim = c(NA, 500))
# 
# k600_mm1 = k600_mm1 %>%
#   dplyr::filter(K600.daily < 10000)
# 
# # 
# km1 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
# 
# km2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
# 
# km3 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
# 
# km4 <- metab_night(data = ARIK_met_full)
# 
# # 
# k600_mm1 <- get_params(km1) %>% 
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
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# # k600_mm1 %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10') +
# #   facet_wrap(~model, scales = 'free_y')
# # 
# mm2 <- metab_mle(mle_specs, data = ARIK_met_full, data_daily = k600_mm1 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily), info = list(name = 'mm2', model = 'loess'))
# 
# mm3 <- metab_mle(mle_specs, data = ARIK_met_full, data_daily = k600_mm1 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = 'mm3', model = 'lm'))
# 
# mm4 <- metab_mle(mle_specs, data = ARIK_met_full, data_daily = k600_mm1 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily), info = list(name = 'mm4', model = 'mean'))
# 
# mm5 <- metab_mle(mle_specs, data = ARIK_met_full, data_daily = k600_mm1 %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily), info = list(name = 'mm5', model = 'night'))
# ###
# ## 
# mle_specs_sat <- specs(mm_name(type = "mle", GPP_fun = 'satlight'))
# mm1_sat <- metab_mle(mle_specs_sat, data = ARIK_met_full, info = list(name = 'mm1_sat', model = 'raw'))
# 
# # identify negative values
# k600_mm1_sat <- get_params(mm1_sat, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) 
# 
# # k600_mm1_sat %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10', ylim = c(0, 400))
# 
# k600_mm1_sat = k600_mm1_sat %>%
#   dplyr::filter(K600.daily < 10000,
#                 GPP.daily > 0)
# # 
# km1_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1_sat %>% select(-GPP.daily))
# 
# km2_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
# 
# km3_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                           day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
# # 
# k600_mm1_sat <- get_params(km1_sat) %>% 
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
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# # k600_mm1_sat %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10') +
# #   facet_wrap(~model, scales = 'free_y')
# # 
# mm2_sat <- metab_mle(mle_specs_sat, data = ARIK_met_full, data_daily = k600_mm1_sat %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily), info = list(name = 'mm2_sat', model = 'loess'))
# 
# mm3_sat <- metab_mle(mle_specs_sat, data = ARIK_met_full, data_daily = k600_mm1_sat %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = 'mm3_sat', model = 'lm'))
# 
# mm4_sat <- metab_mle(mle_specs_sat, data = ARIK_met_full, data_daily = k600_mm1_sat %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily), info = list(name = 'mm4_sat', model = 'mean'))
# 
# mm5_sat <- metab_mle(mle_specs_sat, data = ARIK_met_full, data_daily = k600_mm1_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily), info = list(name = 'mm5', model = 'night'))
# ## satq10temp
# mle_specs_satq10 <- specs(mm_name(type = "mle", GPP_fun = 'satlight', ER_fun = 'q10temp'))
# mm1_satq10 <- metab_mle(mle_specs_satq10, data = ARIK_met_full, info = list(name = 'mm1_satq10', model = 'raw'))
# 
# # identify negative values
# k600_mm1_satq10 <- get_params(mm1_satq10, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) 
# 
# # k600_mm1_satq10 %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10', ylim = c(0, 400))
# 
# k600_mm1_satq10 = k600_mm1_satq10 %>%
#   dplyr::filter(K600.daily < 10000,
#                 GPP.daily > 0)
# # 
# km1_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
# 
# km2_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1_satq10%>% select(-GPP.daily))
# 
# km3_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                               day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
# # 
# k600_mm1_satq10 <- get_params(km1_satq10) %>% 
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
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# # k600_mm1_satq10 %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily, y = K600.daily)) +
# #   coord_trans('log10') +
# #   facet_wrap(~model, scales = 'free_y')
# 
# # 
# mm2_satq10 <- metab_mle(mle_specs_satq10, data = ARIK_met_full, data_daily = k600_mm1_satq10 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily), info = list(name = 'mm2_satq10', model = 'loess'))
# 
# mm3_satq10 <- metab_mle(mle_specs_satq10, data = ARIK_met_full, data_daily = k600_mm1_satq10 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = 'mm3_satq10', model = 'lm'))
# 
# mm4_satq10 <- metab_mle(mle_specs_satq10, data = ARIK_met_full, data_daily = k600_mm1_satq10 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily), info = list(name = 'mm4_satq10', model = 'mean'))
# 
# mm5_satq10 <- metab_mle(mle_specs_satq10, data = ARIK_met_full, data_daily = k600_mm1_satq10 %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily), info = list(name = 'mm5_satq10', model = 'night'))
# ####
# ## satlightq10tempGPP-----
# mle_specs_satq10gpp <- specs(mm_name(type = "mle", GPP_fun = 'satlightq10temp'))
# mm1_satq10gpp <- metab_mle(mle_specs_satq10gpp, data = ARIK_met_full, info = list(name = 'mm1_satq10gpp', model = 'raw'))
# 
# # identify negative values
# k600_mm1_satq10gpp <- get_params(mm1_satq10gpp, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_satq10gpp) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) 
# 
# # k600_mm1_satq10gpp %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily, y = K600.daily)) +
# #   coord_trans('log10', ylim = c(NA, 150))
# 
# k600_mm1_satq10gpp = k600_mm1_satq10gpp %>%
#   dplyr::filter( K600.daily < 10000,
#                  GPP.daily > 0)
# # 
# km1_satq10gpp <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                                     day_start = -1, day_end = 23), data_daily = k600_mm1_satq10gpp %>% select(-GPP.daily))
# 
# km2_satq10gpp <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                                     day_start = -1, day_end = 23), data_daily = k600_mm1_satq10gpp %>% select(-GPP.daily))
# 
# km3_satq10gpp <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                                     day_start = -1, day_end = 23), data_daily = k600_mm1_satq10gpp %>% select(-GPP.daily))
# # 
# k600_mm1_satq10gpp <- get_params(km1_satq10gpp) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2_satq10gpp) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3_satq10gpp) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   bind_rows(get_params(km4) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'night')) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# # k600_mm1_satq10gpp %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily, y = K600.daily)) +
# #   coord_trans('log10') +
# #   facet_wrap(~model, scales = 'free_y')
# # 
# mm2_satq10gpp <- metab_mle(mle_specs_satq10gpp, data = ARIK_met_full, data_daily = k600_mm1_satq10gpp %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily), info = list(name = 'mm2_satq10gpp', model = 'loess'))
# 
# mm3_satq10gpp <- metab_mle(mle_specs_satq10gpp, data = ARIK_met_full, data_daily = k600_mm1_satq10gpp %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = 'mm3_satq10gpp', model = 'lm'))
# 
# mm4_satq10gpp <- metab_mle(mle_specs_satq10gpp, data = ARIK_met_full, data_daily = k600_mm1_satq10gpp %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = 'mm4_satq10gpp', model = 'mean'))
# 
# mm5_satq10gpp <- metab_mle(mle_specs_satq10gpp, data = ARIK_met_full, data_daily = k600_mm1_satq10gpp %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = 'mm5_satq10gpp', model = 'night'))
# ## satlightq10tempall
# mle_specs_satq10all <- specs(mm_name(type = "mle", GPP_fun = 'satlightq10temp', ER_fun = 'q10temp'))
# mm1_satq10all <- metab_mle(mle_specs_satq10all, data = ARIK_met_full,info = list(name = 'mm1_satq10all', model = 'raw'))
# 
# # identify negative values
# k600_mm1_satq10all <- get_params(mm1_satq10all, uncertainty = 'ci') %>%
#   left_join(predict_metab(mm1_satq10all) %>% select(date, GPP.daily = 'GPP')) %>%
#   select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) 
# 
# # k600_mm1_satq10all %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10', ylim = c(NA, 400))
# 
# k600_mm1_satq10all = k600_mm1_satq10all %>%
#   dplyr::filter(K600.daily < 10000,
#                 GPP.daily > 0)
# # 
# km1_satq10all <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1_satq10all %>% select(-GPP.daily))
# 
# km2_satq10all <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1_satq10all %>% select(-GPP.daily))
# 
# km3_satq10all <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
#                                  day_start = -1, day_end = 23), data_daily = k600_mm1_satq10all %>% select(-GPP.daily))
# # 
# k600_mm1_satq10all <- get_params(km1_satq10all) %>% 
#   select(date, K600.daily) %>%  
#   dplyr::mutate(model = 'loess') %>%
#   bind_rows(get_params(km2_satq10all) %>%
#               select(date, K600.daily) %>%  
#               dplyr::mutate(model = 'lm')) %>%
#   bind_rows(get_params(km3_satq10all) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'mean')) %>%
#   bind_rows(get_params(km4) %>%
#               select(date, K600.daily) %>%
#               dplyr::mutate(model = 'night')) %>%
#   left_join(ARIK_met_full %>%
#               dplyr::mutate(date = as.Date(solar.time)) %>%
#               group_by(date) %>%
#               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
# # k600_mm1_satq10all %>%
# #   ggplot()+
# #   geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
# #   coord_trans('log10') +
# #   facet_wrap(~model, scales = 'free_y')
# # 
# mm2_satq10all <- metab_mle(mle_specs_satq10all, data = ARIK_met_full, data_daily = k600_mm1_satq10all %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = 'mm2_satq10all', model = 'loess'))
# 
# mm3_satq10all <- metab_mle(mle_specs_satq10all, data = ARIK_met_full, data_daily = k600_mm1_satq10all %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily),info = list(name = 'mm3_satq10all', model = 'lm'))
# 
# mm4_satq10all <- metab_mle(mle_specs_satq10all, data = ARIK_met_full, data_daily = k600_mm1_satq10all %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = 'mm4_satq10all', model = 'mean'))
# 
# mm5_satq10all <- metab_mle(mle_specs_satq10all, data = ARIK_met_full, data_daily = k600_mm1_satq10all %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = 'mm5_satq10all', model = 'night'))
# 
# ####
# modList = ls()[grep("^mm\\d{1}.*", ls())] %>% purrr::map(~eval(as.symbol(.x))) %>% setNames(.,unlist(ls()[grep("^mm\\d{1}.*", ls())]))
# save(modList, file = "./ignore/metab-models/mleModLists/ARIKmlemods.Rdata")
# 
# siteCode = 'ARIK'
# rmarkdown::render(here::here("./code/resources/metModelsMLE/MLE-model-report-template.Rmd"),
#                   output_format = "pdf_document",
#                   output_dir = here::here("./code/resources/metModelsMLE/reports"),
#                   output_file = paste0(siteCode,"-MLE-report.pdf"),
#                   params = list(site = siteCode,
#                                 modList = modList))
# 
# # model assessment
# # rm(list = ls())
# # source("./code/resources/01_load-packages.R")
# # load(file = "./ignore/metab-models/mleModLists/ARIKmlemods.Rdata")
# # modList_sub = unlist(lapply(modList, function(x) !is.data.frame(x)))
# # modList = modList[modList_sub]
# # 
# # modList = setNames(modList, nm = c("mm1","mm2","mm3","mm4","mm5","mm1_sat","mm2_sat","mm3_sat","mm4_sat","mm5_sat","mm1_satq10gpp","mm2_satq10gpp","mm3_satq10gpp","mm4_satq10gpp","mm5_satq10gpp","mm1_satq10","mm2_satq10","mm3_satq10","mm4_satq10","mm5_satq10","mm1_satq10all","mm2_satq10all","mm3_satq10all","mm4_satq10all","mm5_satq10all"))
# # 
# # list2env(modList, .GlobalEnv)
# # 
# # mods = data.frame(
# #   modelID = c("mm1","mm2","mm3","mm4","mm5","mm1_sat","mm2_sat","mm3_sat","mm4_sat","mm5_sat","mm1_satq10gpp","mm2_satq10gpp","mm3_satq10gpp","mm4_satq10gpp","mm5_satq10gpp","mm1_satq10","mm2_satq10","mm3_satq10","mm4_satq10","mm5_satq10","mm1_satq10all","mm2_satq10all","mm3_satq10all","mm4_satq10all","mm5_satq10all"),
# #   modelType = c("raw", "loess","lm","mean","night","raw", "loess","lm","mean","night","raw", "loess","lm","mean","night","raw", "loess","lm","mean","night","raw", "loess","lm","mean","night"),
# #   # gppTot = c(sum(mm1_lin@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm2@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm3@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm4@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm1_sat@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm2_sat@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm3_sat@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm4_sat@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm1_satq10gpp@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm2_satq10gpp@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm3_satq10gpp@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm4_satq10gpp@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm1_satq10@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm2_satq10@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm3_satq10@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm4_satq10@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm1_satq10all@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm2_satq10all@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm3_satq10all@metab_daily$GPP, na.rm = TRUE),
# #   #            sum(mm4_satq10all@metab_daily$GPP, na.rm = TRUE)),
# #   RSME = c(calc_mod_RSME(plot_DO_preds(mm1), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm2), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm3), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm4), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm5), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm1_sat), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm2_sat), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm3_sat), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm4_sat), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm5_sat), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm1_satq10gpp), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm2_satq10gpp), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm3_satq10gpp), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm4_satq10gpp), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm5_satq10gpp), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm1_satq10), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm2_satq10), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm3_satq10), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm4_satq10), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm5_satq10), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm1_satq10all), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm2_satq10all), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm3_satq10all), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm4_satq10all), relative = TRUE),
# #            calc_mod_RSME(plot_DO_preds(mm5_satq10all), relative = TRUE)),
# #   negatives = c(count_negative_dates(mm1),
# #                 count_negative_dates(mm2),
# #                 count_negative_dates(mm3),
# #                 count_negative_dates(mm4),
# #                 count_negative_dates(mm5),
# #                 count_negative_dates(mm1_sat),
# #                 count_negative_dates(mm2_sat),
# #                 count_negative_dates(mm3_sat),
# #                 count_negative_dates(mm4_sat),
# #                 count_negative_dates(mm5_sat),
# #                 count_negative_dates(mm1_satq10gpp),
# #                 count_negative_dates(mm2_satq10gpp),
# #                 count_negative_dates(mm3_satq10gpp),
# #                 count_negative_dates(mm4_satq10gpp),
# #                 count_negative_dates(mm5_satq10gpp),
# #                 count_negative_dates(mm1_satq10),
# #                 count_negative_dates(mm2_satq10),
# #                 count_negative_dates(mm3_satq10),
# #                 count_negative_dates(mm4_satq10),
# #                 count_negative_dates(mm5_satq10),
# #                 count_negative_dates(mm1_satq10all),
# #                 count_negative_dates(mm2_satq10all),
# #                 count_negative_dates(mm3_satq10all),
# #                 count_negative_dates(mm4_satq10all),
# #                 count_negative_dates(mm5_satq10all)),
# #   meanGPP = c(calc_gpp_mean(mm1, scaler = "*0.365"),
# #                 calc_gpp_mean(mm2, scaler = "*0.365"),
# #                 calc_gpp_mean(mm3, scaler = "*0.365"),
# #                 calc_gpp_mean(mm4, scaler = "*0.365"),
# #                 calc_gpp_mean(mm5, scaler = "*0.365"),
# #                 calc_gpp_mean(mm2_sat, scaler = "*0.365"),
# #                 calc_gpp_mean(mm3_sat, scaler = "*0.365"),
# #                 calc_gpp_mean(mm1_sat, scaler = "*0.365"),
# #                 calc_gpp_mean(mm4_sat, scaler = "*0.365"),
# #                 calc_gpp_mean(mm5_sat, scaler = "*0.365"),
# #                 calc_gpp_mean(mm1_satq10gpp, scaler = "*0.365"),
# #                 calc_gpp_mean(mm2_satq10gpp, scaler = "*0.365"),
# #                 calc_gpp_mean(mm3_satq10gpp, scaler = "*0.365"),
# #                 calc_gpp_mean(mm4_satq10gpp, scaler = "*0.365"),
# #                 calc_gpp_mean(mm5_satq10gpp, scaler = "*0.365"),
# #                 calc_gpp_mean(mm1_satq10, scaler = "*0.365"),
# #                 calc_gpp_mean(mm2_satq10, scaler = "*0.365"),
# #                 calc_gpp_mean(mm3_satq10, scaler = "*0.365"),
# #                 calc_gpp_mean(mm4_satq10, scaler = "*0.365"),
# #                 calc_gpp_mean(mm5_satq10, scaler = "*0.365"),
# #                 calc_gpp_mean(mm1_satq10all, scaler = "*0.365"),
# #                 calc_gpp_mean(mm2_satq10all, scaler = "*0.365"),
# #                 calc_gpp_mean(mm3_satq10all, scaler = "*0.365"),
# #                 calc_gpp_mean(mm4_satq10all, scaler = "*0.365"),
# #                 calc_gpp_mean(mm5_satq10all, scaler = "*0.365"))
# # )
# # 
# # 
# # 
# # knitr::kable(mods)
# # 
# # topMod = pick_model(mods)
# # 
# # saveRDS(mm1_satq10, "./ignore/metab-models/ARIK_full_mle.rds")

# , predictors = c('discharge.daily'), other_args = list(span = 0.6),
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
# metab_fun(specs = bayes_specs, data = ARIK_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = ARIK_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = ARIK_met_mod,    # named list of data
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
# mm_bin <- metab(bayes_specs, data=ARIK_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/ARIK_test.rds")