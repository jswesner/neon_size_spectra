library(brms)

dat_fishinvert = readRDS(file = "data/derived_data/dat_fishinvert.rds")

dat_fishinvert_quantile0.8 = brm(bf(log_dw_c ~ mat_s*animal_type + (1|year) + 
                                      (1|season), quantile = 0.2), 
                                 data = dat_fishinvert, 
                                 family = asym_laplace(),
                                 prior = c(prior(normal(1, 0.5), class = "Intercept"),
                                           prior(normal(0, 1), class = "b"),
                                           prior(exponential(2), class = "sigma")),
                                 iter = 2000, chains = 4)

saveRDS(dat_fishinvert_quantile0.8, file = "models/dat_fishinvert_quantile0.8.rds")