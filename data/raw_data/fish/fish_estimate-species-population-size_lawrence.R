library(ubms)

three_pass_taxon_model = update(readRDS(file = "models/three_pass_taxon_model.rds"),
                                chains = 4, iter = 2000)

saveRDS(three_pass_taxon_model, file = "models/three_pass_taxon_model_4chains.rds")