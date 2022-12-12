# functions for estimating macroinvertebrate dry weights 
# written by Justin Pomeranz
# jfpomeranz@gmail.com
# started June 2020
# modified from analysis in Pomeranz et al. 2022 Global change Biology

# estimate dry weight (dw) values from length-weight regression coefficients

# run this function AFTER adding length-weight coefficients using custon-function/LW_coeff()

est_dw <- function(x, fieldData){
  # x = inv_taxonomyProcessed table from NEON data product "DP1.20120.001" with LW coefficients added using the LW_coeff() function
  # fieldData = inv_fieldData table from NEON data product "DP1.20120.001"
  
  # functions uses tidyverse functions
  require(tidyverse)
  
  # simplify fieldData to three columns
  field = fieldData %>%
    select(siteID, sampleID, benthicArea) %>%
    distinct()
  # add benthicArea column from fieldData to x. This is necessary to calculate number per m2 below
  
  # join by siteID and sampleID
  x <- left_join(x, field, by = c("siteID", "sampleID"))
  
  # correct counts to per meter squared. Round to nearest integer
  x <- x %>%
    mutate(no_m2 = round(estimatedTotalCount / benthicArea))
  
  # calculate dw based on different formula types
  # sizeClass = length of individual in mm
  x <- x %>% 
    mutate(dw = case_when(formula_type == 1 ~ a * sizeClass^b,
                          formula_type == 2 ~ exp(a + b * log(sizeClass))))
  return(x)
}
