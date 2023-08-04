
#' @title mass_corr_bio
#' @description
#' This function "corrects" the observed standing biomass for body size differences
#' 
#' @param a This variable is the observed "uncorrected" biomass value
#'@param b This variable is the mean community bodysize weighted by relative biomass
#'


mass_corr_bio = function(a,b){a*(b^0.25)}
