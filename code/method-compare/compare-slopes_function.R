# function to compare slopes

# originally written for SI in Pomeranz, Junker, and Wesner 2022 *Global Change Biology* 

compare_slopes <- function(data, dw_range, rsp_var, ...){
  # define breaks widths and mid bins for log2 and 6 equal log bins
  # log2
  breaks2 <- 2^(floor(log2(min(dw_range))):
                  ceiling(log2(max(dw_range))) )
  mid_bin_2 <- log10(breaks2[floor(length(breaks2)/2)]) 
  # 6 equal log bins
  breaks_log_6 <- exp(seq(floor(log(min(dw_range))),
                          ceiling(log(max(dw_range))),
                          length.out = 7))
  mid_bin_log_6 <- log10(breaks_log_6[floor(length(breaks_log_6)/2)])
  
  # Normalized Abundance Spectra
  NAS <- bin_and_center(data, var = rsp_var, breaks = breaks2)
  NAS$log_mids_center <- NAS$log_mids - mid_bin_2
  NAS_coefs <- coef(lm(log_count_corrected~log_mids_center,
                       data = NAS))
  # non-normalized abundance
  AS_coefs<- coef(lm(log_count~log_mids_center,
                     data = NAS))
  # equal logarithmic binning method
  
  # breaks_log_6 <- exp(seq(floor(log(min(dw_range))),
  #                         ceiling(log(max(dw_range))),
  #                         length.out = 7))
  # mid_bin_log_6 <- log10(breaks_log_6[floor(length(breaks_log_6)/2)])
  
  perkins <- bin_and_center(data, var = rsp_var, breaks = breaks_log_6)
  perkins$log_mids_center <- perkins$log_mids - mid_bin_log_6
  p_coef <- coef(lm(log_count~log_mids_center, data = perkins))
  pn_coef <- coef(lm(log_count_corrected~log_mids_center, data = perkins))
  
  # MLE method from Edwards et al. 2018
  mle_b <- MLE_tidy(data, rsp_var)$b
  
  slopes = data.frame(NAS = NAS_coefs[2],
                      AS = AS_coefs[2],
                      Perkins = p_coef[2],
                      PN = pn_coef[2],
                      mle = mle_b)
  slopes
}