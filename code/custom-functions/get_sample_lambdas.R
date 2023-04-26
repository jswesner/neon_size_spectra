get_sample_lambdas <- function(model = NA, data = NA, grp1_contains = "alpha_raw_year",
                               grp2_contains = "alpha_raw_site",
                               grp3_contains = "alpha_raw_sample"){
  
  year_pivot = as_draws_df(model) %>% 
    pivot_longer(contains('alpha_raw_year'), 
                 names_to = "year_int", 
                 values_to = "year_offset") %>% 
    mutate(year_int = parse_number(year_int)) %>% 
    select(.iteration, .draw, contains("year_"))
  
  site_pivot = as_draws_df(model) %>% 
    pivot_longer(contains('alpha_raw_site'), 
                 names_to = "site_int", 
                 values_to = "site_offset") %>% 
    mutate(site_int = parse_number(site_int)) %>% 
    select(.iteration, .draw, contains("site_"))
  
  sample_pivot = as_draws_df(model) %>% 
    pivot_longer(contains('alpha_raw_sample'),
                 names_to = "sample_int", 
                 values_to = "sample_offset") %>% 
    select(!contains("alpha_raw")) %>% 
    mutate(sample_int = parse_number(sample_int)) %>% 
    left_join(data %>% ungroup %>% distinct(sample_int, year_int, site_int, 
                                                mat_s, log_gpp_s, year, site_id, temp_mean,
                                                gpp, log_gpp, log_om_s, log_om, mean_om)) %>% 
    left_join(year_pivot, by = c(".iteration",".draw","year_int")) %>% 
    left_join(site_pivot, by = c(".iteration",".draw","site_int")) 
  
  posts_long_parsed = sample_pivot %>%
    mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
             beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
             beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s + 
             sigma_site*site_offset + sigma_sample*sample_offset + sigma_site*site_offset)
  
  return(posts_long_parsed)
}
