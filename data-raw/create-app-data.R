library(dplyr)
    df <- readr::read_csv(here::here("data-raw", "summary.csv")) %>% 
        dplyr::rename(effect_size.drift = Drift_effect_size,
                      effect_size.accuracy = Acc_effect_size,
                      effect_size.reaction_time = RT_effect_size,
                      n = sample_size,
                      power.accuracy = Acsig, 
                      power.reaction_time = RTsig,
                      power.drift = Drsig) %>% 
        tidyr::gather(condition, value, -n, -X1) %>% 
        tidyr::separate(condition, c("var", "condition"), sep = "\\.") %>%
        tidyr::spread(var, value) %>% select(-X1)
    
    nbins_effect_size <- df %>% group_by(condition, n) %>% count() %>% pull(nn) %>% unique()
    assertthat::assert_that(length(nbins_effect_size) == 1)
    
    powersim <- df %>% group_by(condition, n) %>% mutate(bin = order(effect_size))
        use_data(powersim, overwrite = T)


