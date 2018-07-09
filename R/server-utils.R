read_data <- function(file = "summary.csv"){
    df <- read_csv(file) %>% 
        dplyr::rename(effect_size.drift = Drift_effect_size,
                      effect_size.accuracy = Acc_effect_size,
                      effect_size.reaction_time = RT_effect_size,
                      n = sample_size,
                      power.accuracy = Acsig, 
                      power.reaction_time = RTsig,
                      power.drift = Drsig) %>% 
        gather(condition, value, -n, -X1) %>% 
        separate(condition, c("var", "condition"), sep = "\\.") %>%
        spread(var, value) %>% select(-X1)
    
    nbins_effect_size <- df %>% group_by(condition, n) %>% count() %>% pull(nn) %>% unique()
    assertthat::assert_that(length(bins_effect_size) == 1)
    
    df %>% group_by(condition, n) %>% mutate(bin = order(effect_size))
}

