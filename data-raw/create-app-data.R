library(devtools)
library(dplyr)

#looks for summary.csv
# the output of ddm_sims/collate.py

powersim <- readr::read_csv(here::here("data-raw", "summary.csv")) %>% 
        dplyr::rename(effect_size.drift = Drift_effect_size,
                      n = sample_size,
                      power.accuracy = Acsig, 
                      power.reaction_time = RTsig,
                      power.drift = Drsig) %>% 
        dplyr::mutate(
            effect_size.accuracy = effect_size.drift,
            effect_size.reaction_time = effect_size.drift) %>%
        dplyr::select(-Acc_effect_size, -RT_effect_size) %>%
        tidyr::gather(key = "condition", value = value, -n, -X1 ) %>% 
        tidyr::separate(condition, c("var", "condition"), sep = "\\.") %>%
        tidyr::spread(var, value) %>% select(-X1) %>%
        mutate(condition = factor(recode(condition, reaction_time = "reaction time"), 
                                  levels = c("drift", 
                                             "accuracy", 
                                             "reaction time")))
        
    use_data(powersim, overwrite = T)



        
