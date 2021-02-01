write_csv(boot::melanoma, here("data", "raw_melanoma.csv")) 
#saving raw data, usually raw data won't be saved from R, so this step can be omitted

raw_melanoma <- read_csv(here("data", "raw_melanoma.csv"))

melanoma <- raw_melanoma %>% 
  mutate(status = 
           status %>% 
           factor() %>%  
           fct_recode("dead_melanoma" = "1", 
                      "alive" = "2", 
                      "dead_other" = "3")) %>% 
  mutate(sex = 
           sex %>% 
           factor() %>% 
           fct_recode("female" = "0", 
                      "male" = "1")) %>% 
  mutate(ulcer = 
           ulcer %>% 
           factor() %>% 
           fct_recode("absent" = "0", 
                      "present" = "1")) %>% 
  mutate(status_os = if_else(status == "alive", 0, 
                             1)) %>%
  mutate(status_dss = if_else(status == "dead_melanoma", 1, 
                              0)) %>% 
  mutate(status_cr = case_when(status == "alive" ~ 0, 
                               status == "dead_melanoma" ~ 1, 
                               status == "dead_other" ~ 2)) %>% 
  mutate(t_stage = cut(thickness, breaks = c(0, 1, 2, 4, max(thickness, na.rm = TRUE)), include.lowest = TRUE, 
         labels = c("T1", "T2", "T3", "T4"))) %>% 
  mutate(dead_melanoma_5yr = ifelse((time/365.25) < 5 & (status == "dead_melanoma"), "Yes", "No") %>% 
           fct_relevel("No"))

write_csv(melanoma, here("data", "cleaned_melanoma.csv")) #saving cleaned data

save(melanoma, file = here("data", "melanoma_final.rda"))


