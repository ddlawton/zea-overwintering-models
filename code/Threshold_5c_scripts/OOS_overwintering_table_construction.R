library(tidyverse)

OOS <- read_csv("models/Threshold_5c/Revision_models/OW_OOS_model_selection.csv") %>%
  select(!...1) %>%
  select(starts_with("deviance_mean"),zone_30y) %>%
  pivot_longer(cols=1:7) %>%
  pivot_wider(names_from = zone_30y,values_from = value)

write.csv(OOS,file='models/Threshold_5c/Revision_models/OW_OOS_final_table.csv')
