########
# Structure Testing
# I ran this on a computational server
#
########
library(tidyverse)
library(mgcv)


dat <- as_tibble(fread("models/Threshold_5c/Multiyear/data_with_preds_resids_Dec302021.csv")) %>%
  mutate(multiyear_2_zone = factor(case_when(
    multiyear_zones == 0 ~ "NL",
    multiyear_zones == 1 ~ "SR",
    multiyear_zones == 2 ~ "SR")),
    y30_2_zone = factor(case_when(
      zone_30y == 0 ~ "NL",
      zone_30y == 1 ~ "SR",
      zone_30y == 2 ~ "SR")),
    parallel_40_split = factor(case_when(
      latitude >= 40 ~ "OW",
      latitude < 40 ~ "non_ow"
    )))

summary(dat)

multiyear_OW_3_zone <- bam(CEW_sum ~
                          s(woy,bs="cc",k=15) +
                          s(year,bs="cc",k=15) +
                          te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                          s(location,bs="re") + s(multiyear_zones ,bs="re"), 
                        family=tw(), discrete = TRUE, nthreads=24, select=TRUE,
                        data=dat4) 

multiyear_OW_2_zone <- bam(CEW_sum ~
                             s(woy,bs="cc",k=15) +
                             s(year,bs="cc",k=15) +
                             te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                             s(location,bs="re") + s(multiyear_2_zone ,bs="re"), 
                           family=tw(), discrete = TRUE, nthreads=24, select=TRUE,
                           data=dat4) 

y30_OW_3_zone <- bam(CEW_sum ~
                             s(woy,bs="cc",k=15) +
                             s(year,bs="cc",k=15) +
                             te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                             s(location,bs="re") + s(zone_30y ,bs="re"), 
                           family=tw(), discrete = TRUE, nthreads=24, select=TRUE,
                           data=dat4) 

y30_OW_2_zone <- bam(CEW_sum ~
                       s(woy,bs="cc",k=15) +
                       s(year,bs="cc",k=15) +
                       te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                       s(location,bs="re") + s(y30_2_zone ,bs="re"), 
                     family=tw(), discrete = TRUE, nthreads=24, select=TRUE,
                     data=dat4) 

parallel_40 <- bam(CEW_sum ~
                       s(woy,bs="cc",k=15) +
                       s(year,bs="cc",k=15) +
                       te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                       s(location,bs="re") + s(parallel_40_split ,bs="re"), 
                     family=tw(), discrete = TRUE, nthreads=24, select=TRUE,
                     data=dat4) 


### Model selection stuff



AIC_table <- AIC(multiyear_OW_3_zone,multiyear_OW_2_zone,y30_OW_3_zone,y30_OW_2_zone,parallel_40) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,AIC, deltaAIC), 
            .funs = funs(round,.args = list(digits=0))) %>% arrange(deltaAIC)

BIC_table <- AIC(multiyear_OW_3_zone,multiyear_OW_2_zone,y30_OW_3_zone,y30_OW_2_zone,parallel_40) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaBIC = BIC - min(BIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,BIC, deltaBIC), 
            .funs = funs(round,.args = list(digits=0))) %>% arrange(deltaBIC)


#### R-square

r2_parallel <- read.csv("models/Threshold_5c/structure_testing/structure_kfold/scores_zoneParalle.csv") %>%
  rename(Parallel = V1)
r2_zone2 <- read.csv("models/Threshold_5c/structure_testing/structure_kfold/scores_zone2.csv") %>%
  rename(Zone2 = V1)
r2_zone3 <- read.csv("models/Threshold_5c/structure_testing/structure_kfold/scores_zone3.csv") %>%
  rename(Zone3 = V1)

r2s <- r2_parallel %>% left_join(r2_zone2,by="X") %>% left_join(r2_zone3,by="X")

mean(r2_parallel$Parallel)
mean(r2_zone2$Zone2)
mean(r2_zone3$Zone3)






