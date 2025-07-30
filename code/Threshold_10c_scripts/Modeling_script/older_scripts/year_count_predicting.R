rm(list=ls())

library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(data.table)
library(spatialEco)
library(mgcv)

points <- as_tibble(fread("data/processed/overwintering/Zea_overwintering_zone_aug26.csv"))


pts <- points %>% select(CEW_sum, Latitude, Longitude, min, location, year, woy) %>% drop_na(CEW_sum) 
pts$min[pts$min > 278] <- 1

pts$CEW_sum <- round(pts$CEW_sum)

south <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south = sum(CEW_sum))

middle <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle = sum(CEW_sum))

north <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north = sum(CEW_sum))




southy1 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))

middley1 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))

northy1 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))



southy2 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))

middley2 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))

northy2 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))


southy3 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))

middley3 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))

northy3 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))


southy4 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy")

middley4 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy") 

northy4 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy") 


southy5 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy")

middley5 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy") 

northy5 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy") 



yearly <- north %>% left_join(south,by=c("year","woy")) %>% left_join(middle,by=c("year","woy"))
yearly$combined <- yearly$CEW_count_south + yearly$CEW_count_middle



yearly$year1 <- yearly$year - 1
yearly <- yearly %>% left_join(southy1,by=c("year1","woy")) %>% left_join(middley1,by=c("year1","woy")) %>% 
  left_join(northy1,by=c("year1","woy"))
yearly$combined1 <- yearly$CEW_count_south1 + yearly$CEW_count_middle1


yearly$year2 <- yearly$year - 2
yearly <- yearly %>% left_join(southy2,by=c("year2","woy")) %>% left_join(middley2,by=c("year2","woy")) %>% 
  left_join(northy2,by=c("year2","woy"))
yearly$combined2 <- yearly$CEW_count_south2 + yearly$CEW_count_middle2

yearly$year3 <- yearly$year - 3
yearly <- yearly %>% left_join(southy3,by=c("year3","woy")) %>% left_join(middley3,by=c("year3","woy")) %>% 
  left_join(northy3,by=c("year3","woy"))
yearly$combined3 <- yearly$CEW_count_south3 + yearly$CEW_count_middle3


yearly$year4 <- yearly$year - 4
yearly <- yearly %>% left_join(southy4,by=c("year4","woy")) %>% left_join(middley4,by=c("year4","woy")) %>% 
  left_join(northy4,by=c("year4","woy"))
yearly$combined4 <- yearly$CEW_count_south4 + yearly$CEW_count_middle4

yearly$year5 <- yearly$year - 5
yearly <- yearly %>% left_join(southy5,by=c("year5","woy")) %>% left_join(middley5,by=c("year5","woy")) %>% 
  left_join(northy5,by=c("year5","woy"))
yearly$combined5 <- yearly$CEW_count_south5 + yearly$CEW_count_middle5

yearly_long <- yearly %>% pivot_longer(c(starts_with("CEW_count_south"),starts_with("CEW_count_middle")),names_to = "lag", values_to = 'count') %>% drop_na(count)

yearly_long$lag <- factor(yearly_long$lag)
yearly_long$year <- factor(yearly_long$year)
str(yearly_long)

modGS <- bam(CEW_count_north ~ te(count,lag,bs="fs") + 
             s(year,bs='re') + s(lag,bs='re'),data=yearly_long,select=TRUE,family=tw(),
           discrete = TRUE,nthreads=23)




modS <- bam(CEW_count_north ~ te(count,lag,bs="fs") + 
             s(year,bs='re'),data=yearly_long,select=TRUE,family=tw(),
           discrete = TRUE,nthreads=23)



modI <- bam(CEW_count_north ~ te(count,by=lag,bs="tp") + 
             s(year,bs='re'),data=yearly_long,select=TRUE,family=tw(),
           discrete = TRUE,nthreads=23)

modGI <- bam(CEW_count_north ~ te(count,by=lag,bs="tp") + 
              s(year,bs='re') + s(lag,bs='re'),data=yearly_long,select=TRUE,family=tw(),
            discrete = TRUE,nthreads=23)


saveRDS(modGS,file="models/pop_dynamics/yearly_predicting/Year_modGS")
saveRDS(modS,file="models/pop_dynamics/yearly_predicting/Year_modS")
saveRDS(modI,file="models/pop_dynamics/yearly_predicting/Year_modI")
saveRDS(modGI,file="models/pop_dynamics/yearly_predicting/Year_modGI")



AIC(modGS,modGI,modS,modI)

concurvity(modI,full = TRUE)

summary(modS)
plot(modS)
k.check(modI)
library(DHARMa)
simresid <- simulateResiduals(modS)
plot(simresid)
check


summary(mod2)
gratia::draw(modS)

AIC(mod,mod2)

gratia::appraise(modS)
k.check(mod)

yearly$year <- factor(yearly$year)



yearly_long$pred <- predict(modS,type="response",newdata=yearly_long)


summary(yearly_long$pred)



ggplot(yearly_long,aes(x=count,y=pred,color=lag)) + geom_smooth(method="gam") +
  coord_cartesian(ylim=c(0,NA))


yearly_long <- yearly %>% select(1:5) %>% pivot_longer(starts_with("CEW_"),names_to = "zone", values_to = "count")

ggplot(yearly,aes(x=CEW_count_north1,y=pred)) + geom_point() + geom_smooth(method = "gam")

ggplot(yearly_long,aes(x=as.factor(year),y=count,color=zone)) + geom_boxplot()




# Predicting next year south from north 

south <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south = sum(CEW_sum))

middle <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle = sum(CEW_sum))

north <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north = sum(CEW_sum))




southy1 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))

middley1 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))

northy1 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north1 = sum(CEW_sum)) %>%
  rename(year1 = "year",woy="woy") %>%
  mutate(year1 = as.integer(year1))



southy2 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))

middley2 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))

northy2 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north2 = sum(CEW_sum)) %>%
  rename(year2 = "year",woy="woy") %>%
  mutate(year2 = as.integer(year2))


southy3 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))

middley3 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))

northy3 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north3 = sum(CEW_sum)) %>%
  rename(year3 = "year",woy="woy") %>%
  mutate(year3 = as.integer(year3))


southy4 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy")

middley4 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy") 

northy4 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north4 = sum(CEW_sum)) %>%
  rename(year4 = "year",woy="woy") 


southy5 <- pts %>% filter(min == 2) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_south5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy")

middley5 <- pts %>% filter(min == 1) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_middle5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy") 

northy5 <- pts %>% filter(min == 0) %>%
  group_by(year,woy) %>%
  summarize(CEW_count_north5 = sum(CEW_sum)) %>%
  rename(year5 = "year",woy="woy") 



yearly_pt2 <- north %>% left_join(south,by=c("year","woy")) %>% left_join(middle,by=c("year","woy"))
yearly_pt2$combined <- yearly_pt2$CEW_count_south + yearly_pt2$CEW_count_middle



yearly_pt2$year1 <- yearly_pt2$year - 1
yearly_pt2 <- yearly_pt2 %>% left_join(southy1,by=c("year1","woy")) %>% left_join(middley1,by=c("year1","woy")) %>% 
  left_join(northy1,by=c("year1","woy"))
yearly_pt2$combined1 <- yearly_pt2$CEW_count_south1 + yearly_pt2$CEW_count_middle1


yearly_pt2$year2 <- yearly_pt2$year - 2
yearly_pt2 <- yearly_pt2 %>% left_join(southy2,by=c("year2","woy")) %>% left_join(middley2,by=c("year2","woy")) %>% 
  left_join(northy2,by=c("year2","woy"))
yearly_pt2$combined2 <- yearly_pt2$CEW_count_south2 + yearly_pt2$CEW_count_middle2

yearly_pt2$year3 <- yearly_pt2$year - 3
yearly_pt2 <- yearly_pt2 %>% left_join(southy3,by=c("year3","woy")) %>% left_join(middley3,by=c("year3","woy")) %>% 
  left_join(northy3,by=c("year3","woy"))
yearly_pt2$combined3 <- yearly_pt2$CEW_count_south3 + yearly_pt2$CEW_count_middle3


yearly_pt2$year4 <- yearly_pt2$year - 4
yearly_pt2 <- yearly_pt2 %>% left_join(southy4,by=c("year4","woy")) %>% left_join(middley4,by=c("year4","woy")) %>% 
  left_join(northy4,by=c("year4","woy"))
yearly_pt2$combined4 <- yearly_pt2$CEW_count_south4 + yearly_pt2$CEW_count_middle4

yearly_pt2$year5 <- yearly_pt2$year - 5
yearly_pt2 <- yearly_pt2 %>% left_join(southy5,by=c("year5","woy")) %>% left_join(middley5,by=c("year5","woy")) %>% 
  left_join(northy5,by=c("year5","woy"))
yearly_pt2$combined5 <- yearly_pt2$CEW_count_south5 + yearly_pt2$CEW_count_middle5

yearly_pt2_long <- yearly_pt2 %>% pivot_longer(c(starts_with("CEW_count_south"),starts_with("CEW_count_middle")),names_to = "lag", values_to = 'count') %>% drop_na(count)

yearly_pt2_long$lag <- factor(yearly_pt2_long$lag)
yearly_pt2_long$year <- factor(yearly_pt2_long$year)
str(yearly_pt2_long)



modGS <- bam(CEW_count_north ~ te(count,lag,bs="fs") + 
               s(year,bs='re') + s(lag,bs='re'),data=yearly_long,select=TRUE,family=tw(),
             discrete = TRUE,nthreads=23)




modS <- bam(CEW_count_north ~ te(count,lag,bs="fs") + 
              s(year,bs='re'),data=yearly_long,select=TRUE,family=tw(),
            discrete = TRUE,nthreads=23)



modI <- bam(CEW_count_north ~ te(count,by=lag,bs="tp") + 
              s(year,bs='re'),data=yearly_long,select=TRUE,family=tw(),
            discrete = TRUE,nthreads=23)

modGI <- bam(CEW_count_north ~ te(count,by=lag,bs="tp") + 
               s(year,bs='re') + s(lag,bs='re'),data=yearly_long,select=TRUE,family=tw(),
             discrete = TRUE,nthreads=23)


saveRDS(modGS,file="models/pop_dynamics/yearly_predicting/Year_modGS")
saveRDS(modS,file="models/pop_dynamics/yearly_predicting/Year_modS")
saveRDS(modI,file="models/pop_dynamics/yearly_predicting/Year_modI")
saveRDS(modGI,file="models/pop_dynamics/yearly_predicting/Year_modGI")



# slice_sample() allows you to random select with or without replacement
mtcars %>% slice_sample(n = 5)
mtcars %>% slice_sample(n = 5, replace = TRUE)

