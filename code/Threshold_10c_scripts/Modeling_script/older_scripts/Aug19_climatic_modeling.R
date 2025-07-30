rm(list=ls())
library(tidyverse)
library(data.table)
library(splitstackshape)
library(anytime)
library(lubridate)
library(mgcv)

dat <- as_tibble(fread("data/processed/overwintering/monthly_soil_temp/Zea_wintertemp_chunk1.csv"))
dat2 <- as_tibble(fread("data/processed/overwintering/monthly_soil_temp/Zea_wintertemp_chunk2.csv"))
dat3 <- as_tibble(fread("data/processed/overwintering/monthly_soil_temp/Zea_wintertemp_chunk3.csv"))
dat4 <- as_tibble(fread("data/processed/overwintering/monthly_soil_temp/Zea_wintertemp_chunk4.csv"))
dat5 <- as_tibble(fread("data/processed/overwintering/monthly_soil_temp/Zea_wintertemp_chunk5.csv"))


datc <- rbind(dat,dat2,dat3,dat4,dat5)

datc$soil_temp <- gsub('soil_temperature_level_2=','',datc$soil_temp)


datc <- cSplit(setDT(datc)[, lapply(.SD, gsub, pattern = "[][{}}]", 
                                  replacement = "")], names(datc), sep=",", fixed = FALSE, "long")

datc<- as_tibble(datc %>% fill(c("ID","location","state","trap_type","unix_date","winter_end_date","winter_start_date","woy",
                               "year",'CEW_sum',"Latitude","Longitude","NA_L1NAME","NA_L2NAME","NA_L3NAME","contact","date"),.direction="down") ##%>% dplyr::select(2:20)
) %>% drop_na(soil_temp)


datc$dates <- anytime(as.numeric(datc$dates2)/1000)
datc$date <- parse_date_time(datc$date,order="mdy")

datc$dates_dif <- as.numeric(difftime(datc$dates,datc$date,unit="days"))
datc$months <- month(datc$dates)
datc$soil_temp <- as.numeric(as.character(datc$soil_temp)) - 273.15
summary(as.numeric(datc$dates_dif))



datc_filtered <- datc %>% filter(between(woy,20,45)) #%>% filter(state=="NC")

datc_filtered$CEW_sum <- round(datc_filtered$CEW_sum)

datc_filtered$location  <- factor(datc_filtered$location )

datc_filtered <- datc_filtered %>% filter(tmean > -1000) %>% filter(precip > 0)

monthsfact <- c("11","12","1","2","3","4","5")

datc_filtered2 <- datc_filtered %>% mutate(months = as.factor(months)) %>% filter(months %in% monthsfact)

summary(datc_filtered)


p1 <- ggplot(datc_filtered2,aes(x=dates_dif,y=(soil_temp),z=round(CEW_sum))) + stat_summary_hex(bins=30) + viridis::scale_fill_viridis() + ggpubr::theme_pubr(legend="right") + coord_cartesian(ylim=c(-40,40)) +
  ylab("Soil temperature (c)") + xlab("Days before")

p2 <- ggplot((datc_filtered2 %>% filter(tmean >= -50)),aes(x=dates_dif,y=(tmean),z=round(CEW_sum))) + stat_summary_hex(bins=30) + viridis::scale_fill_viridis()  + coord_cartesian(ylim=c(-40,40)) + ggpubr::theme_pubr(legend="right") +
  ylab("Air temperature (c)") + xlab("Days before")

p3 <- ggplot((datc_filtered2 %>% filter(precip >= 0)),aes(x=dates_dif,y=(precip),z=round(CEW_sum))) + stat_summary_hex(bins=30) +
  viridis::scale_fill_viridis() + 
  ggpubr::theme_pubr(legend="right") +
  ylab("Precipitation (mm)") + xlab("Days before")

p1 + p2 + p3





mod <- bam(CEW_sum ~ te(dates_dif,soil_temp) +
                     te(woy,Latitude,bs=c("cc","gp")) +
                     s(location,bs="re"),select=TRUE,
           data=datc_filtered2)

summary(mod)
gratia::draw(mod)
gratia::appraise(mod)
concurvity(mod)

library(DHARMa)
simresid <- simulateResiduals(mod)
plot(simresid)






ggplot(datc_filtered2,aes(color=months,x=(soil_temp),y=round(CEW_sum))) + geom_point() + ggpubr::theme_pubr(legend="right") + 
  ylab("CEW count") + xlab("Temperature")


ggplot(datc_filtered2,aes(x=(soil_temp),y=round(CEW_sum),color=as.factor(months))) + geom_smooth() + ggpubr::theme_pubr(legend="right") +
  ylab("CEW_sum") + xlab("soil_temp")

ggplot((datc_filtered %>% filter(tmean >= -50)),aes(x=dates_dif,y=(tmean),z=round(CEW_sum))) + stat_summary_hex(bins=30) + viridis::scale_fill_viridis(option="A")  + coord_cartesian(ylim=c(-40,40)) + ggpubr::theme_pubr(legend="right") +
  ylab("Air temperature (c)") + xlab("Days before")

ggplot((datc_filtered %>% filter(precip >= 0)),aes(x=dates_dif,y=(precip),z=round(CEW_sum))) + stat_summary_hex(bins=30) +
  viridis::scale_fill_viridis(option="A") + 
  ggpubr::theme_pubr(legend="right") +
  ylab("Precipitation (mm)") + xlab("Days before")

p1 + p2 + p3




datc_filtered3 <- datc_filtered2 %>% mutate(season =
                            case_when(
                              months == "12" ~ "winter",
                              months == "11" ~ "winter",
                              months == "1" ~ "winter",
                              months == "2" ~ "winter",
                              months == "3" ~ "spring",
                              months == "4" ~ "spring",
                              months == "5" ~ "spring",
                              TRUE ~ "Other"
                            ))


st <- datc_filtered3 %>% drop_na(soil_temp)
at <- datc_filtered3 %>% drop_na(tmean) %>% filter(tmean > -50)
precip <- datc_filtered3 %>% drop_na(tmean) %>% filter(precip >= 0)


p1 <- ggplot(st,aes(x=soil_temp,y=CEW_sum, color=season)) + geom_smooth()
p2 <- ggplot(at,aes(x=tmean,y=CEW_sum, color=season)) + geom_smooth()
p3 <- ggplot(precip,aes(x=precip,y=CEW_sum, color=season)) + geom_smooth()

p1 + p2 + p3



parallel::detectCores()


mod_st <- bam(CEW_sum ~ te(soil_temp,by=as.factor(season),k=20),select=TRUE,
           data=st,discrete = TRUE,
           nthreads=10,family=tw())


mod_at <- bam(CEW_sum ~ te(tmean,by=as.factor(season),k=20),select=TRUE,
           data=at,discrete = TRUE,
           nthreads=10,family=tw())

mod_precip <- bam(CEW_sum ~ te(precip,by=as.factor(season),k=20),select=TRUE,
           data=precip,discrete = TRUE,
           nthreads=10,family=tw())



summary(mod_st)
summary(mod_at)
summary(mod_precip)



gratia::draw(mod_st)



datc %>% filter(state == "NC") %>% mutate(obs_month = month(date)) %>% filter(obs_month == 1)



