#########
# Winter soil temp modeling
#
#
library(splitstackshape)
library(data.table)
library(viridis)
library(anytime)
library(plyr)
library(lubridate)
library(patchwork)
if (!require(remotes)) {
  install.packages("remotes")
}

remotes::install_github('jorvlan/raincloudplots')

library("raincloudplots")

#monthly shit

dat <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_monthly.csv")) %>% rowid_to_column(var="temp_ID")

dat$soil_temp <- gsub('soil_temperature_level_2=','',dat$soil_temp)


dat <- cSplit(setDT(dat)[, lapply(.SD, gsub, pattern = "[][{}}]", 
                          replacement = "")], names(dat), sep=",", fixed = FALSE, "long")

dat<- as_tibble(dat %>% fill(c("temp_ID","ID","location","state","trap_type","unix_date","winter_end_date","winter_start_date","woy",
               "year",'CEW_sum',"Latitude","Longitude","NA_L1NAME","NA_L2NAME","NA_L3NAME","contact","date"),.direction="down") ##%>% dplyr::select(2:20)
) %>% drop_na(soil_temp)

dat$dates <- anytime(dat$dates/1000)
dat$winter_months <- month(dat$dates)
flevels <- c("11","12","1")
dat$winter_months <- factor(dat$winter_months,levels=flevels)
dat$soil_temp <- as.numeric(as.character(dat$soil_temp)) - 273.15
dat$temp_ID <- as.factor(dat$temp_ID)

dat2 <- dat %>% dplyr::group_by(ID) %>% 
  dplyr::summarize(winter_min =  min(soil_temp,na.rm=TRUE),winter_max =  max(soil_temp,na.rm=TRUE),
                   winter_mean = mean(soil_temp,na.rm=TRUE),
                   CEW_sum = mean(CEW_sum),
                   State = as.factor(first(state))) %>%
  pivot_longer(cols=c('winter_min',"winter_max","winter_mean"),names_to = "stat",values_to = "temp")

ggplot(dat,aes(x=as.numeric(soil_temp),y=as.integer(CEW_sum))) + geom_smooth() +
  coord_cartesian(ylim=c(0,NA))


ggplot(dat,aes(x=as.numeric(soil_temp),y=as.integer(CEW_sum))) + geom_smooth() +
  coord_cartesian(ylim=c(0,NA))

ggplot(dat2,aes(x=temp,y=(CEW_sum),color=stat)) + geom_smooth() +
 coord_cartesian(ylim=c(0,NA))



dat3 <- dat2 %>% drop_na(temp)
mod <- mgcv::gam(CEW_sum ~ s(winter_min) + s(winter_max) + s(winter_mean),family=poisson(),data=dat2)
summary(mod)
gratia::draw(mod)
k.check(mod)



ohio <- st_as_sf(ne_states(country="united states of america")) %>% filter(name_tr == "Ohio")
plot(ohio)

ggplot(data=ohio) + geom_sf(aes(geometry = geometry)) +
  geom_point(data=(dat %>% filter(state == "OH")),aes(x=Longitude,y=Latitude))




#daily shit

daily1 <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_chunk1.csv"))
daily2 <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_chunk2.csv"))
daily3 <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_chunk3.csv"))
daily4 <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_chunk4.csv"))
daily5 <- as_tibble(read.csv("data/processed/overwintering/Zea_wintertemp_chunk5.csv"))
daily <- rbind(daily1,daily2,daily3,daily4,daily5)

daily$soil_temp <- gsub('daily_soil_temperature_level_2=','',daily$soil_temp)


daily <- cSplit(setDT(daily)[, lapply(.SD, gsub, pattern = "[][{}}]", 
                                  replacement = "")], names(daily), sep=",", fixed = FALSE, "long")

daily2<- as_tibble(daily %>% rowid_to_column(var="temp_ID") %>% fill(c("temp_ID","ID","location","state","trap_type","unix_date","winter_end_date","winter_start_date","woy",
                               "year",'CEW_sum',"Latitude","Longitude","NA_L1NAME","NA_L2NAME","NA_L3NAME","contact","date"),.direction="down") ##%>% dplyr::select(2:20)
) %>% drop_na(soil_temp)


daily2$dates <- anytime(daily2$dates/1000)

daily2$doy_adjust <- ((yday(daily2$dates) + 183) %% 366)
daily2$doy <- ((yday(daily2$dates)))
daily2$soil_temp <- gsub('daily_','',daily2$soil_temp)
daily2$soil_temp <- as.numeric(as.character(daily2$soil_temp)) - 273.15
daily2 <- daily2  %>% drop_na(soil_temp)

p1 <- ggplot((daily2 %>% sample_n(size=100000)),aes(x=doy_adjust,y=(soil_temp))) + geom_smooth()
p2 <- ggplot((daily2 %>% sample_n(size=100000)),aes(x=doy,y=(soil_temp))) + geom_smooth()

p1 + p2


daily2 <- daily2 %>%
  mutate(lag_year = )

