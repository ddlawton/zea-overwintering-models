#####
# SST anaomlies modeling with 
#  Zea data
#####

rm(list=ls()) #if needed

#Various libraries

library(tidyverse)
library(ggpubr)
library(patchwork)
library(mgcv)
library(gratia)
library(data.table)
library(lubridate)
library(rsoi) #this is just a conveinent package to download SST data from. I use it only for MEI (ENSO)

# Importing data and formatting
#####
#Downloading ENSO (MEI), PDO, and NAO metrics. These are monthly estimates.

MEI <- download_mei(use_cache = FALSE, file = NULL) %>% # this is a multivariate approach to determining La/El Nino. 
  mutate(
    Month = month(Date),
    Season = case_when(
      Month == 12 ~ "Winter",
      Month == 11 ~ "Fall",
      Month == 10 ~ "Fall",
      Month == 9 ~ "Fall",
      Month == 8 ~ "Summer",
      Month == 7 ~ "Summer",
      Month == 6 ~ "Summer",
      Month == 5 ~ "Spring",
      Month == 4 ~ "Spring",
      Month == 3 ~ "Spring",
      Month == 2 ~ "Winter",
      Month == 1 ~ "Winter"
    )) %>% group_by(Year,Season) %>% summarize(MEI_avg = mean(MEI)) %>% filter(Year >= 1979)

MEI_phase <- MEI %>% select(Year,Phase)


NAO <- as_tibble(fread("https://www.ncdc.noaa.gov/teleconnections/nao/data.csv")) %>%
  separate(col = Date,into=c("Year","Month"),sep=-2) %>% mutate_at(vars(1:2),as.integer) %>%
  mutate(
    Season = case_when(
      Month == 12 ~ "Winter",
      Month == 11 ~ "Fall",
      Month == 10 ~ "Fall",
      Month == 9 ~ "Fall",
      Month == 8 ~ "Summer",
      Month == 7 ~ "Summer",
      Month == 6 ~ "Summer",
      Month == 5 ~ "Spring",
      Month == 4 ~ "Spring",
      Month == 3 ~ "Spring",
      Month == 2 ~ "Winter",
      Month == 1 ~ "Winter"
    )
  ) %>% group_by(Year,Season) %>% summarize(NAO_avg = mean(Value)) %>% filter(Year >= 1979)

PDO <- as_tibble(fread("https://www.ncdc.noaa.gov/teleconnections/pdo/data.txt"))

PDO2 <- PDO %>% pivot_longer(cols=c(2:13),names_to= "months",values_to = "PDO") %>%
  mutate(
    Season = case_when(
      months == "Dec" ~ "Winter",
      months == "Nov" ~ "Fall",
      months == "Oct" ~ "Fall",
      months == "Sep" ~ "Fall",
      months == "Aug" ~ "Summer",
      months == "Jul" ~ "Summer",
      months == "Jun" ~ "Summer",
      months == "May" ~ "Spring",
      months == "Apr" ~ "Spring",
      months == "Mar" ~ "Spring",
      months == "Feb" ~ "Winter",
      months == "Jan" ~ "Winter",
    )) %>% group_by(Year,Season) %>% summarize(PDO_avg = mean(PDO)) %>% filter(Year >= 1979)

#this is all SSTs averaged by year and season
SSTs <- MEI %>% left_join(NAO,by=c("Year","Season")) %>% 
  pivot_wider(names_from = Season,values_from = c(MEI_avg,NAO_avg))

#this is all SSTs averaged by year 
SSTs_year <- MEI %>% left_join(NAO,by=c("Year","Season"))  %>%
  group_by(Year) %>% summarize(MEI_avg = mean(MEI_avg),NAO_avg = mean(NAO_avg))



ggplot(SSTs_year,aes(x=MEI_avg,y=NAO_avg)) + geom_point() + geom_smooth() + theme_pubr() +   #there are some correlations....
  ggplot(SSTs_year,aes(x=PDO_avg,y=NAO_avg)) + geom_point() + geom_smooth() + theme_pubr() +
  ggplot(SSTs_year,aes(x=MEI_avg,y=PDO_avg)) + geom_point() + geom_smooth() + theme_pubr() 


#now calculating scaled mean and formatting for zea data

dat <- as_tibble(fread("data/processed/overwintering/Zea_overwintering_zone.csv"))  %>%  
  filter(NA_L1NAME != "WATER")

loc_year1 <- dat %>% group_by(location,year) %>% #this chunk of code procudes yearly averages.
  summarize(CEW_mean = mean(CEW_sum),
            CEW_max = max(CEW_sum),
            n_woy = n(),
            overwinter_zone = first(min),
            latitude = first(Latitude),
            longitude = first(Longitude),
            eco = first(NA_L2NAME),
            woy = first(woy),
            year= first(year)) %>%
  ungroup() %>% group_by(location) %>%
  mutate(overall_average = mean(CEW_mean),
         scaled_mean = scale(CEW_mean)[,1]) %>%
  drop_na(scaled_mean) %>% drop_na(overwinter_zone) %>%
  filter(overwinter_zone < 4) %>%
  mutate(overwinter_zone = as.factor(overwinter_zone),
         overwinter_zone = fct_recode(overwinter_zone, "Northern_limits" = "0", "Transitional_zone" = "1", "Southern Range" = "2"))

ggplot(loc_year, aes(x=CEW_mean,y=scaled_mean)) + geom_point() + geom_smooth() + theme_pubr() +
  ggplot(loc_year,aes(x=year,y=scaled_mean)) + geom_smooth() + theme_pubr() +
  ggplot(loc_year,aes(x=year,y=CEW_mean)) + geom_smooth() + theme_pubr() 


loc <- dat %>% group_by(location) %>%
  mutate(scaled_mean = scale(CEW_sum )[,1],
         CEW_max = max(CEW_sum),
         overwinter_zone = min) %>%
  drop_na(overwinter_zone) %>%
  filter(overwinter_zone < 4) %>%
  mutate(overwinter_zone = as.factor(overwinter_zone),
         overwinter_zone = fct_recode(overwinter_zone, 
                                      "Northern_limits" = "0", 
                                      "Transitional_zone" = "1", 
                                      "Southern Range" = "2")) %>%
  dplyr::select(!ends_with("_level_2")) %>% drop_na(scaled_mean) %>%
  select(CEW_sum,scaled_mean,Latitude,Longitude,NA_L2NAME,date,woy,year,overwinter_zone,)

ggplot((loc %>% filter(overwinter_zone == "Northern_limits")), aes(x=woy,y=CEW_sum,color=overwinter_zone)) + geom_smooth() + geom_rug()


ggplot(loc, aes(x=CEW_sum,y=scaled_mean)) + geom_point() + geom_smooth() + theme_pubr()+
  ggplot(loc,aes(x=year,y=scaled_mean)) + geom_smooth() + geom_hline(yintercept = 0) + theme_pubr() +
  ggplot(loc,aes(x=year,y=CEW_sum)) + geom_smooth() + theme_pubr() 


Loc


#now lets add SSTs and dat together


SSTs_year1 <- SSTs_year %>% rename("MEI_avg1" = "MEI_avg","NAO_avg1" = "NAO_avg","PDO_avg1" = "PDO_avg")
SSTs_year2 <- SSTs_year %>% rename("MEI_avg2" = "MEI_avg","NAO_avg2" = "NAO_avg","PDO_avg2" = "PDO_avg")


loc_year <- loc_year1 %>% left_join(SSTs_year,by= c("year"="Year")) %>%
  drop_na(MEI_avg) %>% left_join((SSTs),by= c("year"="Year")) %>%
  drop_na(MEI_avg_Fall) %>% mutate(
    year_1 = year  - 1,
    year_2 = year  - 2
  ) %>% left_join(SSTs_year1,by= c("year_1"="Year")) %>% left_join(SSTs_year2,by= c("year_2"="Year"))

?left_join


loc <- loc   %>% left_join(SSTs_year,by= c("year"="Year")) %>%
  drop_na(MEI_avg) %>% left_join((SSTs),by= c("year"="Year")) %>%
  drop_na(MEI_avg_Fall)

loc_year_MEI_phase <- loc_year1 %>% left_join(MEI_phase,by= c("year"="Year"))


# Exploratory graphing 
#####

# Yearly
##Exploratory modeling 

#scaled_mean x overall yearly SST avg.

names <- c("NAO_avg","MEI_avg")
plots1 <- list()
for (i in names){
  plots1[[i]] <- (ggplot(loc_year,aes_string(x=i,y="scaled_mean")) + geom_smooth(method="gam") +
                    geom_smooth(method="lm",color="red") +
                    geom_hline(yintercept = 0) + theme_pubr() + ggtitle(print(i)))
}



#CEW_mean x overall yearly SST avg.

names <- c("NAO_avg","MEI_avg")
plots2 <- list()
for (i in names){
  plots2[[i]] <- (ggplot(loc_year,aes_string(x=i,y="CEW_mean")) + geom_smooth()  +
                    geom_smooth(method="lm",color="red") +
                    theme_pubr() + ggtitle(print(i)))
}


(wrap_plots(plots1))/
  (wrap_plots(plots2))

mod <- lm(loc_year$scaled_mean~loc_year$MEI_avg)
nl_mod <- gam(loc_year$scaled_mean~s(loc_year$MEI_avg),family=scat())

summary(mod)
summary(nl_mod)
draw(nl_mod)
#broken into seasons

names <- colnames(loc_year %>% ungroup() %>% select(ends_with("_Winter"),ends_with("_Fall"),ends_with("_Spring"),ends_with("_Summer")))

#scaled_mean x seasonal SST avg.

plots3 <- list()
for (i in names){
  plots3[[i]] <- (ggplot(loc_year,aes_string(x=i,y="scaled_mean")) + geom_smooth() +
                    geom_hline(yintercept = 0) + theme_pubr() + ggtitle(print(i)))
}

wrap_plots(plots3)

#CEW_mean x overall yearly SST avg.

plots4 <- list()
for (i in names){
  plots4[[i]] <- (ggplot(loc_year,aes_string(x=i,y="CEW_mean")) + geom_smooth() +
                    theme_pubr() + ggtitle(print(i)))
}

(wrap_plots(plots3,ncol = 6))/
  (wrap_plots(plots4,ncol = 6))

# Basal Data
##Exploratory modeling 

#scaled_mean x overall yearly SST avg.

names <- c("NAO_avg","PDO_avg","MEI_avg")
plots5 <- list()
for (i in names){
  plots5[[i]] <- (ggplot(loc,aes_string(x=i,y="scaled_mean")) + geom_smooth() +
                    geom_hline(yintercept = 0) + theme_pubr() + ggtitle(print(i)))
}



#CEW_mean x overall yearly SST avg.

plots6 <- list()
for (i in names){
  plots6[[i]] <- (ggplot(loc,aes_string(x=i,y="CEW_sum")) + geom_smooth() +
                    theme_pubr() + ggtitle(print(i)))
}

(wrap_plots(plots1))/
  (wrap_plots(plots2))/
  (wrap_plots(plots5))/
  (wrap_plots(plots6))

#broken into seasons

names <- colnames(loc_year %>% ungroup() %>% select(ends_with("_Winter"),ends_with("_Fall"),ends_with("_Spring"),ends_with("_Summer")))

#scaled_mean x seasonal SST avg.

plots7 <- list()
for (i in names){
  plots7[[i]] <- (ggplot(loc,aes_string(x=i,y="scaled_mean")) + geom_smooth() +
                    geom_hline(yintercept = 0) + theme_pubr() + ggtitle(print(i)))
}

wrap_plots(plots7)

#CEW_mean x overall yearly SST avg.

plots8 <- list()
for (i in names){
  plots8[[i]] <- (ggplot(loc,aes_string(x=i,y="CEW_sum")) + geom_smooth() +
                    theme_pubr() + ggtitle(print(i)))
}

(wrap_plots(plots7,ncol = 6))/
  (wrap_plots(plots8,ncol = 6))


# now by ecoregion


Eco_names <- unique(loc_year$eco)


plots9 <- list()
plots10 <- list()
plots11 <- list()
for (i in Eco_names){
    plots9[[i]] <- (ggplot((loc_year %>% filter(eco == i)),aes_string(x="MEI_avg",y="scaled_mean")) + 
                        geom_smooth(method="gam") +geom_hline(yintercept = 0) + theme_pubr() + 
                        labs(x=x, y="Scaled Mean",title=i,subtitle="MEI_avg"))
    
    plots10[[i]] <- (ggplot((loc_year %>% filter(eco == i)),aes_string(x="NAO_avg",y="scaled_mean")) + 
                      geom_smooth(method="gam") +geom_hline(yintercept = 0) + theme_pubr() + 
                      labs(x=x, y="Scaled Mean",title=i,subtitle="NAO_avg"))
    
    plots11[[i]] <- (ggplot((loc_year %>% filter(eco == i)),aes_string(x="PDO_avg",y="scaled_mean")) + 
                       geom_smooth(method="gam") +geom_hline(yintercept = 0) + theme_pubr() + 
                       labs(x=x, y="Scaled Mean",title=i,subtitle="PDO_avg"))
  }


wrap_plots(plots9)
wrap_plots(plots10)
wrap_plots(plots11)


#CEW_mean x overall yearly SST avg.

names <- c("NAO_avg","PDO_avg","MEI_avg")
plots2 <- list()
for (i in names){
  plots2[[i]] <- (ggplot(loc_year,aes_string(x=i,y="CEW_mean")) + geom_smooth() +
                    theme_pubr() + ggtitle(print(i)))
}


(wrap_plots(plots1))/
  (wrap_plots(plots2))



####





# Lets build some basic models
#####
loc_year$eco <- as.factor(loc_year$eco)
loc_year$location <- as.factor(loc_year$location)
# Yearly

m1 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) + 
            s(PDO_avg), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m1)
draw(m1)
appraise(m1)


m2 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp"), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m2)
draw(m2)
appraise(m2)

AIC(m1,m2)

m3 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            te(longitude,latitude,bs=c('gp','gp')), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m3)
draw(m3)
appraise(m3)

AIC(m1,m2,m3)

m4 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(MEI_avg,eco,bs="fs") + 
            s(NAO_avg,eco,bs="fs") +
            s(PDO_avg,eco,bs="fs") +
            te(longitude,latitude,bs=c('gp','gp')), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m4)
draw(m4)
appraise(m4)

AIC(m1,m2,m3,m4)

m5 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(MEI_avg,eco,bs="fs") + 
            s(NAO_avg,eco,bs="fs") +
            s(PDO_avg,eco,bs="fs") +
            s(eco,bs="re") +
            s(year,bs="gp") +
            te(longitude,latitude,bs=c('gp','gp')), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m5)
draw(m5)
appraise(m5)

AIC(m1,m2,m3,m4,m5)

m6 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(MEI_avg,eco,bs="fs") + 
            s(NAO_avg,eco,bs="fs") +
            s(PDO_avg,eco,bs="fs") +
            s(year,bs="gp") +
            s(eco,bs="re") + s(location,bs="re") +
            te(longitude,latitude,bs=c('gp','gp')), 
          data=loc_year,family = scat(),
          discrete = TRUE, nthreads = 23)

summary(m6)
draw(m6)
appraise(m6)

AIC(m1,m2,m3,m4,m5,m6)

#now weekly trends

loc$NA_L2NAME     <- as.factor(loc$NA_L2NAME    )
loc$location <- as.factor(loc$location)

m7 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) + 
            s(PDO_avg), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m7)
#draw(m7)
#appraise(m7)


m8 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(woy,bs="cc") +
            s(year,bs="gp"), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m8)
#draw(m8)
#appraise(m8)

#AIC(m7,m8)

m9 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(woy,bs="cc") +
            s(year,bs="gp") +
            te(Longitude,Latitude,bs=c('gp','gp')), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m9)
#draw(m9)
#appraise(m9)

#AIC(m1,m2,m3)

m10 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(MEI_avg,NA_L2NAME,bs="fs") + 
            s(NAO_avg,NA_L2NAME,bs="fs") +
            s(PDO_avg,NA_L2NAME,bs="fs") +
            s(year,bs="gp") +
            s(woy,bs="cc") +
            te(Longitude,Latitude,bs=c('gp','gp')), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m10)
#draw(m10)
#appraise(m10)

#AIC(m1,m2,m3,m4)

m11 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(MEI_avg,NA_L2NAME,bs="fs") + 
            s(NAO_avg,NA_L2NAME,bs="fs") +
            s(PDO_avg,NA_L2NAME,bs="fs") +
            s(year,bs="gp") +
            s(woy,bs="cc") +
            s(NA_L2NAME,bs="re") +
            te(Longitude,Latitude,bs=c('gp','gp')), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m11)
#draw(m11)
#appraise(m11)

#AIC(m1,m2,m3,m4,m5)

m12 <- bam(scaled_mean ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(year,bs="gp") +
            s(woy,bs="cc") +
            s(MEI_avg,NA_L2NAME,bs="fs") + 
            s(NAO_avg,NA_L2NAME,bs="fs") +
            s(PDO_avg,NA_L2NAME,bs="fs") +
            s(NA_L2NAME,bs="re") + s(location,bs="re") +
            te(Longitude,Latitude,bs=c('gp','gp')), 
          data=loc,family = scat(),
          discrete = TRUE, nthreads = 23)

#summary(m12)
#draw(m12)
#appraise(m12)

m13 <- bam(scaled_mean ~ 
             s(MEI_avg) + 
             s(NAO_avg) +
             s(PDO_avg) +
             s(year,bs="gp") +
             s(woy,bs="cc") +
             s(woy,NA_L2NAME,bs="fs") +
             s(MEI_avg,NA_L2NAME,bs="fs") + 
             s(NAO_avg,NA_L2NAME,bs="fs") +
             s(PDO_avg,NA_L2NAME,bs="fs") +
             s(NA_L2NAME,bs="re") + #s(location,bs="re") +
             te(Longitude,Latitude,bs=c('gp','gp')), 
           data=loc,family = scat(),
           discrete = TRUE, nthreads = 23)

#summary(m13)
#draw(m13)
#appraise(m13)

AIC(m13,m12,m11,m10,m9,m8,m7)
BIC(m13,m12,m11,m10,m9,m8,m7)

summary(m10)
summary(m11)
summary(m12)
summary(m13)
draw(m13)
#####

#now weekly trends but raw data

loc$NA_L2NAME     <- as.factor(loc$NA_L2NAME    )
loc$location <- as.factor(loc$location)
loc$CEW_sum <- round(loc$CEW_sum)
m14 <- bam(CEW_sum  ~ 
            s(MEI_avg) + 
            s(NAO_avg) + 
            s(PDO_avg), 
          data=loc,family = tw(),
          discrete = TRUE, nthreads = 23)

#summary(m7)
#draw(m7)
#appraise(m7)


m15 <- bam(CEW_sum  ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(woy,bs="cc") +
            s(year,bs="gp"), 
          data=loc,family = tw(),
          discrete = TRUE, nthreads = 23)

summary(m8)
#draw(m8)
#appraise(m8)

#AIC(m7,m8)

m16 <- bam(CEW_sum  ~ 
            s(MEI_avg) + 
            s(NAO_avg) +
            s(PDO_avg) +
            s(woy,bs="cc") +
            s(year,bs="gp") +
            te(Longitude,Latitude,bs=c('gp','gp')), 
          data=loc,family = tw(),
          discrete = TRUE, nthreads = 23)

summary(m9)
#draw(m9)
#appraise(m9)

#AIC(m1,m2,m3)

m17 <- bam(CEW_sum  ~ 
             s(MEI_avg) + 
             s(NAO_avg) +
             s(PDO_avg) +
             s(year,bs="gp") +
             s(MEI_avg,NA_L2NAME,bs="fs") + 
             s(NAO_avg,NA_L2NAME,bs="fs") +
             s(PDO_avg,NA_L2NAME,bs="fs") +
             s(year,bs="gp") +
             s(woy,bs="cc") +
             te(Longitude,Latitude,bs=c('gp','gp')), 
           data=loc,family = tw(),
           discrete = TRUE, nthreads = 23)

summary(m10)
#draw(m10)
#appraise(m10)

#AIC(m1,m2,m3,m4)

m18 <- bam(CEW_sum  ~ 
             s(MEI_avg) + 
             s(NAO_avg) +
             s(PDO_avg) +
             s(year,bs="gp") +
             s(MEI_avg,NA_L2NAME,bs="fs") + 
             s(NAO_avg,NA_L2NAME,bs="fs") +
             s(PDO_avg,NA_L2NAME,bs="fs") +
             s(year,bs="gp") +
             s(woy,bs="cc") +
             s(NA_L2NAME,bs="re") +
             te(Longitude,Latitude,bs=c('gp','gp')), 
           data=loc,family = tw(),
           discrete = TRUE, nthreads = 23)

summary(m11)
#draw(m11)
#appraise(m11)

#AIC(m1,m2,m3,m4,m5)


names <- unique((loc %>% group_by(location,year) %>%
  summarize(n=n()) %>% filter(n > 10) %>% droplevels())$location)

loc_filtered <- loc %>% filter(location %in% names)

m19 <- bam(CEW_sum  ~ 
             s(MEI_avg) + 
             s(NAO_avg) +
             s(PDO_avg) +
             s(year,bs="gp") +
             s(woy,bs="cc") +
             ti(woy,overwinter_zone,bs="fs") +
             ti(MEI_avg,overwinter_zone,bs="fs") + 
             ti(NAO_avg,overwinter_zone,bs="fs") +
             ti(PDO_avg,overwinter_zone,bs="fs") +
             s(overwinter_zone,bs="re") + s(location,bs="re") +
             te(Longitude,Latitude,bs=c('gp','gp')), 
           data=loc_filtered,family = tw(),
           discrete = TRUE, nthreads = 23)

summary(m19)
draw(m19)
#appraise(m12)

m20 <- bam(CEW_sum  ~ 
             s(MEI_avg) + 
             s(NAO_avg) +
             s(PDO_avg) +
             s(year,bs="gp") +
             s(woy,bs="cc") +
             s(woy,NA_L2NAME,bs="fs") +
             s(MEI_avg,NA_L2NAME,bs="fs") + 
             s(NAO_avg,NA_L2NAME,bs="fs") +
             s(PDO_avg,NA_L2NAME,bs="fs") +
             s(NA_L2NAME,bs="re") + #s(location,bs="re") +
             te(Longitude,Latitude,bs=c('gp','gp')), 
           data=loc,family = tw(),
           discrete = TRUE, nthreads = 23)

summary(m13)
#draw(m13)
#appraise(m13)

AIC(m20,m19,m18,m17,m16,m15,m14)
BIC(m20,m19,m18,m17,m16,m15,m14)

summary(m10)
summary(m11)
summary(m12)
summary(m13)
draw(m19)
#####

ggplot(loc_year,aes(x=MEI_avg_Winter,y=CEW_mean,color=overwinter_zone)) + geom_smooth() +
ggplot(loc_year,aes(x=MEI_avg_Summer,y=CEW_mean,color=overwinter_zone)) + geom_smooth() +
ggplot(loc_year,aes(x=MEI_avg_Spring,y=CEW_mean,color=overwinter_zone)) + geom_smooth() +
ggplot(loc_year,aes(x=MEI_avg_Fall,y=CEW_mean,color=overwinter_zone)) + geom_smooth()


ggplot(loc_year,aes(x=MEI_avg,y=scaled_mean)) + geom_smooth() +
  ggplot(loc_year,aes(x=MEI_avg1,y=scaled_mean)) + geom_smooth() +
  ggplot(loc_year,aes(x=MEI_avg2,y=scaled_mean)) + geom_smooth() 
  



loc_year_MEI_phase$Phase
m <- bam(scaled_mean  ~ 
           te(MEI_avg,k=20) +
           te(MEI_avg1,k=20) +
           te(MEI_avg2,k=20),
         data=loc_year,family = scat(),select=TRUE,
         discrete = TRUE, nthreads = 23)


m1 <- bam(CEW_mean  ~ 
           #s(year,bs='gp') + 
           #te(longitude,latitude,bs=c('gp','gp')) +
           s(NAO_avg_Winter) +
           #s(NAO_avg_Summer) +
           #s(NAO_avg_Spring) +
           s(NAO_avg_Winter,overwinter_zone,bs="fs") + 
           #s(NAO_avg_Summer,overwinter_zone,bs="fs") +
           #s(NAO_avg_Spring,overwinter_zone,bs="fs") +
           s(overwinter_zone,bs="re"), 
         data=loc_year,family = tw(),
         discrete = TRUE, nthreads = 23,select=TRUE)

m2 <- bam(CEW_mean  ~ 
            s(year,bs='gp') + 
            te(longitude,latitude,bs=c('gp','gp')) +
            (MEI_avg_Winter) +
            (MEI_avg_Winter)*(overwinter_zone) + 
            s(overwinter_zone,bs="re"), 
          data=loc_year,family = tw(),
          discrete = TRUE, nthreads = 23,select=TRUE)


summary(m)
summary(m1)
summary(m2)
plot(m,all.terms=T)
draw(m)
appraise(m)
AIC(m,m1,m2)



loc_year$pred <- predict(m2,newdata=loc_year)

ggplot(loc_year,aes(x=PDO_avg_Winter,y=pred,color=overwinter_zone)) + geom_smooth()






m <- bam(CEW_sum ~
           s(woy,bs="cc",k=10) +
           s(year,bs="gp",k=10) +
           s(MEI_avg,bs="tp",k=10) +
           s(MEI_avg,overwinter_zone, bs="fs",k=10) +
           te(Latitude,Longitude,bs=c('gp','gp'),k=10),
         family=tw(),discrete=TRUE,nthreads=23,data=loc
           )

m2 <- bam(scaled_mean ~
           s(woy,bs="cc",k=10) +
           s(year,bs="gp",k=10) +
           s(MEI_avg,bs="tp",k=10) +
           s(MEI_avg,overwinter_zone, bs="fs",k=10) +
           te(Latitude,Longitude,bs=c('gp','gp'),k=10),
         family=scat(),discrete=TRUE,nthreads=23,data=loc
)
summary(m)
summary(m2)
draw(m)
draw(m2)


m3 <- bam(scaled_mean  ~
           s(year,bs="gp",k=40) +
           s(MEI_avg,bs="tp",k=30) +
           s(MEI_avg,overwinter_zone, bs="fs",k=30) +
           te(longitude,latitude,bs=c('gp','gp'),k=30),
         family=scat(),discrete=TRUE,nthreads=23,data=loc_year, select=TRUE
)

m4 <- bam(scaled_mean  ~
            s(year,bs="gp",k=40) +
            s(MEI_avg,bs="tp",k=30) +
            #s(MEI_avg,overwinter_zone, bs="fs",k=30) +
            te(longitude,latitude,bs=c('gp','gp'),k=30),
          family=scat(),discrete=TRUE,nthreads=23,data=loc_year, select=TRUE
)

m5 <- bam(scaled_mean  ~
            s(year,bs="gp",k=40) +
            #s(MEI_avg,bs="tp",k=30) +
            s(MEI_avg,overwinter_zone, bs="fs",k=30) +
            te(longitude,latitude,bs=c('gp','gp'),k=30),
          family=scat(),discrete=TRUE,nthreads=23,data=loc_year, select=TRUE
)

m5.5 <- bam(scaled_mean  ~
            s(year,bs="gp",k=40) +
            #s(MEI_avg,bs="tp",k=30) +
            s(MEI_avg,by=overwinter_zone, bs="tp",k=30) +
            te(longitude,latitude,bs=c('gp','gp'),k=30),
          family=scat(),discrete=TRUE,nthreads=23,data=loc_year, select=TRUE
)

m6 <- bam(scaled_mean  ~
            s(year,bs="gp",k=40) +
            (MEI_avg) +
            #s(MEI_avg,overwinter_zone, bs="fs",k=30) +
            te(longitude,latitude,bs=c('gp','gp'),k=30),
          family=scat(),discrete=TRUE,nthreads=23,data=loc_year, select=TRUE
)

summary(m5.5)
appraise(m3)
k.check(m3)

summary(m4)
draw(m3)
draw(m4)
draw(m5.5)

AIC(m3,m4,m5,m6,m5.5)


mod<-readRDS("m5.5.rdata")
summary(mod)
draw(mod)
