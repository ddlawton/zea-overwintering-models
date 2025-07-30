
rm(list=ls())

library(tidyverse)
library(ggpubr)
library(patchwork)
library(mgcv)
library(gratia)
library(data.table)
library(lubridate)
library(rsoi)

MEI <- download_mei(use_cache = FALSE, file = NULL) %>%
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

PDO <- fread("https://www.ncdc.noaa.gov/teleconnections/pdo/data.txt")

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


SSTs <- MEI %>% left_join(NAO,by=c("Year","Season")) %>% left_join(PDO2,by=c("Year","Season")) %>%
  pivot_wider(names_from = Season,values_from = c(MEI_avg,NAO_avg,PDO_avg))


SSTs_year <- MEI %>% left_join(NAO,by=c("Year","Season")) %>% left_join(PDO2,by=c("Year","Season")) %>%
  group_by(Year) %>% summarize(MEI_avg = mean(MEI_avg),NAO_avg = mean(NAO_avg), PDO_avg = mean(PDO_avg))

ggplot(SSTs,aes(x=fDate,y=value,color=as.factor(SST))) + geom_point() + geom_line()


ggplot(SSTs,aes(x=MEI_avg,y=NAO_avg)) + geom_point() + geom_smooth() +
  ggplot(SSTs,aes(x=MEI_avg,y=PDO_avg)) + geom_point() + geom_smooth() +
  ggplot(SSTs,aes(x=NAO_avg,y=PDO_avg)) + geom_point() + geom_smooth()


dat <- as_tibble(fread("data/processed/overwintering/Zea_overwintering_zone.csv"))  %>%
  filter(NA_L1NAME != "WATER")



#functions

audps <-
  function(evaluation, dates, type = "absolute") {
    if(!(is.matrix(evaluation) | is.data.frame(evaluation))){
      evaluation <- rbind(evaluation)
    }
    n<-length(dates)
    k<-ncol(evaluation)
    if (n!=k) {
      cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
      return()
    }
    d1<-(dates[2]-dates[1])/2
    d2<-(dates[n]-dates[n-1])/2
    d<-d1+d2+dates[n]-dates[1]
    audps<-0
    for (i in 1:(n-1)) {
      audps <- audps + evaluation[,i]*(dates[i+1]-dates[i])
    }
    audps <- audps + evaluation[,n]*(dates[n]-dates[n-1])
    if (type =="relative" ) audps <- audps/(d*100)
    if (type =="absolute" | type =="relative" ) {
      return(audps)
    }
    else cat("Error: type is 'absolute' or 'relative'\n\n")
  }

audpc <-
  function(evaluation, dates, type = "absolute") {
    if(!(is.matrix(evaluation) | is.data.frame(evaluation))){
      evaluation<-rbind(evaluation)
    }
    n<-length(dates)
    k<-ncol(evaluation)
    if (n!=k) {
      cat("Error:\nThe number of dates of evaluation \nmust agree with the number of evaluations\n")
      return()
    }
    audpc<-0
    area.total<- 100*(dates[n]-dates[1])
    for (i in 1:(n-1)) {
      audpc<- audpc + (evaluation[,i]+evaluation[,i+1])*(dates[i+1]-dates[i])/2
    }
    if (type =="relative" ) audpc <-audpc/area.total
    if (type =="absolute" | type =="relative" ) {
      return(audpc)
    }
    else cat("Error: type is 'absolute' or 'relative'\n\n")
  }



loc_year <- dat %>% group_by(location,year) %>%
  summarize(audps = (audps(CEW_sum,woy,type="absolute")),
            CEW_mean = mean(CEW_sum),
            CEW_max = max(CEW_sum),
            n_woy = n(),
            overwinter_zone = first(min),
            latitude = first(Latitude),
            longitude = first(Longitude),
            eco = first(NA_L2NAME),
            woy = first(woy),
            year= first(year)) %>%
  ungroup() %>% group_by(location) %>%
  mutate(overall_average = mean(audps),
         scaled_audps = scale(audps)[,1],
         scaled_mean = scale(CEW_mean)[,1]) %>%
  drop_na(scaled_audps) %>% drop_na(overwinter_zone) %>%
  filter(overwinter_zone < 4) %>%
  mutate(overwinter_zone = as.factor(overwinter_zone),
         overwinter_zone = fct_recode(overwinter_zone, "Northern_limits" = "0", "Transitional_zone" = "1", "Southern Range" = "2"))

summary(loc_year$overwinter_zone)

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
  dplyr::select(!ends_with("_level_2")) %>% drop_na(scaled_mean)


summary(loc)


ggplot(loc_year,aes(x=year,y=scaled_audps)) + geom_smooth() +
  ggplot(loc,aes(x=year,y=scaled_mean)) + geom_smooth()


good_years <- unique((loc_year %>% group_by(location) %>% summarize(n = n()) %>% filter(n >= 10))$location)

viz_data <- loc_year %>% filter(location %in% good_years)


P1_auc <- ggplot(viz_data,aes(x=year,y=scaled_audps)) +
  #geom_point(color="grey",alpha=.5) +
  theme_pubr() +
  geom_smooth(se=TRUE) +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -1,color="red",linetype=2)+
  geom_hline(yintercept = 1,color="red",linetype=2) +
  #coord_cartesian(ylim=c(-4,4)) + #geom_rug() +
  xlab("Year") + ylab("Scaled AUDPS") +
  ggtitle("Scaled Area Under Disease Progress Stairs")

P1_mean <- ggplot(viz_data,aes(x=year,y=scaled_mean)) +
  #geom_point(color="grey",alpha=.5) +
  theme_pubr() +
  geom_smooth(se=TRUE) +
  geom_hline(yintercept = 0)+
  geom_hline(yintercept = -1,color="red",linetype=2)+
  geom_hline(yintercept = 1,color="red",linetype=2) +
  #coord_cartesian(ylim=c(-4,4)) + #geom_rug() +
  xlab("Year") + ylab("Scaled CEW mean") +
  ggtitle("Scaled Yearly CEW mean")

correlation <- ggplot(loc_year, aes(x=scaled_mean,y=scaled_audps)) +
  geom_point() + geom_smooth() + theme_pubr() +
  ggtitle("Correlation between AUDPS and mean")



mod <- bam(scaled_audps ~ s(year, bs="gp",m=3,k=20),data=viz_data,family=scat(),select=TRUE)
mod_mean <- bam(scaled_mean ~ s(year, bs="gp",m=3,k=20),data=viz_data,family=scat(),select=TRUE)

viz_data$pred_mod_audps <- predict(mod,data=viz_data,type="response")
viz_data$pred_mod_mean <- predict(mod_mean,newdata = viz_data,type="response")

loc_year_mod <- viz_data %>% pivot_longer(cols=c("pred_mod_audps","pred_mod_mean"),names_to="model",values_to="response")

auc <- ggplot(loc_year_mod,aes(x=year,y=response,color=model)) + geom_smooth() + theme_pubr()+
  scale_color_manual(values=c("#1b9e77","#d95f02")) +
  ggtitle("Predictive model differences") +
  ylab("Model predictions")


(P1_auc + P1_mean) / (correlation + auc)


#Now lets add basic SSTs to the mix

loc_year_yearly <- loc_year %>% left_join(SSTs_year,by= c("year"="Year"))

loc_year2 <- loc_year %>% left_join((SSTs),by= c("year"="Year")) %>%
  drop_na(MEI_avg_Fall)

loc2 <- loc  %>% left_join((SSTs),by= c("year"="Year")) %>%
  drop_na(MEI_avg_Fall)

#basic viz plots

NAO_plot <- ggplot((loc_year2 %>% drop_na(NAO_avg)),aes(x=NAO_avg,y=scaled_mean))  + geom_smooth(method="lm") + facet_grid(Season~overwinter_zone)

SOI_plot <- ggplot(loc_year2,aes(x=MEI_avg,y=scaled_mean)) + geom_point() + geom_smooth()

SOIxNAO_plot <-  ggplot(loc_year2,aes(x=NAO_avg,y=x=MEI,z=scaled_mean)) + stat_summary_2d() + viridis::scale_fill_viridis()


Hex_graph <- ggplot(loc_year2,aes(x=year,y=SOI_avg,z= scaled_mean)) +
  theme_pubr() +
  stat_summary_hex(bins=10) +
  viridis::scale_fill_viridis()

# lets see how they curves overlap

overlapping_curves <- loc_year2 %>% pivot_longer(cols=c(ends_with("_avg"),"scaled_mean"),names_to = "variable", values_to = "values")



NAO_lines <- ggplot((overlapping_curves %>% filter(variable == "NAO_avg")),aes(x=as.integer(as.character(year)),y=values,color=variable)) + geom_line() + geom_point() +
  geom_hline(yintercept = 0) + theme_pubr() + xlim(1980,2021)  +
  geom_smooth(data = (overlapping_curves %>% filter(variable == "scaled_mean")),se=FALSE) + coord_cartesian(ylim=c(-1.5,1.5)) +
  scale_color_manual(values=c("#1b9e77","#d95f02")) + xlab("Year")

SOI_lines <- ggplot((overlapping_curves %>% filter(variable == "SOI_avg")),aes(x=as.integer(as.character(year)),y=values,color=variable)) + geom_line() + geom_point() +
  geom_hline(yintercept = 0) + theme_pubr() + xlim(1980,2021)  +
  geom_smooth(data = (overlapping_curves %>% filter(variable == "scaled_mean")),se=FALSE) + coord_cartesian(ylim=c(-1.5,1.5)) +
  scale_color_manual(values=c("#d95f02","#7570b3")) + xlab("Year")



NAO_lines_smoothed <- ggplot((overlapping_curves %>% filter(variable == "NAO_avg")),aes(x=as.integer(as.character(year)),y=values,color=variable)) + geom_point() + geom_line() +
  geom_hline(yintercept = 0) + theme_pubr() + xlim(1980,2021)  +
  geom_smooth(data = (overlapping_curves %>% filter(variable == "scaled_mean")),se=FALSE) + coord_cartesian(ylim=c(-1.5,1.5)) +
  #facet_wrap(~overwinter_zone) +
  #scale_color_manual(values=c("#1b9e77","#d95f02")) + xlab("Year") +
  annotate(geom="text", x=2000, y=1.4, label="Positive NOA: \nfewer cold-air outbreaks and decreased storminess", color="red")


SOI_lines_smoothed <- ggplot((overlapping_curves %>% filter(variable == "SOI_avg")),aes(x=as.integer(as.character(year)),y=values,color=variable)) + geom_point() + geom_smooth() +
  geom_hline(yintercept = 0) + theme_pubr() + xlim(1980,2021)  +
  geom_smooth(data = (overlapping_curves %>% filter(variable == "scaled_mean")),se=FALSE) + coord_cartesian(ylim=c(-1.5,1.5)) +
  scale_color_manual(values=c("#d95f02","#7570b3")) + xlab("Year") +
  facet_wrap(~overwinter_zone) +
  annotate(geom="text", x=2000, y=1.4, label="Positive SOI (la Nina): \nhotter and drier weather in Southern USA", color="red")

NAO_SOI_correlation <- NAO %>% left_join(SOI, by="Year") %>% drop_na()

coor_plot <- ggplot(NAO_SOI_correlation,aes(x=NAO_avg,y=SOI_avg)) + geom_point() + geom_smooth()
cor(NAO_SOI_correlation$NAO_avg,NAO_SOI_correlation$SOI_avg)

NAO_lines_smoothed / SOI_lines_smoothed


NAO <- ggplot(loc_year2,aes(x=NAO_avg,y=scaled_mean,color=overwinter_zone)) + geom_smooth() +
  geom_hline(yintercept = 0) + theme_pubr() + #xlim(1980,2021)  +
  coord_cartesian(ylim=c(-1.5,1.5)) +
  scale_color_manual(values=c("#feb24c","#fc4e2a","#bd0026")) +
  annotate(geom="text", x=0, y=1.4, label="Positive NOA: \nfewer cold-air outbreaks and decreased storminess", color="red")

SOI <- ggplot(loc_year2,aes(x=SOI_avg,y=scaled_mean,color=overwinter_zone)) + geom_smooth() +
  geom_hline(yintercept = 0) + theme_pubr() + #xlim(1980,2021)  +
  coord_cartesian(ylim=c(-1.5,1.5)) +
  scale_color_manual(values=c("#feb24c","#fc4e2a","#bd0026")) +
  annotate(geom="text", x=0, y=1.4, label="Positive NOA: \nfewer cold-air outbreaks and decreased storminess", color="red")

loc_year2$location <- as.factor(loc_year2$location)

slevels <- c("Winter","Spring","Summer","Fall")
loc_year2$Season <- factor(loc_year2$Season,levels=slevels)

loc_year3 <- loc_year2 %>% filter(between(NAO_avg,-.9,.9))


qqnorm((loc_year2$scaled_mean))
qqnorm(log(loc_year2$scaled_mean+10))


loc_year2_yearly <- loc_year2_yearly %>% drop_na(MEI_avg)
loc_year2_yearly$eco <- factor(loc_year2_yearly$eco)
loc_year2_yearly$location <- factor(loc_year2_yearly$location)

library(mgcv)
loc_year2 <- loc_year2 %>% drop_na(scaled_mean)
summary(loc_year2)


loc_year3 <- loc_year2 %>% filter(between(NAO_avg_Winter,-1.5,1.5))

mod <- bam(CEW_sum ~
             #s(NAO_avg_Fall,bs="ad") +
             (MEI_avg_Winter)  +
             s(year) +
             (MEI_avg_Spring)  +
             (MEI_avg_Summer)  +
             #te(MEI_avg_Winter,overwinter_zone,bs='fs') +
             #te(MEI_avg_Spring,overwinter_zone,bs='fs') +
             #te(MEI_avg_Summer,overwinter_zone,bs='fs') +
             s(woy) +
             te(woy,overwinter_zone,bs="fs") +
             te(Longitude,Latitude) +
             s(NA_L2NAME,bs="re") +
             s(location,bs='re'), data = loc2,select=TRUE, family=tw(),
           discrete=TRUE,nthreads=23)

summary(mod)
plot(mod,all.terms = T)
appraise(mod)


loc2$pred <- predict(mod,newdata=loc2,type="response")



m1 <- gam(list(CEW_max ~ s(year) + s(MEI_avg_Winter),
               ~ s(year) + s(MEI_avg_Winter),
               ~ 1),
          data = loc2, method = "fREML",
          family = gevlss(link = list("identity", "identity", "identity")),
          discrete=TRUE,nthreads=23)



           
ggplot((loc2 %>% drop_na(pred)),aes(x=MEI_avg_Summer,y=pred))  + geom_smooth(method="lm") +
 geom_vline(xintercept=0) + geom_vline(xintercept=-.5,linetype=2,color="red") + geom_vline(xintercept=.5,linetype=2,color="red")

ggplot((loc2 %>% drop_na(pred)),aes(x=MEI_avg_Spring,y=pred))  + geom_smooth(method="lm") +
  geom_vline(xintercept=0) + geom_vline(xintercept=-.5,linetype=2,color="red") + geom_vline(xintercept=.5,linetype=2,color="red")

ggplot((loc2 %>% drop_na(pred)),aes(x=MEI_avg_Winter,y=CEW_sum)) + geom_smooth(method="lm") +
  geom_vline(xintercept=0) + geom_vline(xintercept=-.5,linetype=2,color="red") + geom_vline(xintercept=.5,linetype=2,color="red")

ggplot((loc2 %>% drop_na(pred)),aes(x=woy,y=pred)) + facet_wrap(~overwinter_zone,scale="free") + geom_smooth()




# lets just dredge it out
library(MuMIn)
loc_year_yearly

ggplot(loc_year_yearly,aes(y=scaled_mean,x=MEI_avg)) + geom_smooth(method="loess") + 
ggplot(loc_year_yearly,aes(y=scaled_mean,x=NAO_avg)) + geom_smooth(method="loess") +
ggplot(loc_year_yearly,aes(y=scaled_mean,x=PDO_avg)) + geom_smooth(method="loess") 


mod <- bam(scaled_mean ~ s(MEI_avg) + s(NAO_avg) + s(PDO_avg),data=loc_year_yearly,family=scat(),discrete=TRUE,nthreads=23)
summary(mod)
draw(mod)

