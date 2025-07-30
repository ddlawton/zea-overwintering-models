########################################################
########################################################
########       Multiyear modeling              ##########
########           Dec 30 2021                 #########
########         Douglas Lawton               ##########
########################################################
########################################################
rm(list=ls())

library(tidyverse)
library(mgcv)
library(gratia)
library(data.table)
library(patchwork)
library(ggpubr)
library(viridis)
library(ROCR)
library(caret)

dat <- as_tibble(fread("models/Threshold_5c/Multiyear/data_with_preds_resids_Dec302021.csv"))

dat$CEW_sum <- round(dat$CEW_sum)
dat2 <- dat %>% filter(year >= 1990)

NL_data <- dat2 %>% filter(zone_30y == 0) %>% filter(woy >= 10)

dat2 <- dat2 %>% filter(zone_30y != 0) %>% rbind(NL_data) %>%
  mutate(
    location = factor(location),
    zone_30y = factor(zone_30y)
  )

std <- function(x) sd(x)/sqrt(length(x))



dat3 <- dat2 %>% drop_na(CEW_sum,woy,year,Longitude,Latitude,location,zone_30y)

dat_levels <- dat3 %>% group_by(location) %>% tally() %>% filter(n>10) %>% droplevels()

flevels <- unique(levels(dat_levels$location))

dat4 <- dat3 %>% filter(location %in% flevels)


mydf <- 
  dat3 %>%
  mutate(all = paste(zone_30y,location)) %>%
  group_by(all) %>%
  summarise(total=n()) %>%
  filter(total>=2) %>% droplevels()

flevels <- unique(levels(factor(mydf$all)))

dat4 <- dat3[paste(dat3$zone_30y,dat3$location) %in% flevels,] %>% droplevels() %>%
  mutate(multiyear_zones = factor(multiyear_zones))


GS <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_GS.rds")
S <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_S.rds")
GI <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_GI.rds")
I <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_I.rds")
G <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_G.rds")
N <- readRDS("models/Threshold_5c/Multiyear/mod_spatiotemp_N.rds")


AIC_table <- AIC(GS,S,GI,I,G,N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,AIC, deltaAIC), 
            .funs = funs(round,.args = list(digits=0))) %>% arrange(deltaAIC)

BIC_table <- BIC(GS,S,GI,I,G,N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaBIC = BIC - min(BIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,BIC, deltaBIC), 
            .funs = funs(round,.args = list(digits=0))) %>% arrange(deltaBIC)

GS$xlevels[['multiyear_zone']]
lm2$xlevels[["state"]] = union(lm2$xlevels[["state"]] , levels(LM_full_test$state))


dat4 <- dat4 %>% drop_na(woy, multiyear_zones,Longitude, Latitude,location,year) %>% mutate(GS_pred = predict(GS,newdata=dat4,type="response"))


year_global_GI <- ggplot(dat4,aes(x=year,y=(GS_pred))) + geom_smooth() + ylab("CEW count") + 
  xlab("") + coord_cartesian(ylim=c(0,300)) + theme_pubr() +
  theme(legend.title = element_blank())

woy_global_GI <- ggplot(dat4,aes(x=woy,y=(GS_pred))) + geom_smooth() + ylab("") + 
  xlab("")+ coord_cartesian(ylim=c(0,300)) + theme_pubr() +
  theme(legend.title = element_blank())

year_zone_GI <- ggplot(dat4,aes(x=year,y=(GS_pred), color=multiyear_zones))  + geom_point(aes(y = 0.1), alpha = 0) +
  geom_smooth(linetype="dashed",se=TRUE) + 
  ylab("Model Predictions") + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), 
                     labels = c("NL", "TZ", "SR")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())

woy_zone_GI <- ggplot(dat4,aes(x=woy,y=(GS_pred), color=multiyear_zones)) + geom_smooth(linetype="dashed",se=TRUE) + ylab("") +
  xlab("Week of year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), labels = c("NL", "TZ", "SR"))  +
  geom_smooth(data=dat4, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("")+
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())


year_zone_GI <- ggplot(dat4,aes(x=year,y=(GS_pred), color=multiyear_zones)) + geom_smooth(linetype="dashed",se=TRUE) + 
  ylab(expression(paste("Modeled weekly"," ",italic("Helicoverpa zea")," ", "count"))) + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), 
                     labels = c("NL", "TZ", "SR")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())


pw1_GI <- (year_zone_GI + woy_zone_GI)  + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom") 

