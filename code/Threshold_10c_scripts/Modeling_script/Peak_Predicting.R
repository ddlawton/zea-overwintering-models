rm(list=ls())

# libraries

library(tidyverse)
library(mgcv)
library(gratia)
library(patchwork)
library(ggpubr)
library(viridis)
library(data.table)

# data loading
#####
dat <- as_tibble(fread("data/raw/National_data_combined/National_dat_Oct13.csv"))

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

dat4 <- dat3[paste(dat3$zone_30y,dat3$location) %in% flevels,] %>% droplevels()

GS <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GS.rds")
S <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_S.rds")
GI <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GI.rds")
I <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_I.rds")
G <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_G.rds")
N <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_N.rds")
Inter <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_0.rds")

dat4 <- dat4 %>% mutate(GS_pred = predict(GS,type="response"),
                        GI_pred = predict(GI,type="response")) %>%
  mutate(zone_30y = fct_recode(zone_30y, "NL" = "0", "TZ" = "1", "SR" = "2"))


#####


ggplot(dat4,aes(x=woy,y=GS_pred,color=zone_30y)) + geom_smooth() +
  coord_cartesian(xlim=c(30,40)) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) + 
  geom_vline(xintercept = 32.1,color="#7570b3")+
  geom_vline(xintercept = 33.75,color="#d95f02")+
  geom_vline(xintercept = 35.5,color="#1b9e77")
dat4$zone_30y


summary(SR)

NL <- dat4 %>% filter(zone_30y == "NL") %>% filter(between(woy,36,36)) %>%
  group_by(year) %>% summarize(GI_pred = sum(GI_pred ),NL_sum = sum(CEW_sum ))

TZ <- dat4 %>% filter(zone_30y == "TZ") %>% filter(between(woy,34,34)) %>%
  group_by(year)  %>% summarize(GI_pred = sum(GI_pred ),TZ_sum = sum(CEW_sum ))

SR <- dat4 %>% filter(zone_30y == "SR") %>% filter(between(woy,32,32)) %>%
  group_by(year)  %>% summarize(GI_pred = sum(GI_pred ),SR_sum = sum(CEW_sum ))

peaks <- NL %>% left_join(TZ, by="year") %>% left_join(SR, by="year") %>%
  mutate(total_pop = TZ_sum + SR_sum) %>% drop_na()


OW_points <- ggplot(dat4,aes(x=woy,y=GS_pred,color=zone_30y)) + geom_smooth() +
  coord_cartesian(xlim=c(30,40)) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) + 
  geom_vline(xintercept = 32,color="#7570b3",linetype=2)+
  geom_vline(xintercept = 34,color="#d95f02",linetype=2)+
  geom_vline(xintercept = 36,color="#1b9e77",linetype=2) + ggpubr::theme_pubr() +
  scale_x_continuous(limits = c(30,40),breaks=c(30,32,34,36,38,40)) +
  ylab(expression(paste("Modeled weekly"," ",italic("Helicoverpa zea")," ", "count"))) + xlab("Week of year") + theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))


NL_TZ_mod <- gam(NL_sum ~ TZ_sum,data=peaks,family=tw())
NL_SR_mod <- gam(NL_sum ~ SR_sum,data=peaks,family=tw())
TZ_SR_mod <- gam(TZ_sum ~ SR_sum,data=peaks,family=tw())

NL_SR <- cor.test(peaks$NL_sum,peaks$SR_sum,method="pearson")
NL_TZ <- cor.test(peaks$NL_sum,peaks$TZ_sum,method="pearson")
TZ_SR <- cor.test(peaks$TZ_sum,peaks$SR_sum,method="pearson")

lb1 <- paste0("Rho == ",round(NL_SR$estimate,digits=2))
lb1.5 <- paste0("p-value = ",round(NL_SR$p.value,digits=2))
SR_NL <- ggplot(peaks,aes(x=SR_sum,y=NL_sum)) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr() +
  xlab("SR yearly peak count")  + ylab("NL yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,45000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 25000, x= 10000, label=lb1, parse = TRUE) +
  annotate("text", y = 23900, x= 10000, label=lb1.5)

lb2 <- paste0("Rho == ",round(NL_TZ$estimate,digits=2))
lb2.5 <- paste0("p-value = ",round(NL_TZ$p.value,digits=4))
TZ_NL <- ggplot(peaks,aes(x=TZ_sum,y=NL_sum)) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr()  +
  xlab("TZ yearly peak count")  + ylab("NL yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,21000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 25000, x= 5000, label=lb2, parse = TRUE)+
  annotate("text", y = 23900, x= 5000, label="p-value = < 0.01")


lb3 <- paste0("Rho == ",round(TZ_SR$estimate,digits=2))
lb3.5 <- paste0("p-value = ",round(TZ_SR$p.value,digits=2))
SR_TZ <- ggplot(peaks,aes(x=SR_sum,y=TZ_sum)) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr()  +
  xlab("SR yearly peak count")  + ylab("TZ yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,45000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 25000, x= 10000, label=lb3, parse = TRUE)+
  annotate("text", y = 23900, x= 10000, label="p-value = < 0.01")

week_predictions <- OW_points / (SR_TZ + SR_NL + TZ_NL) + plot_annotation(tag_levels = 'A')

ggsave(week_predictions,file="manuscript/figures/week_predictions.png",dpi=300,width = 10.5,height=10,units="in")



# USING THE piecewiseSEM PACKAGE...
library(nlme)
library(lme4)
library(piecewiseSEM)
library(sem)

# first, use the toy model

OW.modlist<-list(
  lm(NL_sum~TZ_sum + SR_sum,data=peaks,na.action=na.omit),
  lm(TZ_sum~SR_sum,data=peaks,na.action=na.omit))

piecewiseSEM::sem.fit(modelList=OW.modlist,data=peaks,conditional=T)
sem.coefs(modelList=OW.modlist,data=peaks)
sem.model.fits(modelList=OW.modlist)



model <- psem(glm(NL_sum ~ TZ_sum,data=peaks), 
              glm(TZ_sum ~ SR_sum,data=peaks))
summary(model)
coefs(model, standardize = "scale")
