########
# Overwinter Data
#    Zea
########
rm(list=ls())

library(tidyverse)
library(data.table)
library(gganimate)
library(transformr)
library(ggforce)

dat <- as_tibble(fread("data/processed/Zea_overwintering_zone.csv"))

names <- substr(colnames(dat[,2:42]), start=1, stop=2)
names <- sub("_","",names)
names <- as.character((as.numeric(names)+1981))

overwinter <- dat %>% dplyr::select(2:42)
colnames(overwinter) <- names

names(dat2)

dat2 <- dat %>% dplyr::select(43:58) %>% cbind(overwinter) %>% mutate(
 Date = parse_date_time(Date, order = 'mdy')) %>%
  pivot_longer(cols=17:57, values_to = "zone", names_to = "year")

dat3 <- dat2[dat2$Year==dat2$year, ]

IDs <- as.character(dat$ID)

summary(as.factor(dat4$min))


dat4 <- dat3 %>% filter(as.character(ID) %in% IDs) %>%
  filter(zone <= 2) %>% drop_na(zone) %>%
  filter(min <= 2) %>% drop_na(min) %>%
  mutate( zone = as.factor(zone),
          min = as.factor(min),
    zone = fct_recode(zone, "< 5c" = "0", "5 - 10 c" = "1", "> 10 c" = "2"),
    min = fct_recode(min, "< 5c" = "0", "5 - 10 c" = "1", "> 10 c" = "2"))


DOY_zone <- ggplot((dat4 %>% filter(between(year,2000,2020))),aes(x=DOY,y=CEW,fill=min,color=min)) +
  geom_smooth(method = "gam") +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  + 
  theme_pubr()+
  #scale_color_viridis(discrete = T) +
  #scale_fill_viridis(discrete = T) +
  #geom_rug(sides = "b") +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
  #geom_vline(xintercept=60,linetype = "dashed") +
  geom_vline(xintercept=152,linetype = "dashed") +
  geom_vline(xintercept=244,linetype = "dashed") +
  #geom_hline(yintercept=32) +
  #geom_vline(xintercept=221) +
  #geom_vline(xintercept=335,linetype = "dashed") +
  annotate("text", x = 125, y = 90, label = "Spring") +
  annotate("text", x = 200, y = 90, label = "Summer") +
  annotate("text", x = 275, y = 90, label = "Fall") +
  coord_cartesian(ylim=c(0,NA)) 
#facet_wrap(~NA_L2NAME)
#xlim(60,335) + ylim(0,NA) 

Year <- ggplot((dat4 %>% filter(between(year,2000,2020))),aes(x=DOY,y=CEW,fill=min,color=min)) +
  geom_smooth(method = "gam") +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  + 
  theme_pubr()+
  #scale_color_viridis(discrete = T) +
  #scale_fill_viridis(discrete = T) +
  #geom_rug(sides = "b") +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
  #geom_vline(xintercept=60,linetype = "dashed") +
  geom_vline(xintercept=152,linetype = "dashed") +
  geom_vline(xintercept=244,linetype = "dashed") +
  #geom_hline(yintercept=32) +
  #geom_vline(xintercept=221) +
  #geom_vline(xintercept=335,linetype = "dashed") +
  facet_wrap(~year, scales = "free") +
  #annotate("text", x = 125, y = 90, label = "Spring") +
  #annotate("text", x = 200, y = 90, label = "Summer") +
  #annotate("text", x = 275, y = 90, label = "Fall") +
  coord_cartesian(ylim=c(0,NA)) 






pdf("figures/DOW_CEW_Year.pdf")
for(i in 1:10){
print(  ggplot((dat4),aes(x=DOY,y=CEW,fill=zone,color=zone)) +
    geom_smooth(method = "gam") +
    theme_pubr()+
    scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
    scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
    geom_vline(xintercept=152,linetype = "dashed") +
    geom_vline(xintercept=244,linetype = "dashed") +
    facet_wrap_paginate(~year, scales = "free", ncol = 2, nrow = 2, page = i) +
    coord_cartesian(ylim=c(0,NA))
  )
}
dev.off()

random_round <- function(x, seed = 123, tol = 1.e-6) { 
  set.seed(seed) 
  round(jitter(x, amount = tol))
}
?rnorm
random_round(rnorm(1000))

### modeling this relationship
dat4 <- dat4 %>%  #rounding to get whole numbers for poisson
  
  mutate(CEW = ceiling(CEW))


mod <- bam()

