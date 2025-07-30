###
# Modeling 
#  Zea with
#   Change in zone
#
rm(list=ls())
library(tidyverse)
library(mgcv)
library(data.table)


dat <- as_tibble(fread("data/raw/National_data_combined/National_dat_Sept29.csv"))
zone <- as_tibble(fread("data/processed/overwintering/area_change_data.csv"))



dat$CEW_sum <- round(dat$CEW_sum)

dat2 <- dat %>% filter(year >= 1990)

NL_data <- dat2 %>% filter(zone_30y == 0) %>% filter(woy >= 10)

dat2 <- dat2 %>% filter(zone_30y != 0) %>% rbind(NL_data) %>%
  mutate(
    location = factor(location),
    zone_30y = factor(zone_30y)
  )


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



zone_scaled <- zone %>% select(year,zone,scaled) %>%
  pivot_wider(names_from = "zone", values_from = scaled)

zone_percent <- zone %>% select(year,zone,area_percent ) %>%
  pivot_wider(names_from = "zone", values_from = area_percent )

dat_summarized <- dat4 %>% left_join(zone_scaled,by="year") %>%
  filter(SR > -1.5) #%>%
  #pivot_longer(cols=c(4:6),names_to="zone",values_to = "scaled") 

ggplot(zone,aes(x=year,y=scaled,color=zone)) + geom_smooth(se=FALSE) + ylim(-2,2)

ggplot(dat_summarized,aes(x=SR,y=CEW_sum)) + geom_smooth(se=FALSE)  

mod <- bam(CEW_sum ~ s(SR,bs="tp") + s(year,bs="tp"),data=dat_summarized,family=tw(),
           discrete = TRUE,nthreads=15, select=TRUE)


mod <-bam(CEW_sum ~
        s(SR,bs="tp",k=15) +
        s(woy,bs="cc",k=15) +
        s(year,bs="cc",k=15) +
        te(woy,zone_30y ,bs=c("cc","re"),k=15) +
        te(year,zone_30y ,bs=c("gp","re"),k=15) +
        te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
        s(location,bs="re") + s(zone_30y ,bs="re"), 
    family=tw(), discrete = TRUE, nthreads=23,
    data=dat_summarized) 



mod_lm <- bam(CEW_sum ~ s(SR) + s(year,bs="tp"),data=dat_summarized,family=tw(),
           discrete = TRUE,nthreads=15, select=TRUE)

library(Ternary)

coordinates <-  dat4 %>% group_by(year) %>% summarise(CEW = sum(CEW_sum)) %>% left_join(zone_scaled,by="year") %>%
  dplyr::select("NL","TZ","SR",CEW) %>% filter(SR > -1)


ggplot(coordinates,aes(x=SR,y=(CEW+1))) + geom_smooth() + geom_point()

mod <- gam(CEW ~ s(SR,k=10) + s(TZ,k=10) + s(NL,k=10),family=tw(),data=coordinates,select=TRUE)
mod_lm <- gam(CEW ~ (SR) + (TZ) + (NL),family=tw(),data=coordinates,select=TRUE)
plot(mod_lm, all.terms = TRUE)
gratia::draw(mod)
summary(mod_lm)
k.check(mod)



coordinates <-  zone_percent %>%
  dplyr::select("NL","TZ","SR")

par(mar = rep(0.2, 4))
TernaryPlot(axis.labels = seq(0.5, .6, by = 0.1))

nPoints <- 4000L
coordinates <- cbind(abs(rnorm(nPoints, 2, 3)),
                     abs(rnorm(nPoints, 1, 1.5)),
                     abs(rnorm(nPoints, 1, 0.5)))

ColourTernary(TernaryDensity(coordinates, resolution = 10L))
TernaryPoints(coordinates, col = 'red', pch = '.')
TernaryDensityContour(coordinates, resolution = 30L)

summary(coordinates)
