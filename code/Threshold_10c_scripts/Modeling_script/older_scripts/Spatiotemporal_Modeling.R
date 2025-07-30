#####
# Tracking Seasonal
#   Flow of population
#####
rm(list=ls())

library(tidyverse)
library(lubridate)
library(ggpubr)
library(rnaturalearth)
library(gganimate)
library(viridis)
library(patchwork)
library(mgcv)
library(gratia)
require(rgdal)
library(spatialEco)
library(raster)
library(sp)
library(anytime)

dat <- as_tibble(read.csv("data/raw/National_data_combined/National_dat_July15.csv"))

std <- function(x) sd(x)/sqrt(length(x))

### Looking at NC

NC <- dat %>% filter(State == "North Carolina")

summary(NC$Year)

model   <- bam(CEW ~ s(DOY, bs = "cs"), data = NC)
summary(model)
plot(model)
NC$prediction <- stats::predict(model,newdata = NC)

max(NC$prediction,na.rm=TRUE)

?geom_smooth

DOY <- ggplot((NC),aes(x=DOY,y=CEW,)) +
  geom_smooth(method = "gam",se = FALSE) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  + 
  theme_pubr()+
  #scale_color_viridis(discrete = T) +
  #scale_fill_viridis(discrete = T) +
  #geom_rug(sides = "b") +
  #scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
  #scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
  #geom_vline(xintercept=60,linetype = "dashed") +
  geom_vline(xintercept=152,linetype = "dashed") +
  geom_vline(xintercept=244,linetype = "dashed") +
  geom_hline(yintercept=32,color="red") +
  geom_vline(xintercept=221,color="red") +
  #geom_vline(xintercept=335,linetype = "dashed") +
  annotate("text", x = 125, y = 90, label = "Spring") +
  annotate("text", x = 200, y = 90, label = "Summer") +
  annotate("text", x = 275, y = 90, label = "Fall") 
  #coord_cartesian(ylim=c(0,100)) 
  #facet_wrap(~NA_L2NAME)
#xlim(60,335) + ylim(0,NA) 

DOY



DOY_YEAR <- ggplot((NC %>% filter(between(Year,1995,2019))),aes(x=DOY,y=CEW,color=as.factor(Year))) +
  geom_smooth(method = "gam",se=FALSE) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  + 
  theme_pubr()+
  #scale_color_viridis(discrete = T) +
  scale_color_viridis(discrete = T) +
  #geom_rug(sides = "b") +
  #scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
  #scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
  #geom_vline(xintercept=60,linetype = "dashed") +
  geom_vline(xintercept=152,linetype = "dashed") +
  geom_vline(xintercept=244,linetype = "dashed") +
  #geom_vline(xintercept=335,linetype = "dashed") +
  annotate("text", x = 125, y = 200, label = "Spring") +
  annotate("text", x = 200, y = 200, label = "Summer") +
  annotate("text", x = 275, y = 200, label = "Fall") +
  ylim(0,250) + theme(legend.title = element_blank()) 
  #coord_cartesian(ylim=c(0,250)) 
  #facet_wrap(~NA_L2NAME)
  #xlim(60,335) + ylim(0,NA) 

DOY_YEAR


Year_250 <- ggplot((dat ),aes(x=Year,y=CEW)) +
  geom_smooth(data = (dat), se=FALSE, formula = y ~ s(x, bs = "cs", k=10)) +
  #geom_smooth(data = (dat), formula = y ~ s(x, bs = "cs", k=5)) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  +
  #facet_wrap(~NA_L2NAME) +
  theme_pubr() +
  #geom_rug(sides = "b") +
  #geom_rug(sides='b') +
  coord_cartesian(ylim=c(0,250))

summary(NC$Year)

Year <- ggplot((NC),aes(x=Year,y=CEW)) +
  geom_smooth(data = (NC), se=FALSE, formula = y ~ s(x, bs = "cs", k=10)) +
  #geom_smooth(data = (dat), formula = y ~ s(x, bs = "cs", k=5)) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  +
  #facet_wrap(~NA_L2NAME) +
  theme_pubr() +
  #geom_rug(sides = "b") +
  #geom_rug(sides='b') +
  coord_cartesian(ylim=c(0,NA))

ggsave(DOY,file="DOY.pdf")
ggsave(DOY_YEAR,file="DOY_YEAR.pdf")
ggsave(Year,file="YEAR.pdf")
ggsave(Year_250,file="Year_250.pdf")



NC$Date <- parse_date_time(NC$Date, order= "mdy")
NC$week <- week(NC$Date)

?distinct

NC2 <- NC %>% dplyr::select(CEW, Latitude, Longitude) %>% distinct()

count_week_trap <- NC  %>% filter(Year < 2020) %>%
  group_by(as.factor(Year),as.factor(week)) %>%
  summarize(mean_count = (mean(CEW)/length(Location)), Location_numbers = length(Location))  %>%
  mutate(Year = as.numeric(as.character(`as.factor(Year)`))) %>% drop_na()

ggplot(count_week_trap,aes(x=Year,y=mean_count)) + geom_smooth()

ggplot(count_week_trap,aes(y=Location_numbers,x=as.factor(Year))) + geom_boxplot()

DOY_numbers <- dat %>% group_by(as.factor(DOY)) %>%
  summarize(mean = mean(CEW),std_error = std(CEW))




#Extraction EPA ecoregions II to dataset

ecoregions <- readOGR(dsn = "data/raw/GIS_data/na_cec_eco_l2",
                      layer = "NA_CEC_Eco_Level2")

dat_projected <- SpatialPointsDataFrame(coords=dat[6:5],data=dat,
                                        proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

ecoregions <- spTransform(ecoregions,projection(dat_projected))

projection(ecoregions) == projection(dat_projected)
st_crs(ecoregions) == st_crs(dat_projected) 

dat_projected2 <- point.in.poly(dat_projected,ecoregions)

dat <- as_tibble(dat_projected2@data) 

ecoregions_used <- ((dat %>% group_by(NA_L2NAME) %>% summarize(count = n())     %>% filter(count > 10) %>% 
                   drop_na()))
ecoregions_used <- unique(ecoregions_used$NA_L2NAME)

dat <- dat %>% filter(NA_L2NAME %in% ecoregions_used) 

dat$Date <- parse_date_time(dat$Date,orders="ymd")
dat$Year <- year(dat$Date)
dat$month <- month(dat$Date)
dat$DOY <- as.integer(yday(dat$Date))
dat$State <- as.factor(dat$State)
dat$latitude <- dat$Latitude
dat$longitude <- dat$Longitude
dat$unix <- as.numeric(anytime(dat$Date)) * 1000

write.csv(dat,file="data/raw/National_data_combined/National_dat_July15.csv")

summary(dat)

DOY <- ggplot((dat),aes(x=DOY,y=CEW,fill=NA_L2NAME,color=NA_L2NAME)) +
  geom_smooth(method = "gam",aes(fill=NA_L2NAME,color=NA_L2NAME),alpha=.3) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  + 
  theme_pubr()+
  scale_color_viridis(discrete = T) +
  scale_fill_viridis(discrete = T) +
  geom_rug(sides = "b") +
  #scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) +
  #scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3","#e7298a")) + 
  #geom_vline(xintercept=60,linetype = "dashed") +
  geom_vline(xintercept=152,linetype = "dashed") +
  geom_vline(xintercept=244,linetype = "dashed") +
  #geom_vline(xintercept=335,linetype = "dashed") +
  annotate("text", x = 125, y = 200, label = "Spring") +
  annotate("text", x = 200, y = 200, label = "Summer") +
  annotate("text", x = 275, y = 200, label = "Fall") +
  geom_rug(sides="b") +
  coord_cartesian(ylim=c(0,300)) +
  facet_wrap(~NA_L2NAME)
  #xlim(60,335) + ylim(0,NA) 

DOY



Year <- ggplot((dat ),aes(x=Year,y=CEW)) +
  geom_smooth(data = (dat %>% filter(State != "New York")), aes(fill=NA_L2NAME,color=NA_L2NAME), formula = y ~ s(x, bs = "cs", k=10)) +
  geom_smooth(data = (dat %>% filter(State == "New York")), aes(fill=NA_L2NAME,color=NA_L2NAME), formula = y ~ s(x, bs = "cs", k=5)) +
  #geom_smooth(method = "gam", color="black",size=2,se=FALSE)  +
  facet_wrap(~NA_L2NAME) +
  theme_pubr()+
  #geom_rug(sides = "b") +
  geom_rug(sides='b') +
  coord_cartesian(ylim=c(0,400))

Year

DOYxYEAR <- ggplot(((dat %>% filter(DOY >= 125))),aes(y=Year,x=DOY,z=(CEW))) + stat_summary_hex(bins=15) + theme_pubr() +
  scale_fill_viridis()



DOYxYEAR_wis <- ggplot(((dat %>% filter(DOY >= 125)) %>% filter(State == "Wisconsin")),aes(y=Year,x=DOY,z=(CEW))) + stat_summary_hex(bins=15) + theme_pubr() +
  scale_fill_viridis() + ggtitle("Wisconsin")

DOYxYEAR_mis <- ggplot(((dat %>% filter(DOY >= 125)) %>% filter(State == "Mississippi")),aes(y=Year,x=DOY,z=(CEW))) + stat_summary_hex(bins=15) + theme_pubr() +
  scale_fill_viridis() + ggtitle("Mississippi")

DOYxYEAR_NC <- ggplot(((dat %>% filter(DOY >= 125)) %>% filter(State == "North Carolina")),aes(y=Year,x=DOY,z=(CEW))) + stat_summary_hex(bins=15) + theme_pubr() +
  scale_fill_viridis() + ggtitle("North Carolina")

DOYxYEAR_NY <- ggplot(((dat %>% filter(DOY >= 125)) %>% filter(State == "New York")),aes(y=Year,x=DOY,z=(CEW))) + stat_summary_hex(bins=15) + theme_pubr() +
  scale_fill_viridis() + ggtitle("New York")

DOY / Year / (DOYxYEAR_wis + DOYxYEAR_mis ) / ( DOYxYEAR_NC + DOYxYEAR_NY)


ggplot(dat,aes(x=DOY,y=CEW)) + geom_point() + geom_smooth(method = "gam") + theme_pubr()


# Just looking at Wisconsin 

Wis <- dat %>% filter(State == "Wisconsin") %>% filter(!between(Year, 1999,2003))
ggplot(Wis,aes(x=as.factor(Year),y=CEW)) + geom_boxplot(outlier.size = -1)  +
  coord_cartesian(ylim=c(0,5))




world <- ne_countries(scale = "medium", returnclass = "sf")
states <- ne_states(country = "United States of America")

p <- ggplot(data = world) +   geom_sf() +
  geom_sf(data = st_as_sf(states)) + 
  coord_sf(xlim = c(-94,-74), ylim = c(30,47), expand = FALSE)+
  stat_summary_2d(data = (dat), aes(x=Longitude,y=Latitude,z=CEW)) +
  scale_fill_viridis() +
  facet_wrap(~State) +
  transition_time(DOY) +
  labs(title = 'DOY: {frame_time}', cumulative = FALSE) + 
  theme_pubr() + theme(legend.position = "none")

animate(p,fps=5)

mod <- bam(as.integer(CEW) ~ te(Latitude,Longitude) + s(Year) + s(DOY) + s(DOY, by= State) + te(Year,DOY,bs=c("tp","cc")) +
             State, family=poisson(),data=dat)

summary(mod)
draw(mod)
appraise(mod)


       