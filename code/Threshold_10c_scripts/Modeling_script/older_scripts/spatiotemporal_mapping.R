#####
# Spatiotemporal mapping
#
#

rm(list=ls())
library(lubridate)
library(tidyverse)
library(rnaturalearth)
library(viridis)
library(rgdal)
library(sp)
library(raster)
library(spatialEco)
library(ggpubr)
library(gridExtra)
library(rnaturalearth)
library(sf)
library(mgcv)
library(gratia)
library(gganimate)


dat <- as_tibble(read.csv("data/raw/National_data_combined/National_dat_Jul26.csv")) %>%
  filter(NA_L1NAME != "WATER")

N_america <- ne_states(country="united states of america")

ggplot(data = st_as_sf(N_america)) + geom_sf(aes(geometry=geometry)) + 
  geom_point(data=dat,aes(y=latitude,x=longitude)) +
  coord_sf(xlim = c(-130,-50), ylim = c(15,60), expand = FALSE) +
  theme_pubr() + theme(legend.position = "none")


ggplot((dat),aes(x=woy,y=(CEW_sum))) + 
  #geom_point() +
  geom_smooth(method="gam") +
  coord_cartesian(xlim=c(10,56),ylim=c(0,NA))

ggplot((dat),aes(x=year,y=(CEW_sum))) + 
  #geom_point() +
  geom_smooth(method="gam") +
  coord_cartesian(xlim=c(1990,2021),ylim=c(0,NA))

#plotting by NA ecoregion level I to III

N_america <- st_as_sf(ne_countries(continent = "north america")) # just a polygon to use


#Level 1

ecoregions_L1 <- readOGR(dsn = "data/raw/GIS_data/na_cec_eco_l1",
                         layer = "NA_CEC_Eco_Level1")
ecoregions_L1_sf <- st_as_sf(ecoregions_L1)

eco_names <- unique(dat$NA_L1NAME)

bio <- list()
for(i in eco_names){
  print(paste("Now computing",i))
  
  eocregn <- dat %>% filter(NA_L1NAME == i)
  
  g1 <- ggplot(eocregn,aes(x=as.integer(woy),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(0,56))+
    xlab("Week of year") + ylab("CEW count")
  
  g2 <- ggplot(eocregn,aes(x=as.integer(year),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(1972,2021))+
    xlab("Year") + ylab("CEW count")
  
  g3 <- ggplot(eocregn,aes(x=as.integer(woy))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$woy),max(dat$woy))) +
    xlab("Week of year") + ylab("Number of zea observations")
  
  g4 <- ggplot(eocregn,aes(x=as.integer(year))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$year),max(dat$year))) +
    xlab("year") + ylab("Number of zea observations")
  
  g5 <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
    geom_sf(data = (ecoregions_L1_sf %>% dplyr::filter(NA_L1NAME == i)), aes(fill = NA_L1NAME)) +
    coord_sf(xlim = c(-130,-50), ylim = c(15,60), expand = FALSE) +
    theme_pubr() + theme(legend.position = "none")
  
  t <- toString(i)
  p1 <- ggplotGrob(g1)
  p2 <- ggplotGrob(g2)
  p3 <- ggplotGrob(g3)
  p4 <- ggplotGrob(g4)
  p5 <- ggplotGrob(g5)
  
  lay <- rbind(c(5,5,5,5),
               c(5,5,5,5),
               c(1,1,2,2),
               c(3,3,4,4))
  
  bio[[i]] <- grid.arrange(
    grobs = list(p1,p2,p3,p4,p5),
    layout_matrix = lay,
    top=t
  )
}


ml <- marrangeGrob(bio, nrow=1, ncol=1)
ggsave("figures/EcoregionL1.pdf", ml,height=11,width=8.5,units="in")


#Level 2

ecoregions_L2 <- readOGR(dsn = "data/raw/GIS_data/na_cec_eco_l2",
                         layer = "NA_CEC_Eco_Level2")
ecoregions_L2_sf <- st_as_sf(ecoregions_L1)

eco_names <- unique(dat$NA_L2NAME)

bio <- list()
for(i in eco_names){
  print(paste("Now computing",i))
  
  eocregn <- dat %>% filter(NA_L2NAME == i)
  
  g1 <- ggplot(eocregn,aes(x=as.integer(woy),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(0,56))+
    xlab("Week of year") + ylab("CEW count")
  
  g2 <- ggplot(eocregn,aes(x=as.integer(year),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(1972,2021))+
    xlab("Year") + ylab("CEW count")
  
  g3 <- ggplot(eocregn,aes(x=as.integer(woy))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$woy),max(dat$woy))) +
    xlab("Week of year") + ylab("Number of zea observations")
  
  g4 <- ggplot(eocregn,aes(x=as.integer(year))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$year),max(dat$year))) +
    xlab("year") + ylab("Number of zea observations")
  
  g5 <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
    geom_sf(data = (ecoregions_L2_sf %>% dplyr::filter(NA_L2NAME == i)), aes(fill = NA_L2NAME)) +
    coord_sf(xlim = c(-130,-50), ylim = c(15,60), expand = FALSE) +
    theme_pubr() + theme(legend.position = "none")
  
  t <- toString(i)
  p1 <- ggplotGrob(g1)
  p2 <- ggplotGrob(g2)
  p3 <- ggplotGrob(g3)
  p4 <- ggplotGrob(g4)
  p5 <- ggplotGrob(g5)
  
  lay <- rbind(c(5,5,5,5),
               c(5,5,5,5),
               c(1,1,2,2),
               c(3,3,4,4))
  
  bio[[i]] <- grid.arrange(
    grobs = list(p1,p2,p3,p4,p5),
    layout_matrix = lay,
    top=t
  )
}


ml <- marrangeGrob(bio, nrow=1, ncol=1)
ggsave("figures/EcoregionL2.pdf", ml,height=11,width=8.5,units="in")


#Level 3

ecoregions_L3 <- readOGR(dsn = "data/raw/GIS_data/NA_CEC_Eco_Level3",
                         layer = "NA_CEC_Eco_Level3")

ecoregions_L3_sf <- st_as_sf(ecoregions_L3)

eco_names <- unique(dat$NA_L3NAME)

bio <- list()
for(i in eco_names){
  print(paste("Now computing",i))
  
  eocregn <- dat %>% filter(NA_L3NAME == i)
  
  g1 <- ggplot(eocregn,aes(x=as.integer(woy),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(0,56))+
    xlab("Week of year") + ylab("CEW count")
  
  g2 <- ggplot(eocregn,aes(x=as.integer(year),y=CEW_sum)) + geom_smooth() + 
    theme_pubr() +
    coord_cartesian(ylim=c(0,NA),xlim=c(1972,2021))+
    xlab("Year") + ylab("CEW count")
  
  g3 <- ggplot(eocregn,aes(x=as.integer(woy))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$woy),max(dat$woy))) +
    xlab("Week of year") + ylab("Number of zea observations")
  
  g4 <- ggplot(eocregn,aes(x=as.integer(year))) + geom_histogram(stat="count") + 
    theme_pubr()+
    coord_cartesian(xlim=c(min(dat$year),max(dat$year))) +
    xlab("year") + ylab("Number of zea observations")
  
  g5 <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
    geom_sf(data = (ecoregions_L3_sf %>% dplyr::filter(NA_L3NAME == i)), aes(fill = NA_L3NAME)) +
    coord_sf(xlim = c(-130,-50), ylim = c(15,60), expand = FALSE) +
    theme_pubr() + theme(legend.position = "none")
  
  t <- toString(i)
  p1 <- ggplotGrob(g1)
  p2 <- ggplotGrob(g2)
  p3 <- ggplotGrob(g3)
  p4 <- ggplotGrob(g4)
  p5 <- ggplotGrob(g5)
  
  lay <- rbind(c(5,5,5,5),
               c(5,5,5,5),
               c(1,1,2,2),
               c(3,3,4,4))
  
  bio[[i]] <- grid.arrange(
    grobs = list(p1,p2,p3,p4,p5),
    layout_matrix = lay,
    top=t
  )
}


ml <- marrangeGrob(bio, nrow=1, ncol=1)
ggsave("figures/EcoregionL2.pdf", ml,height=11,width=8.5,units="in")


####
# Creating heatmap gif
#

getSeason <- function(DATES) {
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, "Winter",
          ifelse (d >= SE & d < SS, "Spring",
                  ifelse (d >= SS & d < FE, "Summer", "Fall")))
}

dat$Season <- getSeason(dat$date)


?stat_summary_hex

N_america <- st_as_sf(ne_states(country="united states of america")) %>%
  filter(name_tr != "Alaska")  %>%
  filter(name_tr != "Hawaii")

map_color <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
  stat_summary_hex(data=dat,aes(x = longitude, y = latitude, z = (CEW_sum+1)),bins=10) +
  scale_fill_viridis(
    trans = "log10"
  ) +
  #scale_fill_viridis() +
  transition_states(as.factor(woy),
       transition_length = 2,
       state_length = 1) + 
  ggtitle('Week {closest_state}')


map_size_year <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
  geom_point(aes(x = longitude, y = latitude, size = (CEW_sum+1), color=(CEW_sum+1)),
             data = final_dat, alpha = .5) +
  scale_size_continuous(trans = "log10") +
  scale_color_viridis(trans = "log10") +
  transition_states(as.factor(year),
                    transition_length = 2,
                    state_length = 1) +
  labs(size = 'log CEW Count',color = 'log CEW Count') + 
  ggtitle('Year {closest_state}')

map_size_unlogged <- ggplot(data = N_america) + geom_sf(aes(geometry=geometry)) + 
  geom_point(aes(x = longitude, y = latitude, size = (CEW_sum), color=(CEW_sum)),
             data = final_dat, alpha = .5) +
  scale_color_viridis() +
  transition_states(as.factor(woy),
                    transition_length = 2,
                    state_length = 1) +
  labs(size = 'CEW Count',color = 'CEW Count') + 
  ggtitle('Year {closest_state}')

anim_save(file="point.gif",animation = map_size,path = "figures/exploratory/")
anim_save(file="point_unlogged.gif",animation = map_size_unlogged,path = "figures/exploratory/")

anim_save(file="hex.gif",animation = map_color,path = "figures/exploratory/")
anim_save(file="point_year.gif",animation = map_size_year,path = "figures/exploratory/")
