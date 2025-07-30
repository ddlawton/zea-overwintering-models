rm(list=ls())
library(tidyverse)
library(rasterVis)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(stars)
library(patchwork)

N_america <- rnaturalearth::ne_countries(continent = "north america") #N. america shapefile
zones <- raster::stack("data/processed/overwintering/Dec3_overwinteringZones (1).tif")
raster::plot(zones)

zone1 <- c(1,2)
zone2 <- c(0,1)
tif <- list()

for(j in 1:raster::nlayers(zones)){
  
  cat(paste("Currently processing layer:", j,"/",raster::nlayers(zones), "\n"))
  
  tif[[j]] <- zones[[j]]
  tif[[j]][tif[[j]] > 283] <- sample(zone1, length(tif[[j]][tif[[j]] > 283]),replace=TRUE)
  
  
  tif[[j]][tif[[j]] > 2] <- sample(zone2, tif[[j]][tif[[j]] > 2],replace=TRUE)
  
}

m <- do.call(raster::stack, tif)
m <- m[[1:4]]






# projected air temp

zones <- raster::stack("data/processed/Threshold_5c/GIS_data/air_Dec21_overwinteringZones.tif")




baseline <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(zones[[1]]), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(baseline = factor(baseline))

y22_47 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(zones[[2]]), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(y22_47 = factor(y22_47))

y48_73 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(zones[[3]]), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(y48_73 = factor(y48_73))

y74_99  <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(zones[[4]]), 
                                       as_points = FALSE, merge = TRUE)
) %>% st_as_sf()%>% mutate(y74_99 = factor(y74_99))



baseline_graph <- ggplot() +  
  geom_sf(data=baseline, aes(fill=baseline))  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 1950-2021")

y22_47_graph <- ggplot() +  
  geom_sf(data=y22_47, aes(fill=y22_47))  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank())  +
  ggtitle("years 2022-2047")

y48_73_graph <- ggplot() +  
  geom_sf(data=y48_73, aes(fill=y48_73))  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2048-2073")

y74_99_graph <- ggplot() +  
  geom_sf(data=y74_99, aes(fill=y74_99))  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2074-2099")





# GAMs

air_soil_temps <- read.csv("data/processed/Threshold_5c/ari_soil_temps_250k.csv") %>% as_tibble() %>% select(ends_with("temp")) %>%
  mutate(air.temp_C = air.temp - 273.15,soil.temp_C = soil.temp - 273.15)

#ggplot(air_soil_temps,aes(x=air.temp,y=soil.temp)) + geom_point(alpha=.1) + geom_smooth(method="lm",se=FALSE,size=1.1) +
#  ggpubr::theme_pubr()

temp_correlation <- ggplot(air_soil_temps,aes(x=air.temp_C,y=soil.temp_C)) + geom_point(alpha=.1) + geom_smooth(method="gam",formula = y~s(x, bs="tp"),se=FALSE,size=1.5) +
  ggpubr::theme_pubr() + ylab("Soil Temperature (C)") + xlab("Air Temperature (C)") + annotate("text", x = -16, y = 18, label = "GAM R^2: 0.87")

library(mgcv)
library(gratia)

mod <- bam(soil.temp ~ s(air.temp,bs="tp",k=15), data=air_soil_temps,select=TRUE)


summary(mod)
gratia::draw(mod)
gratia::appraise(mod)
k.check(mod)


projection_figure <- (baseline_graph + y22_47_graph +  y48_73_graph + y74_99_graph) + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")

ggsave(projection_figure,file="manuscript/figures/Threshold_5c/supplementary/Supp_Fig_4.png",dpi=300,width=10,height=10,units="in")
ggsave(temp_correlation,file="manuscript/figures/Threshold_5c/supplementary/Supp_Fig_3.png",dpi=300,width=5,height=5,units="in")



#now lets predict onto the air value rasters

air_values_rasters <- stack("data/processed/Threshold_5c/GIS_data/air_values_Dec22_overwinteringZones.tif")

current <- air_values_rasters[[1]]
y22_47 <- air_values_rasters[[2]]
y48_73 <- air_values_rasters[[3]]
y74_99  <- air_values_rasters[[4]]

names <- "air.temp"

names(current)<-names
names(y22_47)<-names
names(y48_73)<-names
names(y74_99)<-names

current <- raster::predict(current,mod)
y22_47 <- raster::predict(y22_47,mod)
y48_73 <- raster::predict(y48_73,mod)
y74_99 <- raster::predict(y74_99,mod)

current_ow <- raster::reclassify(current, 
                    c(0, 273.15, 0,
                      273.1500000000001, 278.15, 1,
                    278.15000000000001, 500,2))

y22_47_ow <- raster::reclassify(y22_47, 
                                c(0, 273.15, 0,
                                  273.1500000000001, 278.15, 1,
                                  278.15000000000001, 500,2))

y48_73_ow <- raster::reclassify(y48_73, 
                                c(0, 273.15, 0,
                                  273.1500000000001, 278.15, 1,
                                  278.15000000000001, 500,2))

y74_99_ow <- raster::reclassify(y74_99, 
                                c(0, 273.15, 0,
                                  273.1500000000001, 278.15, 1,
                                  278.15000000000001, 500,2))


### Getting area percentage for the rasters

future_stack <- stack(current_ow,y22_47_ow,y48_73_ow,y74_99_ow)
areas <- list()
for(j in 1:nlayers(future_stack)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(future_stack), "\n"))
  future_stack4 <- future_stack[[j]]
  areas[[j]] <- tapply(raster::area(future_stack4), future_stack4[], sum)
  
}


names <- c("current","y22_47_ow","y48_73_ow",'y74_99_ow')


flevels <- c("NL","TZ","SR")

future_areas <- bind_rows(areas) 
future_areas$year <- names

future_areas <- future_areas %>% mutate(`NL` = (`0`/(`0`+`1`+`2`)),
           `TZ` = (`1`/(`0`+`1`+`2`)),
           `SR` = (`2`/(`0`+`1`+`2`))) %>% select(4:7) %>% pivot_longer(cols=c(2:4),names_to = "zone",values_to = "proportion") %>%
  mutate(zone = factor(zone,levels=flevels))

baseline <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(current_ow), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(layer = factor(layer))

y22_47 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y22_47_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(layer = factor(layer))

y48_73 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y48_73_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(layer = factor(layer))

y74_99  <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y74_99_ow), 
                                       as_points = FALSE, merge = TRUE)
) %>% st_as_sf()%>% mutate(layer = factor(layer))


baseline_graph <- ggplot() +  
  geom_sf(data=baseline, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 1950-2021")  + 
  scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 12)) 

y22_47_graph <- ggplot() +  
  geom_sf(data=y22_47, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("") + ylab("") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank())  +
  ggtitle("years 2022-2047")  + 
  scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 12)) 

y48_73_graph <- ggplot() +  
  geom_sf(data=y48_73, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2048-2073")  + 
  scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 12)) 

y74_99_graph <- ggplot() +  
  geom_sf(data=y74_99, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2074-2099")  + 
  scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 12)) 


labs <- c("1950-2021", "2022-2047", "2048-2073", "2074-2099")

are_change <- ggplot(future_areas,aes(x=year,y=(proportion * 100),color=zone,group=zone)) + geom_point(size=3) +
  geom_line()  +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    na.translate = FALSE) + theme_pubr() + coord_cartesian(ylim=c(0,100)) +
  scale_y_continuous(breaks=c(0,50,100)) + ylab("Area (%)") + xlab("Year range") + 
  scale_x_discrete(labels= labs) +
  theme(text = element_text(size = 12))   +  
  theme(legend.position = "none")




layout <- "
AB
CD
EE
"

projection_figure <-baseline_graph + y22_47_graph +  y48_73_graph + y74_99_graph + are_change + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect',design = layout) & theme(legend.position = "bottom")

ggsave(projection_figure,file="manuscript/figures/Threshold_5c/Figure3.tiff",dpi=600,width=18,height=22,units="cm")
