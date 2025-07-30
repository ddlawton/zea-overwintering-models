####
# methods
#  Figure
#   Construction
#######
rm(list=ls())

# libraries
library(dplyr)
library(data.table)
library(tidyverse)
library(magick)
library(mgcv)
library(sf)
library(gratia)
library(stars)
library(patchwork)
library(raster)
library(rasterVis)
library(rnaturalearth)
library(ggpubr)
library(gtsummary)
library(vtable)
library(viridis)
library(gganimate)
library(ggridges)
library(ggthemes)
library(ggExtra)
# data loading
#####



dat <- as_tibble(fread("data/processed/Threshold_5c/final_zea_data/dat_with_GS_Jan5.csv")) %>%
  mutate(zone_30y = factor(zone_30y))



#Point map

US_sh <- (ne_states(country="United States of America")) %>% st_as_sf() %>%
  dplyr::filter(name !="Alaska") %>% dplyr::filter(name !="Hawaii") %>% st_union() 

CA_sh <- st_as_sf(ne_states(country="Canada")) %>% st_union() %>% st_as_sf()



NA_bind <- rbind(US_sh, CA_sh) %>% st_as_sf()


simplepolys <- rmapshaper::ms_simplify(input = as(NA_bind, 'Spatial')) %>%
  st_as_sf()


dat4 %>% filter(contact == "William Hutchison")
location_dat <- dat %>% group_by(location) %>%
  summarize(CEW_sum = mean(CEW_sum), longitude = first(longitude),latitude = first(latitude),
            zone_30y = first(zone_30y), state=first(state) )

global <- ggplot(simplepolys,aes(geometry=x)) + geom_sf() +
  geom_point(data=(location_dat),aes(x=longitude,y=latitude),inherit.aes = F,size=1) + 
  theme_pubr() +
  ylim(25,52) + xlim(-130,-70)  +
  xlab("Longitude") + ylab("Latitude") +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank()) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

ggsave(global,file="manuscript/figures/Threshold_5c/Methods_figures/Trap_map.svg")


### 40 year average map


year30 <- raster("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif")





year30 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(year30), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(multiyear_zones = factor(Zea_over_winter_zones))

year30_graph <- ggplot() +  
  geom_sf(data=year30, aes(fill=multiyear_zones), lwd = 0,color = NA)  +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank())  + 
  scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 12)) 


ggsave(year30_graph,file="manuscript/figures/Threshold_5c/Methods_figures/30_year_avg_zone_map.svg")


##### Projected zone shift


# GAM 

linear <- read.csv("data/processed/Threshold_5c/air_soil_temps.csv") %>% as_tibble() %>% select(ends_with("temp")) %>%
  mutate(air.temp_C = air.temp - 273.15,soil.temp_C = soil.temp - 273.15)


mod <- bam(soil.temp ~ s(air.temp,k=100), data=linear,select=TRUE,discrete=TRUE)

summary(mod)
gratia::draw(mod)
gratia::appraise(mod)
k.check(mod)
concurvity(mod)


projection_figure <- (baseline_graph + y31_53_graph +  y54_76_graph + y77_99_graph) + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")

ggsave(projection_figure,file="manuscript/figures/maps/air_projection_maps.png",dpi=300,width=10,height=10,units="in")
ggsave(temp_correlation,file="manuscript/figures/air_soil_correlation.png",dpi=300,width=10,height=10,units="in")



#now lets predict onto the air value rasters

air_values_rasters <- stack("data/processed/Threshold_5c/GIS_data/overwintering/projected_overwinter/air_values_Dec3_overwinteringZones.tif")

current <- air_values_rasters[[1]]
y31_53 <- air_values_rasters[[2]]
y54_76 <- air_values_rasters[[3]]
y77_99  <- air_values_rasters[[4]]

names <- "air.temp"

names(current)<-names
names(y31_53)<-names
names(y54_76)<-names
names(y77_99)<-names

current <- raster::predict(current,mod)
y31_53 <- raster::predict(y31_53,mod)
y54_76 <- raster::predict(y54_76,mod)
y77_99 <- raster::predict(y77_99,mod)

current_ow <- raster::reclassify(current, 
                                 c(0, 273.15, 0,
                                   273.150000000000001, 278.15, 1,
                                   278.150000000000001, 500,2))

y31_53_ow <- raster::reclassify(y31_53, 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))

y54_76_ow <- raster::reclassify(y54_76, 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))

y77_99_ow <- raster::reclassify(y77_99, 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))


baseline <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(current_ow), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(layer = factor(layer))

y31_53 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y31_53_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(layer = factor(layer))

y54_76 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y54_76_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(layer = factor(layer))

y77_99  <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y77_99_ow), 
                                       as_points = FALSE, merge = TRUE)
) %>% st_as_sf()%>% mutate(layer = factor(layer))


baseline_graph <- ggplot() +  
  geom_sf(data=baseline, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 1950-2005")

y31_53_graph <- ggplot() +  
  geom_sf(data=y31_53, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank())  +
  ggtitle("years 2031-2053")

y54_76_graph <- ggplot() +  
  geom_sf(data=y54_76, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2054-2076")

y77_99_graph <- ggplot() +  
  geom_sf(data=y77_99, aes(fill=layer),color=NA,lwd=0)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2077-2099")


projection_figure <- baseline_graph + y31_53_graph +  y54_76_graph + y77_99_graph + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")

ggsave(projection_figure,file="manuscript/figures/Threshold_5c/Methods_figures/projected_Zone_change.svg")


##### Week of year smooths

woy_zone_GI <- ggplot(dat,aes(x=woy,y=(GS_pred), color=zone_30y)) + geom_smooth(se=FALSE) + ylab("") +
  xlab("Week of year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), labels = c("NL", "TZ", "SR"))  +
  #geom_smooth(data=dat, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("")+
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 12)) 

ggsave(woy_zone_GI,file="manuscript/figures/Threshold_5c/Methods_figures/week_of_year_zone_dynamics.svg")

