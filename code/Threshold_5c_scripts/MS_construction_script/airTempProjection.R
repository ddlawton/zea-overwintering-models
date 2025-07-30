rm(list=ls())
library(tidyverse)
library(rasterVis)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(sf)
library(stars)
library(patchwork)
library(mgcv)
library(gratia)

N_america <- rnaturalearth::ne_countries(continent = "north america") #N. america shapefile


US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)



# projected air temp




zones <- raster::stack("data/processed/Threshold_5c/GIS_data/air_values_Dec22_overwinteringZones.tif")




current_ow <- raster::reclassify(zones[[1]], 
                                 c(0, 273.15, 0,
                                   273.150000000000001, 278.15, 1,
                                   278.150000000000001, 500,2))

y31_53_ow <- raster::reclassify(zones[[2]], 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))

y54_76_ow <- raster::reclassify(zones[[3]], 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))

y77_99_ow <- raster::reclassify(zones[[4]], 
                                c(0, 273.15, 0,
                                  273.150000000000001, 278.15, 1,
                                  278.150000000000001, 500,2))



baseline <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(current_ow), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(baseline = factor(baseline))

y22_47 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y31_53_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf() %>% mutate(y22_47 = factor(y22_47))

y48_73 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y54_76_ow), 
                                      as_points = FALSE, merge = TRUE)
)%>% st_as_sf()%>% mutate(y48_73 = factor(y48_73))

y74_99  <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(y77_99_ow), 
                                       as_points = FALSE, merge = TRUE)
) %>% st_as_sf()%>% mutate(y74_99 = factor(y74_99))


baseline_graph <- ggplot()  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +  
  geom_sf(data=baseline, aes(fill=baseline)) + 
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") + 
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("years 1950-2021")+
  theme(text=element_text(size=30))

y22_47_graph <- ggplot()  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +  
  geom_sf(data=y22_47, aes(fill=y22_47))  +  
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") + 
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2022-2047") +
  theme(text=element_text(size=30)) + 
  theme(legend.position="none")


y48_73_graph <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +  
  geom_sf(data=y48_73, aes(fill=y48_73))  +  
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") + 
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2048-2073") +
  theme(text=element_text(size=30)) + 
  theme(legend.position="none")


y74_99_graph <- ggplot()  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  geom_sf(data=y74_99, aes(fill=y74_99)) +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +  
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + 
  ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("years 2074-2099")+
  theme(text=element_text(size=30)) + 
  theme(legend.position="none")


layout <- "
ABCD
"

projection_figure <- baseline_graph + y22_47_graph +  y48_73_graph + y74_99_graph + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")
projection_figure

ggsave(projection_figure,file="manuscript/figures/Threshold_5c/Supplementary/Supp_Figure_8.png",dpi=600,width=30,height=20,units="cm",bg="white")





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


r1 <- raster("data/processed/Threshold_5c/GIS_data/overwintering/Global_ensemble_tiffs/global_ensemble_baseline_winter.tif")
r2 <- raster("data/processed/Threshold_5c/GIS_data/overwintering/Global_ensemble_tiffs/global_ensemble_y22_47_winter.tif")
r3 <- raster("data/processed/Threshold_5c/GIS_data/overwintering/Global_ensemble_tiffs/global_ensemble_y48_73_winter.tif")
r4 <- raster("data/processed/Threshold_5c/GIS_data/overwintering/Global_ensemble_tiffs/global_ensemble_y74_99_winter.tif")

air_values_rasters <- stack(r1,r2,r3,r4)



m2 <- crop(air_values_rasters, (NA_map))
m3 <- mask(air_values_rasters, NA_map)


current <- m3[[1]]
y31_53 <- m3[[2]]
y54_76 <- m3[[3]]
y77_99  <- m3[[4]]

names <- "air.temp"

names(current)<-names
names(y31_53)<-names
names(y54_76)<-names
names(y77_99)<-names

current <- raster::predict(current,mod)
y31_53 <- raster::predict(y31_53,mod)
y54_76 <- raster::predict(y54_76,mod)
y77_99 <- raster::predict(y77_99,mod)

#plot(current)

#plot(y31_53)

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
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=baseline, aes(fill=layer),color=NA,lwd=0)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("1950-2021") +
  theme(text=element_text(size=30))

y31_53_graph <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=y31_53, aes(fill=layer),color=NA,lwd=0)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank())  +
  ggtitle("2022-2047")  +
  theme(text=element_text(size=30)) + theme(legend.position="none")

y54_76_graph <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=y54_76, aes(fill=layer),color=NA,lwd=0)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("2048-2073")  +
  theme(text=element_text(size=30)) + theme(legend.position="none")

y77_99_graph <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=y77_99, aes(fill=layer),color=NA,lwd=0)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +  
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("") +
  theme_void() +
  theme(legend.title = element_blank()) +
  ggtitle("2074-2099")  +
  theme(text=element_text(size=30)) + theme(legend.position="none")



layout <- "
ABCD
"

projection_figure <- baseline_graph + y31_53_graph +  y54_76_graph + y77_99_graph + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")
projection_figure

ggsave(projection_figure,file="manuscript/PNAS_revisions/Figures/Figure4.png",dpi=600,width=30,height=20,units="cm",bg="white")


# Alright now calculating area
plot(current_ow)

?group_by

historical = as.data.frame(current_ow) %>%
  group_by(layer) %>%
  tally() %>%
  drop_na() %>%
  mutate(area = n * res(current_ow)[1]* res(current_ow)[2],
         area_percent = area / sum(area),
         map = "historical")

y77_99_area = as.data.frame(y77_99_ow) %>%
  group_by(layer) %>%
  tally() %>%
  drop_na() %>%
  mutate(area = n * res(current_ow)[1]* res(current_ow)[2],
         area_percent = area / sum(area),
         map = "y77_99") %>%
  rbind(historical) %>%
  select(!c(area,n)) %>%
  pivot_wider(names_from = 'map',values_from = area_percent)
y77_99_area
