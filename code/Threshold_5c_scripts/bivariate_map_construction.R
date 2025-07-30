####
# Daylight/minimum temperature bivariate chloropeth
#
###
rm(list=ls())

library(tidyverse)
library(raster)
library(sf)
library(biscale)
library(stars)
library(exactextractr)
library(patchwork)


#importing the NA shapefile
N_america <- rnaturalearth::ne_countries(continent = "north america") #N. america shapefile


US_out <- rnaturalearth::ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- rnaturalearth::ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- rnaturalearth::ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)

#loading in data

dl <- stack("data/processed/Threshold_5c/GIS_data/day_light_temp/daylight_10k.tif")

temp <- stack("data/processed/Threshold_5c/GIS_data/day_light_temp/minimum_temperature_10k.tif")


data <- bi_class(stl_race_income, x = pctWhite, y = medInc, style = "quantile", dim = 3)

poly_grid <- st_make_grid(temp,cellsize = 0.5) %>% st_as_sf()

temp_cols <- exact_extract(temp,poly_grid,fun="mean",append_cols=TRUE) %>% as_data_frame()
dl_cols <- exact_extract(dl,poly_grid,fun="mean",append_cols=TRUE) %>% as_data_frame()

poly_grid2 <- poly_grid %>% cbind(dl_cols,temp_cols)

values <- poly_grid2 %>% drop_na() %>% st_as_sf() %>% as_data_frame()
names(values)


july_data <- bi_class(values, x = mean.X0_tmin, y = mean.X0_dayl, style = "quantile", dim = 3)
August_data <- bi_class(values, x = mean.X1_tmin, y = mean.X1_dayl, style = "quantile", dim = 3)
Sept_data <- bi_class(values, x = mean.X2_tmin, y = mean.X2_dayl, style = "quantile", dim = 3)
Oct_data <- bi_class(values, x = mean.X3_tmin, y = mean.X3_dayl, style = "quantile", dim = 3)
Nov_data <- bi_class(values, x = mean.X4_tmin, y = mean.X4_dayl, style = "quantile", dim = 3)
Dec_data <- bi_class(values, x = mean.X5_tmin, y = mean.X5_dayl, style = "quantile", dim = 3)


July <- ggplot() +
  geom_sf(data = july_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "July") +
  bi_theme()

Aug <- ggplot() +
  geom_sf(data = August_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "August") +
  bi_theme()

Sept <- ggplot() +
  geom_sf(data = Sept_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "September") +
  bi_theme()

Oct <- ggplot() +
  geom_sf(data = Oct_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "October") +
  bi_theme()

Nov <- ggplot() +
  geom_sf(data = Nov_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "November") +
  bi_theme()

Dec <- ggplot() +
  geom_sf(data = Dec_data, mapping = aes(fill = bi_class,color=bi_class,geometry=x), size = 0.1, show.legend = FALSE) +
  geom_sf(data=NA_map, fill="transparent",color="black") +
  bi_scale_fill(pal = "DkBlue", dim = 3) +
  bi_scale_color(pal = "DkBlue", dim = 3) +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) + 
  labs(
    title = "December") +
  bi_theme()
?bi_legend
legend <- bi_legend(pal = "DkBlue",
                    dim = 3,
                    xlab = "Higher temperatures",
                    ylab = "Longer days",
                    size = 12)

J_finalPlot <- ggdraw() +
  draw_plot(July, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.25, 0.25)

A_finalPlot <- ggdraw() +
  draw_plot(Aug, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.2, 0.2)

S_finalPlot <- ggdraw() +
  draw_plot(Sept, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.2, 0.2)

O_finalPlot <- ggdraw() +
  draw_plot(Oct, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.2, 0.2)

N_finalPlot <- ggdraw() +
  draw_plot(Nov, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.2, 0.2)

D_finalPlot <- ggdraw() +
  draw_plot(Dec, 0, 0, 1, 1) +
  draw_plot(legend, 0.01, 0.1, 0.2, 0.2)


plots <- J_finalPlot + A_finalPlot + S_finalPlot + O_finalPlot + N_finalPlot + D_finalPlot

plots <- finalPlot + Aug + Sept + Oct + Nov + Dec

ggdraw() +
  plot_grid(July, Aug, Sept, Oct, Nov, Dec) +
  draw_plot(legend, 0.1, 0.3, 0.2, 0.2)

ggsave(plots,file="data/processed/Threshold_5c/GIS_data/day_light_temp/test.png",height=10,width=15,units="in",dpi=600)

?draw_plot
finalPlot <- ggdraw() +
  draw_plot(July, 0.2, .3,scale=0.75) +
  #draw_plot(Aug, 1, .1, .1, 1) +
  draw_plot(legend, 0.2, .65, 0.2, 0.2)
  #draw_plot(Sept, 0.5, 1, 1, 1) +
  #draw_plot(Oct, 0.5, 0.5, 1, 1) +
  #draw_plot(Nov, 0, 0, 1, 1) +
  #draw_plot(Dec, 1, 0, 1, 1) #+
  #draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
finalPlot
