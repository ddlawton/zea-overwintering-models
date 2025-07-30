rm(list=ls())

library(gratia)
library(tidyverse)
library(sf)
library(rnaturalearth)

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
  mutate(trap_type = factor(trap_type))


modGS <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GS.rds")

output <- smooth_estimates(modGS,smooth="te(Latitude,Longitude,year)",n=50)

output2 <- smooth_estimates(modGS,smooth="te(Latitude,Longitude,year)",n=10,dist=0.1)




#now plotting the 3d smooth splice

N_america <- ne_countries(continent = "north america") #N. america shapefile


US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)


output3 <- output %>% select(est,Latitude,Longitude,year) %>%
  mutate(year = round(year)) %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326)

library(fasterize)


unique(levels(factor(output3$year)))

output3 %>%
  filter(year==2010) %>%
  ggplot(aes(x=Longitude,y=Latitude,z=est)) +
  stat_summary_2d()







?rasterize
years <- unique(levels(factor(output3$year)))

?raster
template <- raster::raster(temp_dat,res=1.5)

r <- list()
for (i in years){
  temp_dat <- output3 %>% filter(year == i)
  r[[i]] <- rasterize(temp_dat, template, field="est", fun=function(x,...)mean(x))
}

output_stack <- stack(r)

library(stars)

## crop and mask
output_stack2 <- crop(output_stack, extent(NA_map))
output_stack3 <- mask(output_stack2, NA_map)



#creating list of stars dataframes
#stars_list <- list()
#for (i in seq(1:nlayers(output_stack3))){
#  temp_stack <- output_stack3[[i]]
#  stars_list[[i]] <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(output_stack3), 
#                                                 as_points = FALSE, merge = FALSE)) %>% st_as_sf() 
#}

names(stars_df)
stars_df <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(output_stack3),as_points = FALSE, merge = FALSE)) %>% st_as_sf() %>%
  pivot_longer(cols = 1:32,names_to = "year",values_to = "est") %>%
  mutate(year = gsub("X","",year))
                          



facet_year_tensor <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry)) +
  geom_sf(data=stars_df,aes(fill=est,color=est,geometry=geometry),alpha=0.8,color=NA)  +
  scale_fill_viridis() +
  scale_color_viridis() +
  scale_x_continuous(limits=c(-125,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  xlab("") + ylab("") +
  theme_void() +
  theme(text = element_text(size = 12)) +
  facet_wrap(~year) 


ggsave(facet_year_tensor,file="manuscript/figures/Threshold_5c/revision_figures/Supp_Figure_5.png",dpi=600,units="cm",bg="white")








dat4 <- dat4 %>% mutate(GSpred = predict(modGS,type="link"))


draw(modGS,smooth="whoa")
?draw

?smooth_estimates
years <- c(1990,1998,2006,2014,2021)

output %>%
  ggplot(aes(x=year)) + geom_histogram()


output %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarize(n=n())

output2 <- output %>%
  filter(year == 1998) %>%
  mutate(year = factor(year))


unique(output2$year)

ggplot(output,aes(y=Latitude,x=Longitude)) +
  geom_point()

output %>%
  mutate(year = as.integer(year)) %>%
  mutate(year = factor(year)) %>%
ggplot(aes(y=Latitude,x=Longitude,z=est )) +
  stat_summary_hex(bins=) +
  viridis::scale_fill_viridis() +
  geom_contour() +
  facet_wrap(~year)

plot(modGS)


