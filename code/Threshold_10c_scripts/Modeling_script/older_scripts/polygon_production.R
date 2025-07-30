####
#  Polygon 
#    production
#
####
rm(list=ls())

library(tidyverse)
library(sp)
library(rgdal)
library(sf)
library(data.table)
library(spatialEco)

points <- as_tibble(fread("data/processed/overwintering/Zea_overwintering_zone_aug26.csv"))


grid_400 <- st_as_sf(readOGR(dsn = "data/processed/Grids", layer = "grid_400"))
grid_200 <- st_as_sf(readOGR(dsn = "data/processed/Grids", layer = "grid_200"))


grid_400$center <- st_centroid(grid_400)$geometry
grid_200$center <- st_centroid(grid_200)$geometry

points_sf = st_as_sf(points, coords = c("Longitude", "Latitude"),crs=4326)

st_crs(points_sf) == st_crs(grid_400)

test <- st_as_sf(point.in.poly(points_sf,grid_400))
names(test)

dat_w_id <- st_drop_geometry(test %>% select(2:3,5,8,9,10,11,16:17,"id","woy"))


polygon_dat <- dat_w_id %>% group_by(id,year,woy) %>%
  summarise(
    CEW_count = sum(CEW_sum),
    CEW_mean = mean(CEW_sum),
    CEW_se = sd(CEW_sum)/sqrt(length(CEW_sum)),
    num_obs = n()
  )



ID_coords <- grid_400 %>% filter(id %in% unique(polygon_dat$id)) %>%
  select(id,center) %>%
  mutate(coords = gsub('POINT ','',center),
         coords = gsub('c\\(','',coords),
         coords = gsub(')','',coords)) %>%
  st_drop_geometry() %>%
  select(!center) %>%
  separate(col=coords,sep=", ",into=c("Longitude","Latitude")) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  )
  
gridded_400km <- polygon_dat %>% left_join(ID_coords,by="id")



ggplot(polygon_dat2,aes(x=(Longitude_400),y=(Latitude_400),color=CEW_count+1)) + geom_point() +
  scale_color_viridis_b(trans="log10")



st_crs(points_sf) == st_crs(grid_200)

test_200 <- st_as_sf(point.in.poly(points_sf,grid_200))
names(test)

dat_w_id <- st_drop_geometry(test_200 %>% select(2:3,5,8,9,10,11,16:17,"id","woy"))


length(unique(dat_w_id$id))

polygon_dat <- dat_w_id %>% group_by(id,year,woy) %>%
  summarise(
    CEW_count = sum(CEW_sum),
    CEW_mean = mean(CEW_sum),
    CEW_se = sd(CEW_sum)/sqrt(length(CEW_sum)),
    num_obs = n()
  )

ID_coords <- grid_200 %>% filter(id %in% unique(polygon_dat$id)) %>%
  select(id,center) %>%
  mutate(coords = gsub('POINT ','',center),
         coords = gsub('c\\(','',coords),
         coords = gsub(')','',coords)) %>%
  st_drop_geometry() %>%
  select(!center) %>%
  separate(col=coords,sep=", ",into=c("Longitude","Latitude")) %>%
  mutate(
    Longitude = as.numeric(Longitude),
    Latitude = as.numeric(Latitude)
  ) 

gridded_200km <- polygon_dat %>% left_join(ID_coords,by="id")


ggplot(polygon_dat2,aes(x=(Longitude_400),y=(Latitude_400),color=CEW_count+1)) + geom_point() +
  scale_color_viridis_b(trans="log10") +

ggplot(polygon_dat3,aes(x=(Longitude_200),y=(Latitude_200),color=CEW_count+1)) + geom_point() +
  scale_color_viridis_b(trans="log10")

write.csv(gridded_200km,file="data/processed/grids/gridded_csvs/gridded_200km.csv")
write.csv(gridded_400km,file="data/processed/grids/gridded_csvs/gridded_400km.csv")


af_dat <- points %>%
  select(CEW_sum, Latitude, Longitude, min, location, year, woy) %>%
  rename(overwinter_zones = "min")
write.csv(af_dat,file="data/processed/grids/gridded_csvs/point_obs.csv")
