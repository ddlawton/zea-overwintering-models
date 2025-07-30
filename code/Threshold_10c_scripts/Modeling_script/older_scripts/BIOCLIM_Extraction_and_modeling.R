#####
# BIOCLIM extraction
#  Present to 2100
#   Then Modeling and predicting
##
rm(list=ls())

library(tidyverse)
library(raster)
library(sp)
library(sf)
library(data.table)
library(exactextractr)
library(tidyverse)
library(ggpubr)
library(patchwork)
library(ggforce)
library(ggridges)
library(randomForest)
library(corrplot)
library(ggthemes)
library(scales)
library(iml)


dat <- as_tibble(fread("data/raw/National_data_combined/National_dat_Sept14.csv"))

#project the data table and buffer around each point

dat_projected <- SpatialPointsDataFrame(coords=dat[6:7],data=dat,
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
crs
dat_projected2 <- spTransform(dat_projected,CRSobj = crs(current_bio))

plot(dat_projected)

dat_sf <-st_as_sf(dat_projected)

dat_metric = st_transform(dat_sf, "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs ")

dat_buffered = st_buffer(dat_metric, 2500, endCapStyle = 'ROUND')



dat_wgs = st_transform(dat_buffered, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

plot(st_geometry(dat_wgs))

#Alright now to extract raster values

#Now
?list.files

img <- list.files("E:/CURRENT_BIOCLIM",pattern='\\.tif$', full.names = TRUE, include.dirs = TRUE,recursive=TRUE)

current_bio <- raster::stack(img)


dat_wgs2 <- dat_wgs

crs(dat_wgs2) = crs(current_bio)


test <- exact_extract(current_bio,dat_wgs2, fun="mean") 

test2<- test %>% cbind(dat_wgs2)

names(bioclim)
bioclim <- test2 %>% mutate(
    Annual_mean_temp = mean.CHELSA_bio10_01*.1,
    Mean_dinural_range = mean.CHELSA_bio10_02*.1,
    Isothermality = mean.CHELSA_bio10_03,
    Temp_seasonality = mean.CHELSA_bio10_04*0.01,
    Max_temp_warmest_month = mean.CHELSA_bio10_05*0.1,
    Min_temp_coldest_moth = mean.CHELSA_bio10_06 * 0.1,
    Temp_annual_range = mean.CHELSA_bio10_07 * 0.1,
    Mean_temp_wettest_quarter = mean.CHELSA_bio10_08 * 0.1,
    Mean_temp_driest_quarter = mean.CHELSA_bio10_09 * 0.1,
    Mean_temp_warmest_quarter = mean.CHELSA_bio10_10 * 0.1,
    Mean_temp_coldest_quarter = mean.CHELSA_bio10_11 * 0.1,
    Annual_precip = mean.CHELSA_bio10_12,
    Preip_wettest_month = mean.CHELSA_bio10_13,
    Precip_driest_month = mean.CHELSA_bio10_14,
    Precip_seasonality = mean.CHELSA_bio10_15,
    Precip_wettest_quarter = mean.CHELSA_bio10_16,
    Precip_driest_quarter = mean.CHELSA_bio10_17,
    Precip_warmest_quater = mean.CHELSA_bio10_18,
    Precip_coldest_quater = mean.CHELSA_bio10_19
  ) %>% dplyr::select(20:69)

bioclim_rf <- bioclim %>% dplyr::select(CEW_sum, 32:50) %>% drop_na() %>% as_tibble()

?randomForest

rf <- randomForest(CEW_sum ~ ., data = bioclim_rf, ntree = 400,do.trace=TRUE)

varImpPlot(rf)


memory.limit()



?summarise_all
names(bioclim_year)
bioclim_year <- test2 %>% group_by(year,location) %>% dplyr::select(starts_with("mean."),year,CEW_sum) %>% 
#  mutate(pheromone = case_when(
#    CEW_sum > 0 ~ "1",
#    CEW_sum == 0 ~ "0",
#    CEW_sum == NA ~ '0')) %>%
  summarize(across(.cols=mean.CHELSA_bio10_01:mean.CHELSA_bio10_19,mean),CEW_sum = sum(CEW_sum)) %>%
  transmute(
  Annual_mean_temp = mean.CHELSA_bio10_01*.1,
  Mean_dinural_range = mean.CHELSA_bio10_02*.1,
  Isothermality = mean.CHELSA_bio10_03,
  Temp_seasonality = mean.CHELSA_bio10_04*0.01,
  Max_temp_warmest_month = mean.CHELSA_bio10_05*0.1,
  Min_temp_coldest_moth = mean.CHELSA_bio10_06 * 0.1,
  Temp_annual_range = mean.CHELSA_bio10_07 * 0.1,
  Mean_temp_wettest_quarter = mean.CHELSA_bio10_08 * 0.1,
  Mean_temp_driest_quarter = mean.CHELSA_bio10_09 * 0.1,
  Mean_temp_warmest_quarter = mean.CHELSA_bio10_10 * 0.1,
  Mean_temp_coldest_quarter = mean.CHELSA_bio10_11 * 0.1,
  Annual_precip = mean.CHELSA_bio10_12,
  Preip_wettest_month = mean.CHELSA_bio10_13,
  Precip_driest_month = mean.CHELSA_bio10_14,
  Precip_seasonality = mean.CHELSA_bio10_15,
  Precip_wettest_quarter = mean.CHELSA_bio10_16,
  Precip_driest_quarter = mean.CHELSA_bio10_17,
  Precip_warmest_quater = mean.CHELSA_bio10_18,
  Precip_coldest_quater = mean.CHELSA_bio10_19,
  CEW = CEW_sum
)


bioclim_rf_binary <- bioclim_year %>% ungroup() %>% dplyr::select(!year) %>% drop_na() %>% as_tibble()
names(bioclim_rf_binary)



rf <- randomForest(CEW ~ ., data = bioclim_rf_binary, ntree = 400,do.trace=TRUE)
varImpPlot(rf)
