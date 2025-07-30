###
# Overwinter zone
# area mapping
##

rm(list=ls())

library(tidyverse)
library(raster)
library(rnaturalearth)
library(sf)
library(scales)
library(viridis)
library(patchwork)
library(mgcv)
library(DHARMa)
library(gratia)

OW_zones <- stack("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif")
OW_zones <- dropLayer(OW_zones,42)


N_america <- ne_countries(continent = "north america") #N. america shapefile


US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)
plot(NA_map)
## crop and mask
r2 <- crop(OW_zones, extent(NA_map))
r3 <- mask(r2, NA_map)
plot(OW_zones)
areas <- list()




for(j in 1:nlayers(r3)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(r3), "\n"))
  r4 <- r3[[j]]
  areas[[j]] <- tapply(raster::area(r4), r4[], sum)

}


year_areas <- bind_rows(areas)

year_areas_percent <- year_areas %>% mutate( year = seq(1981,2021,by=1)) %>%
  mutate(`NL` = (`0`/(`0`+`1`+`2`)),
         `TZ` = (`1`/(`0`+`1`+`2`)),
         `SR` = (`2`/(`0`+`1`+`2`))) %>%
  pivot_longer(cols=c(`NL`,`TZ`,`SR`),names_to = "zone",values_to = "area") %>%
  mutate(zone= factor(zone))

year_scaled <- year_areas %>% mutate( year = seq(1981,2021,by=1)) %>%
  pivot_longer(cols=c(`1`,`2`,`0`),names_to = "zone",values_to = "area")  %>%
  group_by(zone) %>%
  mutate(scaled = scale(area)[,1]) %>%
  mutate(zone = fct_recode(zone,NL = "0", TZ = "1",SR = "2"))


years_data <- year_areas_percent %>% dplyr::select(year,zone,area) %>% left_join(year_scaled,by=c("year","zone")) %>%
  rename(area_percent = "area.x",area_value = "area.y")

write.csv(years_data,file="data/processed/Threshold_5c/OW_zone_area_change.csv")

years_data <- read_csv(file="data/processed/Threshold_5c/OW_zone_area_change.csv")

SR_expansion <- years_data %>% 
 filter(zone == "SR") %>%
  group_by(year) 


flevels<-c("NL","TZ","SR")
year_areas_percent$zone <- factor(year_areas_percent$zone,levels=flevels)
year_scaled$zone <- factor(year_scaled$zone,levels=flevels)



mod <- gam(area ~ s(year,zone,bs="fs",k=30),data=year_areas_percent,family=tw(),select=TRUE)
mod_by <- gam(area ~ s(year,zone,bs=c("ad")),data=year_areas_percent,family=betar(),select=TRUE)

draw(mod)
draw(mod_by)
AIC(mod,mod_by)
BIC(mod,mod_by)


summary(mod_by)
summary(mod)
gratia::draw(mod_by)
k.check(mod)
simresid <- simulateResiduals(mod_by)
plot(simresid)





b0 <- coef(mod)[1]

test <- gratia::smooth_estimates(mod,type='response')

test$adj_est <- test$est + b0
year_areas_percent$pred <- predict(mod,data=year_areas_percent,type = "response")


flevels <- c("NL","TZ","SR")

year_areas_percent %>%
  mutate(zone = factor(zone,levels=flevels)) %>%
  ggplot(aes(x=year,y=pred,color=zone)) +
  geom_smooth() +
  geom_point(aes(y=area)) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"),
                     labels = c("Northern Limits", "Transitional Zone", "Southern Range"))



date_trt <- test %>% #filter(smooth == "te(Date_numeric,Trt)") %>%
  ggplot(aes(x=year,group=zone)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=zone),alpha=.1) +
  geom_line(aes(y = adj_est, color=zone)) + theme_pubr() #+
  #scale_color_manual(values = c("#1b9e77","#d95f02")) +
  #scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("date by treatment smooth") + xlab("Date")




y_percentage <- ggplot(year_areas_percent,aes(x=year,y=area,color=factor(zone))) + geom_point(size=.8) +
  geom_smooth(method="gam",formula = y~s(x, bs="tp"), method.args=list("betar"),se=FALSE) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"))# + scale_y_continuous(limits = c(0,.5),breaks=c(0,0.25,0.5))

y_scaled <- ggplot(year_scaled,aes(x=year,y=scaled,color=factor(zone))) + geom_point() +
  geom_smooth(method="gam",formula = y~s(x, bs="tp"),se=FALSE) + geom_hline(yintercept = 0,color="black",linetype=2) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"))

y_percentage + y_scaled


# creating spatial change in zones raster



allSame <- function(x) length(unique(x)) == 1


ras_function <- function(x) {
  if(allSame(x) == TRUE){
    return(0)
  } else {
    return(1)
  }
}

plot(r3[[1]])
ras_function(c(2,0))

diff_stack <- list()

for(j in 1:(nlayers(r3)-1)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(r3), "\n"))
  
  tif <- r3[[j]]
  tif_2 <- r3[[j+1]]
  tif_stack <- stack(tif,tif_2)
  diff_stack[[j]] <- calc(tif_stack,fun=ras_function)
}

diff_stack2 <- do.call(stack,diff_stack)


dif_combined <- calc(diff_stack2,fun=sum)

plot(dif_combined,
     col=viridis(5,option="C"))



## crop and mask
dif_combined2 <- crop(dif_combined, extent(NA_map))
dif_combined3 <- mask(dif_combined2, NA_map)

plot(dif_combined3,
     col=viridis(5,option="B"))

writeRaster(dif_combined3,file="data/processed/Threshold_5c/GIS_data/Zone_differences.tif",overwrite=TRUE)

