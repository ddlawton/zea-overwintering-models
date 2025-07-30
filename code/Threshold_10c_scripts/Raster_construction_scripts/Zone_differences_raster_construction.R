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

OW_zones <- stack("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif")
OW_zones <- dropLayer(OW_zones,42)

NA_sh <- st_as_sf(ne_states(country="United States of America")) %>% filter(name !="Alaska") %>% filter(name !="Hawaii")


## crop and mask
r2 <- crop(OW_zones, extent(NA_sh))
r3 <- mask(r2, NA_sh)
plot(r3)
areas <- list()

for(j in 1:nlayers(r3)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(r3), "\n"))
  r4 <- r3[[j]]
  areas[[j]] <- tapply(area(r4), r4[], sum)

}


year_areas <- bind_rows(areas)

year_areas_percent <- year_areas %>% mutate( year = seq(1981,2021,by=1)) %>%
  mutate(`NL` = (`0`/(`0`+`1`+`2`)),
         `TZ` = (`1`/(`0`+`1`+`2`)),
         `SR` = (`2`/(`0`+`1`+`2`))) %>%
  pivot_longer(cols=c(`NL`,`TZ`,`SR`),names_to = "zone",values_to = "area")  

year_scaled <- year_areas %>% mutate( year = seq(1981,2021,by=1)) %>%
  pivot_longer(cols=c(`1`,`2`,`0`),names_to = "zone",values_to = "area")  %>%
  group_by(zone) %>%
  mutate(scaled = scale(area)[,1]) %>%
  mutate(zone = fct_recode(zone,NL = "0", TZ = "1",SR = "2"))


years_data <- year_areas_percent %>% dplyr::select(year,zone,area) %>% left_join(year_scaled,by=c("year","zone")) %>%
  rename(area_percent = "area.x",area_value = "area.y")

write.csv(years_data,file="data/processed/overwintering/area_change_data.csv")


flevels<-c("NL","TZ","SR")
year_areas_percent$zone <- factor(year_areas_percent$zone,levels=flevels)
year_scaled$zone <- factor(year_scaled$zone,levels=flevels)
y_percentage <- ggplot(year_areas_percent,aes(x=year,y=area,color=factor(zone))) + geom_point() +
  geom_smooth(se=FALSE) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"))

y_scaled <- ggplot(year_scaled,aes(x=year,y=scaled,color=factor(zone))) + geom_point() +
  geom_smooth(se=FALSE) + geom_hline(yintercept = 0,color="black",linetype=2) +
  ggpubr::theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"))

y_percentage + y_scaled


year_scaled$zone <- factor(year_scaled$zone)

mod <- gam(scaled ~ te(year,by=zone,bs="tp"),data=year_scaled,select=TRUE)
mod_lm <- gam(scaled ~ (year) + zone,data=year_scaled,select=TRUE)


gratia::draw(mod)
gratia::draw(mod_lm,all.terms=TRUE)
gratia::appraise(mod)
k.check(mod)
summary(mod)
summary(mod_lm)
concurvity(mod,full = TRUE)
?geom_hline
ggplot(year_areas,aes(x=year,y=area,color=zone)) + geom_point() + #geom_line() +
  geom_smooth(se=FALSE,size=.5) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) +
  scale_y_continuous(limits=c(0,1),breaks = c(0,.25,.5,.75,1)) +
  ggpubr::theme_pubr()



allSame <- function(x) length(unique(x)) == 1


ras_function <- function(x) {
  if(allSame(x) == TRUE){
    return(0)
  } else {
    return(1)
  }
}


ras_function(c(2,0))

diff_stack <- list()

for(j in 1:(nlayers(m)-1)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(m), "\n"))
  
  tif <- m[[j]]
  tif_2 <- m[[j+1]]
  tif_stack <- stack(tif,tif_2)
  diff_stack[[j]] <- calc(tif_stack,fun=ras_function)
}

diff_stack2 <- do.call(stack,diff_stack)


dif_combined <- calc(diff_stack2,fun=sum)

plot(dif_combined,
     col=viridis(5,option="C"))

NA_sh <- st_as_sf(ne_states(country="United States of America")) %>% filter(name !="Alaska") %>% filter(name !="Hawaii")


## crop and mask
dif_combined2 <- crop(dif_combined, extent(NA_sh))
dif_combined3 <- mask(dif_combined2, NA_sh)

plot(dif_combined3,
     col=viridis(5,option="C"))

writeRaster(dif_combined3,file="data/processed/overwintering/Zone_differences.tif")

