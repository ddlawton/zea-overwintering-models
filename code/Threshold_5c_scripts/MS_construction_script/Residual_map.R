library(mgcv)
library(tidyverse)
library(rnaturalearth)
library(sf)

dat <- as_tibble(fread("data/processed/Threshold_5c/final_zea_data/Hzea_ow_zones_Dec172021.csv"))


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

dat4 <- dat3[paste(dat3$zone_30y,dat3$location) %in% flevels,] %>% droplevels()


mod_spatiotemp_GS <- bam(CEW_sum ~
                           s(woy,bs="cc",k=15) +
                           s(year,bs="cc",k=15) +
                           te(woy,zone_30y ,bs=c("cc","re"),k=15) +
                           te(year,zone_30y ,bs=c("gp","re"),k=15) +
                           te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                           s(location,bs="re") + s(zone_30y ,bs="re"), 
                         family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                         data=dat4) 


dat4$resid <- resid(mod_spatiotemp_GS)



US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)


dat5 <- dat4 %>%
  st_as_sf(.,coords=c("Latitude","Longitude"),crs=4326)

plot(dat5)
geom_sf(data=year30, aes(fill=multiyear_zones), lwd = 0,color = NA)  
resid_map<-ggplot()  +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="grey",color="dark grey") +
  stat_summary_hex(dat4,mapping=aes(x=Longitude,y=Latitude,z=resid),bins=75) +
  viridis::scale_fill_viridis() +
  theme_void() + 
  theme(legend.position = "bottom") + 
  theme(legend.title=element_blank())
resid_map

ggsave(resid_map,file="manuscript/figures/Threshold_5c/supplementary/supp_Figure6.png",width=22,height=10,units="cm",dpi=600,bg="white")

