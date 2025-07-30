########
# Manuscript
# Figure Construction
#
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
library(tmaptools)
# data loading
#####


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

dat4 <- dat3[paste(dat3$zone_30y,dat3$location) %in% flevels,] %>% droplevels() %>%
  mutate(trap_type = factor(trap_type))

GS <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GS.rds")
#S <- readRDS("models/Threshold_5c/zone_model/mod_spatiotemp_S.rds")
#GI <- readRDS("models/Threshold_5c/zone_model/mod_spatiotemp_GI.rds")
#I <- readRDS("models/Threshold_5c/zone_model/mod_spatiotemp_I.rds")
#G <- readRDS("models/Threshold_5c/zone_model/mod_spatiotemp_G.rds")
#N <- readRDS("models/Threshold_5c/zone_model/mod_spatiotemp_N.rds")
#Inter <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_0.rds")


dat$GS_resid <- resid(GS)

dat4 <- dat4 %>% mutate(multiyear_zones = factor(multiyear_zones), GS_pred = predict(GS,type="response"))

#####

##########
# Figures
##########

# Overwintering Raster
#####


N_america <- ne_countries(continent = "north america") #N. america shapefile


US_out <- ne_states(country="United States of America") %>% st_as_sf() %>% 
  filter(name %in% state.name) %>% filter(name != "Hawaii") %>% select(geometry)

MX_out <- ne_states(country="Mexico") %>% st_as_sf()  %>% select(geometry)


CA_out <- ne_states(country="Canada") %>%  st_as_sf() %>% filter(name != "Prince Edward Island") %>% select(geometry)


NA_map <- rbind(US_out, CA_out, MX_out)



year30 <- raster("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif")


year30_stars <- stars::st_as_stars(year30)

year30 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(year30), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(multiyear_zones = factor(Zea_over_winter_zones))



year30_graph <- ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_stars(data=year30_stars)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  scale_fill_manual(c,
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("") + ylab("Latitude") +
  theme_void() +
  # theme(panel.background = element_rect(fill = '#9ac6d7')) +
  theme(legend.title = element_blank())  + 
  #scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  #scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = "bottom")

year30_graph <- ggplot() +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data=year30, aes(fill=multiyear_zones), lwd = 0,color = NA)  +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  geom_segment(aes(x=-123,xend=-75,y=40,yend=40), color = "white",linetype="dashed",size=1) +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE) +
  xlab("") + ylab("Latitude") +
  theme_void() +
 # theme(panel.background = element_rect(fill = '#9ac6d7')) +
  theme(legend.title = element_blank())  + 
  #scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  #scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 10)) +
  theme(legend.position = "bottom")




dif_years <- raster("data/processed/Threshold_5c/GIS_data/Zone_differences.tif") #differences between zones


dif_years2 <- stars::st_as_stars(dif_years)

dif_years3 <-sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(dif_years), 
                            as_points = FALSE, merge = TRUE)
) %>% st_as_sf()


?geom_segment
diff_map <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-123,-63)) +
  scale_y_continuous(limits=c(25,50)) +
  geom_sf(data = (dif_years3),aes(fill=Zone_differences,color=Zone_differences))+
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") +
  geom_segment(aes(x=-123,xend=-75,y=40,yend=40), color = "white",linetype="dashed",size=1) +
  #geom_hline(yintercept = 40, color = "white",linetype="dashed",size=1.25) +
  scale_fill_viridis(option="magma", na.value = NA) +
  scale_color_viridis(option="magma", na.value = NA) +
  theme_void()  +
  #theme(panel.background = element_rect(fill = '#9ac6d7')) +
  theme(legend.title = element_blank()) + 
  theme(panel.grid.major = element_line(colour = "transparent")) + 
  #scale_x_continuous(breaks = c(-120,-100,-80),labels = c("120°W", "100°W", "80°W"))  + 
  #scale_y_continuous(breaks = c(30,40,50),labels = c("30°N", "40°N", "50°N")) +
  theme(text = element_text(size = 10))  +
  theme(legend.position="bottom")
diff_map





flevels <- c("NL","TZ","SR")

year_dat <- as_tibble(read.csv("data/processed/Threshold_5c/OW_zone_area_change.csv")) %>% mutate(
  zone = factor(zone,levels=flevels)
)


mod <- gam(area_percent ~ s(year,zone,bs="fs",k=30),data=year_dat,family=tw(),select=TRUE)
summary(mod)
year_dat <- year_dat %>% mutate(pred = predict(mod))

area_scaled <- ggplot(year_dat,aes(x=year,y=scaled,color=factor(zone))) +
  geom_hline(yintercept = 0,linetype=2) +
  geom_point() +
  geom_smooth(method="gam",formula = y~s(x, bs="tp"),se=FALSE) +
  ylab("scaled area change") +
  theme_pubr() +   
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                     labels = c("NL", "TZ", "SR")) +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank())


area_percent <- ggplot(year_dat,aes(x=year,y=area_percent,color=factor(zone))) +
  #coord_cartesian(ylim=c(0,NA)) +
  ylab("Total area (%)") +
  xlab("Year") +
  geom_point() +
  geom_smooth(method="gam",formula = y~s(x, bs="tp"), method.args=list("betar"),se=FALSE) +
  theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"),
                                   labels = c("Northern Limits", "Transitional Zone", "Southern Range"))+
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 15)) 


maps <- (diff_map + year30_graph) + plot_annotation(tag_levels = "A")  

areas <- ( area_percent ) + 
  plot_layout(guides='collect') & theme(legend.position = "bottom")
  
ggsave(maps,file="manuscript/figures/Threshold_5c/revision_figures/Figure2.tiff",width=19,height=7,units="cm",dpi=300,bg="white")
ggsave(areas,file="manuscript/figures/Threshold_5c/revision_figures/Supplementary/Supp_Figure_4.png",width=20,height=20,units="cm",dpi=600)


# MP4 of raster change

multiyear <-stack("data/processed/Threshold_5c/GIS_data/Zea_over_winter_zones.tif") #differences between zones

m <- multiyear[[1:41]]

#writeRaster(m,file="data/processed/overwintering/overyears_cleaned.tif")

m2 <- crop(m, (N_america))
m3 <- mask(m2, N_america)

m3[is.na(m3)] <- NA


#lakes110 <- ne_download(scale = 50, type = 'lakes', category = 'physical')

#lakes110_trnsfrmd = spTransform(lakes110,crs(m))

#crs(lakes110_trnsfrmd) == crs(m)

#N_america_erase <- erase(N_america,lakes110)

#m4 <- m3 %>% mask(N_america_erase) %>%
#  crop(N_america_erase)

years <- seq(1981,2021,by=1)
Rnames <- paste("Year",sep=" ",years)
names(m) <- Rnames




cols <- c("0" = "#1b9e77", "1" = "#d95f02", "2" = "#7570b3")
names(m)

gid_plot <- list()
for(j in 1:nlayers(m)){
cat(paste("Currently processing layer:", j,"/",nlayers(m), "\n"))
  name <- names(m[[j]])
  stars_format <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(m[[j]]), 
                              as_points = FALSE, merge = TRUE)
  ) %>% st_as_sf() %>% dplyr::mutate_at(vars(-geometry),factor)
gid_plot[[j]] <- ggplot() +
  geom_sf(data=NA_map,aes(geometry=geometry),fill="light grey",color="dark grey") +
  scale_x_continuous(limits=c(-123,-64)) +
  scale_y_continuous(limits=c(25,50)) + 
  geom_sf(data = (stars_format),aes_string(geometry="geometry",fill=(name))) + 
  geom_sf(data=NA_map,aes(geometry=geometry),fill="transparent",color="dark grey") + 
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9"),
                    na.value = "transparent",
                    labels = c("Northern Limits", "Transitional Zone", "Southern Range"),
                    na.translate = FALSE)+
  theme_void() +
  theme(legend.title = element_blank(),text=element_text(size=30)) + ggtitle(paste("Year",str_sub(names(m[[j]]),-4,-1),sep=" "))
   
}




lapply(1:41, function(i)
  ggsave(filename=paste0("manuscript/figures/Threshold_5c/supplementary/OW_plots/",i,".png"),
         plot=gid_plot[[i]],height=10,width = 17,dpi=600))

library(gifski)


png_files <- list.files("manuscript/figures/Threshold_5c/supplementary/OW_plots/", pattern = ".*png$", full.names = TRUE)
files <- gtools::mixedsort(png_files)
gifski(files, gif_file = "manuscript/figures/Threshold_5c/supplementary/Supplementary_Figure_1.gif", width = 1200, height = 600, delay = 0.75)

imgs %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_write_video("manuscript/figures/Threshold_5c/supplementary/Supplementary_Figure_1.mp4",framerate = 2.5)

#####

# Database plotting
#####

US_sh <- (ne_states(country="United States of America")) %>% st_as_sf() %>%
  dplyr::filter(name !="Alaska") %>% dplyr::filter(name !="Hawaii") %>% st_union() 

CA_sh <- st_as_sf(ne_states(country="Canada")) %>% st_union() %>% st_as_sf()


  
NA_bind <- rbind(US_sh, CA_sh) %>% st_as_sf()

dat4 %>% filter(contact == "William Hutchison")
location_dat <- dat4 %>% group_by(location) %>%
  summarize(CEW_sum = mean(CEW_sum), longitude = first(longitude),latitude = first(latitude),
            zone_30y = first(zone_30y), state=first(state) )



global <- ggplot(NA_map,aes(geometry=geometry)) + geom_sf() +
    geom_point(data=(location_dat),aes(x=longitude,y=latitude,color=zone_30y),inherit.aes = F,size=1) + 
  theme_void() +
  scale_x_continuous(limits=c(-123,-64)) +
  scale_y_continuous(limits=c(25,50)) + 
  scale_color_manual(values = c("#49555d","#ffc02c","#0092d9"),
                     labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank()) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm")) +  theme(legend.position = "bottom")

?ggMarginal
ggMarginal(global, groupColour = TRUE, groupFill = TRUE)


ggsave(ggMarginal(global, groupColour = TRUE, groupFill = TRUE),file="manuscript/figures/Threshold_5c/supplementary/Figure2_map.png",width=19,height=12.10459,units="cm",dpi=300)


#####


# Raw trends
#####

year_global <- ggplot(dat4,aes(x=year,y=(CEW_sum))) + geom_smooth() + ylab("CEW count") + 
  xlab("") + coord_cartesian(ylim=c(0,75)) + theme_pubr() +
  theme(legend.title = element_blank())

woy_global <- ggplot(dat4,aes(x=woy,y=(CEW_sum))) + geom_smooth() + ylab("") + 
  xlab("") + coord_cartesian(ylim=c(0,75)) + theme_pubr() +
  theme(legend.title = element_blank())

year_zone_raw <- ggplot(dat4,aes(x=year,y=(CEW_sum), color=zone_30y))  + geom_point(aes(y = 0.1), alpha = 0) +
  geom_smooth(linetype="dashed") + 
  ylab("CEW count") + xlab("") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), 
                     labels = c("NL", "TZ", "SR")) + coord_cartesian(ylim=c(0,NA)) +
  geom_smooth(data=dat4, aes(x=year,y=(CEW_sum)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())




woy_zone_raw <- ggplot(dat4,aes(x=woy,y=(CEW_sum), color=zone_30y)) + geom_smooth(linetype="dashed") + ylab("") +
  xlab("") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), labels = c("NL", "TZ", "SR"))   +
  geom_smooth(data=dat4, aes(x=woy,y=(CEW_sum)), inherit.aes = FALSE, se=FALSE,color="black") + 
  coord_cartesian(ylim=c(0,300)) + ylab("") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())


ggsave(year_global,file="manuscript/figures/year_global.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_global,file="manuscript/figures/woy_global.png",width=5,height=5,units="in",dpi=300)
ggsave(year_zone_raw,file="manuscript/figures/year_zone_raw.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_zone_raw,file="manuscript/figures/woy_zone_raw.png",width=5,height=5,units="in",dpi=300)

pw1 <- (year_global + woy_global) / (year_zone + woy_zone) + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")

ggsave(pw1,file="manuscript/figures/raw_trends_combined.png",width=10,height=10,units="in",dpi=300)

#####

#modeled results
#####
dat4 <- dat4 %>% mutate( zone_30y = factor(zone_30y))

year_global_GI <- ggplot(dat4,aes(x=year,y=(GS_pred))) + geom_smooth(linetype="solid",data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("Modeled weekly catch") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme_pubr() +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 30)) + theme(legend.position="none") + xlab("Year")

woy_global_GI <- ggplot(dat4,aes(x=woy,y=(GS_pred))) + geom_smooth(linetype="solid",data=dat4, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("Modeled weekly catch") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme_pubr() +
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 30))  + theme(legend.position="none") + xlab("Week of Year")


year_zone_GI <- ggplot(dat4,aes(x=year,y=(GS_pred), color=zone_30y))  + geom_point(aes(y = 0.1), alpha = 0) +
  geom_smooth(linetype="dashed",se=TRUE) + 
  ylab("Model Predictions") + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), 
                     labels = c("Northern Limits", "Transitional Zone", "Southern Range")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 30)) + theme(legend.position="none")

woy_zone_GI <- ggplot(dat4,aes(x=woy,y=(GS_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + ylab("") +
  xlab("Week of Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +
  geom_smooth(linetype="solid",data=dat4, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("Modeled weekly catch") +
  guides(colour = guide_legend(override.aes = list(size = 4,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 7)) + theme(legend.position="none")


year_zone_GI <- ggplot(dat4,aes(x=year,y=(GS_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + 
  ylab("Modeled weekly catch") + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), 
                     labels = c("Northern Limits", "Transitional Zone", "Southern Range")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 4,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size =7)) 




ggsave(year_global_GI,file="presentations/yearly_global.png",width=7.97,height=5.94,units="in",dpi=600)
ggsave(woy_global_GI,file="presentations/weekly_global.png",width=7.97,height=5.94,units="in",dpi=600)

ggsave(year_zone_GI,file="presentations/yearly.png",width=7.97,height=5.94,units="in",dpi=600)
ggsave(woy_zone_GI,file="presentations/weekly.png",width=7.97,height=5.94,units="in",dpi=600)

ggsave(year_global_GI,file="manuscript/figures/GS_mod/year_global_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_global_GS,file="manuscript/figures/GS_mod/woy_global_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(year_zone_GS,file="manuscript/figures/GS_mod/year_zone_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_zone_GS,file="manuscript/figures/GS_mod/woy_zone_GS.png",width=5,height=5,units="in",dpi=300)

pw1_GI <- (year_zone_GI / woy_zone_GI)  + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom") 

ggsave(pw1_GI,file="manuscript/figures/Threshold_5c/revision_figures/Figure3.tiff",width=8.64809,height=15,units="cm",dpi=300)

levels(factor(dat4$contact))

dat4 %>%
  filter(contact == "Izailda Barbosa dos Santos")



#####

 
#####
# Peak predicting
#####

OW_points <- ggplot(dat4,aes(x=woy,y=GS_pred,color=zone_30y)) + geom_smooth() +
  coord_cartesian(xlim=c(30,40)) +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3")) + 
  geom_vline(xintercept = 32,color="black",linetype=2)+
  geom_vline(xintercept = 33,color="black",linetype=2)+
  geom_vline(xintercept = 35,color="black",linetype=2)+
  geom_vline(xintercept = 36,color="black",linetype=2) + ggpubr::theme_pubr() +
  scale_x_continuous(limits = c(30,40),breaks=c(30,32,34,36,38,40)) +
  ylab(expression(paste("Modeled weekly"," ",italic("Helicoverpa zea")," ", "count"))) + xlab("Week of year") + theme(legend.title = element_blank()) +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))
OW_points



NL <- dat4 %>% filter(zone_30y == "0") %>% filter(between(woy,32,33)) %>%
  group_by(year) %>% summarize(GS_pred = sum(GS_pred),NL_sum = sum(CEW_sum ))

TZ <- dat4 %>% filter(zone_30y == "1") %>% filter(between(woy,35,36)) %>%
  group_by(year)  %>% summarize(GS_pred = sum(GS_pred ),TZ_sum = sum(CEW_sum ))

SR <- dat4 %>% filter(zone_30y == "2") %>% filter(between(woy,35,36)) %>%
  group_by(year)  %>% summarize(GS_pred = sum(GS_pred ),SR_sum = sum(CEW_sum ))

peaks <- NL %>% left_join(TZ, by="year") %>% left_join(SR, by="year") %>%
  mutate(total_pop = TZ_sum + SR_sum) %>% drop_na()


NL_SR <- cor.test(peaks$NL_sum,peaks$SR_sum,method="pearson")
NL_TZ <- cor.test(peaks$NL_sum,peaks$TZ_sum,method="pearson")
TZ_SR <- cor.test(peaks$TZ_sum,peaks$SR_sum,method="pearson")

lb1 <- paste0("Rho == ",round(NL_SR$estimate,digits=2))
lb1.5 <- paste0("p-value: ",round(NL_SR$p.value,digits=2))
SR_NL <- ggplot(peaks,aes(x=SR_sum,y=NL_sum)) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr() +
  xlab("SR yearly peak count")  + ylab("NL yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 30000, x= 8000, label=lb1, parse = TRUE)+
  theme(text = element_text(size = 12)) # +
  #annotate("text", y = 29000, x= 20000, label=lb1.5)

lb2 <- paste0("Rho == ",round(NL_TZ$estimate,digits=2))
lb2.5 <- paste0("p-value = ",round(NL_TZ$p.value,digits=4))
TZ_NL <- ggplot(peaks,aes(x=TZ_sum,y=NL_sum)) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr()  +
  xlab("TZ yearly peak count")  + ylab("NL yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 30000, x= 8000, label=lb2, parse = TRUE)+
  theme(text = element_text(size = 12)) #+
 # annotate("text", y = 30000, x= 20000, label="p-value: < 0.01")


lb3 <- paste0("Rho == ",round(TZ_SR$estimate,digits=2))
lb3.5 <- paste0("p-value = ",round(TZ_SR$p.value,digits=2))
SR_TZ <- ggplot(peaks,aes(x=(SR_sum),y=(TZ_sum))) + geom_smooth(method = 'lm') + geom_point() + ggpubr::theme_pubr()  +
  xlab("SR yearly peak count")  + ylab("TZ yearly peak count") + 
  scale_y_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  scale_x_continuous(limits=c(0,30000),breaks = scales::pretty_breaks(n=3)) +
  annotate("text", y = 30000, x= 8000, label=lb3, parse = TRUE)+
  theme(text = element_text(size = 12)) #+
  #annotate("text", y = 30000, x= 20000, label="p-value: < 0.01")

week_predictions <-(SR_TZ + SR_NL + TZ_NL) + plot_annotation(tag_levels = 'A')

ggsave(week_predictions,file="manuscript/figures/Threshold_5c/peak_predicting_supplementary_figure.png",dpi=600,width = 15,height=10,units="in")

 
pw1_GI <- (year_zone_GI + woy_zone_GI)  / (SR_TZ + SR_NL + TZ_NL) +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom") 

ggsave(pw1_GI,file="manuscript/figures/Threshold_5c/Figure2.eps",width=20,height=20,units="cm",dpi=600)

 
 
##########
# Tables
##########

# Model summaries
#####





#Descriptive Statistics Table if wanted
#####

des_dat <- dat4 %>% dplyr::select(location, year,woy,CEW_sum,trap_type,zone_30y)


variable <- c("year","woy","CEW_sum")


CEW_sum_dat <- des_dat %>% 
  summarize(CEW_sum_N = length(CEW_sum),
            CEW_sum_mean = mean(CEW_sum),
            CEW_sum_min = min(CEW_sum),
            CEW_sum_25perc = quantile(CEW_sum,.25),
            CEW_sum_75perc = quantile(CEW_sum,.75),
            CEW_sum_max = max(CEW_sum))

year <- des_dat %>% 
  summarize(year_N = length(year),
            year_mean = mean(year),
            year_min = min(year),
            year_25perc = quantile(year,.25),
            year_75perc = quantile(year,.75),
            year_max = max(year))

woy <- des_dat %>% 
  summarize(woy_N = length(woy),
            woy_mean = mean(woy),
            woy_min = min(woy),
            woy_25perc = quantile(woy,.25),
            woy_75perc = quantile(woy,.75),
            woy_max = max(woy))

des_dat2 <- cbind(CEW_sum_dat,year,woy) %>%
  pivot_longer(cols=everything(),
               names_to = c("model",".value"), 
               names_pattern = "(.*)_(.*)") %>%
  rename(`25 percentile` = "25perc",
         `75 percentile` = "75perc") %>%
  mutate(mean = round(mean,3))


write.csv(des_dat2,file="manuscript/tables/descriptive_stats.csv")





#####




#Model selection criteria table

# OOS deviance by overwinter zones
#####
flevels <- c("N","G","I","GI","S","GS")


OW_OOS <- round(as_tibble(read.csv("models/pop_dynamics/Zone_curves/OW_OOS_model_selection.csv")),3) %>%
  as.data.frame()%>%
  pivot_longer(cols=c(starts_with("deviance_mean"),starts_with("deviance_std")),
               names_to = c(".value", "model"), 
               names_pattern = "(.*)_(.*)") %>%
  dplyr::select(!X) %>% dplyr::select(!deviance_std)%>%
  pivot_wider(names_from = zone_30y,values_from = deviance_mean) %>%
  rename(Model = "model",`Northern Limits` = "0",`Transitional Zone` = "1",`Southern Range` = "2") %>%
  filter(Model != "0") %>%
  mutate(Model = factor(Model,levels=flevels)) %>% arrange(Model)


write.csv(OW_OOS,"manuscript/tables/OW_OOS.csv")

## Overall model selection

model_selection <- as_tibble(read.csv("models/pop_dynamics/Zone_curves/kfold_model_selection.csv")) %>%
  dplyr::select(models,ends_with("_mean")) %>% arrange("RSQ_mean") %>% filter(models != "intercept") %>%
  mutate(models = factor(models,levels=flevels)) %>% arrange(models)


AIC <- AIC(GS,S,GI,I,G,N) 
BIC <- BIC(GS,S,GI,I,G,N)


AIC_table <- AIC(GS,S,GI,
                 I,G,N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  mutate_at(.vars = vars(df,AIC, deltaAIC), 
            .funs = funs(round,.args = list(digits=0)))

BIC_table <- BIC(GS,S,GI,
                 I,G,N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaBIC = BIC - min(BIC))%>%
  mutate_at(.vars = vars(df,BIC, deltaBIC), 
            .funs = funs(round,.args = list(digits=0))) %>% left_join(AIC_table,by="Model") %>%
  rename(`BIC DF` = "df.x", `AIC DF` = "df.y",models = "Model") %>%
  as_tibble() %>%
  mutate(models = factor(models,levels = flevels)) %>%
  left_join(model_selection,by="models") %>% arrange(models) %>% 
  mutate(deltaOOS = round((OOS_mean - min(OOS_mean)),3),
         deltaRMSE = round((RMSE_mean - min(RMSE_mean)),3),
         deltaRSQ = round((RSQ_mean - max(RSQ_mean)),3))  %>%
  dplyr::select(!ends_with("DF"))

write.csv(BIC_table,"manuscript/tables/model_selection.csv")



BIC_selection <- BIC_table %>% dplyr::select(models,deltaBIC) %>% arrange(deltaBIC) %>% #select(models) %>%
  rename(BIC = "models")
AIC_selection <- BIC_table %>% dplyr::select(models,deltaAIC) %>% arrange(deltaAIC)%>% #select(models) %>%
  rename(AIC = "models")
OOS_selection <- BIC_table %>% dplyr::select(models,deltaOOS) %>% arrange(deltaOOS)%>% #select(models) %>%
  rename(OOS = "models")
RMSE_selection <- BIC_table %>% dplyr::select(models,deltaRMSE) %>% arrange(deltaRMSE)%>% #select(models) %>%
  rename(RMSE = "models")
RSQ_selection <- BIC_table %>% dplyr::select(models,deltaRSQ) %>% arrange(desc(deltaRSQ))%>% #select(models) %>%
  rename(RSQ = "models")

selection <- cbind(BIC_selection,AIC_selection,OOS_selection,RMSE_selection,RSQ_selection) %>% 
  rowwise() 



write.csv(selection,file="manuscript/tables/model_selection_ranking.csv")

#####

# Supplementary Figures
#####
# Overwinter MP4 is with the raster maps section above

# Descriptive Statistics

independent_viz$zone_30y
independent_viz <- dat4  %>%
  mutate(zone_30y = factor(zone_30y, levels=c("0","1","2"))) %>%
  rename(`week of year` = "woy", `H. zea count` = "CEW_sum") %>%
  dplyr::select(`H. zea count`,year,`week of year`,zone_30y) %>%
  mutate(`log H. zea count` = log1p(`H. zea count`)) %>%
  pivot_longer(names_to = "x", values_to="y", cols=c(1:3,5)) %>%
  ggplot(aes(x = y, y = zone_30y, color = zone_30y, fill = zone_30y)) +
  facet_wrap( ~ x, scale = "free", ncol = 2) +   
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9")) +
  scale_fill_manual(values=c("#49555d","#ffc02c","#0092d9")) +
  scale_y_discrete(labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE) +
  ylab("") +
  xlab("") + ggpubr::theme_pubr()

ggsave(independent_viz,file="manuscript/figures/Threshold_5c/supplementary/Supp_Figure_3.png",width=10,height=10,units="in",dpi=300)


# Diagnostic plots (I decided to omit this)

GS <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GS.rds")
S <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_S.rds")
GI <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GI.rds")
I <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_I.rds")
G <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_G.rds")
N <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_N.rds")


library(DHARMa)

GS_sim_resid <- simulateResiduals(GS)
S_sim_resid <- simulateResiduals(S)
GI_sim_resid <- simulateResiduals(GI)
I_sim_resid <- simulateResiduals(I)
G_sim_resid <- simulateResiduals(I)
N_sim_resid <- simulateResiduals(N)

library(ggplotify)



plot(GS_sim_resid) + plot(S_sim_resid)

png(filename = "DHARMA.png",plot(GS_sim_resid))

test <- plot(GS_sim_resid)
ggsave(test,file="DHARMA.png")

GS_plot <- appraise(GS)
S_plot <- appraise(S)
GI_plot <- appraise(GI)
I_plot <- appraise(I)
G_plot <-  appraise(G)
N_plot <- appraise(N)



appraise_plots <- (GS_plot + S_plot) /
  (GI_plot + I_plot) /
  (G_plot + N_plot) +
  plot_annotation(tag_levels = "A")
  

ggsave(appraise_plots,file="manuscript/figures/supplementary/diagnostic_plots.png",width=7.5,height=10.5)


#Residual map
NA_sf <- st_as_sf(NA_bind)
NA_sp <- as(NA_bind, 'Spatial')

resid_map <- dat %>% mutate(GS_resid = resid(GS)) %>%
  dplyr::select(latitude,longitude,GS_resid) %>% 
  ggplot(aes(y=latitude,x=longitude,z=GS_resid))  +
  geom_polygon(data=NA_sp, aes(x=long, y=lat, group=group),fill="grey", color="grey50", size=0.25,inherit.aes = FALSE) +
  #stat_binhex() +
  stat_summary_hex(bins=100) +
  scale_fill_viridis() +
  theme_pubr()   + coord_cartesian(ylim=c(25,55),xlim=c(-125,-60))
?coord_map
ggsave(resid_map,file="manuscript/figures/Threshold_5c/supplementary/Supp_Figure_6.png",width=10,height=10,dpi=300)

#space time tensor

plot.gam(GS)

#####

# Supplementary tables
#####

# K checks
GS_k.check_table <-round(k.check(GS),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf)%>%
  mutate(model = "GS")

S_k.check_table <-round(k.check(S),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf) %>%
  mutate(model = "S")

GI_k.check_table <-round(k.check(GI),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf) %>%
  mutate(model = "GI")

I_k.check_table <-round(k.check(I),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf)%>%
  mutate(model = "I")

G_k.check_table <-round(k.check(G),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf)%>%
  mutate(model = "G")

N_k.check_table <-round(k.check(N),2) %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  rename(`Model term` = rowname,
         EDF = edf)%>%
  mutate(model = "N")


k_checks <- rbind(GS_k.check_table,S_k.check_table,GI_k.check_table,I_k.check_table,
                  G_k.check_table,N_k.check_table)

GS_S <- k_checks %>% filter(model == "GS" |model == "S" )

write.csv(k_checks,file="manuscript/tables/Threshold_5c/k_checks.csv")

#Model Summaries 

GI_summary <- read.csv("manuscript/tables/Threshold_5c/model_GI_results.csv")

mod_spatiotemp_GI_summary <- summary(GI) 
mod_spatiotemp_G_summary <- summary(G)
mod_spatiotemp_N_summary <- summary(N) 
mod_spatiotemp_I_summary <- summary(I)


GI_list <- rbind(mod_spatiotemp_GI_summary$dev.expl,mod_spatiotemp_GI_summary$r.sq,mod_spatiotemp_GI_summary$p.table,mod_spatiotemp_GI_summary$s.table)
G_list <- rbind(mod_spatiotemp_G_summary$dev.expl,mod_spatiotemp_G_summary$r.sq,mod_spatiotemp_G_summary$p.table,mod_spatiotemp_G_summary$s.table)
N_list <- rbind(mod_spatiotemp_N_summary$dev.expl,mod_spatiotemp_N_summary$r.sq,mod_spatiotemp_N_summary$p.table,mod_spatiotemp_N_summary$s.table)
I_list <- rbind(mod_spatiotemp_I_summary$dev.expl,mod_spatiotemp_I_summary$r.sq,mod_spatiotemp_I_summary$p.table,mod_spatiotemp_I_summary$s.table)


write.csv(GI_list,file="manuscript/tables/supplementary/model_results/model_GI.csv")
write.csv(G_list,file="manuscript/tables/supplementary/model_results/model_G.csv")
write.csv(N_list,file="manuscript/tables/supplementary/model_results/model_N.csv")
write.csv(I_list,file="manuscript/tables/supplementary/model_results/model_I.csv")

#####