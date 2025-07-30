########
# Manuscript
# Figure Construction
#
#######
rm(list=ls())

# libraries
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
dat <- as_tibble(fread("data/raw/National_data_combined/National_dat_Oct13.csv"))

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

GS <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GS.rds")
S <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_S.rds")
GI <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_GI.rds")
I <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_I.rds")
G <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_G.rds")
N <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_N.rds")
Inter <- readRDS("models/pop_dynamics/Zone_curves/mod_spatiotemp_0.rds")

dat4 <- dat4 %>% mutate(GS_pred = predict(GS,type="response"),
                        GI_pred = predict(GI,type="response"))
#####

##########
# Figures
##########

# Overwintering Raster
#####


N_america <- ne_countries(continent = "north america") #N. america shapefile

year30 <- raster("data/processed/overwintering/multiyear_zones.tif")

zone1 <- c(1,2)
year30[year30 > 283] <- sample(zone1, length(year30[year30 > 283]),replace=TRUE)

zone2 <- c(0,1)

year30[year30 > 2] <- sample(zone2, length(year30[year30 > 2]),replace=TRUE) # 30 year averaged map

year30[is.nan(year30)] <- NA


plot(year30)



year30 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(year30), 
                                        as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% mutate(multiyear_zones = factor(multiyear_zones))

year30_graph <- ggplot() +  
  geom_sf(data=year30, aes(fill=multiyear_zones), lwd = 0,color = NA)  +
  scale_fill_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                    na.value = "transparent",
                    labels = c("NL", "TZ", "SR"),
                    na.translate = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() +
  theme(legend.title = element_blank())


dif_years <- raster("data/processed/overwintering/Zone_differences.tif") #differences between zones


dif_years2 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(dif_years), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() 

diff_map <- ggplot(dif_years2) +  
  geom_sf(aes(fill=(Zone_differences)), lwd = 0,color = NA) + 
  scale_fill_viridis(option="magma", na.value = "white") +
  ggpubr::theme_pubr() +
  xlab("Longitude") + ylab("Latitude") +
  theme(legend.title = element_blank())




flevels <- c("NL","TZ","SR")

year_dat <- as_tibble(read.csv("data/processed/overwintering/area_change_data.csv")) %>% mutate(
  zone = factor(zone,levels=flevels)
)

area_scaled <- ggplot(year_dat,aes(x=year,y=scaled,color=factor(zone))) +
  geom_hline(yintercept = 0,linetype=2) +
  geom_point() +
  geom_smooth(se=FALSE) +
  ylab("scaled area change") +
  theme_pubr() +   
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                     labels = c("NL", "TZ", "SR")) +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank())


area_percent <- ggplot(year_dat,aes(x=year,y=area_percent,color=factor(zone))) +
  ylim(0,1) +
  ylab("total area (%)") +
  xlab("") +
  geom_point() +
  geom_smooth(se=FALSE) +
  theme_pubr() +  
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"),
                     labels = c("NL", "TZ", "SR")) +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank())





(LT_map  + diff_map) / (area_scaled + area_percent) 

maps <- (year30_graph  / diff_map) + plot_annotation(tag_levels = "A")  +
  plot_layout(guides='collect') & theme(legend.position = "bottom")

areas <- ( area_percent / area_scaled) + plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom")
  
ggsave(maps,file="manuscript/figures/Maps/maps.png",width=10,height=10,units="in",dpi=300)
ggsave(areas,file="manuscript/figures/Maps/areas.png",width=5,height=10,units="in",dpi=300)


# MP4 of raster change

mulityear <- stack("data/processed/overwintering/multiyear_zones.tif") #differences between zones
zone1 <- c(1,2)
zone2 <- c(0,1)
tif <- list()

for(j in 1:nlayers(mulityear)){
  
  cat(paste("Currently processing layer:", j,"/",nlayers(mulityear), "\n"))
  
  tif[[j]] <- mulityear[[j]]
  tif[[j]][tif[[j]] > 283] <- sample(zone1, length(tif[[j]][tif[[j]] > 283]),replace=TRUE)
  
  
  tif[[j]][tif[[j]] > 2] <- sample(zone2, tif[[j]][tif[[j]] > 2],replace=TRUE)
  
}

m <- do.call(stack, tif)
m <- m[[1:41]]

#writeRaster(m,file="data/processed/overwintering/overyears_cleaned.tif")

m2 <- crop(m, (N_america))
m3 <- mask(m2, N_america)

m3[is.na(m3)] <- NA


lakes110 <- ne_download(scale = 50, type = 'lakes', category = 'physical')

lakes110_trnsfrmd = spTransform(lakes110,crs(m))

crs(lakes110_trnsfrmd) == crs(m)

N_america_erase <- erase(N_america,lakes110)

m4 <- m3 %>% mask(N_america_erase) %>%
  crop(N_america_erase)

years <- seq(1981,2021,by=1)
Rnames <- paste("Year",sep=" ",years)
names(m4) <- Rnames


cols <- c("0" = "#1b9e77", "1" = "#d95f02", "2" = "#7570b3")


gid_plot <- list()
for(j in 1:nlayers(m4)){
  
cat(paste("Currently processing layer:", j,"/",nlayers(mulityear), "\n"))
  
gid_plot[[j]] <- gplot(m4[[j]]) + 
    geom_raster(aes(fill = factor(value))) +
    scale_fill_manual(breaks = c("0","1","2"),
                      values=cols,
                      na.value = "transparent",
                      labels = c("NL", "TZ", "SR"),
                      na.translate = FALSE) + 
    geom_polygon(data=N_america2, aes(x=long, y=lat, group=group), 
                 fill=NA, color="grey50", size=0.25)  +
    xlab("Longitude") + ylab("Latitude") +
    coord_equal() +
    theme_pubr() +
    theme(legend.title = element_blank()) + ggtitle(paste("Year",str_sub(names(m4[[j]]),-4,-1),sep=" "))
   
}

gid_plot[[1]]


lapply(1:41, function(i)
  ggsave(filename=paste0("manuscript/figures/supplementary/OW_gif/plots/",i,".png"),
         plot=gid_plot[[i]],width=10,height=10,units="in",dpi=300))

imgs <- paste("manuscript/figures/supplementary/OW_gif/plots/", 1:41, ".png", sep = "")

imgs %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_write_video("manuscript/figures/supplementary/OW_gif/OW_video.mp4",framerate = 2.5)

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

global <- ggplot(NA_bind,aes(geometry=x)) + geom_sf() +
    geom_point(data=(location_dat),aes(x=longitude,y=latitude,color=zone_30y),inherit.aes = F,size=1) + 
  theme_pubr() +
  ylim(25,52) + xlim(-130,-70)  +
  scale_color_manual(values = c("#66c2a5","#fc8d62","#8da0cb"),
                     labels = c("NL", "TZ", "SR")) +
  xlab("Longitude") + ylab("Latitude") +
  guides(colour = guide_legend(override.aes = list(size = 10)))+
  theme(legend.title = element_blank()) + theme(plot.margin=grid::unit(c(0,0,0,0), "mm"))

?ggMarginal
ggMarginal(global, groupColour = TRUE, groupFill = TRUE)


ggsave(ggMarginal(global, groupColour = TRUE, groupFill = TRUE),file="manuscript/figures/Maps/global_point.png",width=10,height=10,units="in",dpi=300)


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
year_global_GI <- ggplot(dat4,aes(x=year,y=(GI_pred))) + geom_smooth() + ylab("CEW count") + 
  xlab("") + coord_cartesian(ylim=c(0,300)) + theme_pubr() +
  theme(legend.title = element_blank())

woy_global_GI <- ggplot(dat4,aes(x=woy,y=(GI_pred))) + geom_smooth() + ylab("") + 
  xlab("")+ coord_cartesian(ylim=c(0,300)) + theme_pubr() +
  theme(legend.title = element_blank())

year_zone_GI <- ggplot(dat4,aes(x=year,y=(GI_pred), color=zone_30y))  + geom_point(aes(y = 0.1), alpha = 0) +
  geom_smooth(linetype="dashed",se=TRUE) + 
  ylab("Model Predictions") + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), 
                     labels = c("NL", "TZ", "SR")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GI_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())

woy_zone_GI <- ggplot(dat4,aes(x=woy,y=(GI_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + ylab("") +
  xlab("Week of year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), labels = c("NL", "TZ", "SR"))  +
  geom_smooth(data=dat4, aes(x=woy,y=(GI_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("")+
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())


year_zone_GI <- ggplot(dat4,aes(x=year,y=(GI_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + 
  ylab(expression(paste("Modeled weekly"," ",italic("Helicoverpa zea")," ", "count"))) + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#1b9e77","#d95f02","#7570b3"), 
                     labels = c("NL", "TZ", "SR")) + coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GI_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank())








ggsave(year_global_GS,file="manuscript/figures/GS_mod/year_global_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_global_GS,file="manuscript/figures/GS_mod/woy_global_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(year_zone_GS,file="manuscript/figures/GS_mod/year_zone_GS.png",width=5,height=5,units="in",dpi=300)
ggsave(woy_zone_GS,file="manuscript/figures/GS_mod/woy_zone_GS.png",width=5,height=5,units="in",dpi=300)

pw1_GI <- (year_zone_GI + woy_zone_GI)  + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom") 

 ggsave(pw1_GI,file="manuscript/figures/GI_mod/GI_combined.png",width=10,height=5,units="in",dpi=300)

 

#####

 
#####
# Weekly predictiosn
#####
 
 
 
 
 
 
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
  mutate(zone_30y = case_when(
           zone_30y == 0 ~ "NL",
           zone_30y == 1 ~ "TZ",
           zone_30y == 2 ~ "SR"),
         zone_30y = factor(zone_30y, levels=c("SR","TZ","NL"))) %>%
  rename(`week of year` = "woy", `CEW count` = "CEW_sum") %>%
  dplyr::select(`CEW count`,year,`week of year`,zone_30y) %>%
  mutate(`log CEW count` = log(`CEW count`+1)) %>%
  pivot_longer(names_to = "x", values_to="y", cols=c(1:3,5)) %>%
  ggplot(aes(x = y, y = zone_30y, color = zone_30y, fill = zone_30y)) +
  facet_wrap( ~ x, scale = "free", ncol = 2) +   
  scale_color_manual(values=c("#7570b3","#d95f02","#1b9e77")) +
  scale_fill_manual(values=c("#7570b3","#d95f02","#1b9e77")) +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE) +
  ylab("overwintering zones") +
  xlab("") + ggpubr::theme_pubr()

ggsave(independent_viz,file="manuscript/figures/supplementary/data_distributions.png",width=10,height=10,units="in",dpi=300)


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

resid_map <- dat4 %>% mutate(GI_resid = resid(GI)) %>%
  dplyr::select(latitude,longitude,GI_resid) %>% 
  ggplot(aes(y=latitude,x=longitude,z=GI_resid))  +
  geom_polygon(data=NA_sp, aes(x=long, y=lat, group=group),fill="grey", color="grey50", size=0.25,inherit.aes = FALSE) +
  stat_summary_hex(bins=100) +
  scale_fill_viridis() +
  theme_pubr()   + coord_map(ylim = c(25, 50),lim = c(25, 50),projection = "mercator") 
?coord_map
ggsave(resid_map,file="manuscript/figures/supplementary/resid_map.png",width=10,height=10,dpi=300)

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

write.csv(k_checks,file="manuscript/tables/supplementary/k_checks.csv")

#Model Summaries 

GI_summary <- read.csv("manuscript/tables/model_GI_results.csv")

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