library(tidyverse)
library(mgcv)
library(data.table)
library(patchwork)
library(ggpubr)
library(DHARMa)

modGS <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GS.rds")
modS <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_S.rds")
modGI <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GI.rds")
modI <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_I.rds")
modG <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_G.rds")
modN <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_N.rds")
#mon0 <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_0.rds")

simresid <- simulateResiduals(modGS)
plot(simresid)

AIC_table <- AIC(modGS,modS,modGI,modI,modG,modN) %>%
  mutate(deltaAIC = AIC - min(AIC)) %>%
  arrange(deltaAIC)

BIC_table <- BIC(modGS,modS,modGI,modI,modG,modN) %>%
  mutate(deltaBIC = BIC - min(BIC)) %>%
  arrange(deltaBIC)


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
  mutate(trap_type = factor(trap_type), GS_pred = predict(modGS,type="response"))




b0 <- coef(modGS)[1]

test <- gratia::smooth_estimates(modGS)

test$adj_est <- test$est + b0

date_trt <- test %>% filter(smooth == "te(Date_numeric,Trt)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Trt)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Trt),alpha=.1) +
  geom_line(aes(y = adj_est, color=Trt)) + theme_pubr() +
  scale_color_manual(values = c("#1b9e77","#d95f02")) +
  scale_fill_manual(values = c("#1b9e77","#d95f02")) + ggtitle("date by treatment smooth") + xlab("Date")

date_block <- test %>% filter(smooth == "te(Date_numeric,Block)") %>%
  ggplot(aes(x=anytime::anytime(Date_numeric),group=Block)) + geom_ribbon(aes(ymin=adj_est-se, ymax=adj_est+se,fill=Block),alpha=.1) +
  geom_line(aes(y = adj_est, color=Block)) + theme_pubr() +
  scale_color_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) +
  scale_fill_manual(values = c("#003f5c","#7a5195","#ef5675","#ffa600")) + ggtitle("date by block smooth")+ xlab("Date")









year_zone_GI <- ggplot(dat4,aes(x=year,y=(GS_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + 
  ylab("Modeled weekly catch") + xlab("Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), 
                     labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +# coord_cartesian(ylim=c(0,300)) +
  geom_smooth(data=dat4, aes(x=year,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  guides(colour = guide_legend(override.aes = list(size = 1,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 10)) 


woy_zone_GI <- ggplot(dat4,aes(x=woy,y=(GS_pred), color=zone_30y)) + geom_smooth(linetype="dashed",se=TRUE) + ylab("") +
  xlab("Week of Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +
  geom_smooth(linetype="solid",data=dat4, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("Modeled weekly catch") +
  guides(colour = guide_legend(override.aes = list(size = 1,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 10)) + theme(legend.position="none")


pw1_GI <- (year_zone_GI / woy_zone_GI)  + 
  plot_annotation(tag_levels = "A") +
  plot_layout(guides='collect') & theme(legend.position = "bottom") 

ggsave(pw1_GI,file="manuscript/figures/Threshold_5c/Figure3_1.png",width=11,height=15,units="cm",dpi=600)


plot(modGS)
ggplot(dat4,aes(x=trap_type,y=(GS_pred))) + geom_boxplot() + ylab("") +
  xlab("Week of Year") + theme_pubr() +
  scale_color_manual(values=c("#49555d","#ffc02c","#0092d9"), labels = c("Northern Limits", "Transitional Zone", "Southern Range")) +
  geom_smooth(linetype="solid",data=dat4, aes(x=woy,y=(GS_pred)), inherit.aes = FALSE, se=FALSE,color="black") +
  coord_cartesian(ylim=c(0,300)) + ylab("Modeled weekly catch") +
  guides(colour = guide_legend(override.aes = list(size = 10,fill=NA)))+
  theme(legend.title = element_blank()) +
  theme(text = element_text(size = 15)) + theme(legend.position="none")
