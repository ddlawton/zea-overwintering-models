########################
#    BIOCLIM exploratory
#       Modeling
#        Zea
######################
rm(list=ls())

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


bioclim <- read.csv("data/processed/BIOClim_Zea_Jul12021.csv")
str(bioclim)
head(bioclim)
names(bioclim)

bioclim[1:3][is.na(bioclim[1:3])] <- 0

bioclim <- bioclim %>% select(2:53) %>% mutate(CEW_total = CEW + CEWp)

bioclim <- bioclim %>% mutate(pheromone = case_when(
  CEWp > 0 ~ "1",
  CEWp == 0 ~ "0",
  CEWp == NA ~ '0')) %>% mutate(
    Annual_mean_temp = bio01_mean*.1,
    Mean_dinural_range = bio02_mean*.1,
    Isothermality = bio03_mean,
    Temp_seasonality = bio04_mean*0.01,
    Max_temp_warmest_month = bio05_mean*0.1,
    Min_temp_coldest_moth = bio06_mean * 0.1,
    Temp_annual_range = bio07_mean * 0.1,
    Mean_temp_wettest_quarter = bio08_mean * 0.1,
    Mean_temp_driest_quarter = bio09_mean * 0.1,
    Mean_temp_warmest_quarter = bio10_mean * 0.1,
    Mean_temp_coldest_quarter = bio11_mean * 0.1,
    Annual_precip = bio12_mean,
    Preip_wettest_month = bio13_mean,
    Precip_driest_month = bio14_mean,
    Precip_seasonality = bio15_mean,
    Precip_wettest_quarter = bio16_mean,
    Precip_driest_quarter = bio17_mean,
    Precip_warmest_quater = bio18_mean,
    Precip_coldest_quater = bio19_mean
  )


bioclim$county <- as.factor(bioclim$county)
bioclim$location <- as.factor(bioclim$location)
bioclim$state <- as.factor(bioclim$state)
bioclim$date <- as.Date(bioclim$date, format="%m/%d/%Y")

# Looking out count spread


p1 <- bioclim %>%
  ggplot(aes(x = (CEW_total), y=state,fill=state)) +
  geom_density_ridges() + 
  scale_fill_tableau() +
  guides(fill = FALSE) + ggtitle("CEW totals by state")

bioclim %>%
  ggplot(aes(x = (CEW), y=state,fill=state)) +
  geom_density_ridges() + 
  scale_fill_tableau() +
  guides(fill = FALSE) + ggtitle("CEW totals by state")

bioclim %>%
  ggplot(aes(x = (CEWp), y=state,fill=state)) +
  geom_density_ridges() + 
  scale_fill_tableau() +
  guides(fill = FALSE) + ggtitle("CEW totals by state")



#obviously very biased towards zeros. No worries though.

# look at independent variables
names(indepndent_vars)
indepndent_vars <- bioclim %>% select(state,55:73)


independent_viz <- indepndent_vars %>%
  pivot_longer(names_to = "x", values_to="y", cols=2:20)  %>%
  ggplot(aes(x = y, y = state, color = state, fill = state)) +
  facet_wrap( ~ x, scale = "free", ncol = 10) +
  scale_fill_tableau() +
  scale_color_tableau() +
  geom_density_ridges(alpha = 0.8) +
  guides(fill = FALSE, color = FALSE)


#correlation within the bioclims


M <- cor((indepndent_vars %>% select(!state)))
corrplot(M, method="pie") # surprise surprise they are all correlated with one another

?corrplot

# Now plotting with count 
names(bioclim)

raw_data_viz <- bioclim %>%
  pivot_longer(names_to = "x", values_to="y", cols=55:73) %>%
  ggplot(aes(x = y, y = CEW_total, color = state, fill = state)) +
  facet_wrap( ~ x, scale = "free", ncol = 10) +
  scale_fill_tableau() +
  scale_color_tableau() +
  geom_smooth() 

ggsave(p1,file="figures/exploratory/Count_distribution.png",width=5,height = 5, units="in")
ggsave(independent_viz,file="figures/exploratory/independent_viz.png",height=5,width = 20, units="in")
ggsave(raw_data_viz,file="figures/exploratory/raw_data_viz.png",height=5,width = 20, units="in")

#### Lets build models!!!

str(bioclim)

bioclim_rf <- bioclim %>% select(CEW_total, ends_with("_mean"),Latitude,Longitude,year,doy)

?randomForest

rf <- randomForest(CEW_total ~ ., data = bioclim_rf, ntree = 2000)

varImpPlot(rf)

X <- bioclim_rf[which(names(bioclim_rf) != "CEW_total")]
predictor <- Predictor$new(rf, data = X, y = bioclim_rf$CEW_total)
 
imp <- FeatureImp$new(predictor, loss = "mae")
plot(imp)

bio06 <- FeatureEffect$new(predictor, feature = "bio06_mean")
bio06$plot()

effs <- FeatureEffects$new(predictor)
plot(effs)
