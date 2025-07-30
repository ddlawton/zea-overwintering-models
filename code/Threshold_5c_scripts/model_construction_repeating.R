rm(list=ls())

library(tidyverse)
library(mgcv)
library(gratia)
library(data.table)
library(patchwork)
library(ggpubr)
library(viridis)
library(ROCR)
library(caret)


dat <- as_tibble(fread("data/processed/Threshold_5c/final_zea_data/Hzea_ow_zones_Dec172021.csv"))

cleaned_dat <- dat %>% select(2:7,9,13)

names(cleaned_dat)
write.csv(cleaned_dat,file="data/processed/Lawton_etal_data.csv")


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


# K-fold validation

k <- 10 #Define number of folds

#RMSE

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#This function calculates the deviance of out-of-sample data,
#conditional on their mean predicted value from the model

get_deviance <- function(model, y_pred, y_obs, weights = NULL){
  stopifnot(length(y_obs)==length(y_pred))
  if(is.null(weights)) weights = rep(1, times= length(y_obs))
  dev_residuals = model$family$dev.resids(y_obs, y_pred, weights)
  return(sum(dev_residuals))
}

# model G
repeat {
  
#Apply k-fold cross validation
rmses_G <- data.frame() #Create empty df to store CV root mean squared error values
OOS_G <- data.frame() #Create empty df to store out of sample deviance
OOS_OW_G <- list()
scores_G <- data.frame() #Create empty df to store CV R2 value

#For loop for each fold
for (i in 1:k) {
  
  #progress notifications
  cat(paste("Currently processing fold:", i,"/",k, "\n"))
  
  
  IND_TRAIN <- createDataPartition(paste(dat4$zone_30y,dat4$location),p=.7)$Resample
  
  #train set
  train_set <- dat4[ IND_TRAIN,]
  #test set
  test_set <- dat4[-IND_TRAIN,]
  
  #Run GAM 
  m <- bam(CEW_sum ~
             s(woy,bs="cc",k=15) +
             s(year,bs="cc",k=15) +
             te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
             s(location,bs="re") + s(zone_30y ,bs="re")+
             s(trap_type,bs="re"), 
           data    = train_set,
           method  = 'fREML', 
           nthreads = 15,
           discrete = TRUE,
           select = TRUE,
           family  = tw() 
  )
  
  #Predict test data
  test_set$pred <- predict(m,test_set,type="response")
  
  #Extract out of sample deviance
  OOS_G[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
  
  OOS_OW_G[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
    group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
  
  rmses_G[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
  
  scores_G[i,1] <- summary(m)$r.sq
}
if(max(unlist(OOS_G)) < 200000) {
  break
}


}

write.csv(OOS_G %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_G.csv")
write.csv(OOS_OW_G %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_G.csv")
write.csv(scores_G %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/scores_G.csv")
write.csv(rmses_G %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/rmses_G.csv")



# model I
repeat {
  #model I
  
  #Apply k-fold cross validation
  rmses_I <- data.frame() #Create empty df to store CV root mean squared error values
  OOS_I <- data.frame() #Create empty df to store out of sample deviance
  OOS_OW_I <- list()
  scores_I <- data.frame() #Create empty df to store CV R2 value
  
  #For loop for each fold
  for (i in 1:k) {
    
    #progress notifications
    cat(paste("Currently processing fold:", i,"/",k, "\n"))
    
    
    IND_TRAIN <- createDataPartition(paste(dat4$zone_30y,dat4$location),p=.7)$Resample
    
    #train set
    train_set <- dat4[ IND_TRAIN,]
    #test set
    test_set <- dat4[-IND_TRAIN,]
    
    #Run GAM 
    m <- bam(CEW_sum ~
               te(woy,by=zone_30y ,bs=c("cc"),k=15) +
               te(year,by=zone_30y ,bs=c("gp"),k=15) +
               te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
               s(location,bs="re") + s(zone_30y ,bs="re")+
               s(trap_type,bs="re"), 
             data    = train_set,
             method  = 'fREML', 
             nthreads = 15,
             discrete = TRUE,
             select = TRUE,
             family  = tw() 
    )
    
    #Predict test data
    test_set$pred <- predict(m,test_set,type="response")
    
    #Extract out of sample deviance
    OOS_I[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
    
    OOS_OW_I[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
      group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
    
    rmses_I[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
    
    scores_I[i,1] <- summary(m)$r.sq
  }
  if(max(unlist(OOS_I)) < 200000) {
    break
  }
  
  
}

write.csv(OOS_I %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_I.csv")
write.csv(OOS_OW_I %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_I.csv")
write.csv(scores_I %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/scores_I.csv")
write.csv(rmses_I %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/rmses_I.csv")

# model GS
repeat {
  #model GS
  
  #Apply k-fold cross validation
  rmses_GS <- data.frame() #Create empty df to store CV root mean squared error values
  OOS_GS <- data.frame() #Create empty df to store out of sample deviance
  OOS_OW_GS <- list()
  scores_GS <- data.frame() #Create empty df to store CV R2 value
  
  #For loop for each fold
  for (i in 1:k) {
    
    #progress notifications
    cat(paste("Currently processing fold:", i,"/",k, "\n"))
    
    
    IND_TRAIN <- createDataPartition(paste(dat4$zone_30y,dat4$location),p=.7)$Resample
    
    #train set
    train_set <- dat4[ IND_TRAIN,]
    #test set
    test_set <- dat4[-IND_TRAIN,]
    
    #Run GAM 
    m <- bam(CEW_sum ~
               s(woy,bs="cc",k=15) +
               s(year,bs="cc",k=15) +
               te(woy,zone_30y ,bs=c("cc","re"),k=15) +
               te(year,zone_30y ,bs=c("gp","re"),k=15) +
               te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
               s(location,bs="re") + s(zone_30y ,bs="re")+
               s(trap_type,bs="re"), 
             data    = train_set,
             method  = 'fREML', 
             nthreads = 15,
             discrete = TRUE,
             select = TRUE,
             family  = tw() 
    )
    
    #Predict test data
    test_set$pred <- predict(m,test_set,type="response")
    
    #Extract out of sample deviance
    OOS_GS[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
    
    OOS_OW_GS[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
      group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
    
    rmses_GS[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
    
    scores_GS[i,1] <- summary(m)$r.sq
  }
  if(max(unlist(OOS_GS)) < 200000) {
    break
  }
  
  
}


write.csv(OOS_GS %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_GS.csv")
write.csv(OOS_OW_GS %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_GS.csv")
write.csv(scores_GS %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/scores_GS.csv")
write.csv(rmses_GS %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/rmses_GS.csv")


# model GI
repeat {
  
  #model GI
  
  #Apply k-fold cross validation
  rmses_GI <- data.frame() #Create empty df to store CV root mean squared error values
  OOS_GI <- data.frame() #Create empty df to store out of sample deviance
  OOS_OW_GI <- list()
  scores_GI <- data.frame() #Create empty df to store CV R2 value
  
  #For loop for each fold
  for (i in 1:k) {
    
    #progress notifications
    cat(paste("Currently processing fold:", i,"/",k, "\n"))
    
    
    IND_TRAIN <- createDataPartition(paste(dat4$zone_30y,dat4$location),p=.7)$Resample
    
    #train set
    train_set <- dat4[ IND_TRAIN,]
    #test set
    test_set <- dat4[-IND_TRAIN,]
    
    #Run GAM 
    m <- bam(CEW_sum ~
               s(woy,bs="cc",k=15)+
               s(year,bs="cc",k=15) +
               te(woy,by=zone_30y ,bs=c("cc"),k=15) +
               te(year,by=zone_30y ,bs=c("gp"),k=15) +
               te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
               s(location,bs="re") + s(zone_30y ,bs="re")+
               s(trap_type,bs="re"), 
             data    = train_set,
             method  = 'fREML', 
             nthreads = 15,
             discrete = TRUE,
             select = TRUE,
             family  = tw() 
    )
    
    #Predict test data
    test_set$pred <- predict(m,test_set,type="response")
    
    #Extract out of sample deviance
    OOS_GI[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
    
    OOS_OW_GI[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
      group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
    
    rmses_GI[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
    
    scores_GI[i,1] <- summary(m)$r.sq
  }
  if(max(unlist(OOS_GI)) < 200000) {
    break
  }
  
  
}





# model N

  
  #model N
  
  #Apply k-fold cross validation
  rmses_N <- data.frame() #Create empty df to store CV root mean squared error values
  OOS_N <- data.frame() #Create empty df to store out of sample deviance
  OOS_OW_N <- list()
  scores_N <- data.frame() #Create empty df to store CV R2 value
  
  #For loop for each fold
  for (i in 1:k) {
    
    #progress notifications
    cat(paste("Currently processing fold:", i,"/",k, "\n"))
    repeat {
    
    IND_TRAIN <- createDataPartition(paste(dat4$zone_30y,dat4$location),p=.7)$Resample
    
    #train set
    train_set <- dat4[ IND_TRAIN,]
    #test set
    test_set <- dat4[-IND_TRAIN,]
    
    #Run GAM 
    m <- bam(CEW_sum ~
               s(woy,bs="cc",k=15) +
               s(year,bs="cc",k=15) +
               te(Longitude,Latitude,year,bs=c("gp","gp","gp")) +
               s(location,bs="re") + s(zone_30y ,bs="re")+
               s(trap_type,bs="re"), 
             data    = train_set,
             method  = 'fREML', 
             nthreads = 15,
             discrete = TRUE,
             select = TRUE,
             family  = tw() 
    )
    
    #Predict test data
    test_set$pred <- predict(m,test_set,type="response")
    
    #Extract out of sample deviance
    OOS_N[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
    
    OOS_OW_N[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
      group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
    
    rmses_N[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
    
    scores_N[i,1] <- summary(m)$r.sq

  if(max(unlist(OOS_N)) < 500000) {
    break
  }
    }
    
  
}

  
  
mean(OOS_N$V1)
mean(rmses_N$V1)
mean(scores_N$V1)


OOS_OW_N2 <- OOS_OW_N %>% bind_rows() %>% group_by(zone_30y) %>%
  summarize(mean = mean(deviance))


write.csv(OOS_GI %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_GI.csv")
write.csv(OOS_OW_GI %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_GI.csv")
write.csv(scores_GI %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/scores_GI.csv")
write.csv(rmses_GI %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/rmses_GI.csv")


write.csv(OOS_OW_GI %>% as_tibble(), file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_GI.csv")



OOS_OW_GI_sum <- OOS_OW_GI %>% bind_rows() %>%
  group_by(zone_30y) %>%
  summarize(deviance_std = std(deviance),deviance = mean(deviance)) %>%
  mutate(model = "GI")

OOS_OW_GS_sum <- OOS_OW_GS %>% bind_rows() %>%
  group_by(zone_30y) %>%
  summarize(deviance_std = std(deviance),deviance = mean(deviance)) %>%
  mutate(model = "GS")

OOS_OW_G_sum <- OOS_OW_G %>% bind_rows() %>%
  group_by(zone_30y) %>%
  summarize(deviance_std = std(deviance),deviance = mean(deviance)) %>%
  mutate(model = "G")

OOS_OW_I_sum <- OOS_OW_I %>% bind_rows() %>%
  group_by(zone_30y) %>%
  summarize(deviance_std = std(deviance),deviance = mean(deviance)) %>%
  mutate(model = "I")

OOS_OW_combined = OOS_OW_GI_sum %>% rbind(OOS_OW_GS_sum,OOS_OW_G_sum,OOS_OW_I_sum)


write.csv(OOS_OW_combined, file="models/Threshold_5c/Revision_models/repeated_OOS_values/OOS_OW_combined.csv")

