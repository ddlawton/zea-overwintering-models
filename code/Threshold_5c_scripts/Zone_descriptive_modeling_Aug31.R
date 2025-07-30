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

cleaned_dat <- dat %>% select(location,year,woy,CEW_sum,longitude,latitude,date,unix_date,trap_type,zone_30y)
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


test <- dat4 %>%
  group_by(location,trap_type) %>%
  summarize(n=length(unique(levels((trap_type)))))


test %>%
  group_by(location) %>%
  summarize(n = unique(levels(trap_type)))




mod_spatiotemp_GS <- bam(CEW_sum ~
                           s(woy,bs="cc",k=15) +
                           s(year,bs="cc",k=15) +
                           te(woy,zone_30y ,bs=c("cc","re"),k=15) +
                           te(year,zone_30y ,bs=c("gp","re"),k=15) +
                           te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                           s(location,bs="re") + s(zone_30y ,bs="re") +
                           s(trap_type,bs="re"), 
                         family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                         data=dat4) 

summary(mod_spatiotemp_GS)


mod_spatiotemp_S <- bam(CEW_sum ~ 
                          te(woy,zone_30y ,bs=c("cc","re"),k=15) +
                          te(year,zone_30y ,bs=c("gp","re"),k=15) +
                          te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                          s(location,bs="re") + s(zone_30y ,bs="re")+
                          s(trap_type,bs="re"), 
                        family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                        data=dat4) 

mod_spatiotemp_GI <- bam(CEW_sum ~
                           s(woy,bs="cc",k=15)+
                           s(year,bs="cc",k=15) +
                           te(woy,by=zone_30y ,bs=c("cc"),k=15) +
                           te(year,by=zone_30y ,bs=c("gp"),k=15) +
                           te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                           s(location,bs="re") + s(zone_30y ,bs="re")+
                           s(trap_type,bs="re"), 
                         family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                         data=dat4) 

mod_spatiotemp_I <- bam(CEW_sum ~
                          te(woy,by=zone_30y ,bs=c("cc"),k=15) +
                          te(year,by=zone_30y ,bs=c("gp"),k=15) +
                          te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                          s(location,bs="re") + s(zone_30y ,bs="re")+
                          s(trap_type,bs="re"), 
                        family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                        data=dat4) 

mod_spatiotemp_G <- bam(CEW_sum ~
                          s(woy,bs="cc",k=15) +
                          s(year,bs="cc",k=15) +
                          te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                          s(location,bs="re") + s(zone_30y ,bs="re")+
                          s(trap_type,bs="re"), 
                        family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                        data=dat4) 

mod_spatiotemp_N <- bam(CEW_sum ~
                          s(woy,bs="cc",k=15) +
                          s(year,bs="cc",k=15) +
                          te(Latitude,Longitude,year,bs=c("gp","gp","gp")) +
                          s(location,bs="re")+
                          s(trap_type,bs="re"), 
                        family=tw(), discrete = TRUE, nthreads=23, select=TRUE,
                        data=dat4) 

mod_spatiotemp_0 <- bam(CEW_sum ~
           s(location,bs="re") + s(zone_30y ,bs="re")+
             s(trap_type,bs="re"), 
         data    = train_set,
         method  = 'fREML', 
         nthreads = 5,
         discrete = TRUE,
         select = TRUE,
         family  = tw() 
)

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


#model S

#Apply k-fold cross validation
rmses_S <- data.frame() #Create empty df to store CV root mean squared error values
OOS_S <- data.frame() #Create empty df to store out of sample deviance
OOS_OW_S <- list()
scores_S <- data.frame() #Create empty df to store CV R2 value

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
  OOS_S[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
  
  OOS_OW_S[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
    group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
  
  rmses_S[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
  
  scores_S[i,1] <- summary(m)$r.sq
}


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


#model G

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
}


#model intercept

#Apply k-fold cross validation
rmses_0 <- data.frame() #Create empty df to store CV root mean squared error values
OOS_0 <- data.frame() #Create empty df to store out of sample deviance
OOS_OW_0 <- list()
scores_0 <- data.frame() #Create empty df to store CV R2 value

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
             s(location,bs="re") + s(zone_30y ,bs="re")+
             s(trap_type,bs="re"), 
           data    = train_set,
           method  = 'fREML', 
           nthreads = 5,
           discrete = TRUE,
           select = TRUE,
           family  = tw() 
  )
  
  #Predict test data
  test_set$pred <- predict(m,test_set,type="response")
  
  #Extract out of sample deviance
  OOS_0[i,1] <- get_deviance(m, test_set$pred,test_set$CEW_sum)
  
  OOS_OW_0[[i]] <- test_set %>% mutate(pred = predict(m,test_set,type="response")) %>% 
    group_by(zone_30y) %>% summarize(deviance = get_deviance(m,pred,CEW_sum))
  
  rmses_0[i,1] <- RMSE(test_set$CEW_sum, test_set$pred)
  
  scores_0[i,1] <- summary(m)$r.sq
}


head(rbindlist(OOS_OW_0))

OOS <- data.frame(cbind(OOS_GS$V1,OOS_S$V1,OOS_GI$V1,OOS_I$V1,OOS_G$V1,OOS_N$V1,OOS_0$V1))
OOS <-  OOS %>% rename(GS="X1",S="X2",GI="X3",I="X4",G="X5",N="X6",intercept="X7")

RMSE <- data.frame(cbind(rmses_GS$V1,rmses_S$V1,rmses_GI$V1,rmses_I$V1,rmses_G$V1,rmses_N$V1,rmses_0$V1))
RMSE <-  RMSE %>% rename(GS="X1",S="X2",GI="X3",I="X4",G="X5",N="X6",intercept="X7")

RSQ <- data.frame(cbind(scores_GS$V1,scores_S$V1,scores_GI$V1,scores_I$V1,scores_G$V1,scores_N$V1,scores_0$V1))
RSQ <-  RSQ %>% rename(GS="X1",S="X2",GI="X3",I="X4",G="X5",N="X6",intercept="X7")


OOS1 <- OOS %>% pivot_longer(cols = everything(),names_to = "models",values_to = "OOS") %>%
  group_by(models) %>%
  summarize(OOS_mean = mean(OOS),OOS_std = std(OOS)) %>%
  arrange(OOS_mean)

RMSE1 <- RMSE %>% pivot_longer(cols = everything(),names_to = "models",values_to = "RMSE") %>%
  group_by(models) %>%
  summarize(RMSE_mean = mean(RMSE),RMSE_std = std(RMSE)) %>%
  arrange(RMSE_mean)

RSQ1 <- RSQ %>% pivot_longer(cols = everything(),names_to = "models",values_to = "RSQ") %>%
  group_by(models) %>%
  summarize(RSQ_mean = mean(RSQ),RSQ_std = std(RSQ)) %>%
  arrange(RSQ_mean)

model_selection <- OOS1 %>% left_join(RMSE1,by="models") %>% left_join(RSQ1,by="models") %>% arrange(OOS_mean)


OW_OOS_GS <- rbindlist(OOS_OW_GS) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_GS = mean(deviance),deviance_std_GS = std(deviance))

OW_OOS_S <- rbindlist(OOS_OW_S) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_S = mean(deviance),deviance_std_S = std(deviance))

OW_OOS_GI <- rbindlist(OOS_OW_GI) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_GI = mean(deviance),deviance_std_GI = std(deviance))

OW_OOS_I <- rbindlist(OOS_OW_I) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_I = mean(deviance),deviance_std_I = std(deviance))

OW_OOS_G <- rbindlist(OOS_OW_G) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_G = mean(deviance),deviance_std_G = std(deviance))

OW_OOS_N <- rbindlist(OOS_OW_N) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_N = mean(deviance),deviance_std_N = std(deviance))


OW_OOS_0 <- rbindlist(OOS_OW_0) %>% group_by(zone_30y) %>%
  summarize(deviance_mean_0 = mean(deviance),deviance_std_0 = std(deviance))


OW_OOS <- OW_OOS_0 %>% left_join(OW_OOS_N,by="zone_30y") %>% left_join(OW_OOS_G,by="zone_30y") %>%
  left_join(OW_OOS_I,by="zone_30y") %>% left_join(OW_OOS_GI,by="zone_30y") %>%
  left_join(OW_OOS_S,by="zone_30y") %>% left_join(OW_OOS_GS,by="zone_30y")



model_selection_OW <- OW_OOS_GS %>% left_join(OW_OOS_S,by="zone_30y") %>% left_join(OW_OOS_GI,by="zone_30y") %>%
  left_join(OW_OOS_N,by="zone_30y") %>% left_join(OW_OOS_G,by="zone_30y") %>%
  left_join(OW_OOS_I,by="zone_30y") %>% left_join(OW_OOS_0,by="zone_30y") %>%
  pivot_longer(cols=c(starts_with("deviance")),
               names_to = c(".value", "models"), 
               names_pattern = "(.*)_(.*)") %>% group_by(zone_30y) %>%
  arrange()


mod_spatiotemp_GS <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GS.rds")
mod_spatiotemp_S <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_S.rds")
mod_spatiotemp_GI <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_GI.rds")
mod_spatiotemp_I <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_I.rds")
mod_spatiotemp_G <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_G.rds")
mod_spatiotemp_N <- readRDS("models/Threshold_5c/Revision_models/mod_spatiotemp_N.rds")

AIC_table <- AIC(mod_spatiotemp_GS,mod_spatiotemp_S,mod_spatiotemp_GI,mod_spatiotemp_I,mod_spatiotemp_G,mod_spatiotemp_N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaAIC = AIC - min(AIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,AIC, deltaAIC), 
            .funs = funs(round,.args = list(digits=2))) %>% arrange(deltaAIC)

BIC_table <- BIC(mod_spatiotemp_GS,mod_spatiotemp_S,mod_spatiotemp_GI,mod_spatiotemp_I,mod_spatiotemp_G,mod_spatiotemp_N) %>%
  rownames_to_column(var= "Model")%>%
  mutate(deltaBIC = BIC - min(BIC))%>%
  ungroup()%>%
  mutate_at(.vars = vars(df,BIC, deltaBIC), 
            .funs = funs(round,.args = list(digits=2))) %>% arrange(deltaBIC) %>%
  left_join(AIC_table,by="Model")

write.csv(BIC_table,file="models/Threshold_5c/Revision_models/AIC_BIC.csv")


dat4$pred <- predict(mod_spatiotemp_GS,type="response",newdata=dat4)



saveRDS(mod_spatiotemp_GS,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_GS.rds")
saveRDS(mod_spatiotemp_S,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_S.rds")
saveRDS(mod_spatiotemp_GI,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_GI.rds")
saveRDS(mod_spatiotemp_I,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_I.rds")
saveRDS(mod_spatiotemp_G,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_G.rds")
saveRDS(mod_spatiotemp_N,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_N.rds")
saveRDS(mod_spatiotemp_0,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_0.rds")

write.csv(model_selection,file="models/pop_dynamics/Zone_curves/kfold_model_selection.csv")
write.csv(OW_OOS,file="models/pop_dynamics/Zone_curves/OW_OOS_model_selection.csv")

#model summaries

GS_summary <- broom::tidy(mod_spatiotemp_GS)
S_summary <- broom::tidy(mod_spatiotemp_S)
GI_summary <- broom::tidy(mod_spatiotemp_GI)
I_summary <- broom::tidy(mod_spatiotemp_I)
G_summary <- broom::tidy(mod_spatiotemp_G)
N_summary <- broom::tidy(mod_spatiotemp_N)
inter_summary <- broom::tidy(mod_spatiotemp_0)


GS_k <- k.check(mod_spatiotemp_GS)
GS_k %>% as_tibble() %>% rownames_to_column("VALUE")



write.csv(S_summary,"manuscript/tables/model_S_results.csv")
write.csv(GI_summary,"manuscript/tables/model_GI_results.csv")
write.csv(GS_summary,"manuscript/tables/model_GS_results.csv")
write.csv(I_summary,"manuscript/tables/model_I_results.csv")
write.csv(G_summary,"manuscript/tables/model_G_results.csv")
write.csv(N_summary,"manuscript/tables/model_N_results.csv")
write.csv(inter_summary,"manuscript/tables/model_inter_results.csv")






#################################################################
# Old shit






summary(mod_spatiotemp_S)
plot(mod_spatiotemp_GS)


test_mod1 <- bam(CEW_sum ~ 
                   te(woy,Latitude,bs=c("cc","gp"),m=(1))+
                   te(year,k=15,bs="gp") +
                   s(location,bs="re") + s(zone_30y ,bs="re"), 
                 family=tw(link="log"), discrete = TRUE, nthreads=23,
                 data=dat2) 

test_mod2 <- bam(CEW_sum ~ 
                   te(woy,Latitude,bs=c("cc","gp"),m=(2))+
                   te(year,k=15,bs="gp") +
                   s(location,bs="re") + s(zone_30y ,bs="re"), 
                 family=tw(link="log"), discrete = TRUE, nthreads=23,
                 data=dat2) 


test_mod3 <- bam(CEW_sum ~ 
                   te(woy,Latitude,bs=c("cc","gp"),m=(3))+
                   te(year,k=15,bs="gp") +
                   s(location,bs="re") + s(zone_30y ,bs="re"), 
                 family=tw(link="log"), discrete = TRUE, nthreads=23,
                 data=dat2) 

vis.gam(test_mod1,view=c("woy","Latitude"),plot.type = 'contour',color="topo")
vis.gam(test_mod2,view=c("woy","Latitude"),plot.type = 'contour',color="topo")
vis.gam(test_mod3,view=c("woy","Latitude"),plot.type = 'contour',color="topo")

gratia::draw(test_mod2)


plot(test_mod)



library(tidyverse)
ggplot(dat2,aes(x=woy,y=Latitude,z=CEW_sum+1)) + stat_summary_2d() + 
  scale_fill_viridis_b(trans="log10")


vis.gam(test_mod, theta=170, main = "Matern")

summary(test_mod)
gratia::draw(test_mod)
gratia::appraise(test_mod)
k.check(test_mod)


saveRDS(mod_spatiotemp_GS,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_GS.rds")
saveRDS(mod_spatiotemp_S,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_S.rds")
saveRDS(mod_spatiotemp_GI,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_GI.rds")
saveRDS(mod_spatiotemp_I,file="models/pop_dynamics/Zone_curves/mod_spatiotemp_I.rds")


BIC(mod,mod2,mod_spatiotemp)
AIC(mod,mod2,mod_spatiotemp)

plot(mod_spatiotemp)
summary(mod)           
draw(mod_spatiotemp)  
appraise(mod)

summary(mod2)

dat2$pred <- predict(mod_spatiotemp_I,type="response",newdata=dat2)

p1 <- ggplot(dat2,aes(x=year,y=pred)) + geom_smooth(color="black") + theme_pubr() + 
  geom_smooth(aes(color=overwinter)) +
  coord_cartesian(ylim=c(0,NA))

p2 <- ggplot(dat2,aes(x=woy,y=pred)) + geom_smooth(color="black") + theme_pubr() + 
  geom_smooth(aes(color=overwinter)) +
  coord_cartesian(ylim=c(0,NA))

p2 <- ggplot(dat2,aes(x=woy,y=pred,color=overwinter)) + geom_smooth() + theme_pubr()+
  coord_cartesian(ylim=c(0,NA)) + ylim(0,NA)
p3 <- ggplot(dat2,aes(y=Latitude,x=Longitude,z=pred)) + stat_summary_hex(bins=25) +
  scale_fill_viridis() + theme_pubr()

(p1 + p2) / (p3 + p4)

summary(mod_spatiotemp)
appraise(mod_spatiotemp_GS)
library(DHARMa)
simresid <- simulateResiduals(mod_spatiotemp_GS)
plot(simresid)
testDispersion(simresid)
testZeroInflation(simresid)
testSpatialAutocorrelation(simresid,x=dat2$Longitude,y=dat2$Latitude)


dat2$resid <- resid(mod_spatiotemp)

p4 <- ggplot(dat2,aes(y=Latitude,x=Longitude,z=resid)) + stat_summary_hex(bins=25) +
  scale_fill_viridis() + theme_pubr()



## slightly modified from Wood (2017, pp. 362)
library(mgcv)
REML <- r <- seq(0.1, 0.2, by = 0.05)
for (i in seq_along(r)) {
  m <- gam(CEW_sum ~ te(Longitude,Latitude,bs='gp', m = c(1, r[i])), method="REML", family=poisson(), data=dat2) 
  REML[i] <- m$gcv.ubre
}

plot(REML ~ r, type = "o", pch = 16, ylab = expression(rho))

bam(CEW_sum ~
      te(Longitude,Latitude,year,bs=c("gp","gp","gp"), m = c(3, r[i])), 
    family=tw(), discrete = TRUE, nthreads=23,
    data=dat2) 





library("mgcv")
set.seed(24)
eg <- gamSim(2, n = 300, scale = 0.05)
b  <- gam(y ~ s(x, z, bs= "gp", k = 50, m = c(3, 0.175)), data = eg$data, method = "REML") ## Matern spline
b1 <- gam(y ~ s(x, z, bs = "gp", k = 50, m = c(1, 0.175)), data = eg$data, method = "REML") ## spherical 
b2 <- gam(y ~ s(x, z, bs = "gp", k = 50, m = c(2, 0.175)), data = eg$data, method = "REML") ## exponential 

op <- par(mfrow=c(2,2), mar = c(0.5,0.5,3,0.5))
with(eg$truth, persp(x, z, f, theta = 30, main = "Truth")) ## truth
vis.gam(b, theta=30, main = "Matern")
vis.gam(b1, theta=30, main = "Spherical")
vis.gam(b2, theta=30, main = "Exponential")
par(op)

## slightly modified from Wood (2017, pp. 362)
REML <- r <- seq(0.1, 1.5, by = 0.05)
for (i in seq_along(r)) {
  m <- gam(y ~ s(x, z, bs = "gp", k = 50, m = c(1, r[i])), data = eg$data, method = "REML")
  REML[i] <- m$gcv.ubre
}

plot(REML ~ r, type = "o", pch = 16, ylab = expression(rho))
