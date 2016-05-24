model.data <- read.delim("../data/model_data_volume.csv", header=T, sep=",")  #Read in traffic volume data for road segments

model.data$RDCLASS <- factor(model.data$RDCLASS, levels = 0:5)  #Define road class covariate as a factor with six levels

trans.lglm <- lm(formula = log(AADT) ~ INCOMEPP + KMTOCZ + KMTOHWY + KMTORZ + POPDENS + RDDENS + RDCLASS, data = model.data)  #Fit regression model

#RF
set.seed(123)
trans.rf <- randomForest(formula = log(AADT) ~ INCOMEPP + KMTODEV + KMTOHWY + POPDENS + RDDENS + RDCLASS + X + Y, data = na.omit(model.data))  #Fit random forest model
##

summary(trans.lglm) #Examine fit of regression model

indep.data <- read.delim("../data/VIC_GDA9455_ROADS_VICSTATE_GRID_CLIP_FINAL.csv", header=T, sep=",")  #Read in covariate data for all road segments

indep.data$RDCLASS <- factor(indep.data$RDCLASS, levels = 0:5)  #Define road class covariate as a factor with six levels

lm.preds <- predict(trans.lglm, indep.data, type="response")  #Predict traffic volume to all road segments using model fit


#RF
rf.preds <- predict(trans.rf, model.data, type="response")
vol.preds.rf <- as.data.frame(cbind(model.data$UID,exp(rf.preds)))  #Combine predictions with unique IDs for all road segments
names(vol.preds.rf) <- c("UID","TVOL") 
write.csv(vol.preds.rf, file = "Pred/vol_preds_rf.csv")
##


vol.preds <- as.data.frame(cbind(indep.data$UID,exp(lm.preds)))  #Combine predictions with unique IDs for all road segments

names(vol.preds) <- c("UID","TVOL")  #Rename columns in dataframe

write.csv(vol.preds, file = "../output/vol_preds.csv")  #Write out predictions for all road segments
