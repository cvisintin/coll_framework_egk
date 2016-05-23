model.data <- read.delim("Data/model_data_speed.csv", header=T, sep=",")  #Read in posted speed data for road segments

model.data$RDCLASS <- factor(model.data$RDCLASS, levels = 0:5)  #Define road class covariate as a factor with six levels

speed.lm <- lm(formula = SPEEDLMT ~ RDCLASS + RDDENS, data = model.data)  #Fit regression model

#RF
speed.rf <- randomForest(formula = SPEEDLMT ~ RDCLASS + RDDENS + X + Y, data = model.data)  #Fit random forest model
##

summary(speed.lm)  #Examine fit of regression model

indep.data <- read.delim("Data/VIC_GDA9455_ROADS_VICSTATE_GRID_CLIP_FINAL.csv", header=T, sep=",")  #Read in covariate data for all road segments

indep.data$RDCLASS <- factor(indep.data$RDCLASS, levels = 0:5) #Define road class covariate as a factor with six levels

lm.preds <- predict(speed.lm, indep.data, type="response")  #Predict traffic speed to all road segments using model fit

#RF
indep.data <- read.delim("Data/VIC_GDA9455_ROADS_GRID_CLIP_FINALxy.csv", header=T, sep=",")  #Read in covariate data for all road segments
indep.data$RDCLASS <- factor(indep.data$RDCLASS, levels = 0:5)
rf.preds <- predict(speed.rf, indep.data, type="response")
speed.preds.rf <- as.data.frame(cbind(indep.data$UID,rf.preds))  #Combine predictions with unique IDs for all road segments
names(speed.preds.rf) <- c("UID","TSPD") 
write.csv(speed.preds.rf, file = "Pred/speed_preds_rf.csv")



##


speed.preds <- as.data.frame(cbind(indep.data$UID,lm.preds))  #Combine predictions with unique IDs for all road segments

names(speed.preds) <- c("UID","TSPD")  #Rename columns in dataframe

write.csv(speed.preds, file = "Pred/speed_preds.csv")  #Write out predictions for all road segments
