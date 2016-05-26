model.data <- read.delim("../data/model_data_traffic.csv", header=T, sep=",")  #Read in posted speed data for road segments

model.data$rdclass <- factor(model.data$rdclass, levels = 0:5)  #Define road class covariate as a factor with six levels

speed.lm <- lm(formula = speedlmt ~ rdclass + rddens, data = na.omit(model.data[!is.na(model.data$speedlmt),]))  #Fit regression model

#RF
speed.rf <- randomForest(formula = speedlmt ~ rdclass + rddens + X + Y, data = model.data)  #Fit random forest model
##

summary(speed.lm)  #Examine fit of regression model

indep.data <- read.delim("../data/VIC_GDA9455_ROADS_VICSTATE_GRID_CLIP_FINAL.csv", header=T, sep=",")  #Read in covariate data for all road segments

indep.data$rdclass <- factor(indep.data$rdclass, levels = 0:5) #Define road class covariate as a factor with six levels

lm.preds <- predict(speed.lm, indep.data, type="response")  #Predict traffic speed to all road segments using model fit

#RF
indep.data <- read.delim("../data/VIC_GDA9455_ROADS_GRID_CLIP_FINALxy.csv", header=T, sep=",")  #Read in covariate data for all road segments
indep.data$rdclass <- factor(indep.data$rdclass, levels = 0:5)
rf.preds <- predict(speed.rf, indep.data, type="response")
speed.preds.rf <- as.data.frame(cbind(indep.data$uid,rf.preds))  #Combine predictions with unique IDs for all road segments
names(speed.preds.rf) <- c("uid","tspd") 
write.csv(speed.preds.rf, file = "../output/speed_preds_rf.csv")



##


speed.preds <- as.data.frame(cbind(indep.data$uid,lm.preds))  #Combine predictions with unique IDs for all road segments

names(speed.preds) <- c("uid","tspd")  #Rename columns in dataframe

write.csv(speed.preds, file = "../output/speed_preds.csv")  #Write out predictions for all road segments
