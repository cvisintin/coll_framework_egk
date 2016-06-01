library("data.table")

model.data <- as.data.table(read.delim("data/vic_model_data_traffic.csv", header=T, sep=","))  #Read in traffic volume data for road segments

model.data$rdclass <- factor(model.data$rdclass, levels = 0:5)  #Define road class covariate as a factor with six levels

cor(na.omit(model.data[,.(popdens,kmtohwy,kmtodev,rddens)]))

trans.lglm <- lm(formula = log(aadt) ~ popdens + kmtohwy + kmtodev + rddens + rdclass, data = model.data[!is.na(model.data$aadt),])  #Fit regression model

#RF
set.seed(123)
trans.rf <- randomForest(formula = log(aadt) ~ kmtodev + kmtohwy + popdens + rddens + rdclass + popdens, data = na.omit(model.data[!is.na(model.data$aadt),])) #Fit random forest model
##

summary(trans.lglm) #Examine fit of regression model

indep.data <- read.delim("../data/VIC_GDA9455_ROADS_VICSTATE_GRID_CLIP_FINAL.csv", header=T, sep=",")  #Read in covariate data for all road segments

indep.data$rdclass <- factor(indep.data$rdclass, levels = 0:5)  #Define road class covariate as a factor with six levels

lm.preds <- predict(trans.lglm, indep.data, type="response")  #Predict traffic volume to all road segments using model fit


#RF
rf.preds <- predict(trans.rf, model.data, type="response")
vol.preds.rf <- as.data.frame(cbind(model.data$uid,exp(rf.preds)))  #Combine predictions with unique IDs for all road segments
names(vol.preds.rf) <- c("uid","tvol") 
write.csv(vol.preds.rf, file = "Pred/vol_preds_rf.csv")
##


vol.preds <- as.data.frame(cbind(indep.data$uid,exp(lm.preds)))  #Combine predictions with unique IDs for all road segments

names(vol.preds) <- c("uid","tvol")  #Rename columns in dataframe

write.csv(vol.preds, file = "../output/vol_preds.csv")  #Write out predictions for all road segments
