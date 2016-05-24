library("randomForest")
library("data.table")
library("doMC")

model.data <- as.data.table(read.delim("../data/model_data_traffic.csv", header=T, sep=",", na.strings=c("NA", "NULL")))  #Read in traffic volume data for road segments
model.data$RDCLASS <- factor(model.data$RDCLASS, levels = 0:5)  #Define road class covariate as a factor with six levels

set.seed(123)
volume.rf <- randomForest(formula = log(AADT) ~ INCOMEPP + KMTODEV + KMTOHWY + POPDENS + RDCLASS +RDDENS + X + Y, data = model.data[!is.na(model.data$AADT),], mtry=4)  #Fit random forest model

vol.preds <- predict(volume.rf, model.data, type="response")
vol.preds.df <- as.data.frame(cbind(model.data$UID,exp(vol.preds)))  #Combine predictions with unique IDs for all road segments
names(vol.preds.df) <- c("UID","TVOL") 
write.csv(vol.preds.df, file = "../output/vol_preds_rf.csv")

set.seed(123)
speed.rf3 <- randomForest(formula = SPEEDLMT ~ RDCLASS + RDDENS + X + Y, data = model.data[!is.na(model.data$SPEEDLMT),], mtry=2)  #Fit random forest model

speed.preds <- predict(speed.rf, model.data, type="response")
speed.preds.df <- as.data.frame(cbind(model.data$UID,speed.preds))  #Combine predictions with unique IDs for all road segments
names(speed.preds.df) <- c("UID","TSPD") 
write.csv(speed.preds.df, file = "../output/speed_preds_rf.csv")


###########Working...

registerDoMC(detectCores() - 1)
volume.rfp <- foreach(y=seq(10), .combine=combine ) %dopar% {
  set.seed(123) # not really needed
  volume.rf <- randomForest(formula = log(AADT) ~ INCOMEPP + KMTODEV + KMTOHWY + POPDENS + RDCLASS +RDDENS + X + Y, data = model.data[!is.na(model.data$AADT),], mtry=4, ntree=50, norm.votes=FALSE)
  volume.rf
}

sqrt(sum((exp(volume.rf$predicted) - model.data[!is.na(model.data$AADT),.(AADT)])^2) / nrow(model.data))


localH2O = h2o.init() 
volume.hex <-  h2o.uploadFile(localH2O, path = "../data/model_data_traffic.csv")
summary(volume.hex)

volume.hex[,8] <- as.factor(volume.hex[,8])

volume.train = volume.hex[!is.na(volume.hex$AADT),]
volume.train$L_AADT <- log(volume.train$AADT)


volume.val = volume.hex[is.na(volume.hex$AADT),]
volume.val$L_AADT <- log(volume.val$AADT)


volume.h2orf <- h2o.randomForest(
  x=c("INCOMEPP","KMTODEV","KMTOHWY","POPDENS","RDCLASS","RDDENS","X","Y"),
  y="L_AADT",
  training_frame=volume.train,
  validation_frame=NULL,
  mtries = -1,
  sample_rate = 0.67,
  build_tree_one_node = FALSE,
  ntrees = 500,
  binomial_double_trees = FALSE,
  balance_classes = FALSE,
  seed=123)

volume.preds <- h2o.predict(volume.h2orf, volume.hex)

vol.preds <- cbind(as.data.frame(volume.hex[["UID", exact = TRUE]]),as.data.frame(exp(volume.preds[["predict", exact = TRUE]])))

names(vol.preds) <- c("UID","TVOL")

write.csv(vol.preds, file = "../output/vol_preds_rf.csv")
