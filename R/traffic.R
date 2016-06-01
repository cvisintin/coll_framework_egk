library("randomForest")
library("data.table")

model.data <- as.data.table(read.delim("data/vic_model_data_traffic.csv", header=T, sep=","))  #Read in traffic volume data for road segments
#model.data$rdclass <- factor(model.data$rdclass, levels = 0:5)  #Define road class covariate as a factor with six levels

model.data[!is.na(aadt),.N]

model.data[!is.na(speedlmt),.N]

cor(na.omit(model.data[,.(popdens,kmtohwy,kmtodev,rddens,x,y)]))

# volume.lm <- lm(log(aadt) ~ kmtodev + kmtohwy + popdens + rdclass + rddens, data = model.data[!is.na(model.data$aadt),])
# 
# vol.preds <- predict(volume.lm, model.data, type="response")
# vol.preds.df <- cbind("uid"=model.data$uid,"tvol"=exp(vol.preds)) #Combine predictions with unique IDs for all road segments

set.seed(123)
volume.rf <- randomForest(formula = log(aadt) ~ kmtodev + kmtohwy + popdens + rdclass + rddens, data = model.data[!is.na(model.data$aadt),], mtry=2, importance = TRUE, sampsize = 1000)  #Fit random forest model

vol.preds <- predict(volume.rf, model.data, type="response")

vol.preds.df <- cbind("uid"=model.data$uid,"tvol"=exp(vol.preds)) #Combine predictions with unique IDs for all road segments
write.csv(vol.preds.df, file = "output/tvol_preds_rf.csv", row.names=FALSE)

vol.preds.dt <- as.data.table(vol.preds.df)
setkey(vol.preds.dt,uid)
perf.vol <- merge(vol.preds.dt,model.data[!is.na(model.data$aadt),])
plot(perf.vol$aadt,perf.vol$tvol)
abline(a=0,b=.5, lty=2)

# speed.lm <- lm(speedlmt ~ rdclass + rddens, data = model.data[!is.na(model.data$speedlmt),])
# 
# speed.preds <- predict(speed.lm, model.data, type="response")
# speed.preds.df <- cbind("uid"=model.data$uid,"tspd"=speed.preds) #Combine predictions with unique IDs for all road segments

set.seed(123)
speed.rf <- randomForest(formula = speedlmt ~ rdclass + rddens, data = model.data[!is.na(model.data$speedlmt),], mtry=2, importance = TRUE, sampsize = 1000)  #Fit random forest model

speed.preds <- predict(speed.rf, model.data, type="response")

speed.preds.df <- cbind("uid"=model.data$uid,"tspd"=speed.preds)  #Combine predictions with unique IDs for all road segments
write.csv(speed.preds.df, file = "output/tspd_preds_rf.csv", row.names=FALSE)

speed.preds.dt <- as.data.table(speed.preds.df)
setkey(speed.preds.dt,uid)
perf.spd <- merge(speed.preds.dt,model.data[!is.na(model.data$speedlmt),])
plot(perf.spd$speedlmt,perf.spd$tspd)
abline(a=0,b=.5, lty=2)

# localH2O = h2o.init(nthreads = -1) 
# 
# traffic.hex <-  h2o.uploadFile(path = "data/vic_model_data_traffic.csv")
# summary(traffic.hex)
# 
# traffic.hex$rdclass <- as.factor(traffic.hex$rdclass)
# 
# volume.train = traffic.hex[!is.na(traffic.hex$aadt),]
# volume.train$l_aadt <- log(volume.train$aadt)
# 
# volume.h2orf <- h2o.randomForest(
#   x=c("kmtodev","kmtohwy","popdens","rdclass","rddens"),
#   y="l_aadt",
#   training_frame=volume.train,
#   validation_frame=NULL,
#   mtries = 1,
#   sample_rate = 0.67,
#   build_tree_one_node = FALSE,
#   ntrees = 500,
#   binomial_double_trees = FALSE,
#   balance_classes = FALSE,
#   seed=123)
# 
# volume.preds <- h2o.predict(volume.h2orf, traffic.hex)
# 
# vol.preds.df <- cbind(as.data.frame(traffic.hex[["uid", exact = TRUE]]),as.data.frame(exp(volume.preds[["predict", exact = TRUE]])))
# 
# names(vol.preds.df) <- c("uid","tvol")
# 
# write.csv(vol.preds.df, file = "output/tvol_preds_rf.csv")
# 
# 
# speed.train = traffic.hex[!is.na(traffic.hex$speedlmt),]
# 
# speed.h2orf <- h2o.randomForest(
#   x=c("rdclass","rddens"),
#   y="speedlmt",
#   training_frame=speed.train,
#   validation_frame=NULL,
#   mtries = -1,
#   sample_rate = 0.67,
#   build_tree_one_node = FALSE,
#   ntrees = 500,
#   binomial_double_trees = FALSE,
#   balance_classes = FALSE,
#   seed=123)
# 
# speed.preds <- h2o.predict(speed.h2orf, traffic.hex)
# 
# speed.preds.df <- cbind(as.data.frame(traffic.hex[["uid", exact = TRUE]]),as.data.frame(speed.preds[["predict", exact = TRUE]]))
# 
# names(speed.preds.df) <- c("uid","tspd")
# 
# write.csv(speed.preds.df, file = "output/tspd_preds_rf.csv")


for(i in 1:5) partialPlot(volume.rf,X,x.var=names(X)[i])