library("RPostgreSQL")
library("raster")

drv <- dbDriver("PostgreSQL")  #Specify a driver for postgreSQL type database
con <- dbConnect(drv, dbname="qaeco_spatial", user="qaeco", password="Qpostgres15", host="boab.qaeco.com", port="5432")  #Connection to database server on Boab

#Define function for receiver operator characteristic (ROC)
"roc" <- function (obsdat, preddat){
    if (length(obsdat) != length(preddat)) 
      stop("obs and preds must be equal lengths")
    n.x <- length(obsdat[obsdat == 0])
    n.y <- length(obsdat[obsdat == 1])
    xy <- c(preddat[obsdat == 0], preddat[obsdat == 1])
    rnk <- rank(xy)
    roc <- ((n.x * n.y) + ((n.x * (n.x + 1))/2) - sum(rnk[1:n.x]))/(n.x * n.y)
    return(round(roc, 4))
  }

model.data <- read.delim("Data/model_data_coll.csv", header=T, sep=",")  #Read in collision data training set (presences/absences of collisions and covariates)

model.preds <- raster("/home/casey/Research/Projects/SDMs/Data/Preds/EGK.tif")

samples.df <- extract(model.preds,model.data[,6:7])

model.data$EGK <- samples.df

model.data <- na.omit(model.data)

#Calculate natural logarithm of each covariate to test multiplicative effect of linear relationship
model.data$log.EGK <- log(model.data$EGK)
model.data$log.TVOL <- log(model.data$TVOL)
model.data$log.TSPD <- log(model.data$TSPD)

#Center logged covariates by subtracting means
model.data$c.log.EGK <- model.data$log.EGK - mean(model.data$log.EGK)
model.data$c.log.TVOL <- model.data$log.TVOL - mean(model.data$log.TVOL)
model.data$c.log.TSPD <- model.data$log.TSPD - mean(model.data$log.TSPD)

coll.glm <- glm(formula = COLL ~ c.log.EGK + c.log.TVOL+ c.log.TSPD, family=binomial(link = "cloglog"), data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.5860  -0.7995  -0.4358   0.9125   3.1441  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -0.96239    0.05416 -17.770  < 2e-16 ***
#   c.log.EGK    0.48298    0.03803  12.700  < 2e-16 ***
#   c.log.TVOL   0.14447    0.04333   3.334 0.000856 ***
#   c.log.TSPD   3.96940    0.27494  14.437  < 2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2026.4  on 1514  degrees of freedom
# Residual deviance: 1520.7  on 1511  degrees of freedom
# AIC: 1528.7
# 
# Number of Fisher Scoring iterations: 8

val.data <- read.delim("Data/model_data_coll2.csv", header=T, sep=",")  #Read in collision data test set (presences/absences of collisions and covariates for road segments) for validation

samples.df <- extract(model.preds,val.data[,7:8])

val.data$EGK <- samples.df

val.data <- na.omit(val.data)

anova(coll.glm)  #Examine contribution of variables 

#Calculate natural logarithm of each covariate
val.data$log.EGK <- log(val.data$EGK)
val.data$log.TVOL <- log(val.data$TVOL)
val.data$log.TSPD <- log(val.data$TSPD)

#Center logged covariates by subtracting means to match covariates used in regression model
val.data$c.log.EGK <- val.data$log.EGK - mean(val.data$log.EGK)
val.data$c.log.TVOL <- val.data$log.TVOL - mean(val.data$log.TVOL)
val.data$c.log.TSPD <- val.data$log.TSPD - mean(val.data$log.TSPD)

val.pred.glm <- predict(coll.glm, val.data, type="response")  #Make predictions with regression model fit

roc.val <- roc(val.data$COLL, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value

# dbGetQuery(con,"
# ALTER TABLE  gis_victoria.vic_gda9455_roads_state ADD COLUMN egk double precision;
# 
# UPDATE
#   gis_victoria.vic_gda9455_roads_state
# SET
#   egk=x.egk
# FROM
# 	(SELECT p.uid AS uid, ST_Value(r.rast, 1, ST_Line_Interpolate_Point(ST_LineMerge(p.geom),0.5), true) AS egk
# 	FROM vline.egk2 AS r, gis_victoria.vic_gda9455_roads_state AS p
# 	WHERE ST_Intersects(r.rast,ST_Line_Interpolate_Point(ST_LineMerge(p.geom),0.5))
# 	) as x
# WHERE
#   x.uid = gis_victoria.vic_gda9455_roads_state.uid;
#   ")

indep.data <- dbGetQuery(con,"SELECT uid, egk, tvol, tspd FROM gis_victoria.vic_gda9455_roads_state;")

#indep.data <- read.delim("Data/VIC_GDA9455_ROADS_VICSTATE_GRID_CLIP_RISKMODEL.csv", header=T, sep=",")  #Read in covariate data for all road segments

colnames(indep.data) <- c("UID","EGK","TVOL","TSPD")

indep.data <- na.omit(indep.data)  #Remove any records with missing information

#Calculate natural logarithm of each covariate
indep.data$log.EGK <- log(indep.data$EGK)
indep.data$log.TVOL <- log(indep.data$TVOL)
indep.data$log.TSPD <- log(indep.data$TSPD)

#Center logged covariates by subtracting means to match covariates used in regression mode
indep.data$c.log.EGK <- indep.data$log.EGK - mean(indep.data$log.EGK)
indep.data$c.log.TVOL <- indep.data$log.TVOL - mean(indep.data$log.TVOL)
indep.data$c.log.TSPD <- indep.data$log.TSPD - mean(indep.data$log.TSPD)

glm.preds <- predict(coll.glm, indep.data, type="response")  #Predict collision probability to all road segments using model fit

coll.preds <- as.data.frame(cbind(indep.data$UID,glm.preds))  #Combine predictions with unique IDs for all road segments

names(coll.preds) <- c("UID","COLL")  #Rename columns in dataframe

write.csv(coll.preds, file = "Pred/coll_preds.csv", row.names=FALSE)  #Write out predictions for all road segments

names(coll.preds) <- c("uid","coll")

dbWriteTable(con, c("gis_victoria","vic_nogeom_roads_egkcollrisk"), coll.preds, row.names = F, append = F)

dbGetQuery(con,"ALTER TABLE gis_victoria.vic_nogeom_roads_egkcollrisk ADD COLUMN pkey BIGSERIAL PRIMARY KEY;")

dbGetQuery(con,"
ALTER TABLE gis_victoria.vic_gda9455_roads_state ADD COLUMN egkrisk DOUBLE PRECISION;

UPDATE
  gis_victoria.vic_gda9455_roads_state
SET
  egkrisk=x.coll
FROM
	gis_victoria.vic_nogeom_roads_egkcollrisk as x
WHERE
  x.uid = gis_victoria.vic_gda9455_roads_state.uid;
  ")
