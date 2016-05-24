require(mapdata)
require(maptools)
require(scales)
require(rgdal)
require(raster)
require(spdep)
require(gbm)

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

model.data <- read.delim("../data/model_data_coll_class.csv", header=T, sep=",")  #Read in collision data training set (presences/absences of collisions and covariates)

victoria <- readShapePoly("../data/VIC_GDA9455_ADMIN_STATE_EXP500M.shp") #Read in shapefile for study area boundary

r <- raster(ncol=822, nrow=563, xmn=-58000, xmx=764000, ymn=5661000, ymx=6224000) #Create raster template to define extents and resolution of maps

vic.rst <- rasterize(victoria, r, 'UFI') #Rasterize shapefile for use in raster calculations

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

ascii.files1 <- list.files(path='../data/grids/envi') #Create vector of filenames

ascii.names1 <- unlist(strsplit(ascii.files1,"\\."))[(1:(2*(length(ascii.files1)))*2)-1][1:length(ascii.files1)] #Create vector of covariate names

#Read in ASCII grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(ascii.files1)) {
  temp <- raster(paste0("../data/grids/envi",ascii.files1[i]))
  temp <- crop(temp, clip)
  assign(ascii.names1[i],temp * vic.rst)
}

ascii.files2 <- list.files(path='../data/grids/anth') #Create vector of filenames

ascii.names2 <- unlist(strsplit(ascii.files2,"\\."))[(1:(2*(length(ascii.files2)))*2)-1][1:length(ascii.files2)] #Create vector of covariate names

#Read in ASCII grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(ascii.files2)) {
  temp <- raster(paste0("../data/grids/anth",ascii.files2[i]))
  temp <- crop(temp, clip)
  assign(ascii.names2[i],temp * vic.rst)
}

ascii.names <- c(ascii.names1, ascii.names2) #Combine both sets of variables

vars <- stack(mget(ascii.names)) #Combine all maps to single stack

setwd('../../') #Change working directory to top level

coord <- model.data[,7:8] #Extract coordinates for sampling

samples.df <- extract(vars,coord) #Sample covariates at coordinates

model.data <- cbind(model.data,samples.df) #Combine presence/absence data with covariate values

model.data$RDCLASS <- factor(model.data$RDCLASS, levels = 0:5)

samples.df <- extract(model.preds,model.data[,7:8])

model.data$EGK <- samples.df

model.data <- na.omit(model.data)


coll.glm <- glm(formula = COLL ~ ELEV + GREEN + INCOMEPP + KMTOCZ + KMTOHWY + KMTORZ + LIGHT + MNTEMPWQ + POPDENS + PRECDM + RDCLASS + RDDENS + SLOPE + TREEDENS, family=binomial(link = "cloglog"), data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.3459  -0.7313  -0.4645   0.8029   2.5166  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -4.0143204  0.7646073  -5.250 1.52e-07 ***
#   ELEV         0.0013709  0.0005044   2.718  0.00657 ** 
#   GREEN        1.7416404  0.7148008   2.437  0.01483 *  
#   INCOMEPP     0.0912081  0.0152404   5.985 2.17e-09 ***
#   KMTOCZ      -0.0036534  0.0030380  -1.203  0.22914    
# KMTOHWY     -0.0150383  0.0099697  -1.508  0.13145    
# KMTORZ      -0.0214754  0.0065530  -3.277  0.00105 ** 
#   LIGHT       -0.0010587  0.0044874  -0.236  0.81349    
# MNTEMPWQ     0.0281632  0.0380418   0.740  0.45910    
# POPDENS     -0.0006680  0.0001516  -4.407 1.05e-05 ***
#   PRECDM       0.0012606  0.0073383   0.172  0.86360    
# RDCLASS1     0.0979980  0.2695348   0.364  0.71617    
# RDCLASS2    -0.0183890  0.2653835  -0.069  0.94476    
# RDCLASS3    -0.2786685  0.2601363  -1.071  0.28406    
# RDCLASS4    -0.3996497  0.3032673  -1.318  0.18757    
# RDCLASS5    -1.4718125  0.2559747  -5.750 8.93e-09 ***
#   RDDENS      -0.0476203  0.0195999  -2.430  0.01511 *  
#   SLOPE        0.0158984  0.0159683   0.996  0.31943    
# TREEDENS     0.1389287  0.2743119   0.506  0.61253    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 2026.4  on 1514  degrees of freedom
# Residual deviance: 1505.5  on 1496  degrees of freedom
# AIC: 1543.5
# 
# Number of Fisher Scoring iterations: 7

anova <- anova(coll.glm)
anova[,2]/(anova[1,4]-anova[15,4])

val.data <- read.delim("../data/model_data_coll_class2.csv", header=T, sep=",")  #Read in collision data test set (presences/absences of collisions and covariates for road segments) for validation
val.data$RDCLASS <- as.factor(val.data$RDCLASS)

val.coord <- val.data[,7:8] #Extract coordinates for sampling

val.samples.df <- extract(vars,val.coord) #Sample covariates at coordinates

val.data <- cbind(val.data,val.samples.df) #Combine presence/absence data with covariate values
val.data <- na.omit(val.data) #Remove any records with missing information 

val.pred.glm <- predict(coll.glm, val.data, type="response")  #Make predictions with regression model fit

roc.val <- roc(val.data$COLL, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value


####### Latex Output - Not required for Manuscript #######

x.names <- c("\\emph{Intercept}","ELEV","GREEN","INCOMEPP","KMTOCZ","KMTOHWY","KMTORZ","LIGHT","MNTEMPWQ","POPDENS","PRECDM","RDCLASS1","RDCLASS2","RDCLASS3","RDCLASS4","RDCLASS5","RDDENS","SLOPE","TREEDENS")
x.coef <- as.numeric(round(coef(summary(coll.glm))[,1],digits=4))
x.se <- as.numeric(round(coef(summary(coll.glm))[,2],digits=4))
x.zvalue <- as.numeric(round(coef(summary(coll.glm))[,3],digits=4))
x.prz <- as.numeric(signif(coef(summary(coll.glm))[,4],digits=3))
x.anova <- c(NA,as.numeric(signif(round((anova(coll.glm)[c(2:11),2]/sum(anova(coll.glm)[2:15,2])),digits=3),digits=4)), as.numeric(signif(round(rep((anova(coll.glm)[12,2]/sum(anova(coll.glm)[2:15,2])),5),digits=3),digits=4)), as.numeric(signif(round((anova(coll.glm)[c(13:15),2]/sum(anova(coll.glm)[2:15,2])),digits=3),digits=4)))
x.anova[c(1,13:16)] <- " "
x.all <- data.frame(cbind("",x.names,as.numeric(x.coef),as.numeric(x.se),as.numeric(x.zvalue),as.numeric(x.prz),x.anova),stringsAsFactors=FALSE,row.names=NULL) 
# x.all[,3] <- as.numeric(x.all[,3])
# x.all[,4] <- as.numeric(x.all[,4])
# x.all[,5] <- as.numeric(x.all[,5])
# x.all[,6] <- as.numeric(x.all[,6])
# x.all[,7] <- as.numeric(x.all[,7])
colnames(x.all) <- c("","Variable","Coefficient","Std. Error","$Z\\text{-value}$","$\\PRZ$","ANOVA")

str(x.all)

print(xtable(x.all), include.rownames=FALSE, sanitize.text.function=function(x){x}, floating=FALSE, digits=c(0,0,3,3,3,2,4))
