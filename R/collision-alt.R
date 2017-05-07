require(RPostgreSQL)
require(data.table)
require(raster)

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

roads.alt <- as.data.table(read.csv("data/vic_model_data_traffic.csv"))#Read in collision data training set (presences/absences of collisions and covariates)
setkey(roads.alt, uid)

grid.files <- list.files(path='data/grids/envi') #Create vector of filenames

grid.names <- substring(unlist(strsplit(grid.files,"\\_1000."))[(1:(2*(length(grid.files)))*2)-1][1:length(grid.files)],18) #Create vector of covariate names

vic.rst <- raster("data/grids/VIC_GDA9455_GRID_STATE_1000.tif")

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

#Read in grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/envi/",grid.files[i]))
  temp <- crop(temp, clip)
  assign(grid.names[i],temp * vic.rst)
}
vars <- stack(mget(grid.names)) #Combine all maps to single stack

samples.df <- extract(vars,roads.alt[,.(x,y)]) #Sample covariates at coordinates

colnames(samples.df) <- tolower(colnames(samples.df))

cov.data.alt <- cbind(roads.alt,samples.df) #Combine occurrence data with covariate values

setkey(cov.data.alt, uid)

#cov.data.alt$rdclass <- factor(cov.data.alt$rdclass, levels = 0:5)

cov.data.alt$aadt <- NULL
cov.data.alt$speedlmt <- NULL

coll <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
	FROM
    gis_victoria.vic_gda9455_roads_state as r,
      (SELECT
        id, geom
      FROM
        gis_victoria.vic_gda9455_fauna_wv
      WHERE
        species = 'Kangaroo -  Eastern Grey'
      AND
        cause = 'hit by vehicle'
      AND
        year < 2013) AS p
  WHERE ST_DWithin(p.geom,r.geom,100)
  ORDER BY p.id, ST_Distance(p.geom,r.geom)
  "))
setkey(coll,uid)

data1.alt <- merge(cov.data.alt, coll)

set.seed(123)
data0.alt <- cbind(cov.data.alt[sample(seq(1:nrow(cov.data.alt)),2*nrow(data1.alt)),],"coll"=rep(0,2*nrow(data1.alt)))

model.data.alt <- rbind(data1.alt,data0.alt)

model.data.alt <- na.omit(model.data.alt)

cor(model.data.alt[,.(elev,green,kmtodev,kmtohwy,light,mntempwq,popdens,precdm,rddens,slope,treedens,x,y)])

coll.glm.alt <- glm(formula = coll ~ elev + green + kmtodev + kmtohwy + light + mntempwq + popdens + precdm + rdclass + rddens + slope + treedens, family=binomial(link = "cloglog"), data = model.data.alt)  #Fit regression model

summary(coll.glm.alt)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm.alt$null.deviance - coll.glm.alt$deviance)/coll.glm.alt$null.deviance)*100,2),sep="")  #Report reduction in deviance

write.csv(signif(summary(coll.glm.alt)$coefficients, digits=4),"output/coll_coef_alt.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm.alt)[2:13,2]/sum(anova(coll.glm.alt)[2:13,2]), format='f',digits=4),"output/coll_anova_alt.csv",row.names=FALSE)

coll.ind <- as.data.table(dbGetQuery(con,"
  SELECT DISTINCT ON (p.id)
    r.uid AS uid, CAST(1 AS INTEGER) AS coll
  FROM
   gis_victoria.vic_gda9455_roads_state as r,
   (SELECT
     id, geom
   FROM
     gis_victoria.vic_gda9455_fauna_wv
   WHERE
     species = 'Kangaroo -  Eastern Grey'
   AND
     cause = 'hit by vehicle'
   AND
     year >= 2013
      AND
        year != 2014) AS p
   WHERE ST_DWithin(p.geom,r.geom,100)
   ORDER BY p.id, ST_Distance(p.geom,r.geom)
   ")) #~1 second query
setkey(coll.ind,uid)

data1v.alt <- merge(cov.data.alt, coll.ind)

set.seed(123)
data0v.alt <- cbind(cov.data.alt[sample(seq(1:nrow(cov.data.alt)),2*nrow(data1v.alt)),],"coll"=rep(0,2*nrow(data1v.alt)))

val.data <- rbind(data1v.alt,data0v.alt)
val.data <- na.omit(val.data)

val.pred.glm <- predict(coll.glm.alt, val.data, type="response")  #Make predictions with regression model fit

roc.val.alt <- roc(val.data$coll, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value