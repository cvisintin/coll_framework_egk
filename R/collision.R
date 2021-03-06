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

roads <- as.data.table(dbGetQuery(con,"
  SELECT
    r.uid as uid, ST_X(r.geom) AS x, ST_Y(r.geom) AS y
  FROM
	  (SELECT
      uid, ST_ClosestPoint(geom, ST_Centroid(geom)) AS geom
		FROM
      gis_victoria.vic_gda9455_roads_state) AS r
  "))
setkey(roads,uid)

tvol.preds <- as.data.table(read.csv("output/tvol_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

tspd.preds <- as.data.table(read.csv("output/tspd_preds_rf.csv"))  #Read in collision data training set (presences/absences of collisions and covariates)

cov.data <- Reduce(function(x, y) merge(x, y, all=TRUE), list(roads,tvol.preds,tspd.preds))

sdm.preds <- raster("output/egk_preds_brt.tif")

cov.data$egk <- raster::extract(sdm.preds,cov.data[,.(x,y)])

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

data1 <- merge(cov.data, coll)

set.seed(123)
data0 <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1)),],"coll"=rep(0,2*nrow(data1)))

model.data <- rbind(data1,data0)
model.data <- na.omit(model.data)

coll.glm <- glm(formula = coll ~ log(egk) + log(tvol) + log(tspd), family=binomial(link = "cloglog"), data = model.data)  #Fit regression model

summary(coll.glm)  #Examine fit of regression model

paste("% Deviance Explained: ",round(((coll.glm$null.deviance - coll.glm$deviance)/coll.glm$null.deviance)*100,2),sep="")  #Report reduction in deviance

write.csv(signif(summary(coll.glm)$coefficients, digits=4),"output/coll_coef.csv",row.names=FALSE)

write.csv(formatC(anova(coll.glm)[2:4,2]/sum(anova(coll.glm)[2:4,2]), format='f',digits=4),"output/coll_anova.csv",row.names=FALSE)

save(coll.glm,file="output/coll_glm")

save(model.data,file="output/coll_model_data")

coll.preds <- predict(coll.glm, cov.data, type="response")

coll.preds.df <- cbind("uid"=cov.data$uid,"collrisk"=coll.preds) #Combine predictions with unique IDs for all road segments
coll.preds.df <- na.omit(coll.preds.df)

write.csv(coll.preds.df, file = "output/coll_preds_glm.csv", row.names=FALSE)


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

data1v <- merge(cov.data, coll.ind)

set.seed(123)
data0v <- cbind(cov.data[sample(seq(1:nrow(cov.data)),2*nrow(data1v)),],"coll"=rep(0,2*nrow(data1v)))

val.data <- rbind(data1v,data0v)
val.data <- na.omit(val.data)

val.pred.glm <- predict(coll.glm, val.data, type="response")  #Make predictions with regression model fit

roc.val <- roc(val.data$coll, val.pred.glm)  #Compare collision records to predictions using receiver operator characteristic (ROC) function and report value