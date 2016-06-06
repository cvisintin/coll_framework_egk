require(RPostgreSQL)
require(data.table)
require(raster)
require(boot)

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

set.seed(123)
cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
(cv.10.err <- cv.glm(model.data.alt, coll.glm.alt, cost=cost, K = 10)$delta)


require(loo)
require(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores()-1)

N <- nrow(model.data.alt)
y <- model.data.alt$coll
x1 <- model.data.alt$elev
x2 <- model.data.alt$green
x3 <- model.data.alt$kmtodev
x4 <- model.data.alt$kmtohwy
x5 <- model.data.alt$light
x6 <- model.data.alt$mntempwq
x7 <- model.data.alt$popdens
x8 <- model.data.alt$precdm
x9 <- model.data.alt$rdclass
x10 <- model.data.alt$rddens
x11 <- model.data.alt$slope
x12 <- model.data.alt$treedens

scode.alt <- "
data{
int<lower=1> N;
int<lower=0,upper=1> y[N];
real x1[N];
real x2[N];
real x3[N];
real x4[N];
real x5[N];
real x6[N];
real x7[N];
real x8[N];
real x9[N];
real x10[N];
real x11[N];
real x12[N];
}
parameters{
real a;
real b1;
real b2;
real b3;
real b4;
real b5;
real b6;
real b7;
real b8;
real b9;
real b10;
real b11;
real b12;
}
transformed parameters {
real p[N];
for (i in 1:N)
p[i] <- inv_cloglog(a + b1 * x1[i] + b2 * x2[i] + b3 * x3[i] + b4 * x4[i] + b5 * x5[i] + b6 * x6[i] + b7 * x7[i] + b8 * x8[i] + b9 * x9[i] + b10 * x10[i] + b11 * x11[i] + b12 * x12[i]);
}
model{
b12 ~ normal( 0 , 1 );
b11 ~ normal( 0 , 1 );
b10 ~ normal( 0 , 1 );
b9 ~ normal( 0 , 1 );
b8 ~ normal( 0 , 1 );
b7 ~ normal( 0 , 1 );
b6 ~ normal( 0 , 1 );
b5 ~ normal( 0 , 1 );
b4 ~ normal( 0 , 1 );
b3 ~ normal( 0 , 1 );
b2 ~ normal( 0 , 1 );
b1 ~ normal( 0 , 1 );
a ~ normal( 0 , 1 );
y ~ binomial( 1 , p );
}
"
generated quantities{
real log_lik[N];
real yhat[N];
for (i in 1:N) {
log_lik[i] <- y[i]*log(p[i]) + (1-y[i])*log(1-p[i]);  
yhat[i] <- p[i]*1;
}
}


coll_model_fit.alt <- stan(model_code = scode.alt, iter = 100, chains = 1, cores = 1, seed=123, init=100)
#summary(coll_model_fit)
#traceplot(As.mcmc.list(coll_model_fit,c("a","b1","b2","b3")))

log_lik_coll.alt <- extract_log_lik(coll_model_fit.alt)
loo_coll.alt <- loo(log_lik_coll.alt)

diff <- compare(loo_coll, loo_coll.alt)
