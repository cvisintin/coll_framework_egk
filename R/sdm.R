require(maptools)
require(scales)
require(rgdal)
require(raster)
require(gbm)
require(dismo)
require(data.table)

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

model.data <- read.csv("data/vic_model_data_sdm.csv", header=T, sep=",")

length(model.data$OCC[model.data$OCC==1])

length(model.data$OCC[model.data$OCC==0])

sdm.colors = colorRampPalette(c("white","red")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

kang.brt = gbm.step(data = model.data, gbm.x = 4:10, gbm.y = 3, family = "bernoulli", tree.complexity = 7, learning.rate = 0.005, bag.fraction = 0.5) #Create boosted regression tree model

brt.preds <- predict(vars, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps

writeRaster(brt.preds, filename="output/egk_preds_brt.tif", format="GTiff", overwrite=TRUE) #Write out prediction map in tif format

#Use system to translate and uplaod grid to postgis database
system("raster2pgsql -d -I -M -s 28355 -t auto /home/casey/Research/Github/coll_framework_egk/output/egk_preds_brt.tif gis_victoria.vic_gda9455_grid_egk_preds_brt | PGPASSWORD=Qpostgres15 psql -d qaeco_spatial -h boab.qaeco.com -p 5432 -U qaeco -w")

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

########Plot Figures########

tiff('figs/light.tif', pointsize = 20)
gbm.plot.var(kang.brt, i.var=3, type="response", ylab="KANGAROO OCCURRENCE", xlab="LIGHT")
#axis(2, at=seq(0.0,1.0,0.1), labels=seq(0.0,1.0,0.1))
dev.off()

tiff('figs/elev.tif', pointsize = 20)
plot.gbm.var(kang.brt, i.var=1, type="response", ylab="KANGAROO OCCURRENCE", xlab="ELEV")
#axis(2, at=seq(0.0,1.0,0.1), labels=seq(0.0,1.0,0.1))
dev.off()

tiff('figs/precdm.tif', pointsize = 20)
plot.gbm.var(kang.brt, i.var=5, type="response", ylab="KANGAROO OCCURRENCE", xlab="PRECDM")
#axis(2, at=seq(0.0,1.0,0.1), labels=seq(0.0,1.0,0.1))
dev.off()


########Predict to Continental Scale########

extent(-3070000, 1150000, 5160000, 8830000) #Define extent of map

#Read in and convert grid files to raster maps
for(i in 1:length(grid.files)) {
  temp <- raster(paste0("data/grids/envi/",grid.files[i]))
  assign(grid.names[i], temp, immediate=T)
}

vars.aus <- stack(mget(grid.names[1:7])) #Combine all maps to single stack

brt.AUSpreds <- predict(vars.aus, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps - this takes some time...

writeRaster(brt.AUSpreds, filename="output/egk_preds_brt-aus.tif", format="GTiff", overwrite=TRUE) #Write out prediction map in grid format

plot(brt.AUSpreds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme


########Collisions within EGK preds########

all.coll <- as.data.table(dbGetQuery(con,"
      SELECT
        id as uid, ST_X(geom) as x, ST_Y(geom) as y
      FROM
        gis_victoria.vic_gda9455_fauna_wv
      WHERE
        species = 'Kangaroo -  Eastern Grey'
      AND
        cause = 'hit by vehicle'
      AND
        year<2014
  "))
setkey(all.coll,uid)

all.coll[, coll_pred:=extract(brt.preds,all.coll[,.(x,y)])]

hist(all.coll[,na.omit(coll_pred)])

all.coll[coll_pred>=.2,.N]/all.coll[,.N]
