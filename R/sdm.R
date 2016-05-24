#Load in required packages
require(mapdata)
require(maptools)
require(scales)
require(rgdal)
require(raster)
require(spdep)
require(gbm)
require(dismo)

ascii.files <- list.files(path='../data/grids/envi') #Create vector of filenames

ascii.names <- unlist(strsplit(ascii.files,"\\."))[(1:(2*(length(ascii.files)))*2)-1][1:length(ascii.files)] #Create vector of covariate names

victoria <- readShapePoly("../data/VIC_GDA9455_ADMIN_STATE_EXP500M.shp") #Read in shapefile for study area boundary

#expand shapefile with buffer operation???

r <- raster(ncol=822, nrow=563, xmn=-58000, xmx=764000, ymn=5661000, ymx=6224000) #Create raster template to define extents and resolution of maps

vic.rst <- rasterize(victoria, r, 'UFI') #Rasterize shapefile for use in raster calculations

clip <- extent(-58000, 764000, 5661000, 6224000) #Define clipping extent of maps

#Read in ASCII grids, crop, and multiply with template to create consistent covariate maps
for (i in 1:length(ascii.files)) {
  temp <- raster(paste0("../data/grids/envi/",ascii.files[i]))
  temp <- crop(temp, clip)
  assign(ascii.names[i],temp * vic.rst)
}
vars <- stack(mget(ascii.names)) #Combine all maps to single stack

egk.final <- read.csv("../data/model_data_egk.csv", header=T, sep=",") #Read in dependent variable data (presences/absences of kangaroos and locations)
egk.coord <- egk.final[,1:2] #Extract coordinates for sampling

samples.df <- extract(vars,egk.coord) #Sample covariates at coordinates

model.data <- cbind(egk.final,samples.df) #Combine presence/absence data with covariate values
model.data <- na.omit(model.data) #Remove any records with missing information 

sdm.colors = colorRampPalette(c("white","red")) #Define color scheme for plotting

set.seed(123) #Set random seed to make results of gradient boosted regressions identical for each run

kang.brt = gbm.step(data = model.data, gbm.x = 4:10, gbm.y = 3, family = "bernoulli", tree.complexity = 7, learning.rate = 0.005, bag.fraction = 0.5) #Create boosted regression tree model

brt.preds <- predict(vars, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps

writeRaster(brt.preds, filename="../output/EGK_preds.asc", format="ascii", overwrite=TRUE) #Write out prediction map in ASCII format

plot(brt.preds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme

########Predict to Continental Scale########

extent(-3070000, 1150000, 5160000, 8830000) #Define extent of map

#Read in and convert ASCII files to raster maps
for(i in 1:length(ascii.files)) {
  temp <- raster(paste0("../data/grids/envi",ascii.files[i]))
  assign(ascii.names[i], temp, immediate=T)
}

vars.aus <- stack(mget(ascii.names)) #Combine all maps to single stack

brt.AUSpreds <- predict(vars.aus, kang.brt, n.trees=kang.brt$gbm.call$best.trees, type="response") #Make predictions with model fit based on covariate values in maps - this takes some time...

writeRaster(brt.AUSpreds, filename="../output/EGK_preds-AUS.asc", format="ascii", overwrite=TRUE) #Write out prediction map in ASCII format

plot(brt.AUSpreds, col=sdm.colors(100)) #Plot prediction map using red to white color scheme
