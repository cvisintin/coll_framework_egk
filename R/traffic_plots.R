volume.vars <- names(model.data[!is.na(model.data$aadt),.(kmtodev,kmtohwy,popdens,rdclass,rddens)])
for(i in 1:length(volume.vars)){
  temp <- partialPlot(volume.rf,as.data.frame(model.data[!is.na(model.data$aadt),]),x.var=volume.vars[i], xlab=toupper(volume.vars[i]), ylab="AADT", n.pt=100, plot=FALSE)
  tiff(paste0('figs/',volume.vars[i],'_volume.tif'), pointsize = 20)
  plot(temp$x,exp(temp$y), type="l", xlab=toupper(volume.vars[i]), ylab="AADT")
  dev.off()
}

speed.vars <- names(model.data[!is.na(model.data$speedlmt),.(rdclass,rddens)])
for(i in 1:length(speed.vars)){
  temp <- partialPlot(speed.rf,as.data.frame(model.data[!is.na(model.data$speedlmt),]),x.var=speed.vars[i], xlab=toupper(speed.vars[i]), ylab="SPEED", n.pt=100, plot=FALSE)
  tiff(paste0('figs/',speed.vars[i],'_speed.tif'), pointsize = 20)
  plot(temp$x,temp$y, type="l", xlab=toupper(speed.vars[i]), ylab="SPEED")
  dev.off()
}