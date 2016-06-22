volume.vars <- names(model.data[!is.na(model.data$aadt),.(kmtodev,kmtohwy,popdens,rdclass,rddens)])
for(i in 1:length(volume.vars)){
  temp <- partialPlot(volume.rf,as.data.frame(model.data[!is.na(model.data$aadt),]),x.var=volume.vars[i], xlab=toupper(volume.vars[i]), ylab="AADT", n.pt=100, plot=FALSE)
  tiff(paste0('figs/',volume.vars[i],'_volume.tif'), pointsize = 6, compression = "lzw", res=300, width = 600, height = 600)
  par(mgp=c(2.0,0.5,0),mar=c(3.5,3.5,1,1), cex.axis=0.8)
  plot(temp$x,exp(temp$y), type="l", lwd=0.5, xlab=toupper(volume.vars[i]), ylab="AADT", xaxt="n", yaxt="n", bty="n")
  axis(side = 1, lwd = 0.2)
  axis(side = 2, lwd = 0.2)
  box(lwd=0.3)
  dev.off()
}

speed.vars <- names(model.data[!is.na(model.data$speedlmt),.(rdclass,rddens)])
for(i in 1:length(speed.vars)){
  temp <- partialPlot(speed.rf,as.data.frame(model.data[!is.na(model.data$speedlmt),]),x.var=speed.vars[i], xlab=toupper(speed.vars[i]), ylab="SPEED", n.pt=100, plot=FALSE)
  tiff(paste0('figs/',speed.vars[i],'_speed.tif'), pointsize = 6, compression = "lzw", res=300, width = 600, height = 600)
  par(mgp=c(2.0,0.5,0),mar=c(3.5,3.5,1,1), cex.axis=0.8)
  plot(temp$x,temp$y, type="l", xlab=toupper(speed.vars[i]), ylab="SPEED", xaxt="n", yaxt="n", bty="n")
  axis(side = 1, lwd = 0.2)
  axis(side = 2, lwd = 0.2)
  box(lwd=0.3)
  dev.off()
}