require(ggplot2)

invlogit <- function (x) {1/(1+exp(-x))}

invcloglog <- function (x) {1-exp(-exp(x))}

load("output/coll_glm")
load("output/coll_model_data")

ggplot <- function(...) {
  ggplot2::ggplot(...) + 
    ylab("LIKELIHOOD OF COLLISION") +
    theme_bw() +
    theme(legend.key = element_blank(), legend.position="none") +
    theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
    theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
    theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
    theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
    theme(text = element_text(size = 12))
  }

tiff('figs/egk.tif', pointsize = 20, compression = "lzw", res=300, width = 900, height = 900)
occ.range <- seq(0,1,1/(nrow(model.data)+1))[-c(1,length(seq(0,1,1/(nrow(model.data)+1))))]
ggplot(data = model.data) +
  geom_line(aes(x = occ.range, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*log(occ.range) + coef(coll.glm)[3]*mean(log(tvol)) + coef(coll.glm)[4]*mean(log(tspd)) ) ) ) +
  geom_ribbon(aes(x = occ.range, ymin=invcloglog(
    coef(coll.glm)[1] +
      (coef(coll.glm)[2] - coef(summary(coll.glm))[2,2]*1.96)*log(occ.range) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      coef(coll.glm)[4]*mean(log(tspd))
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      (coef(coll.glm)[2] + coef(summary(coll.glm))[2,2]*1.96)*log(occ.range) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      coef(coll.glm)[4]*mean(log(tspd))
  )
  ), alpha=0.3
  ) +
  xlab("EGK") +
  scale_x_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0,1,.2)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0,1,.1))
dev.off()

tiff('figs/tvol.tif', pointsize = 20, compression = "lzw", res=300, width = 900, height = 900)
tvol.range <- seq(0,12500,12500/(nrow(model.data)+1))[-c(1,length(seq(0,12500,12500/(nrow(model.data)+1))))]
ggplot(data = model.data) +
  geom_line(aes(x = tvol.range, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*mean(log(egk)) + coef(coll.glm)[3]*log(tvol.range) + coef(coll.glm)[4]*mean(log(tspd)) ) ) ) +
  geom_ribbon(aes(x = tvol.range, ymin=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      (coef(coll.glm)[3] - coef(summary(coll.glm))[3,2]*1.96)*log(tvol.range) +
      coef(coll.glm)[4]*mean(log(tspd))
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      (coef(coll.glm)[3] + coef(summary(coll.glm))[3,2]*1.96)*log(tvol.range) +
      coef(coll.glm)[4]*mean(log(tspd))
  )
  ), alpha=0.3
  ) +
  xlab("TVOL") +
  scale_x_continuous(limits = c(0,12500), expand = c(0, 0), breaks = seq(0,12500,2500)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0,1,.1))
dev.off()

tiff('figs/tspd.tif', pointsize = 20, compression = "lzw", res=300, width = 900, height = 900)
tspd.range <- tspd.range <- seq(0,100,100/(nrow(model.data)+1))[-c(1,length(seq(0,100,100/(nrow(model.data)+1))))]
ggplot(data = model.data) +
  geom_line(aes(x = tspd.range, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*mean(log(egk)) + coef(coll.glm)[3]*mean(log(tvol)) + coef(coll.glm)[4]*log(tspd.range) ) ) ) +
  geom_ribbon(aes(x = tspd.range, ymin=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      (coef(coll.glm)[4] - coef(summary(coll.glm))[4,2]*1.96)*log(tspd.range)
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      (coef(coll.glm)[4] + coef(summary(coll.glm))[4,2]*1.96)*log(tspd.range)
  )
  ), alpha=0.3
  ) +
  xlab("TSPD") +
  scale_x_continuous(limits = c(0,100), expand = c(0, 0), breaks = seq(0,100,20)) +
  scale_y_continuous(limits = c(0,1), expand = c(0, 0), breaks = seq(0,1,.1))
dev.off()