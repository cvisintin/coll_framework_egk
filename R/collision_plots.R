require(ggplot2)

invlogit <- function (x) {1/(1+exp(-x))}

invcloglog <- function (x) {1-exp(-exp(x))}

load("output/coll_glm")
load("output/coll_model_data")

ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() + ylab("LIKELIHOOD OF COLLISION") + theme(text = element_text(size=6)) + theme(legend.position="none") }

tiff('figs/egk.tif', pointsize = 6, compression = "lzw", res=300, width = 900, height = 900)
ggplot(data = model.data) +
  geom_line(aes(x = egk, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*log(egk) + coef(coll.glm)[3]*mean(log(tvol)) + coef(coll.glm)[4]*mean(log(tspd)) ) ) ) +
  scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  xlab("EGK") +
  geom_ribbon(aes(x = egk, ymin=invcloglog(
    coef(coll.glm)[1] +
      (coef(coll.glm)[2] - coef(summary(coll.glm))[2,2]*1.96)*log(egk) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      coef(coll.glm)[4]*mean(log(tspd))
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      (coef(coll.glm)[2] + coef(summary(coll.glm))[2,2]*1.96)*log(egk) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      coef(coll.glm)[4]*mean(log(tspd))
  )
  ), alpha=0.3
  )
dev.off()

tiff('figs/tvol.tif', pointsize = 6, compression = "lzw", res=300, width = 900, height = 900)
ggplot(data = model.data) +
  geom_line(aes(x = tvol, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*mean(log(egk)) + coef(coll.glm)[3]*log(tvol) + coef(coll.glm)[4]*mean(log(tspd)) ) ) ) +
  scale_x_continuous(limits = c(0,12500), breaks = seq(0,12500,2500)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  xlab("TVOL") +
  geom_ribbon(aes(x = tvol, ymin=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      (coef(coll.glm)[3] - coef(summary(coll.glm))[3,2]*1.96)*log(tvol) +
      coef(coll.glm)[4]*mean(log(tspd))
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      (coef(coll.glm)[3] + coef(summary(coll.glm))[3,2]*1.96)*log(tvol) +
      coef(coll.glm)[4]*mean(log(tspd))
  )
  ), alpha=0.3
  )
dev.off()

tiff('figs/tspd.tif', pointsize = 6, compression = "lzw", res=300, width = 900, height = 900)
ggplot(data = model.data) +
  geom_line(aes(x = tspd, invcloglog(coef(coll.glm)[1] + coef(coll.glm)[2]*mean(log(egk)) + coef(coll.glm)[3]*mean(log(tvol)) + coef(coll.glm)[4]*log(tspd) ) ) ) +
  scale_x_continuous(limits = c(40,100), breaks = seq(40,100,10)) +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
  xlab("TSPD") +
  geom_ribbon(aes(x = tspd, ymin=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      (coef(coll.glm)[4] - coef(summary(coll.glm))[4,2]*1.96)*log(tspd)
  ),
  ymax=invcloglog(
    coef(coll.glm)[1] +
      coef(coll.glm)[2]*mean(log(egk)) +
      coef(coll.glm)[3]*mean(log(tvol)) +
      (coef(coll.glm)[4] + coef(summary(coll.glm))[4,2]*1.96)*log(tspd)
  )
  ), alpha=0.3
  )
dev.off()