require("ggplot2")

invlogit <- function (x) {1/(1+exp(-x))}

ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() }

pdf('../figs/egk.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = egk, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(egk)-ml.egk))))
 + geom_line()
 + scale_x_continuous(limits = c(.1,.8), breaks = seq(.1,.8,.1))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("coll")
 + xlab("egk")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(egk)-ml.egk) - coef(summary(coll.glm))[2,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(egk)-ml.egk) + coef(summary(coll.glm))[2,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()

pdf('../figs/tvol.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = tvol, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(tvol)-ml.tvol))))
 + geom_line()
 + scale_x_continuous(limits = c(0,12500), breaks = seq(0,12500,2500))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("coll")
 + xlab("tvol")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(tvol)-ml.tvol) - coef(summary(coll.glm))[3,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(tvol)-ml.tvol) + coef(summary(coll.glm))[3,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()

pdf('../figs/tspd.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = tspd, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(tspd)-ml.tspd))))
 + geom_line()
 + scale_x_continuous(limits = c(40, 100), breaks = seq(40,100,10))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("coll")
 + xlab("tspd")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(tspd)-ml.tspd) - coef(summary(coll.glm))[4,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(tspd)-ml.tspd) + coef(summary(coll.glm))[4,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()