require("ggplot2")

invlogit <- function (x) {1/(1+exp(-x))}

ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() }

pdf('../figs/EGK.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = EGK, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(EGK)-ml.EGK))))
 + geom_line()
 + scale_x_continuous(limits = c(.1,.8), breaks = seq(.1,.8,.1))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("COLL")
 + xlab("EGK")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(EGK)-ml.EGK) - coef(summary(coll.glm))[2,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[2]*(log(EGK)-ml.EGK) + coef(summary(coll.glm))[2,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()

pdf('../figs/TVOL.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = TVOL, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(TVOL)-ml.TVOL))))
 + geom_line()
 + scale_x_continuous(limits = c(0,12500), breaks = seq(0,12500,2500))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("COLL")
 + xlab("TVOL")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(TVOL)-ml.TVOL) - coef(summary(coll.glm))[3,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[3]*(log(TVOL)-ml.TVOL) + coef(summary(coll.glm))[3,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()

pdf('../figs/TSPD.pdf', pointsize = 24)
(ggplot(data = model.data, aes(x = TSPD, y = invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(TSPD)-ml.TSPD))))
 + geom_line()
 + scale_x_continuous(limits = c(40, 100), breaks = seq(40,100,10))
 + scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1))
 + ylab("COLL")
 + xlab("TSPD")
 + geom_ribbon(aes(ymin=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(TSPD)-ml.TSPD) - coef(summary(coll.glm))[4,2]*1.96),ymax=invlogit(coef(summary(coll.glm))[1,1] + coef(coll.glm)[4]*(log(TSPD)-ml.TSPD) + coef(summary(coll.glm))[4,2]*1.96)),alpha=0.3) + theme(text = element_text(size=18)))
dev.off()