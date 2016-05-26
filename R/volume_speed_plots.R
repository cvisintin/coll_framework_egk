require("ggplot2")

invlogit <- function (x) {1/(1+exp(-x))}

ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() }


####VOLUME####


pdf('../figs/incomepp.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = incomepp, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*incomepp))) + geom_line() + ylab("aadt\n") + xlab("\nincomepp") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*incomepp - coef(summary(trans.lglm))[2,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*incomepp + coef(summary(trans.lglm))[2,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('../figs/kmtocz.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = kmtocz, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*kmtocz))) + geom_line() + ylab("aadt\n") + xlab("\nkmtocz") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*kmtocz - coef(summary(trans.lglm))[3,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*kmtocz + coef(summary(trans.lglm))[3,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('../figs/kmtohwy.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = kmtohwy, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*kmtohwy))) + geom_line() + ylab("aadt\n") + xlab("\nkmtohwy") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*kmtohwy - coef(summary(trans.lglm))[4,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*kmtohwy + coef(summary(trans.lglm))[4,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('../figs/kmtorz.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = kmtorz, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*kmtorz))) + geom_line() + ylab("aadt\n") + xlab("\nkmtorz") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*kmtorz - coef(summary(trans.lglm))[5,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*kmtorz + coef(summary(trans.lglm))[5,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('../figs/popdens.pdf', bg = "white",pointsize = 20)
ggplot(data = model.data, aes(x = popdens, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*popdens))) + geom_line() + ylab("aadt\n") + xlab("\npopdens") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*popdens - coef(summary(trans.lglm))[6,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*popdens + coef(summary(trans.lglm))[6,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('../figs/rddens.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = rddens, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*rddens))) + geom_line() + ylab("aadt\n") + xlab("\nrddens") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*rddens - coef(summary(trans.lglm))[7,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*rddens + coef(summary(trans.lglm))[7,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()


####SPEED####


predframe <- with(model.data, expand.grid(rdclass=levels(rdclass), rddens=seq(min(rddens), max(rddens),length=51)))

minmaxvals <- range(model.data$rddens)

predframe$pred <- predict(speed.lm,newdata=predframe,REform=NA,type="response")

pdf('../figs/rdclass.pdf', bg = "white", pointsize = 20, width=9.5)
(ggplot(predframe,aes(rddens,pred,linetype=rdclass))
 + theme(text = element_text(size=20))
 + geom_line()
 + ylab("speedlmt\n")
 + xlab("\nrddens")
 + labs(linetype='rdclass')
 + scale_linetype_discrete(labels=c('freeway','highway','arterial','subarterial','collector','local')))
dev.off()