library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")

invlogit <- function (x) {1/(1+exp(-x))}

ggplot <- function(...) { ggplot2::ggplot(...) + theme_bw() }


####VOLUME####


pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/INCOMEPP.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = INCOMEPP, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*INCOMEPP))) + geom_line() + ylab("AADT\n") + xlab("\nINCOMEPP") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*INCOMEPP - coef(summary(trans.lglm))[2,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[2]*INCOMEPP + coef(summary(trans.lglm))[2,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/KMTOCZ.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = KMTOCZ, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*KMTOCZ))) + geom_line() + ylab("AADT\n") + xlab("\nKMTOCZ") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*KMTOCZ - coef(summary(trans.lglm))[3,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[3]*KMTOCZ + coef(summary(trans.lglm))[3,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/KMTOHWY.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = KMTOHWY, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*KMTOHWY))) + geom_line() + ylab("AADT\n") + xlab("\nKMTOHWY") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*KMTOHWY - coef(summary(trans.lglm))[4,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[4]*KMTOHWY + coef(summary(trans.lglm))[4,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/KMTORZ.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = KMTORZ, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*KMTORZ))) + geom_line() + ylab("AADT\n") + xlab("\nKMTORZ") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*KMTORZ - coef(summary(trans.lglm))[5,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[5]*KMTORZ + coef(summary(trans.lglm))[5,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/POPDENS.pdf', bg = "white",pointsize = 20)
ggplot(data = model.data, aes(x = POPDENS, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*POPDENS))) + geom_line() + ylab("AADT\n") + xlab("\nPOPDENS") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*POPDENS - coef(summary(trans.lglm))[6,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[6]*POPDENS + coef(summary(trans.lglm))[6,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/RDDENS.pdf', bg = "white", pointsize = 20)
ggplot(data = model.data, aes(x = RDDENS, y = exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*RDDENS))) + geom_line() + ylab("AADT\n") + xlab("\nRDDENS") + geom_ribbon(aes(ymin=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*RDDENS - coef(summary(trans.lglm))[7,2]*1.96),ymax=exp(coef(summary(trans.lglm))[1,1] + coef(trans.lglm)[7]*RDDENS + coef(summary(trans.lglm))[7,2]*1.96)),alpha=0.3) + theme(text = element_text(size=24))
dev.off()


####SPEED####


predframe <- with(model.data, expand.grid(RDCLASS=levels(RDCLASS), RDDENS=seq(min(RDDENS), max(RDDENS),length=51)))

minmaxvals <- range(model.data$RDDENS)

predframe$pred <- predict(speed.lm,newdata=predframe,REform=NA,type="response")

pdf('/home/casey/Research/Projects/EGK_Collisions/Publish/LaTex/Manuscripts/RDCLASS.pdf', bg = "white", pointsize = 20, width=9.5)
(ggplot(predframe,aes(RDDENS,pred,linetype=RDCLASS))
 + theme(text = element_text(size=20))
 + geom_line()
 + ylab("SPEEDLMT\n")
 + xlab("\nRDDENS")
 + labs(linetype='RDCLASS')
 + scale_linetype_discrete(labels=c('FREEWAY','HIGHWAY','ARTERIAL','SUBARTERIAL','COLLECTOR','LOCAL')))
dev.off()