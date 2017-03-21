require(data.table)
require(xtable)
require(ggplot2)

vars <- as.data.table(read.csv("/home/casey/Research/Know_Repo/Literature/Animal-Collisions-Factors.csv"))

count <- vars[, lapply(.SD, sum, na.rm=TRUE), .SDcols=c(6:58), with=T]

percent <- vars[, lapply(.SD, function(x) round((sum(x, na.rm=TRUE)/71)*100,2)), .SDcols=c(6:58), with=T]

names <- vars[, lapply(.SD, function(x) paste(which(x==1),collapse = ", ")), .SDcols=c(6:58), with=T]

vars.final <- data.frame(gsub("_", " ", colnames(count)),as.integer(unlist(count[1])),as.numeric(unlist(percent[1])),as.character(unlist(names[1])))

colnames(vars.final) <- c("Variable Name","Total Studies Using Variable","Percent Representation","Study Reference ID in (\\Cref{studies})")


print(xtable(data.frame("ID"=as.integer(seq(1,nrow(vars),1)), "Author"=gsub("&", "and", vars[,Author]), "Year"=vars[,Publication_Year], "Publication"=gsub("&", "and", vars[,Journal_Proceedings]))), include.rownames=FALSE, sanitize.text.function=function(x){x}, floating=FALSE)

print(xtable(vars.final[ order(-vars.final[,2]), ]), include.rownames=FALSE, sanitize.text.function=function(x){x}, floating=FALSE)

ggplot(vars.final, aes(x = vars.final[order(-vars.final[,2]),2])) + geom_dotplot(method="histodot", binwidth = 1/53)

plot.data <- data.frame("x"=vars.final[order(vars.final[,2]),2],"y"=factor(vars.final[order(vars.final[,2]),1],levels=vars.final[order(vars.final[,2]),1]))

png('/home/casey/Research/Projects/PhD_Thesis/graphics/wvc_studies.png', pointsize = 10, res=300, width = 1100, height = 900)
ggplot(plot.data[34:53,]) +
  geom_point(aes(x=x,y=y)) +
  #coord_flip() +
  ylab("Modelling Variable") + 
  xlab("Number of Studies Using Variable") +
  theme_bw() +
  theme(legend.key = element_blank(),legend.position = 'none') +
  theme(plot.margin=unit(c(.5,.3,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40))
dev.off()
#  theme(panel.grid.major.x = element_blank(),
#    panel.grid.major.y = element_line(linetype=3, color="darkgray"),
#    axis.text.y=element_text(size=rel(0.8))
#    ) 
