library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")
orbreaks=c(2,2.5,3,3.5,4,5,7,10,15,20)


fh=fread("../../Analysis/Threshold/threshold_fh.tab")
ldlc=fread("../../Analysis/Threshold/threshold_mean_ldl.tab")
hc=fread("../../Analysis/Threshold/threshold_firth_hc.tab")
sh=fread("../../Analysis/Threshold/threshold_firth_sh.tab")



a=ggplot(ldlc, aes(x=Percentile))+
	geom_rect(aes(xmin=Percentile-0.5, xmax=Percentile+0.5, ymin=LB, ymax=UB), data=fh[which(fh$Outcome=="LDL-C")], fill=primarycols[1], alpha=0.7)+
	geom_line(aes(y=Beta), color=primarycols[2],data=fh[which(fh$Outcome=="LDL-C")])+
	geom_point(aes(y=Beta), color=primarycols[2],data=fh[which(fh$Outcome=="LDL-C")])+
	geom_errorbar(aes(ymin=LB, ymax=UB))+geom_point(aes(y=Beta))+theme_bw()+
	scale_x_continuous(breaks=c(seq(90,99,1)))+
	ylab("Mean difference\nin LDL-C (mg/dL)")+
	theme(axis.title.x=element_blank(), axis.text.x=element_blank(),
	axis.ticks.x=element_blank())

b=ggplot(hc, aes(x=Percentile))+
	geom_rect(aes(xmin=Percentile-0.5, xmax=Percentile+0.5, ymin=LB, ymax=UB), data=fh[which(fh$Outcome=="HC")], fill=primarycols[1], alpha=0.7)+
	geom_line(aes(y=OR), color=primarycols[2],data=fh[which(fh$Outcome=="HC")])+
	geom_point(aes(y=OR), color=primarycols[2],data=fh[which(fh$Outcome=="HC")])+
	geom_errorbar(aes(ymin=LB, ymax=UB))+geom_point(aes(y=OR))+theme_bw()+
	scale_x_continuous(breaks=c(seq(90,99,1)))+
	scale_y_continuous(trans=log_trans(), breaks=orbreaks)+
	ylab("Odds ratio:\nLDL-C ≥ 160 mg/dL")+
        theme(axis.title.x=element_blank(), axis.text.x=element_blank(), 
	axis.ticks.x=element_blank())


c=ggplot(sh, aes(x=Percentile))+
	geom_rect(aes(xmin=Percentile-0.5, xmax=Percentile+0.5, ymin=LB, ymax=UB), data=fh[which(fh$Outcome=="SH")], fill=primarycols[1], alpha=0.7)+
	geom_line(aes(y=OR), color=primarycols[2],data=fh[which(fh$Outcome=="SH")])+
	geom_point(aes(y=OR), color=primarycols[2],data=fh[which(fh$Outcome=="SH")])+
	geom_errorbar(aes(ymin=LB, ymax=UB))+geom_point(aes(y=OR))+theme_bw()+
	scale_x_continuous(breaks=c(seq(90,99,1)))+
	xlab("PRS high risk threshold percentile")+
	scale_y_continuous(trans=log_trans(), breaks=orbreaks)+
	ylab("Odds ratio:\nLDL-C ≥ 190 mg/dL")

tiff("threshold_plot.tiff", height=7, width=7, units="in", res=300)
grid.arrange(a,b,c, heights=c(1,1,1.2))
dev.off()




