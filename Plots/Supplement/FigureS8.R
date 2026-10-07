library(data.table)
library(ggplot2)

dat=fread("aggregate_management.csv")
primarycols=c("#66CCEE","#4477AA","#BBBBBB")

dat$variant=factor(dat$variant, levels=rev(c("Not high risk","PRS+","FH+")))
dat$prop=dat$pct/100
dat$LB=dat$ci_lower/100
dat$UB=dat$ci_upper/100

dat$outcome=factor(dat$outcome, levels=c("ldl_under_100","ldl_under_70","ldl_under_55"))

tiff("Figure2.tiff", width=3.3, height=3.3, units="in",res=300)
ggplot(dat, aes(x=outcome, fill=variant))+geom_bar(aes(y=prop), stat="identity",position=position_dodge())+geom_errorbar(aes(ymin=LB, ymax=UB), width=0.3, position=position_dodge(0.9))+scale_fill_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"))+xlab("LDL-C target value")+ylab("Proportion of treated participants")+theme_classic()+ylim(c(0,1))+scale_x_discrete(labels=c("≤100 mg/dL","≤70 mg/dL", "≤55 mg/dL"))+theme(legend.title=element_blank(), axis.text=element_text(size=8), axis.title=element_text(size=10), legend.text=element_text(size=8), legend.position=c(0.8,0.8))
dev.off()







