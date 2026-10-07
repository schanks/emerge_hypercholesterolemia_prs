library(data.table)
library(ggplot2)
library(gridExtra)
library(ggdist)
library(patchwork)
primarycols=c("#66CCEE","#4477AA","#BBBBBB")




dat=fread("../Management/Aggregates/treated_targets.tab")
primarycols=c("#66CCEE","#4477AA","#BBBBBB")

dat$variant=factor(dat$variant, levels=c("FH+","PRS+","Not high risk"))
dat$lipoprotein=factor(dat$lipoprotein, levels=unique(dat$lipoprotein))


my_labeller <- function(variable, value) {
  if (variable == "lipoprotein") {
    return(c(ldl100="LDL < 100 mg/dL",
             ldl70="LDL < 70 mg/dL",
             ldl55="LDL < 55 mg/dL",
             nonhdl130="non-HDL < 130 mg/dL",
	     nonhdl100="non-HDL < 100 mg/dL",
             nonhdl85="non-HDL < 85 mg/dL")[value])
  }

  if (variable == "target") {
    return(paste0("Target = ", value))
  }

  value
}

#LDL only
dat=dat[which(dat$lipid=="LDL-C"),]


tiff("Figure4.tiff", height=5, width=5, units="in", res=300)
ggplot(dat, aes(x=lipoprotein, fill=variant))+geom_bar(aes(y=prop), stat="identity", position=position_dodge())+geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=position_dodge(0.9))+scale_fill_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"))+xlab("LDL-C target value")+ylab("Proportion of treated participants")+theme_classic()+ylim(c(0,1))+scale_x_discrete(labels=c("≤100 mg/dL","≤70 mg/dL", "≤55 mg/dL"))+theme(legend.title=element_blank(), axis.text=element_text(size=8), axis.title=element_text(size=10), legend.text=element_text(size=8), legend.position=c(0.8,0.8))
dev.off()





