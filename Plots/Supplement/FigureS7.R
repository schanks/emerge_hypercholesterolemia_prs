library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(survival)
library(survminer)
library(patchwork)
library(adjustedCurves)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")

aoufit=readRDS("adjusted_curve_eiv.rds")
aoufit$group=factor(aoufit$group, levels=c("FH+","PRS+","Not high risk"))

ggplot(aoufit, aes(x=time, y=1-surv, color=group))+
        geom_step()+theme_minimal()+
        geom_ribbon(aes(ymin=1-ci_upper, ymax=1-ci_lower, fill=group), alpha=0.2, color=NA)+
        scale_color_manual(values=primarycols)+
        scale_fill_manual(values=primarycols)+
        xlab("Age (years)")+ylab("Adjusted cumulative ASCVD incidence")+
        annotate("text",x=40, y=0.48, label="eMERGE IV")+
	theme(legend.title=element_blank(), legend.position="bottom",
		axis.title.y=element_text(size=11),
		plot.title=element_text(size=11),
		axis.text=element_text(size=11),
		legend.text=element_text(size=9))+
        scale_x_continuous(limits=c(30,70))+
        scale_y_continuous(limits=c(0,0.6))
