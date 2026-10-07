library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(survival)
library(survminer)
library(patchwork)
library(adjustedCurves)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")

aoufit=fread("../../AoU/adjusted_curve_aou.tab")
aoufit$group=factor(aoufit$group, levels=c("FH+","PRS+","Not high risk"))

c=ggplot(aoufit, aes(x=time, y=1-surv, color=group))+
        geom_step()+theme_minimal()+
        geom_ribbon(aes(ymin=1-ci_upper, ymax=1-ci_lower, fill=group), alpha=0.2, color=NA)+
        scale_color_manual(values=primarycols)+
        scale_fill_manual(values=primarycols)+
        xlab("Age (years)")+ylab("Adjusted cumulative ASCVD incidence")+
        annotate("text",x=40, y=0.48, label="All of Us v8")+
	theme(legend.title=element_blank(), legend.position="bottom",
		axis.title.y=element_text(size=11),
		plot.title=element_text(size=11),
		axis.text=element_text(size=11),
		legend.text=element_text(size=9))+
        scale_x_continuous(limits=c(30,70))+
        scale_y_continuous(limits=c(0,0.5))


coxph=fread("../../Results/ascvd_coxph_meta.tab")

coxph$outcome <- ifelse(coxph$outcome == "stroke",
                          "Stroke/TIA",
                          toupper(coxph$outcome))
coxph$outcome=factor(coxph$outcome, levels=c("ASCVD","Stroke/TIA","PAD","CAD"))

coxph$Comparison[coxph$Comparison=="variantFH+"]="FH+ vs. NHR"
coxph$Comparison[coxph$Comparison=="variantPRS+"]="PRS+ vs. NHR"
coxph$Comparison=factor(coxph$Comparison, levels=c("FH+ vs. NHR","PRS+ vs. NHR"))

hrbreaks=c(0.9,1,1.2,1.4,1.6,1.8,2,2.2)

d=ggplot(coxph, aes(y=outcome, color=Comparison))+
        geom_point(aes(x=HR_meta), position=position_dodge(-0.5))+
        geom_errorbar(aes(xmin=LB_meta, xmax=UB_meta), position=position_dodge(-0.5), width=0.3)+
        scale_color_manual(values=primarycols)+
        scale_x_continuous(trans=log_trans(), breaks=hrbreaks)+
        theme_classic()+geom_vline(xintercept=1, linetype="dotted")+
        xlab("Meta-analyzed hazard ratio")+
	geom_hline(yintercept=1.5)+
        theme(legend.position="bottom", legend.title=element_blank(), axis.title.y=element_blank(),
        axis.text.y=element_text(size=10), 
	axis.text.x=element_text(size=10),
	axis.title.x=element_text(size=11),
	legend.text=element_text(size=9),
	plot.margin=margin(5.5,20,5.5,5.5, "pt"))

tiff("Figure2.tiff", width=7, height=4, units="in",res=300)
grid.arrange(d, c, widths=c(1,1))
dev.off()















