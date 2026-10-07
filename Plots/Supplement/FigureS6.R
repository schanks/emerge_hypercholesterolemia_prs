library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(survival)
library(survminer)
library(patchwork)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")

hrbreaks=c(0.4,0.6,0.8,1,1.5,2,3,4.5)

dat=fread("../../Results/eiv_coxph.tab")

dat$Comparison[dat$Comparison=="variantPRS+"]="PRS+ vs. NHR"
dat$Comparison[dat$Comparison=="variantFH+"]="FH+ vs. NHR"
dat$Comparison=factor(dat$Comparison, levels=c("FH+ vs. NHR","PRS+ vs. NHR"))

dat$outcome <- ifelse(dat$outcome == "stroke",
                          "Stroke/TIA",
                          toupper(dat$outcome))
dat$outcome=factor(dat$outcome, levels=c("ASCVD","Stroke/TIA","PAD","CAD"))


a=ggplot(dat, aes(y=outcome, color=Comparison))+
	geom_point(aes(x=HR), position=position_dodge(-0.5))+
	geom_errorbar(aes(xmin=LB, xmax=UB), position=position_dodge(-0.5), width=0.3)+
	scale_color_manual(values=primarycols)+
	scale_x_continuous(trans=log_trans(), breaks=hrbreaks, limits=c(0.35,4.5))+
	theme_classic()+geom_vline(xintercept=1, linetype="dotted")+
	xlab("Hazard ratio: eMERGE IV")+
	geom_hline(yintercept=1.5)+
	theme(legend.position="bottom", legend.title=element_blank(), axis.title.y=element_blank(),
	axis.text=element_text(size=10), axis.title.x=element_text(size=12),
	plot.margin=margin(5.5,15,5.5,5.5, "pt"))

a_legend=get_legend(a)
a=a+theme(legend.position="none")


aou=fread("../../AoU/aou_coxph.tab")
aou$Comparison[aou$Comparison=="variantPRS+"]="PRS+ vs. NHR"
aou$Comparison[aou$Comparison=="variantFH+"]="FH+ vs. NHR"
aou$Comparison=factor(aou$Comparison, levels=c("FH+ vs. NHR","PRS+ vs. NHR"))

aou$outcome <- ifelse(aou$outcome == "stroke",
                          "Stroke/TIA",
                          toupper(aou$outcome))
aou$outcome=factor(aou$outcome, levels=c("ASCVD","Stroke/TIA","PAD","CAD"))

b=ggplot(aou, aes(y=outcome, color=Comparison))+
        geom_point(aes(x=HR), position=position_dodge(-0.5))+
        geom_errorbar(aes(xmin=LB, xmax=UB), position=position_dodge(-0.5), width=0.3)+
        scale_color_manual(values=primarycols)+
        scale_x_continuous(trans=log_trans(), breaks=hrbreaks, limits=c(0.35,4.5))+
        theme_classic()+geom_vline(xintercept=1, linetype="dotted")+
	geom_hline(yintercept=1.5)+
        xlab("Hazard ratio: All of Us v8")+
        theme(legend.position="none", axis.title.y=element_blank(),
        axis.text=element_text(size=10), axis.title.x=element_text(size=12))


row1=grid.arrange(a,b, widths=c(1,1))

tiff("Supp_HR.tiff", width=7, height=3.5, units="in", res=300)
grid.arrange(row1, a_legend, heights=c(7,1))
dev.off()


