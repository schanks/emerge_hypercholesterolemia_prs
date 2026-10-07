library(data.table)
library(ggplot2)
library(gridExtra)
library(ggdist)
library(ggpubr)
library(ggh4x)
library(dplyr)
library(tidyr)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")
lipidcolor="#9ACA6D"

age_levels=c("All adults\n(18-75)","70-75","60-69","50-59","40-49","30-39","18-29","Pediatric\n(3-17)")
var_levels=c("Not high risk","PRS+","FH+")

#Panel C
hcdx=fread("../Dyslipidemia/Aggregates/age_95ci_hcdx.tab")
hcdx_peds=fread("../Dyslipidemia/Aggregates/peds_95ci_hc.tab")
hcdx=rbind(hcdx, hcdx_peds)
hcdx$age_cat[hcdx$age_cat=="All adults"]="All adults\n(18-75)"
hcdx$age_cat[hcdx$age_cat=="3-17"]="Pediatric\n(3-17)"
hcdx$age_cat=factor(hcdx$age_cat, levels=rev(age_levels))
hcdx$variant=factor(hcdx$variant, levels=var_levels)

c=ggplot(hcdx, aes(color=variant))+facet_grid(age_cat~., switch="y")+
	geom_point(aes(x=mean_hc, y=variant))+
	geom_errorbar(aes(y=variant,xmin=lb_hc, xmax=ub_hc), width=0.3, position=position_dodge(0.4))+
	theme_minimal()+scale_y_discrete(expand=expansion(c(0.5,0.5)))+
	scale_color_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"))+
	theme(legend.position="bottom",axis.text.y=element_blank(), 
	axis.ticks.y=element_blank(),axis.text.x=element_text(size=10),
	axis.title.y=element_blank(), strip.placement="outside", 
	strip.background=element_rect(color=NA), strip.text.y.left=element_text(size=11, angle=0), 
	axis.title.x=element_text(size=10),
	plot.margin=margin(15,5.5,5.5,5.5),
	panel.grid.major.y=element_blank(), panel.grid.minor.x=element_blank(),
	legend.text=element_text(size=10))+
	xlab("Proportion ever diagnosed\nwith a lipid disorder")+
	ylab("Age (yrs)")+xlim(c(0,1))+
	geom_vline(xintercept=-Inf, linewidth=1)+
	geom_hline(yintercept=-Inf, linewidth=1)+
	guides(color=guide_legend(title=NULL))

c_legend=get_legend(c)

c=c+theme(legend.position="none")

#Panel D
llt=fread("../Dyslipidemia/Aggregates/age_95ci_rx.tab")
llt$age_cat[llt$age_cat=="All adults"]="All adults\n(18-75)"
llt$age_cat=factor(llt$age_cat, levels=rev(age_levels))
llt$variant=factor(llt$variant, levels=var_levels)

d=ggplot(llt, aes(color=variant, y=variant))+facet_grid(age_cat~., switch="y")+
	geom_point(aes(x=mean_rx, y=variant))+
	geom_errorbar(aes(y=variant,xmin=lb_rx, xmax=ub_rx), width=0.3, position=position_dodge(0.4))+
	theme_minimal()+scale_y_discrete(expand=expansion(c(0.5,0.5)))+
	scale_color_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"), labels=c("FH+","PRS+","Not high risk"))+
	theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
	axis.title.y=element_blank(),legend.title=element_blank(),
	axis.text.x=element_text(size=10), strip.background=element_blank(), 
	panel.grid.major.y=element_blank(), panel.grid.minor.x=element_blank(),
	strip.text.y.left=element_blank(), strip.placement="outside",
	axis.title.x=element_text(size=10),strip.text=element_text(size=10), 
	legend.position="none", plot.margin=margin(60,5.5,5.5,5.5))+
	xlab("Proportion ever prescribed\nlipid-lowering therapy ")+
	xlim(c(0,1))+
	geom_vline(xintercept=-Inf, linewidth=1)+
	geom_hline(yintercept=-Inf, linewidth=1)

row1=grid.arrange(c,d, widths=c(1.2,1))
tiff("Figure3.tiff", width=5, height=6, unit="in", res=300)
grid.arrange(row1, c_legend, heights=c(7,0.5))
dev.off()






