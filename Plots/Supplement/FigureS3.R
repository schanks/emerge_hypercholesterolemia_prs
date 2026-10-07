library(data.table)
library(ggplot2)
library(scales)
library(gridExtra)
library(ggpubr)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")
aou_colors=c("#FF7F50","#D92014","#494949")

age_levels=c("All adults","70-75","60-69","50-59","40-49","30-39","18-29")

ldl=fread("../Plots/Dyslipidemia/Aggregates/age_95ci_ldl.tab")
ldl=ldl[is.element(ldl$age_cat, age_levels),]
ldl$variant=gsub("^","eMERGE: ",ldl$variant)
colnames(ldl)=c("variant","age_cat","n","mean_ldl","ci_lower","ci_upper")
hcdx=fread("../Plots/Dyslipidemia/Aggregates/age_95ci_hcdx.tab")
hcdx=hcdx[is.element(hcdx$age_cat, age_levels),]
hcdx$variant=gsub("^","eMERGE: ",hcdx$variant)
colnames(hcdx)=c("variant","age_cat","n","pct","ci_lower","ci_upper")
llt=fread("../Plots/Dyslipidemia/Aggregates/age_95ci_rx.tab")
llt=llt[is.element(llt$age_cat, age_levels),]
llt$variant=gsub("^","eMERGE: ",llt$variant)
colnames(llt)=c("variant","age_cat","n","pct","ci_lower","ci_upper")

ldl_aou=fread("aggregate_ldl_most_recent.csv")
ldl_aou$age_cat[ldl_aou$age_cat=="18-75"]="All adults"
ldl_aou$variant=gsub("^","AoU: ",ldl_aou$variant)
hcdx_aou=fread("aggregate_hcdx.csv")
hcdx_aou$age_cat[hcdx_aou$age_cat=="18-75"]="All adults"
hcdx_aou$variant=gsub("^","AoU: ",hcdx_aou$variant)
hcdx_aou$pct=hcdx_aou$pct/100
hcdx_aou$ci_lower=hcdx_aou$ci_lower/100
hcdx_aou$ci_upper=hcdx_aou$ci_upper/100
llt_aou=fread("aggregate_llt.csv")
llt_aou$age_cat[llt_aou$age_cat=="18-75"]="All adults"
llt_aou$variant=gsub("^","AoU: ",llt_aou$variant)
llt_aou$pct=llt_aou$pct/100
llt_aou$ci_lower=llt_aou$ci_lower/100
llt_aou$ci_upper=llt_aou$ci_upper/100

ldl=rbind(ldl, ldl_aou)
hcdx=rbind(hcdx, hcdx_aou)
llt=rbind(llt, llt_aou)

var_levels=c("eMERGE: FH+","eMERGE: PRS+","eMERGE: Not high risk","AoU: FH+","AoU: PRS+","AoU: Not high risk")

ldl$variant=factor(ldl$variant, levels=rev(var_levels))
hcdx$variant=factor(hcdx$variant, levels=rev(var_levels))
llt$variant=factor(llt$variant, levels=rev(var_levels))

ldl$age_cat=factor(ldl$age_cat, levels=rev(age_levels))
hcdx$age_cat=factor(hcdx$age_cat, levels=rev(age_levels))
llt$age_cat=factor(llt$age_cat, levels=rev(age_levels))

a=ggplot(ldl, aes(color=variant))+facet_grid(age_cat~., switch="y")+geom_point(aes(x=mean_ldl, y=variant))+geom_errorbar(aes(y=variant, xmin=ci_lower, xmax=ci_upper), width=0.3, position=position_dodge(0.4))+theme_classic()+scale_y_discrete(expand=expansion(c(0.5,0.5)))+scale_color_manual(values=c(primarycols, aou_colors), breaks=var_levels, labels=var_levels)+theme(legend.position="none",axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.text.x=element_text(size=10),axis.title.y=element_blank(), strip.placement="outside", strip.background=element_rect(color=NA),axis.title.x=element_text(size=10))+xlab("Mean LDL-C (mg/dL)\n")

b=ggplot(hcdx, aes(color=variant))+facet_grid(age_cat~., switch="y")+geom_point(aes(x=pct, y=variant))+geom_errorbar(aes(y=variant,xmin=ci_lower, xmax=ci_upper), width=0.3, position=position_dodge(0.4))+theme_classic()+scale_y_discrete(expand=expansion(c(0.5,0.5)))+scale_color_manual(values=c(primarycols, aou_colors), breaks=var_levels, labels=var_levels)+theme(legend.position="none",axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.text.x=element_text(size=10),axis.title.y=element_blank(), strip.placement="outside", strip.background=element_rect(color=NA), strip.text.y.left=element_blank(), axis.title.x=element_text(size=10))+xlab("Proportion ever diagnosed\nwith a lipid disorder")+ylab("Age (yrs)")+xlim(c(0,1))

c=ggplot(llt, aes(color=variant))+facet_grid(age_cat~., switch="y")+geom_point(aes(x=pct, y=variant))+geom_errorbar(aes(y=variant,xmin=ci_lower, xmax=ci_upper), width=0.3, position=position_dodge(0.4))+theme_classic()+scale_y_discrete(expand=expansion(c(0.5,0.5)))+scale_color_manual(values=c(primarycols, aou_colors), breaks=var_levels, labels=var_levels)+theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),axis.text.x=element_text(size=10), strip.background=element_blank(), axis.title.x=element_text(size=10),strip.text=element_blank(), legend.position="bottom")+xlab("Proportion ever prescribed\nlipid-lowering therapy ")+xlim(c(0,1))+guides(shape="none",color=guide_legend(override.aes=list(size=2, linewidth=1), nrow=2, byrow=TRUE))

c4_legend=get_legend(c)

c=c+theme(legend.position="none")

row1=grid.arrange(a, b, c, widths=c(1.2,1,1))

tiff("Figure1.tiff", width=7, height=7, unit="in", res=300)
grid.arrange(row1, c4_legend,heights=c(7,1))
dev.off()











