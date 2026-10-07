library(ggplot2)
library(data.table)


dat=fread("../../../Results/meta_em_llt_stratified.tab")

dat$Race[dat$Race=="Black, African American, or African"]="Black"
dat$Race[dat$Race=="Hispanic, Latino, or Spanish"]="Hispanic"
dat$Race=factor(dat$Race, levels=c("Other/multiple","White","Hispanic","Black","Asian"))

dat$weight=1/dat$SE_meta^2
fit_var=lm(Beta_meta~Race*llt, data=dat, weights=weight)

anova_tab <- anova(fit_var)
print(anova_tab)

ss_total <- sum(anova_tab$`Sum Sq`)
anova_tab$pct_variance <- 100 * anova_tab$`Sum Sq` / ss_total
print(anova_tab[, c("Sum Sq", "pct_variance")])


dat$llt[dat$llt==1]="Treated"
dat$llt[dat$llt==0]="Untreated"

ggplot(dat, aes(y=Race, color=as.factor(llt)))+theme_minimal()+
	geom_point(aes(x=Beta_meta), position=position_dodge(0.5))+
	geom_errorbar(aes(xmin=LB_meta, xmax=UB_meta), position=position_dodge(0.5), width=0.3)+
	scale_x_continuous(limits=c(0,35))+
	xlab("Meta-analyzed difference in mean LDL-C (mg/dL)\n for PRS+ vs. NHR groups")+
	ylab("Self-reported race/ethnicity")+
	guides(color=guide_legend(title="LLT prescription"))+
	scale_color_manual(values=c("#00B0BA","#FF5768"), breaks=c("Untreated","Treated"))+
	theme(axis.text=element_text(size=10), axis.title=element_text(size=11),
		legend.text=element_text(size=10))




