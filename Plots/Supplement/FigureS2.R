library(data.table)
library(ggplot2)
library(gridExtra)
library(ggpubr)

primarycols=c("#66CCEE","#4477AA","#BBBBBB")


shap=fread("../../Results/shap_values.tab")
dat=fread("../../Analysis/Lipids/impute_results.tab")
res=fread("../../Results/ldl_imputed_untreated.tab")

### Panel B - Shap plot

shap$variable=factor(shap$variable, levels=rev(unique(shap$variable)))
plotlabs=c("PAD","CAD","Stroke/TIA","Sex","Maximum LDL-C","LDL-C slope","Maximum percent drop in LDL-C","Most recent LDL-C","T2D","Minimum LDL-C slope","Age","Lipid disorder")


shapplot=ggplot(shap, aes(y=variable, x=value, color=stdfvalue))+
	geom_point(size=0.8)+theme_minimal()+xlab("SHAP value")+
	scale_y_discrete(labels=plotlabs)+
	scale_color_viridis_c(option="inferno", end=0.8, breaks=c(0, 1), labels=c("Low","High"), name="Feature value",
		guide=guide_colorbar(title.vjust=0.8, barheight=unit(0.2, "cm")))+
	theme(axis.title.y=element_blank(),axis.text.y=element_text(size=8),
	legend.position="bottom", legend.title=element_text(size=9),
	axis.title.x=element_text(size=9), axis.text.x=element_text(size=8),
	legend.text=element_text(size=8))

shap_legend=get_legend(shapplot)
shapplot=shapplot+theme(legend.position="none")

tiff("shap.tiff", height=2.1, width=4.3, units="in", res=300)
grid.arrange(shapplot, shap_legend, heights=c(5,1))
dev.off()

#### Panel E - results

untreated_res=fread("../../Results/ldl_untreated.tab")
all_res=fread("../../Results/ldl_most_recent.tab")
all_res=all_res[1:2,]

untreated_res=untreated_res[,-c("N_FH","N_PRS","N_NHR")]
res=res[,-c("N_FH","N_PRS","N_NHR")]

res$Version=rep("Predicted untreated", 2)
all_res$Version=rep("Full cohort", 2)
untreated_res$Version=rep("EHR untreated", 2)


result=rbind(res, all_res, untreated_res)
result$Version=factor(result$Version, levels=c("Predicted untreated","EHR untreated","Full cohort"))

res_plot=ggplot(result, aes(y=Version, color=Comparison))+
	theme_minimal()+xlab("Average difference in\nmost recent LDL-C (mg/dL)")+
	geom_point(aes(x=Beta), position=position_dodge(width=0.4))+
	geom_errorbar(aes(xmin=LB, xmax=UB), position=position_dodge(width=0.4), width=0.3)+
	scale_color_manual(values=primarycols)+
	theme(axis.title.y=element_blank(), legend.position="bottom",
	axis.text=element_text(size=8), axis.title=element_text(size=9),
	legend.title=element_blank(), legend.text=element_text(size=8))
res_legend=get_legend(res_plot)
res_plot=res_plot+theme(legend.position="none")

tiff("res.tiff", height=4.4, width=2.7, units="in", res=300)
grid.arrange(res_plot, res_legend, heights=c(5,1))
dev.off()


#### Concordance tables

table(dat$llt_pred, dat$llt)

table(dat$llt_pred, dat$meds_lower_cholesterol_2)

