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

first=ggplot(dat, aes(x=lipoprotein, fill=variant))+geom_bar(aes(y=prop), stat="identity", position=position_dodge())+geom_errorbar(aes(ymin=lower, ymax=upper), width=0.3, position=position_dodge(0.9))+scale_fill_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"))+xlab("LDL-C target value")+ylab("Proportion of treated participants")+theme_classic()+ylim(c(0,1))+scale_x_discrete(labels=c("≤100 mg/dL","≤70 mg/dL", "≤55 mg/dL"))+theme(legend.title=element_blank(), axis.text=element_text(size=8), axis.title=element_text(size=10), legend.text=element_text(size=8), legend.position=c(0.8,0.8))





dat=fread("../../Results/res_risk.tab")
dat$Variant=factor(dat$Variant, levels=unique(dat$Variant))
dat$metric=factor(dat$metric, levels=rev(unique(dat$metric)))

dat$label=paste0(dat$count, ": (",round(dat$prop_total*100, digits=1),"%)")

a=ggplot(dat[1:12,], aes(x=prop_total, fill=Variant, y=metric))+facet_grid(.~Variant)+
	theme_classic()+scale_x_continuous(limits=c(0,1))+xlab("Percent of 18-30 year olds")+
	geom_col(aes(x=1, color=Variant), fill=NA, width=0.9)+
	geom_bar(color=NA,stat="identity")+
	theme(axis.line=element_blank(), strip.background=element_rect(color=NA),
	axis.title=element_blank(), axis.ticks.x=element_blank(), 
	legend.position="none", axis.text.x=element_blank(),
	strip.text=element_text(size=11), axis.text=element_text(size=11))+
	scale_fill_manual(values=primarycols)+
	scale_color_manual(values=primarycols)+
	geom_text(aes(label=label), x=0.5)


summary_dat <- data.frame(
  variable = factor(c("FH+","PRS+","Not-high-risk"),
                     levels = c("FH+","PRS+","Not-high-risk")),
  label = c("N/A", "114.64 mg/dL", "95.87 mg/dL")
)

undetected=fread("undetected.tab")
undetected$variant=factor(undetected$variant, levels=c("Not high risk","PRS+"))

undetected=undetected[!is.na(undetected$ldl_most_recent),]

fit=lm(ldl_most_recent~variant+sex_at_birth_deid+age_deid+site, data=undetected)

delta=20.3488
lb=delta-1.96*6.6799
ub=delta+1.96*6.6799
plot_delt=paste0("\u0394 = ",round(delta, digits=2)," (", round(lb, digits=2),"-",round(ub, digits=2),") mg/dL")

label_df <- data.frame(
  variable = "Not high risk",
  x = 0.7, y = -1,
  label = plot_delt
)

b <- ggplot(summary_dat, aes(x=0.5, y=0, label=label)) +
  facet_grid(.~variable) +
  geom_text(size=4) +
  theme_classic() +
  scale_x_continuous(limits=c(0,1)) +
  scale_y_continuous(
    breaks = 0,
    labels = "No trigger mean\nLDL-C (mg/dL)",
    limits = c(-2, 1)          # explicit, real range — no more degenerate scaling
  ) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "plain", angle = 0,
                                 vjust = 0.5, hjust = 1, color = "black",
                                 margin = margin(r = 1)),
    strip.text = element_blank(),
    strip.background = element_blank(),
    panel.spacing = unit(0, "lines"),
    plot.margin=margin(5,5,0,5)
  ) 

c <- ggplot(data.frame(x = 2/3, y = 0, label = plot_delt), 
            aes(x = x, y = y, label = label)) +
  geom_text(size = 4, hjust = 0.5) +
  theme_void() +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(-1, 1))+ 
  theme(plot.margin=margin(0,5,5,5))

combined <- a / b / c + plot_layout(heights = c(4, 0.5,0.5))

all = first / combined + plot_layout(heights = c(1,2))

tiff("Figure5.tiff", height=4, width=7, units="in", res=300)
combined
dev.off()





