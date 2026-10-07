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

dat=fread("../../Data/dat_adult.tab")
dat$in_ldl=as.numeric(!is.na(dat$ldl_most_recent))
adults=dat
adults$age_cat=rep("All adults\n(18-75)", nrow(adults))
dat=rbind(dat, adults)

peds=fread("../../Data/peds_analytical.tab")
peds$age_cat=rep("Pediatric\n(3-17)", nrow(peds))
peds$variant=rep("Not high risk", nrow(peds))
peds$variant[which(peds$prs==1)]="PRS+"
colnames(peds)[17]="ldl_most_recent"

paneldat=rbind(dat[,c("variant","age_cat","in_ldl","ldl_most_recent")], peds[,c("variant","age_cat","in_ldl","ldl_most_recent")])

####Panel A
paneldatagg=aggregate(paneldat$in_ldl, by=list(paneldat$age_cat), FUN=mean)
colnames(paneldatagg)=c("Age","prop")
paneldatagg_n <- aggregate(paneldat$in_ldl, by=list(paneldat$age_cat), FUN=length)
paneldatagg$n <- paneldatagg_n$x
z <- qnorm(0.975)
se <- sqrt(paneldatagg$prop * (1 - paneldatagg$prop) / paneldatagg$n)
paneldatagg$lower <- pmax(0, paneldatagg$prop - z * se)
paneldatagg$upper <- pmin(1, paneldatagg$prop + z * se)
paneldatagg$count=aggregate(paneldat$in_ldl, by=list(paneldat$age_cat), FUN=sum)$x
paneldatagg$Age=factor(paneldatagg$Age, levels=rev(age_levels))
paneldatagg$label=paste0(round(paneldatagg$prop, digits=2)*100, "%")

panelmed=aggregate(dat$n_panel, by=list(dat$age_cat), FUN=median, na.rm=TRUE)
colnames(panelmed)=c("Age","npanel")

paneldatagg=merge(paneldatagg, panelmed, by="Age", all.x=TRUE)
paneldatagg$npanel[paneldatagg$Age=="Pediatric\n(3-17)"]=1

square_df <- paneldatagg %>%
  distinct(Age, npanel) %>%          
  crossing(square_index = 1:6) %>%
  mutate(
    filled = square_index <= npanel,        
    y_pos  = (square_index - 0.5) / 6
  )

a=ggplot(paneldatagg, aes(y=prop))+facet_grid2(Age~., switch="y", axes="x")+
	theme_void()+scale_y_continuous(limits=c(0,1), expand=c(0,0),breaks=c(1))+
	geom_bar(fill=lipidcolor,width=0.85,color=NA, stat="identity", aes(x=1))+
	geom_col(color="black", aes(x=1, y=1), fill=NA)+
	theme(axis.text=element_blank(), axis.title.y=element_blank(), 
	axis.ticks=element_blank(), strip.text.y.left=element_text(angle=0, size=10), 
	strip.placement="outside", strip.background=element_rect(color=NA),
	axis.title.x=element_text(size=10), plot.margin=margin(15,0,10,5.5))+
	xlab("\nHas lipid\npanel")+
	geom_tile(data=square_df,aes(x=1.8,y=y_pos, fill=filled),
	color="black", width=0.6, height=1/6*0.8, inherit.aes=FALSE)+
	scale_fill_manual(values=c('TRUE'="grey20", 'FALSE'=NA), guide="none")+
	geom_text(aes(label=label, x=1, y=0.2), size=3)

####Panel B
ldldat=paneldat[paneldat$in_ldl==1,]
ldldat$age_cat[ldldat$age_cat=="18-75"]="All adults\n(18-75)"
ldldat$age_cat[ldldat$age_cat=="3-17"]="Pediatric\n(3-17)"
ldldat$age_cat=factor(ldldat$age_cat, levels=rev(age_levels))
ldldat$variant=factor(ldldat$variant, levels=var_levels)


b=ggplot(ldldat, aes(fill=variant))+facet_grid2(age_cat~.,switch="y", axes="margins")+
        stat_slab(aes(x=ldl_most_recent), position="dodge", alpha=0.8,linewidth=0.1)+
        stat_spike(aes(x=ldl_most_recent),color="black",linewidth=0.3,size=1,linetype="dotted",at=c(mean), position="dodge",shape=18)+
        scale_y_discrete(expand=expansion(c(-0.08,0.1)))+theme_minimal()+
        scale_fill_manual(values=primarycols, breaks=c("FH+","PRS+","Not high risk"))+
        theme(legend.title=element_blank(),axis.text.x=element_text(size=10),
        axis.title.x=element_text(size=10), axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.title.y=element_blank(),
        strip.text.y.left=element_blank(), strip.placement="outside",
	plot.margin=margin(15,5.5,5.5,5.5),
        strip.background=element_rect(color=NA))+
        geom_vline(xintercept=-Inf, linewidth=1)+xlab("LDL-C (mg/dL)\n ")+
	geom_hline(yintercept=-Inf, linewidth=1)


tiff("Figure1.tiff", width=5, height=6, unit="in", res=300)
grid.arrange(a,b, widths=c(1,2))
dev.off()






