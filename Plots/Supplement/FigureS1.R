library(data.table)
library(ggplot2)


primarycols=c("#66CCEE","#4477AA","#BBBBBB")



dat=fread("../Data/dat_adult.tab")
peds=fread("../Data/peds_analytical.tab")



peds$age_cat=rep("3-17", nrow(peds))
peds$variant=rep("Not high risk", nrow(peds))
peds$variant[peds$prs==1]="PRS+"

dat$in_ldl=as.numeric(!is.na(dat$ldl_most_recent))


dat=rbind(dat[,c("variant","age_cat","in_ldl")], peds[,c("variant","age_cat","in_ldl")])


datagg=aggregate(dat$in_ldl, by=list(dat$variant, dat$age_cat), FUN=mean)
colnames(datagg)=c("variant","Age","prop")
datagg_n <- aggregate(dat$in_ldl, by=list(dat$variant, dat$age_cat), FUN=length)
datagg$n <- datagg_n$x
z <- qnorm(0.975)
se <- sqrt(datagg$prop * (1 - datagg$prop) / datagg$n)
datagg$lower <- pmax(0, datagg$prop - z * se)
datagg$upper <- pmin(1, datagg$prop + z * se)
datagg$count=aggregate(dat$in_ldl, by=list(dat$variant, dat$age_cat), FUN=sum)$x


allagg=aggregate(dat$in_ldl, by=list(dat$age_cat), FUN=mean)
colnames(allagg)=c("Age","prop")
allagg$n=aggregate(dat$in_ldl, by=list(dat$age_cat), FUN=length)$x
se = sqrt(allagg$prop * (1-allagg$prop) / allagg$n)
allagg$lower=pmax(0, allagg$prop - z*se)
allagg$upper = pmin(1, allagg$prop + z*se)
allagg$variant=rep("All", nrow(allagg))
allagg$count=aggregate(dat$in_ldl, by=list(dat$age_cat), FUN=sum)$x

datagg=rbind(datagg, allagg)
datagg$Age=factor(datagg$Age, levels=c("3-17","18-29","30-39","40-49","50-59","60-69","70-75"))
datagg$variant=factor(datagg$variant, levels=c("All","FH+","PRS+","Not high risk"))

#Four panel
ggplot(datagg, aes(x=Age, y=prop))+geom_bar(stat="identity", aes(fill=variant), color="black")+geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)+facet_wrap(variant~.)+scale_fill_manual(values=c("darkolivegreen3",primarycols))+theme_classic()+scale_y_continuous(limits=c(0,1))+ylab("Proportion with LDL-C data")+theme(legend.position="none", axis.text=element_text(size=9), axis.title=element_text(size=12), strip.text=element_text(size=11), strip.background=element_blank())+geom_text(y=0.05, aes(label=count, color=variant), size=3)+scale_color_manual(values=c("black","black","white","black")

#One panel
ggplot(datagg[datagg$variant=="All",], aes(x=Age, y=prop))+geom_bar(stat="identity", fill="darkolivegreen3", color="black")+geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2)+theme_classic()+scale_y_continuous(limits=c(0,1))+ylab("Proportion with LDL-C data")+theme(plot.margin=margin(20,5,5,5))+geom_segment(x=2.6, xend=7.4, y=0.95, yend=0.95)+annotate("text", label="PREVENT equations ages", y=1, x=5)

#Comparison side by side - focus on variant differences
ggplot(datagg, aes(x=Age, y=prop, fill=variant))+geom_bar(stat="identity", color="black", position=position_dodge())+geom_errorbar(aes(ymin=lower, ymax=upper), width=0.2, position=position_dodge(0.9))+scale_fill_manual(values=c("darkolivegreen3",primarycols))+theme_classic()+theme(legend.position="none", axis.text=element_text(size=9), axis.title=element_text(size=12))+ylab("Proportion with LDL-C data")+scale_y_continuous(limits=c(0,1))




