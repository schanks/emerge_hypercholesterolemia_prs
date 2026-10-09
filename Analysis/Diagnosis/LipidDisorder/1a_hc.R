library(data.table)
library(sandwich)
library(lmtest)
library(rms)

dat=fread("../../Data/dat_adult.tab")

dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))
dat$var_reorder=factor(dat$variant, levels=c("PRS+","FH+","Not high risk"))
dat$age_cat=factor(dat$age_cat)

icd_dat=dat[which(dat$in_icd==1),]

fithc=glm(hcdx~variant+rcs(age_deid,4)+sex_at_birth_deid+site, data=icd_dat, family=poisson(link="log"))
robust_se=vcovHC(fithc, type="HC0")

coefhc=coeftest(fithc, vcov = robust_se)

reshc=as.data.table(matrix(rep(0, 7*3), ncol=7,nrow=3))
colnames(reshc)=c("Comparison","Beta","RR","SE","LB","UB","P")
reshc$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR","FH+ vs. PRS+")
reshc$Beta[1:2]=coefhc[2:3,1]
reshc$SE[1:2]=coefhc[2:3,2]
reshc$P[1:2]=coefhc[2:3,4]

fithc2=glm(hcdx~var_reorder+age_cat+sex_at_birth_deid+site, data=icd_dat, family=poisson(link="log"))
robust_se2=vcovHC(fithc2, type="HC0")

coefhc2=coeftest(fithc2, vcov=robust_se2)

reshc$Beta[3]=coefhc2[2,1]
reshc$SE[3]=coefhc2[2,2]
reshc$P[3]=coefhc2[2,4]

reshc$LB=exp(reshc$Beta-1.96*reshc$SE)
reshc$UB=exp(reshc$Beta+1.96*reshc$SE)
reshc$RR=exp(reshc$Beta)

write.table(reshc, "../../Results/hcdx.tab", quote=FALSE, row.names=FALSE, sep="\t")






