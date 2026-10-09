library(data.table)
library(sandwich)
library(lmtest)
library(rms)

dat=fread("../../Data/dat_adult.tab")

dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))
dat$age_cat=factor(dat$age_cat)

llt_dat=dat[which(dat$in_drug==1),]

fitdx=glm(llt~variant+rcs(age_deid,4)+sex_at_birth_deid+site, data=llt_dat, family=poisson(link="log"))
robust_se=vcovHC(fitdx, type="HC0")

coefdx=coeftest(fitdx, vcov=robust_se)

resdx=as.data.table(matrix(rep(0, 7*2), ncol=7,nrow=2))
colnames(resdx)=c("Comparison","Beta","RR","SE","LB","UB","P")
resdx$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR")
resdx$Beta=coefdx[2:3,1]
resdx$SE=coefdx[2:3,2]
resdx$P=coefdx[2:3,4]

resdx$LB=exp(resdx$Beta-1.96*resdx$SE)
resdx$UB=exp(resdx$Beta+1.96*resdx$SE)
resdx$RR=exp(resdx$Beta)

write.table(resdx, "../../Results/llt.tab", quote=FALSE, row.names=FALSE, sep="\t")



