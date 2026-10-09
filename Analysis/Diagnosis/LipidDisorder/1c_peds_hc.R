library(data.table)
library(sandwich)
library(lmtest)

dat=fread("../../Data/peds_analytical.tab")

dat$variant=rep("Not high risk", nrow(dat))
dat$variant[which(dat$prs==1)]="PRS+"

#Modeling
#Base LDL-C model
icd_dat=dat[which(dat$in_icd==1),]

icd_dat$chop=as.factor(icd_dat$site=="CHOP")
icd_dat$variant=as.factor(icd_dat$variant)

fithc=glm(hcdx~variant+sex_at_birth+age_deid+chop, data=icd_dat, family=poisson(link="log"))
robust_se=vcovHC(fithc, type="HC0")

coefhc=coeftest(fithc, vcov = robust_se)

reshc=as.data.table(matrix(rep(0, 7*1), ncol=7,nrow=1))
colnames(reshc)=c("Comparison","Beta","RR","SE","LB","UB","P")
reshc$Comparison="PRS+ vs. NHR"
reshc$Beta=coefhc[2,1]
reshc$SE=coefhc[2,2]
reshc$P=coefhc[2,4]
reshc$Age="3-17"

reshc$LB=exp(reshc$Beta-1.96*reshc$SE)
reshc$UB=exp(reshc$Beta+1.96*reshc$SE)
reshc$RR=exp(reshc$Beta)

write.table(reshc, "../../Results/peds_hcdx.tab", quote=FALSE, row.names=FALSE, sep="\t")


