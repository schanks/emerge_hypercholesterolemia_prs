### For all adults, tests difference in mean value and % with SH using most recent LDL measurement

library(data.table)
#library(lme4)
library(rms)

print(sessionInfo())

dat=fread("../../Data/dat_adult.tab")

ldldat=dat[which(!is.na(dat$ldl_max)),]
ldldat$variant=factor(ldldat$variant, levels=c("Not high risk","PRS+","FH+"))
ldldat$var2=factor(ldldat$variant, levels=c("FH+","Mono+","Not high risk"))
ldldat$age_cat=factor(ldldat$age_cat)

## Mean difference

fit=lm(ldl_most_recent~variant+sex_at_birth_deid+rcs(age_deid,4)+site, data=ldldat)
coefs=as.data.frame(summary(fit)$coef)

fit2=lm(ldl_most_recent~var2+sex_at_birth_deid+rcs(age_deid,4)+site, data=ldldat)
coefs2=as.data.frame(summary(fit2)$coef)

res=as.data.table(matrix(rep(0, 6*3), ncol=6,nrow=3))
colnames(res)=c("Comparison","Beta","SE","LB","UB","P")

res$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR","FH+ vs. PRS+")

res$Beta[1:2]=coefs$Estimate[2:3]
res$SE[1:2]=coefs$`Std. Error`[2:3]
res$P[1:2]=coefs$`Pr(>|t|)`[2:3]
res$Beta[3]=coefs2$Estimate[2]
res$SE[3]=coefs2$`Std. Error`[2]
res$P[3]=coefs2$`Pr(>|t|)`[2]

res$LB=res$Beta-1.96*res$SE
res$UB=res$Beta+1.96*res$SE

write.table(res, "../../Results/ldl_most_recent.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Severe hypercholesterolemia

ldldat$shmax=as.numeric(ldldat$ldl_most_recent>=190)

fitsh=glm(shmax~variant+rcs(age_deid,4)+sex_at_birth_deid+site, data=ldldat, family="binomial")
coefsh=as.data.frame(summary(fitsh)$coef)

fitsh2=glm(shmax~var2+rcs(age_deid,4)+sex_at_birth_deid+site, data=ldldat, family="binomial")
coefsh2=as.data.frame(summary(fitsh2)$coef)

ressh=as.data.table(matrix(rep(0, 7*3), ncol=7,nrow=3))
colnames(ressh)=c("Comparison","Beta","OR","SE","LB","UB","P")
ressh$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR","FH+ vs. PRS+")
ressh$Beta=c(coefsh$Estimate[2:3], coefsh2$Estimate[2])
ressh$SE=c(coefsh$`Std. Error`[2:3], coefsh2$`Std. Error`[2])
ressh$P=c(coefsh$`Pr(>|z|)`[2:3], coefsh2$`Pr(>|z|)`[2])
ressh$LB=exp(ressh$Beta-1.96*ressh$SE)
ressh$UB=exp(ressh$Beta+1.96*ressh$SE)
ressh$OR=exp(ressh$Beta)

write.table(ressh, "../../Results/sh_most_recent.tab", quote=FALSE, row.names=FALSE, sep="\t")




