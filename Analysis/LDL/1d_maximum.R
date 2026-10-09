library(data.table)
library(lme4)
library(rms)

dat=fread("../../Data/dat_adult.tab")

ldldat=dat[which(!is.na(dat$ldl_max)),]
ldldat$variant=factor(ldldat$variant, levels=c("Not high risk","PRS+","FH+"))
ldldat$age_cat=factor(ldldat$age_cat)


fit=lm(ldl_max~variant+sex_at_birth_deid+rcs(age_deid,4)+site, data=ldldat)
coefs=as.data.frame(summary(fit)$coef)

res=as.data.table(matrix(rep(0, 6*2), ncol=6,nrow=2))
colnames(res)=c("Comparison","Beta","SE","LB","UB","P")

res$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR")

res$Beta=coefs$Estimate[2:3]
res$SE=coefs$`Std. Error`[2:3]
res$P=coefs$`Pr(>|t|)`[2:3]

res$LB=res$Beta-1.96*res$SE
res$UB=res$Beta+1.96*res$SE

n_fh=sum(ldldat$variant=="FH+")
n_prs=sum(ldldat$variant=="PRS+")
n_nhr=sum(ldldat$variant=="Not high risk")

res$N_FH=rep(n_fh, nrow(res))
res$N_PRS=rep(n_prs, nrow(res))
res$N_NHR=rep(n_nhr, nrow(res))

write.table(res, "../../Results/ldl_max.tab", quote=FALSE, row.names=FALSE, sep="\t")




