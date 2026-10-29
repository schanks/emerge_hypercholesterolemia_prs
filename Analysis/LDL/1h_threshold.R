library(data.table)
library(rms)
library(logistf)

dat=fread("../../Data/PRS/dat_prs.tab")

ascvd=fread("../../Data/dat_adult.tab")

dat=merge(dat, ascvd[,c("record_id","ascvd")], by="record_id", all.x=TRUE)

dat=dat[which(!is.na(dat$ldl_most_recent)),]
fhdat=dat
dat=dat[which(dat$fh==0),]
#N=15,548

#Most recent LDL-C
res_ldlc=as.data.frame(matrix(c(seq(90,99,1), rep(0, 10*5)), ncol=6, byrow=FALSE))
colnames(res_ldlc)=c("Percentile","Beta","SE","LB","UB","P")

for (i in 1:nrow(res_ldlc)){
	p=res_ldlc$Percentile[i]
	dat$new_high=as.factor(dat$new_perc>=p)
	
	fit=lm(ldl_most_recent~new_high+sex_at_birth_deid+rcs(age_deid,4)+site, data=dat)
	coefs=as.data.frame(summary(fit)$coef)

	res_ldlc$Beta[i]=coefs$Estimate[2]
	res_ldlc$SE[i]=coefs$`Std. Error`[2]
	res_ldlc$P[i]=coefs$`Pr(>|t|)`[2]
}

res_ldlc$LB=res_ldlc$Beta-1.96*res_ldlc$SE
res_ldlc$UB=res_ldlc$Beta+1.96*res_ldlc$SE

write.table(res_ldlc, "threshold_mean_ldl.tab", quote=FALSE, row.names=FALSE, sep="\t")


dat$hc=as.numeric(dat$ldl_max>=160)
dat$sh=as.numeric(dat$ldl_max>=190)

#HC: LDL-C > 160 mg/dL
res_hc=as.data.frame(matrix(c(seq(90,99,1), rep(0, 10*4)), ncol=5, byrow=FALSE))
colnames(res_hc)=c("Percentile","OR","LB","UB","P")

for (i in 1:nrow(res_hc)){
	p=res_hc$Percentile[i]
	dat$new_high=as.factor(dat$new_perc>=p)
	
	fit=logistf(hc~new_high+sex_at_birth_deid+rcs(age_deid,4)+site, data=dat)
	coefs=coef(fit)
	confints=confint(fit)
	pvals=fit$prob

	beta=as.numeric(coef(fit)[2])
	res_hc$OR[i]=exp(beta)
	res_hc$LB[i]=exp(confints[2,1])
	res_hc$UB[i]=exp(confints[2,2])
	res_hc$P[i]=as.numeric(pvals[2])
}


write.table(res_hc, "threshold_firth_hc.tab", quote=FALSE, row.names=FALSE, sep="\t")

#SH: LDL-C > 190 mg/dL
res_sh=as.data.frame(matrix(c(seq(90,99,1), rep(0, 10*4)), ncol=5, byrow=FALSE))
colnames(res_sh)=c("Percentile","OR","LB","UB","P")

for (i in 1:nrow(res_sh)){
	p=res_sh$Percentile[i]
	dat$new_high=as.factor(dat$new_perc>=p)
	
	fit=logistf(sh~new_high+sex_at_birth_deid+rcs(age_deid,4)+site, data=dat)
	coefs=coef(fit)
	confints=confint(fit)
	pvals=fit$prob

	beta=as.numeric(coef(fit)[2])
	res_sh$OR[i]=exp(beta)
	res_sh$LB[i]=exp(confints[2,1])
	res_sh$UB[i]=exp(confints[2,2])
	res_sh$P[i]=as.numeric(pvals[2])
}


write.table(res_sh, "threshold_firth_sh.tab", quote=FALSE, row.names=FALSE, sep="\t")

#ASCVD: curiosity
res_ascvd=as.data.frame(matrix(c(seq(90,99,1), rep(0, 10*4)), ncol=5, byrow=FALSE))
colnames(res_sh)=c("Percentile","OR","LB","UB","P")

for (i in 1:nrow(res_sh)){
        p=res_sh$Percentile[i]
        dat$new_high=as.factor(dat$new_perc>=p)

        fit=logistf(ascvd~new_high+sex_at_birth_deid+rcs(age_deid,4)+site, data=dat)
        coefs=coef(fit)
        confints=confint(fit)
        pvals=fit$prob

        beta=as.numeric(coef(fit)[2])
        res_ascvd$OR[i]=exp(beta)
        res_ascvd$LB[i]=exp(confints[2,1])
        res_ascvd$UB[i]=exp(confints[2,2])
        res_ascvd$P[i]=as.numeric(pvals[2])
}

write.table(res_ascvd, "threshold_firth_ascvd.tab", quote=FALSE, row.names=FALSE, sep="\t")


#FH compared to new NHR definitions!
res_fh=as.data.frame(matrix(c(rep(c("LDL-C","HC","SH"), each=10), rep(seq(90,99,1), 3), rep(0, 30*5)), ncol=7, byrow=FALSE))
colnames(res_fh)=c("Outcome","Percentile","Beta","OR","LB","UB","P")
res_fh$Beta=as.numeric(res_fh$Beta)
fhdat$hc=as.numeric(fhdat$ldl_max>=160)
fhdat$sh=as.numeric(fhdat$ldl_max>=190)


for (i in 1:nrow(res_fh)){
	p=res_fh$Percentile[i]
	fhdat$nhr=as.numeric(fhdat$new_perc<p)

	newdat=fhdat[fhdat$nhr==1 | fhdat$fh==1,]

	if (res_fh$Outcome[i]=="LDL-C"){

		fit=lm(ldl_most_recent~fh+sex_at_birth_deid+rcs(age_deid,4)+site, data=newdat)
		coefs=as.data.frame(summary(fit)$coef)
		res_fh$Beta[i]=as.numeric(coefs$Estimate[2])
		res_fh$LB[i]=res_fh$Beta[i]-1.96*coefs$`Std. Error`[2]
		res_fh$UB[i]=res_fh$Beta[i]+1.96*coefs$`Std. Error`[2]
		res_fh$P[i]=coefs$`Pr(>|t|)`[2]

	} else if (res_fh$Outcome[i]=="HC"){

		fit=logistf(hc~fh+sex_at_birth_deid+rcs(age_deid,4)+site, data=newdat)
		res_fh$Beta[i]=as.numeric(coef(fit)[2])
		res_fh$OR[i]=exp(res_fh$Beta[i])
		res_fh$LB[i]=exp(confint(fit)[2,1])
		res_fh$UB[i]=exp(confint(fit)[2,2])
		res_fh$P[i]=as.numeric(fit$prob[2])

	} else {

		fit=logistf(sh~fh+sex_at_birth_deid+rcs(age_deid,4)+site, data=newdat)
                res_fh$Beta[i]=as.numeric(coef(fit)[2])
                res_fh$OR[i]=exp(res_fh$Beta[i])
                res_fh$LB[i]=exp(confint(fit)[2,1])
                res_fh$UB[i]=exp(confint(fit)[2,2])
                res_fh$P[i]=as.numeric(fit$prob[2])

	}
}

write.table(res_fh, "threshold_fh.tab", quote=FALSE, row.names=FALSE, sep="\t")





