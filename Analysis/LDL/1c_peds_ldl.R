library(data.table)

dat=fread("../../Data/peds_analytical.tab")

dat$variant=rep("Not high risk", nrow(dat))
dat$variant[which(dat$prs==1)]="PRS+"

#Modeling
#Base LDL-C model
ldldat=dat[which(!is.na(dat$ldlc)),]

ldldat$chop=as.factor(ldldat$site=="CHOP")
ldldat$variant=as.factor(ldldat$variant)

lmm_ldlc=lm(ldlc~variant + age_deid+sex_at_birth+chop, data=ldldat)
sumfit_ldlc=summary(lmm_ldlc)$coef

#Extracting results
ldlc=as.data.frame(matrix(rep(0,6),nrow=1))
colnames(ldlc)=c("Age","Beta","SE","LB","UB","P")
ldlc$Age="3-17"

#Pediatric Estimate
ldlc$Beta=sumfit_ldlc[2,1]
ldlc$SE=sumfit_ldlc[2,2]
ldlc$P=sumfit_ldlc[2,4]
ldlc$LB=ldlc$Beta-1.96*ldlc$SE
ldlc$UB=ldlc$Beta+1.96*ldlc$SE

write.table(ldlc, "../../Results/peds_ldl.tab", quote=FALSE, row.names=FALSE, sep="\t")


ldldat$borderline=as.numeric(ldldat$ldlc>=110 & ldldat$ldlc<130)
ldldat$abnormal=as.numeric(ldldat$ldlc>=130)

outcomes <- c("borderline", "abnormal")

results <- lapply(outcomes, function(outcome) {
  formula <- as.formula(paste(outcome, "~ variant + age_deid + sex_at_birth + chop"))
  model <- glm(formula, data = ldldat, family = binomial())
  
  coef_summary <- summary(model)$coefficients
  ci <- confint(model)
  
  # Extract variant row only
  or  <- exp(coef_summary["variantPRS+", "Estimate"])
  ci_lo <- exp(ci["variantPRS+", "2.5 %"])
  ci_hi <- exp(ci["variantPRS+", "97.5 %"])
  pval  <- coef_summary["variantPRS+", "Pr(>|z|)"]
  
  data.frame(
    outcome    = outcome,
    OR         = round(or, 3),
    CI_low     = round(ci_lo, 3),
    CI_high    = round(ci_hi, 3),
    p_value    = signif(pval, 4)
  )
})

results_df <- do.call(rbind, results)
print(results_df)

write.table(results_df, "../../Results/regression_peds_abnormal.tab", quote=FALSE, row.names=FALSE, sep="\t")
















