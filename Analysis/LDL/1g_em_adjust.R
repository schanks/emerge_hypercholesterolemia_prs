library(data.table)
library(rms)
library(car)
library(emmeans)

dat=fread("../../Data/dat_adult.tab")


dat=dat[which(!is.na(dat$ldl_most_recent)),]
dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))


modelA=lm(ldl_most_recent~variant+rcs(age_deid,4)+sex_at_birth_deid+site+race4+variant*race4, data=dat)
int_termsA=grep("variantPRS.*:race|race.*:variantPRS", names(coef(modelA)), value = TRUE)
lhA=linearHypothesis(modelA, int_termsA)

modelB=lm(ldl_most_recent~variant+rcs(age_deid,4)+sex_at_birth_deid+site+race4+variant*race4+llt, data=dat)
int_termsB=grep("variantPRS.*:race|race.*:variantPRS", names(coef(modelB)), value = TRUE)
lhB=linearHypothesis(modelB, int_termsB)

modelC=lm(ldl_most_recent~rcs(age_deid,4)+sex_at_birth_deid+site+variant*race4*llt, data=dat)
int_termsLLT=grep("variantPRS.*:race.*:llt", names(coef(modelC)), value = TRUE)
lhCLLT=linearHypothesis(modelC, int_termsLLT)

emm=emmeans(modelC, ~variant | race4*llt)
contrasts <- contrast(emm, method = "trt.vs.ctrl", ref = 1)
ci_tab <- as.data.frame(unclass(summary(contrasts, infer = c(TRUE, TRUE))))

ci_tab=ci_tab[grep("PRS+", ci_tab$contrast),]

write.table(ci_tab, "../../Results/ldl_em_llt_stratified.tab", quote=FALSE, row.names=FALSE, sep="\t")


beta_3way <- coef(modelC)[int_termsLLT]
vcov_3way <- vcov(modelC)[int_termsLLT, int_termsLLT, drop = FALSE]

deparse_flat <- function(x) {
    paste(deparse(x, width.cutoff = 500), collapse = " ")
}

out <- data.frame(
    Modifier      = "race4",
    Comparison    = "Model_C_three_way",
    P_interaction = lhCLLT[["Pr(>F)"]][2],
    Df            = lhCLLT[["Df"]][2],
    beta          = deparse_flat(beta_3way),
    vcov          = deparse_flat(as.vector(vcov_3way)),
    stringsAsFactors = FALSE
)

write.table(out, "../../Results/ldl_em_llt_pval.tab", quote=FALSE, row.names=FALSE, sep="\t")



