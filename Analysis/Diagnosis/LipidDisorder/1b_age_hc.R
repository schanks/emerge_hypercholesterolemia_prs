library(data.table)
library(sandwich)
library(lmtest)

dat=fread("../../Data/dat_adult.tab")

dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))
dat$var_reorder=factor(dat$variant, levels=c("PRS+","FH+","Not high risk"))
dat$age_cat=factor(dat$age_cat)

icd_dat=dat[which(dat$in_icd==1),]
icd_dat[, age_centered := age_deid - mean(age_deid), by = age_cat]

fithc <- glm(
  hcdx ~ variant * age_cat + age_centered + sex_at_birth_deid + site,
  data = icd_dat,
  family = poisson(link = "log")
)

# robust SE
robust_se <- vcovHC(fithc, type = "HC0")
coefhc <- coeftest(fithc, vcov = robust_se)

coefnames <- rownames(coefhc)

# function to calculate age-specific contrasts
get_effect <- function(main_effect, interaction_effect = NULL) {
  
  beta <- coefhc[main_effect, 1]
  
  if (!is.null(interaction_effect) && interaction_effect %in% coefnames) {
    
    beta <- beta + coefhc[interaction_effect, 1]
    
    se <- sqrt(
      robust_se[main_effect, main_effect] +
      robust_se[interaction_effect, interaction_effect] +
      2 * robust_se[main_effect, interaction_effect]
    )
    
  } else {
    
    se <- coefhc[main_effect, 2]
    
  }
  
  data.table(
    Beta = beta,
    RR = exp(beta),
    SE = se,
    LB = exp(beta - 1.96 * se),
    UB = exp(beta + 1.96 * se),
    P = 2 * pnorm(-abs(beta / se))
  )
}


age_levels <- levels(icd_dat$age_cat)

reshc <- rbindlist(lapply(age_levels, function(age) {
  
  prs_int <- if (age == age_levels[1]) NULL else paste0("variantPRS+:age_cat", age)
  fh_int  <- if (age == age_levels[1]) NULL else paste0("variantFH+:age_cat", age)
  
  rbind(
    cbind(
      data.table(Comparison = "PRS+ vs. NHR", Age = age),
      get_effect("variantPRS+", prs_int)
    ),
    cbind(
      data.table(Comparison = "FH+ vs. NHR", Age = age),
      get_effect("variantFH+", fh_int)
    )
  )
}))

write.table(reshc, "../../Results/hcdx_age.tab", quote=FALSE, row.names=FALSE, sep="\t")




















