library(data.table)
library(sandwich)
library(lmtest)

dat=fread("../../Data/dat_adult.tab")

dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))
dat$var_reorder=factor(dat$variant, levels=c("PRS+","FH+","Not high risk"))
dat$age_cat=factor(dat$age_cat)

llt_dat=dat[which(dat$in_drug==1),]

llt_dat[, age_centered := age_deid - mean(age_deid), by = age_cat]

fitllt <- glm(
  llt ~ variant * age_cat + age_centered + sex_at_birth_deid + site,
  data = llt_dat,
  family = poisson(link = "log")
)

robust_se <- vcovHC(fitllt, type = "HC0")
coefllt <- coeftest(fitllt, vcov = robust_se)

coefnames <- rownames(coefllt)

# function to calculate age-specific effects
get_effect <- function(main_effect, interaction_effect = NULL) {
  
  beta <- coefllt[main_effect, 1]
  
  if (!is.null(interaction_effect) && interaction_effect %in% coefnames) {
    
    beta <- beta + coefllt[interaction_effect, 1]
    
    se <- sqrt(
      robust_se[main_effect, main_effect] +
      robust_se[interaction_effect, interaction_effect] +
      2 * robust_se[main_effect, interaction_effect]
    )
    
  } else {
    
    se <- coefllt[main_effect, 2]
    
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


age_levels <- levels(llt_dat$age_cat)

resllt <- rbindlist(lapply(age_levels, function(age) {
  
  prs_int <- if (age == age_levels[1]) NULL else paste0("variantPRS+:age_cat", age)
  fh_int  <- if (age == age_levels[1]) NULL else paste0("variantFH+:age_cat", age)
  
  rbind(
    cbind(
      data.table(
        Comparison = "PRS+ vs. NHR",
        Age = age
      ),
      get_effect("variantPRS+", prs_int)
    ),
    cbind(
      data.table(
        Comparison = "FH+ vs. NHR",
        Age = age
      ),
      get_effect("variantFH+", fh_int)
    )
  )
}))

write.table(resllt, "../../Results/llt_age.tab", quote=FALSE, row.names=FALSE, sep="\t")













