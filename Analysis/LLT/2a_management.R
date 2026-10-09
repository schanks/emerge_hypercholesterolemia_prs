library(data.table)
library(tidyverse)
library(rms)

dat=fread("../../Data/dat_adult.tab")

treated=dat[which(dat$llt==1),]
treated=treated[which(!is.na(treated$ldl_most_recent)),]
treated=treated[which(treated$age_llt<treated$last_ldl+(1/12)),]

treated$variant=factor(treated$variant, levels=c("Not high risk","PRS+","FH+"))

treated$ldl100=as.numeric(treated$ldl_most_recent<100)
treated$ldl70=as.numeric(treated$ldl_most_recent<70)
treated$ldl55=as.numeric(treated$ldl_most_recent<55)

outcomes <- c("ldl100", "ldl70", "ldl55")

results <- map_dfr(outcomes, function(outcome) {
  df_model <- treated %>% filter(!is.na(.data[[outcome]]))
  
  n_per_level <- df_model %>% count(variant) %>% deframe()
  
  fit <- glm(as.formula(paste0(outcome, " ~ variant + rcs(age_deid, 4) + sex_at_birth_deid + site")),
             family = "binomial", data = df_model)
  coefs <- summary(fit)$coefficients
  
  variant_rows <- coefs[grepl("^variant", rownames(coefs)), , drop = FALSE]
  
  tibble(
    Target  = outcome,
    Comparison     = rownames(variant_rows),
    Beta     = variant_rows[, "Estimate"],
    SE       = variant_rows[, "Std. Error"],
    P  = variant_rows[, "Pr(>|z|)"],
    N_NHR    = n_per_level[1],
    N_PRS = n_per_level[2],
    N_FH = if (length(n_per_level) >= 3) n_per_level[3] else NA_integer_
  )
})

results$Comparison[results$Comparison=="variantPRS+"]="PRS+ vs. NHR"
results$Comparison[results$Comparison=="variantFH+"]="FH+ vs. NHR"

results$OR=exp(results$Beta)
results$LB=exp(results$Beta-1.96*results$SE)
results$UB=exp(results$Beta+1.96*results$SE)

write.table(results, "../../Results/treated_targets.tab", row.names=FALSE, sep="\t", quote=FALSE)


