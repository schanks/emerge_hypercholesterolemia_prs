library(data.table)
library(metafor)
library(purrr)

emerge=fread("../../Results/ldl_em.tab")
aou=fread("../../AoU/ldl_em.tab")

emerge=emerge[which(emerge$Comparison=="PRS+ vs. NHR"),]
aou=aou[which(aou$Comparison=="PRS+ vs. NHR"),]
aou$Modifier_Level[aou$Modifier_Level=="Multiple/Other"]="Other/multiple"
aou$Modifier_Level[aou$Modifier_Level=="Black or African American"]="Black, African American, or African"
aou$Modifier_Level[aou$Modifier_Level=="Hispanic"]="Hispanic, Latino, or Spanish"

meta_fixed <- function(res1, res2) {
  merged <- merge(
    emerge[, .(Modifier, Modifier_Level, Beta1 = Beta, SE1 = SE)],
    aou[, .(Modifier, Modifier_Level, Beta2 = Beta, SE2 = SE)],
    by = c("Modifier", "Modifier_Level")
  )
  
  print(nrow(merged))
  stopifnot(nrow(merged) == nrow(emerge))  # sanity check nothing dropped in the merge
  
  pmap_dfr(merged, function(Modifier, Modifier_Level, Beta1, SE1, Beta2, SE2) {
    fit <- rma(yi = c(Beta1, Beta2), sei = c(SE1, SE2), method = "FE")
    data.table(
      Modifier    = Modifier,
      Modifier_Level = Modifier_Level,
      Beta_meta  = as.numeric(fit$b),
      SE_meta    = fit$se,
      OR_meta    = exp(as.numeric(fit$b)),
      LB_meta    = exp(fit$ci.lb),
      UB_meta    = exp(fit$ci.ub),
      P_meta     = fit$pval,
      Q_pval     = fit$QEp      # test for heterogeneity between the two studies
    )
  })
}


meta_results <- meta_fixed(results_dataset1, results_dataset2)
print(meta_results)

write.table(meta_results, "../../Results/modifier_meta.tab", quote=FALSE, row.names=FALSE, sep="\t")


