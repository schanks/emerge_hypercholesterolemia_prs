library(data.table)
library(metafor)
library(purrr)

emerge=fread("../../Results/eiv_coxph.tab")
aou=fread("../../AoU/aou_coxph.tab")


meta_fixed <- function(res1, res2) {
  merged <- merge(
    emerge[, .(Comparison, outcome, Beta1 = Beta, SE1 = SE)],
    aou[, .(Comparison, outcome, Beta2 = Beta, SE2 = SE)],
    by = c("Comparison","outcome")
  )
  
  stopifnot(nrow(merged) == nrow(emerge))  # sanity check nothing dropped in the merge
  
  pmap_dfr(merged, function(Comparison, outcome, Beta1, SE1, Beta2, SE2) {
    fit <- rma(yi = c(Beta1, Beta2), sei = c(SE1, SE2), method = "FE")
    data.table(
      Comparison = Comparison,
      outcome = outcome,
      Beta_meta  = as.numeric(fit$b),
      SE_meta    = fit$se,
      HR_meta    = exp(as.numeric(fit$b)),
      LB_meta    = exp(fit$ci.lb),
      UB_meta    = exp(fit$ci.ub),
      P_meta     = fit$pval,
      Q_pval     = fit$QEp      # test for heterogeneity between the two studies
    )
  })
}


meta_results <- meta_fixed(results_dataset1, results_dataset2)
print(meta_results)

meta_results$Comparison=factor(meta_results$Comparison, levels=c("variantPRS+","variantFH+"))
meta_results$outcome=factor(meta_results$outcome, levels=c("ascvd","cad","pad","stroke"))
meta_results=meta_results[order(meta_results$outcome,meta_results$Comparison),]

write.table(meta_results, "../../Results/ascvd_coxph_meta.tab", quote=FALSE, row.names=FALSE, sep="\t")


