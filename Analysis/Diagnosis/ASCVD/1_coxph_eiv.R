library(data.table)
library(survival)

########HELPER FUNCTION
library(survival)
library(data.table)

#' Run a Cox PH analysis for a variant vs. reference comparison and summarize results
#'
#' @param dat            data.frame/data.table with all required columns
#' @param time_start_var  name of the start-time column (e.g. "first_icd")
#' @param time_end_var    name of the end-time column (e.g. "time")
#' @param event_var       name of the event/outcome column (e.g. "ascvd")
#' @param variant_var     name of the variant/exposure column (e.g. "variant")
#' @param covariates      character vector of additional covariate column names
#'                        (e.g. c("sex_at_birth_deid", "site"))
#' @param comparisons     list of character vectors, each giving the levels of
#'                        variant_var to keep for that row/comparison, with the
#'                        exposure level listed first and the reference level
#'                        listed second, e.g. list(c("PRS+","Not high risk"),
#'                        c("FH+","Not high risk"))
#' @param event_count_var name of the column to use for counting events
#'                        (defaults to event_var; kept as an option in case you
#'                        want a different indicator, mirroring the original
#'                        script's use of dat$cad for one row and dat$ascvd for
#'                        another)
#' @param output_file     optional path to write the results table (tab-delimited).
#'                        If NULL (default), nothing is written to disk.
#'
#' @return a data.table with one row per comparison
run_coxph_analysis <- function(dat,
                                time_start_var,
                                time_end_var,
                                event_var,
                                variant_var,
                                covariates,
                                comparisons,
                                event_count_var = NULL,
                                output_file = NULL) {

  dat <- as.data.table(dat)
  if (is.null(event_count_var)) event_count_var <- event_var

  # --- Fit the full model across all variant levels present in dat ---
  rhs <- paste(c(variant_var, covariates), collapse = " + ")
  form <- as.formula(
    sprintf("Surv(%s, %s, %s) ~ %s", time_start_var, time_end_var, event_var, rhs)
  )

  fullfit <- coxph(form, data = dat)
  s <- summary(fullfit)

  coefs <- s$coefficients
  ci    <- s$conf.int

  variant_rows <- grep(paste0("^", variant_var), rownames(coefs))

  res <- data.table(
    Comparison = rownames(coefs)[variant_rows],
    Beta       = coefs[variant_rows, "coef"],
    SE         = coefs[variant_rows, "se(coef)"],
    HR         = coefs[variant_rows, "exp(coef)"],
    LB         = ci[variant_rows, "lower .95"],
    UB         = ci[variant_rows, "upper .95"],
    P          = coefs[variant_rows, "Pr(>|z|)"]
  )

  # --- Proportional hazards test for the variant term ---
  zph <- cox.zph(fullfit)
  ph_pval_variant <- zph$table[variant_var, "p"]
  res[, PH_pval_variant := ph_pval_variant]

  setcolorder(res, c("Comparison", "Beta", "SE", "HR", "LB", "UB", "P", "PH_pval_variant"))

  # --- Per-comparison descriptive stats (N at risk, N events, median follow-up) ---
  desc_list <- lapply(comparisons, function(levels_keep) {
    idx <- dat[[variant_var]] %in% levels_keep
    N_at_risk  <- sum(idx)
    N_events   <- sum(dat[[event_count_var]][idx], na.rm = TRUE)
    median_fu  <- median(dat[[time_end_var]][idx] - dat[[time_start_var]][idx], na.rm = TRUE)
    data.table(group_label = levels_keep[1],
               N_at_risk   = N_at_risk,
               N_events    = N_events,
               median_fu   = median_fu)
  })
  desc <- rbindlist(desc_list)

  # --- Match descriptive stats to the coefficient rows by exposure level name ---
  # Coefficient row names look like "<variant_var><level>", e.g. "variantPRS+"
  # so we match on which level name is contained in the row name.
  match_idx <- sapply(desc$group_label, function(lv) {
    which(sapply(res$Comparison, function(cn) grepl(lv, cn, fixed = TRUE)))
  })

  res[, N_at_risk  := desc$N_at_risk[match_idx]]
  res[, N_events   := desc$N_events[match_idx]]
  res[, median_fu  := desc$median_fu[match_idx]]

  res <- res[, c("Comparison", "N_at_risk", "N_events", "median_fu",
                 "Beta", "SE", "HR", "LB", "UB", "P", "PH_pval_variant")]

  if (!is.null(output_file)) {
    write.table(res, output_file, quote = FALSE, row.names = FALSE, sep = "\t")
  }

  return(res[])
}


###Data processing
dat=fread("../../Data/dat_adult.tab")
dat=dat[which(!is.na(dat$last_icd)),]
dat$variant=factor(dat$variant, levels=c("Not high risk","PRS+","FH+"))
dat$time_ascvd=dat$last_icd
dat$time_cad=dat$last_icd
dat$time_pad=dat$last_icd
dat$time_stroke=dat$last_icd
dat$time_ascvd[which(dat$ascvd==1)]=dat$age_ascvd[which(dat$ascvd==1)]
dat$time_cad[which(dat$cad==1)]=dat$age_cad[which(dat$cad==1)]
dat$time_pad[which(dat$pad==1)]=dat$age_pad[which(dat$pad==1)]
dat$time_stroke[which(dat$stroke==1)]=dat$age_stroke[which(dat$stroke==1)]

#Include only those with at least one year of follow-up time:
dat=dat[which(dat$range_icd>=1),]
print(dim(dat))

#Removing individuals with diagnosis on first ICD date (out of 672)
remove_ascvd=which(dat$age_ascvd==dat$first_icd)
remove_cad=which(dat$age_cad==dat$first_icd)
remove_pad=which(dat$age_pad==dat$first_icd)
remove_stroke=which(dat$age_stroke==dat$first_icd)

dat_ascvd=dat[-remove_ascvd,]
dat_cad=dat[-remove_cad,]
dat_pad=dat[-remove_pad,]
dat_stroke=dat[-remove_stroke,]



res_ascvd <- run_coxph_analysis(
  dat              = dat_ascvd,
  time_start_var   = "first_icd",
  time_end_var     = "time_ascvd",
  event_var        = "ascvd",
  variant_var      = "variant",
  covariates       = c("sex_at_birth_deid", "site"),
  comparisons      = list(c("PRS+", "Not high risk"), c("FH+", "Not high risk")),
  event_count_var  = "ascvd",   
  output_file      = "../../Results/ascvd_coxph.tab"
)


res_cad <- run_coxph_analysis(
  dat              = dat_cad,
  time_start_var   = "first_icd",
  time_end_var     = "time_cad",
  event_var        = "cad",
  variant_var      = "variant",
  covariates       = c("sex_at_birth_deid", "site"),
  comparisons      = list(c("PRS+", "Not high risk"), c("FH+", "Not high risk")),
  event_count_var  = "cad",   
  output_file      = "../../Results/cad_coxph.tab"
)


res_pad <- run_coxph_analysis(
  dat              = dat_pad,
  time_start_var   = "first_icd",
  time_end_var     = "time_pad",
  event_var        = "pad",
  variant_var      = "variant",
  covariates       = c("sex_at_birth_deid", "site"),
  comparisons      = list(c("PRS+", "Not high risk"), c("FH+", "Not high risk")),
  event_count_var  = "pad",   
  output_file      = "../../Results/pad_coxph.tab"
)


res_stroke <- run_coxph_analysis(
  dat              = dat_stroke,
  time_start_var   = "first_icd",
  time_end_var     = "time_stroke",
  event_var        = "stroke",
  variant_var      = "variant",
  covariates       = c("sex_at_birth_deid", "site"),
  comparisons      = list(c("PRS+", "Not high risk"), c("FH+", "Not high risk")),
  event_count_var  = "stroke",   
  output_file      = "../../Results/stroke_coxph.tab"
)

res_stroke$outcome=rep("stroke",2)
res_cad$outcome=rep("cad",2)
res_pad$outcome=rep("pad",2)
res_ascvd$outcome=rep("ascvd",2)
res=rbind(res_ascvd, res_cad, res_pad, res_stroke)

write.table(res, "../../Results/eiv_coxph.tab", quote=FALSE, row.names=FALSE, sep="\t")



