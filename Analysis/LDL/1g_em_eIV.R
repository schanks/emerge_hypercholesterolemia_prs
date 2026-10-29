library(data.table)
library(car)
library(rms)

dat=fread("../../Data/dat_adult.tab")

#Prep data
ldldat=dat[which(!is.na(dat$ldl_most_recent)),]
ldldat$variant=factor(ldldat$variant, levels=c("Not high risk","PRS+","FH+"))
levels(ldldat$variant)=paste0("level",seq_along(levels(ldldat$variant)))
ldldat$sex_at_birth_deid=as.factor(ldldat$sex_at_birth_deid)
ldldat$t2d=as.factor(ldldat$t2d)
ldldat$ckd=as.factor(ldldat$ckd)
ldldat$bmi_cat[ldldat$bmi_cat=="<18.5"]="<25"
ldldat$bmi_cat[ldldat$bmi_cat=="18.5-24"]="<25"
ldldat$bmi_cat=as.factor(ldldat$bmi_cat)
ldldat$age_cat=as.factor(ldldat$age_cat)
ldldat$site=as.factor(ldldat$site)
ldldat$race4=as.factor(ldldat$race4)

ldldat_untreated=ldldat[which(ldldat$llt==0),]


#Helper function
run_effect_mod <- function(data, outcome_var, modifier_var, modifier_levels, modifier_labels, is_sex_modifier=FALSE){
	if (is_sex_modifier){
		covariates="rcs(age_deid,4)+site"
	} else {
		covariates="sex_at_birth_deid+rcs(age_deid,4)+site"
	}

	fmla=as.formula(paste0(outcome_var," ~ variant * ",modifier_var," + ",covariates))

	fit=lm(fmla, data=data)
	coefs=summary(fit)$coefficients
	vcov_mat=vcov(fit)

	n_tab <- table(data[[modifier_var]], data$variant)

	int_terms_l2 <- grep(paste0("variantlevel2:", modifier_var), names(coef(fit)), value=TRUE)
	int_terms_l3 <- grep(paste0("variantlevel3:", modifier_var), names(coef(fit)), value=TRUE)

	pval_list=list()

	if (length(int_terms_l2) > 0) {
		lh2 <- car::linearHypothesis(fit, int_terms_l2)
		p2 <- lh2[["Pr(>F)"]][2]
		cat("Interaction F-test p-value for", modifier_var, "(PRS+ vs. NHR):", p2, "\n")
		pval_list[[length(pval_list)+1]]=data.frame(
			Comparison="PRS+ vs. NHR",
			P_interaction=p2,
			stringsAsFactors=FALSE
		)
	}
	if (length(int_terms_l3) > 0) {
		lh3 <- car::linearHypothesis(fit, int_terms_l3)
		p3 <- lh3[["Pr(>F)"]][2]
		cat("Interaction F-test p-value for", modifier_var, "(FH+ vs. NHR):", p3, "\n")
		pval_list[[length(pval_list)+1]]=data.frame(
			Comparison="FH+ vs. NHR",
			P_interaction=p3,
			stringsAsFactors=FALSE
		)
	}
	pval_tab=if(length(pval_list)>0) do.call(rbind, pval_list) else NULL

	results=list()

	for (i in seq_along(modifier_levels)) {
		lvl=modifier_levels[i]
		lbl=modifier_labels[i]
		if (i==1){
			for (comp in c("variantlevel2","variantlevel3")) {
				comp_label=ifelse(comp=="variantlevel2","PRS+ vs. NHR", "FH+ vs. NHR")
				if (comp %in% rownames(coefs)) {
					results[[length(results)+1]]=data.frame(
						Modifier_Level=lbl,
						Comparison=comp_label,
						Beta=coefs[comp, "Estimate"],
						SE=coefs[comp,"Std. Error"],
						P=coefs[comp,"Pr(>|t|)"],
						N_NHR=n_tab[lvl, "level1"],
						N_PRS=n_tab[lvl, "level2"],
						N_FH=if ("level3" %in% colnames(n_tab)) n_tab[lvl, "level3"] else NA_integer_,
						stringsAsFactors=FALSE
					)
				}
			}
		} else {
			int2=paste0("variantlevel2:",modifier_var,lvl)
			int3=paste0("variantlevel3:",modifier_var,lvl)
			for (comp_info in list(
				list(main="variantlevel2",int=int2,label="PRS+ vs. NHR"),
				list(main="variantlevel3",int=int3,label="FH+ vs. NHR")
			)) {
				main_coef=coefs[comp_info$main,]
				int_coef=coefs[comp_info$int,]
				beta_combined=main_coef["Estimate"]+int_coef["Estimate"]
				mn=comp_info$main
				itn=comp_info$int
				se_combined=sqrt(vcov_mat[mn,mn]+vcov_mat[itn, itn]+2*vcov_mat[mn,itn])
				p_combined=2 * pt(-abs(beta_combined / se_combined), df = fit$df.residual)

				results[[length(results)+1]]=data.frame(
					Modifier_Level=lbl,
					Comparison=comp_info$label,
					Beta=beta_combined,
					SE=se_combined,
					P=p_combined,
					N_NHR=n_tab[lvl, "level1"],
					N_PRS=n_tab[lvl, "level2"],
					N_FH=if ("level3" %in% colnames(n_tab)) n_tab[lvl, "level3"] else NA_integer_,
					stringsAsFactors=FALSE
				)
			}
		}
	}
	results_tab=do.call(rbind, results)
	return(list(results=results_tab, pvals=pval_tab))
}

#---------------------------------------------------------------
# Wrapper that loops over all 5 modifiers for a given dataset/outcome
# and stacks the results + interaction p-value tables together
#---------------------------------------------------------------
run_all_modifiers <- function(data, outcome_var){

	sex_lvls=levels(as.factor(data$sex_at_birth_deid))
	race_lvls=levels(as.factor(data$race4))
	t2d_lvls=levels(as.factor(data$t2d))
	ckd_lvls=levels(as.factor(data$ckd))
	bmi_lvls=levels(as.factor(data$bmi_cat))

	res_sex=run_effect_mod(data, outcome_var, "sex_at_birth_deid", modifier_levels=sex_lvls, modifier_labels=sex_lvls, is_sex_modifier=TRUE)
	res_race=run_effect_mod(data, outcome_var, "race4", modifier_levels=race_lvls, modifier_labels=race_lvls)
	res_t2d=run_effect_mod(data, outcome_var, "t2d", modifier_levels=t2d_lvls, modifier_labels=c("T2D-","T2D+"))
	res_ckd=run_effect_mod(data, outcome_var, "ckd", modifier_levels=ckd_lvls, modifier_labels=c("CKD-","CKD+"))
	res_bmi=run_effect_mod(data, outcome_var, "bmi_cat", modifier_levels=bmi_lvls, modifier_labels=bmi_lvls)

	all_res=list(
		list(name="Sex", res=res_sex),
		list(name="Race/Ethnicity", res=res_race),
		list(name="T2D", res=res_t2d),
		list(name="CKD", res=res_ckd),
		list(name="BMI", res=res_bmi)
	)

	results_list=list()
	pvals_list=list()

	for (item in all_res){
		tbl=item$res$results
		tbl$Modifier=rep(item$name, nrow(tbl))
		results_list[[length(results_list)+1]]=tbl

		if (!is.null(item$res$pvals)){
			pv=item$res$pvals
			pv$Modifier=rep(item$name, nrow(pv))
			pvals_list[[length(pvals_list)+1]]=pv
		}
	}

	results_tab=do.call(rbind, results_list)
	rownames(results_tab)=c()

	results_tab$LB=results_tab$Beta-1.96*results_tab$SE
	results_tab$UB=results_tab$Beta+1.96*results_tab$SE
	results_tab=results_tab[,c("Modifier","Modifier_Level","N_FH","N_PRS","N_NHR","Comparison","Beta","SE","LB","UB","P")]

	pvals_tab=do.call(rbind, pvals_list)
	rownames(pvals_tab)=c()
	pvals_tab=pvals_tab[,c("Modifier","Comparison","P_interaction")]

	return(list(results=results_tab, pvals=pvals_tab))
}

#---------------------------------------------------------------
# Run for original (as-measured) LDL
#---------------------------------------------------------------
out_ldl=run_all_modifiers(ldldat, "ldl_most_recent")

write.table(out_ldl$results, "../../Results/ldl_em.tab", quote=FALSE, row.names=FALSE, sep="\t")
write.table(out_ldl$pvals, "../../Results/ldl_em_interaction_pvalues.tab", quote=FALSE, row.names=FALSE, sep="\t")

#---------------------------------------------------------------
# Run for untreated LDL
#---------------------------------------------------------------
out_ldl_untreated=run_all_modifiers(ldldat_untreated, "ldl_most_recent")

write.table(out_ldl_untreated$results, "../../Results/ldl_untreated_em.tab", quote=FALSE, row.names=FALSE, sep="\t")
write.table(out_ldl_untreated$pvals, "../../Results/ldl_untreated_em_interaction_pvalues.tab", quote=FALSE, row.names=FALSE, sep="\t")
