############################################################
# SPY + XGBOOST PU LEARNING FOR STATIN IMPUTATION
############################################################

library(xgboost)
library(dplyr)
library(data.table)
library(SHAPforxgboost)
library(ggplot2)

set.seed(123)

dat=fread("../../Data/dat_adult.tab")

### Include only individuals with non-missing LDL data
dat=dat[!is.na(dat$ldl_most_recent),]

ldl=fread("../../Data/EHR/AdultQC/ldl_long.tab")

ldl_features <- ldl %>%
  arrange(record_id, AGE_AT_EVENT) %>%
  group_by(record_id) %>%
  summarise(
    overall_slope = if (n() > 1) (last(value) - first(value)) / (last(AGE_AT_EVENT) - first(AGE_AT_EVENT)) else NA_real_,
    min_interval_slope = if (n() > 1) min(diff(value) / diff(AGE_AT_EVENT), na.rm = TRUE) else NA_real_,
    max_percent_drop = if (n() > 1) max(-diff(value) / head(value, -1), na.rm = TRUE) else NA_real_,
    n_measurements = n(),
    .groups = "drop"
  )
ldl_features=as.data.frame(ldl_features)
ldl_features[c("overall_slope","min_interval_slope","max_percent_drop")] <-
  lapply(ldl_features[c("overall_slope","min_interval_slope","max_percent_drop")], 
         function(x) { q <- quantile(x, c(0.01,0.99), na.rm=TRUE); pmin(pmax(x, q[1]), q[2]) })

healthdat=dat[,c("record_id","age_deid","sex_at_birth_deid","ldl_most_recent","ldl_max","hcdx","llt","stroke","cad","pad","t2d")]
healthdat=merge(healthdat, ldl_features[,-5], by="record_id")
healthdat$llt[is.na(healthdat$llt)]=0
rownames(healthdat)=healthdat$record_id
d=healthdat[,-c("record_id")]
d$sex_at_birth_deid=as.numeric(d$sex_at_birth_deid=="Male")

############################################################
# 1. CREATE SPY LABELS
############################################################

spy_frac <- 0.10

pos_idx <- which(d$llt == 1)
unlab_idx <- which(d$llt == 0)

n_spy <- ceiling(length(pos_idx) * spy_frac)

spy_idx <- sample(pos_idx, n_spy)

# create temporary labels for spy training
d$spy_label <- d$llt
d$spy_label[spy_idx] <- 0

############################################################
# 2. PREPARE DESIGN MATRIX
############################################################

X <- as.matrix(d %>% select(-llt, -spy_label))

y_spy <- d$spy_label

dtrain_spy <- xgb.DMatrix(
  data = X,
  label = y_spy,
  missing = NA_real_
)

############################################################
# 3. TRAIN SPY XGBOOST MODEL
############################################################

spy_params <- list(
  objective = "binary:logistic",
  eval_metric = "auc",
  max_depth = 4,
  eta = 0.05,
  subsample = 0.8,
  colsample_bytree = 0.8
)

spy_model <- xgb.train(
  params = spy_params,
  data = dtrain_spy,
  nrounds = 200,
  verbose = 1
)

############################################################
# 4. IDENTIFY RELIABLE NEGATIVES
############################################################

# scores for spies
spy_scores <- predict(spy_model, X[spy_idx, ])

# conservative threshold:
# RN must score lower than almost all spies
rn_threshold <- quantile(spy_scores, 0.01)

# score all unlabeled
unlab_scores <- predict(spy_model, X[unlab_idx, ])

RN_idx <- unlab_idx[unlab_scores < rn_threshold]

cat("Reliable negatives identified:", length(RN_idx), "\n")


############################################################
# 6. FINAL PU CLASSIFIER
############################################################

# THIS is the main model used for final statin prediction

final_y <- rep(NA, nrow(d))
final_y[pos_idx] <- 1
final_y[RN_idx] <- 0

train_idx <- which(!is.na(final_y))

dtrain_final <- xgb.DMatrix(
  X[train_idx, ],
  label = final_y[train_idx],
  missing = NA
)

final_model <- xgb.train(
  params = spy_params,
  data = dtrain_final,
  nrounds = 300
)

############################################################
# 7. PREDICT STATIN PROBABILITY FOR EVERYONE
############################################################

d$statin_prob <- predict(final_model, X)

############################################################
# 9. DIRECTIONAL SHAP SUMMARY PLOT
############################################################

shap_values <- shap.values(xgb_model = final_model, X_train = X)

shap_long <- shap.prep(
  shap_contrib = shap_values$shap_score,
  X_train = X
)

write.table(shap_long, "../../Results/shap_values.tab", quote=FALSE, row.names=FALSE, sep="\t")


shap.plot.summary(shap_long)

#########
#10. Understanding probabilities
############

dat=cbind(dat, d[,c("spy_label","statin_prob")])
dat=merge(dat, ldl_features, by="record_id")

ggplot(dat, aes(x=statin_prob))+geom_histogram()

ggplot(dat, aes(x=ldl_max, y=statin_prob, color=age_deid))+geom_point(size=1)+theme_bw()+ylab("Predicted probability")+ggtitle("LDL_max")+geom_smooth(color="red", se=FALSE)

ggplot(dat, aes(x=age_deid, y=statin_prob, color=as.factor(hcdx)))+geom_point(size=1)+theme_bw()+ylab("Predicted probability")+xlab("Age (yrs)")

ggplot(dat[which(!is.na(meds_lower_cholesterol_2))], aes(x=age_deid, y=statin_prob, color=as.factor(meds_lower_cholesterol_2)))+geom_point(size=1)+theme_bw()+ylab("Predicted probability")+ggtitle("Self-report statin rx")+xlab("Age (yrs)")

ggplot(dat, aes(x=statin_prob))+geom_histogram(binwidth=0.01)+xlab("Predicted probability")

dat$llt_pred=rep("Unknown", nrow(dat))
dat$llt_pred[which(dat$statin_prob>0.9)]="LLT user"
dat$llt_pred[which(dat$statin_prob<0.1)]="LLT non-user"

dat$untreated=rep(0, nrow(dat))
dat$untreated[which(dat$llt_pred=="LLT non-user")]=1
dat$untreated[which(dat$llt==1)]=0

untreated=dat[which(dat$untreated==1),]
write.table(untreated, "pred_untreated.tab", quote=FALSE, row.names=FALSE, sep="\t")
write.table(dat, "impute_results.tab", quote=FALSE, row.names=FALSE, sep="\t")

###########################################################
# 11. LDL analysis in high confidence untreated subset 
###########################################################

library(rms)
untreated$variant=factor(untreated$variant, levels=c("Not high risk","PRS+","FH+"))

fit=lm(ldl_most_recent~variant+sex_at_birth_deid+rcs(age_deid,4)+site, data=untreated)
coefs=as.data.frame(summary(fit)$coef)

res=as.data.table(matrix(rep(0, 6*2), ncol=6,nrow=2))
colnames(res)=c("Comparison","Beta","SE","LB","UB","P")
res$Comparison=c("PRS+ vs. NHR","FH+ vs. NHR")
res$N_FH=rep(as.numeric(summary(as.factor(untreated$variant))[3]),2)
res$N_PRS=rep(as.numeric(summary(as.factor(untreated$variant))[2]),2)
res$N_NHR=rep(as.numeric(summary(as.factor(untreated$variant))[1]),2)
res$Beta=coefs$Estimate[2:3]
res$SE=coefs$`Std. Error`[2:3]
res$P=coefs$`Pr(>|t|)`[2:3]
res$LB=res$Beta-1.96*res$SE
res$UB=res$Beta+1.96*res$SE

write.table(res, "../../Results/ldl_imputed_untreated.tab", quote=FALSE, row.names=FALSE, sep="\t")

ci_mean <- function(x) {
  x <- x[!is.na(x)]
  n <- length(x)
  if (n < 2) return(c(n=n, mean=mean(x), lower=NA, upper=NA))
  m <- mean(x); se <- sd(x)/sqrt(n); t <- qt(.975, n-1)
  c(n=n, mean=m, lower=m-t*se, upper=m+t*se)
}

agg=aggregate(untreated$ldl_most_recent, by=list(untreated$variant), FUN=ci_mean)
agg=data.frame(agg[1], agg$x)
colnames(agg)=c("variant","n_ldl","mean_ldl","lb_ldl","ub_ldl")

write.table(agg, "../../Plots/Impute/untreated_agg.tab", quote=FALSE, row.names=FALSE, sep="\t")






