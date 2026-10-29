library(data.table)

dat=fread("../../Data/dat_adult.tab")

#Looking in pre-PREVENT risk group with LDL panel and family history data- in those not already on LLT (to our knowledge)
dat=dat[which(dat$age_deid<30),]
dat=dat[which(dat$llt==0 | is.na(dat$llt)),]
#dat=dat[which(!is.na(dat$ldl_most_recent)),]
summary(as.factor(dat$variant))

dat$hc=as.numeric(dat$ldl_max>=160)
dat$chd_union=pmax(dat$famhx_ascvd, dat$has_chd)

dat$no_trigger=as.numeric(dat$variant!="FH+" & (dat$hc==0 | is.na(dat$hc)) & (dat$chd_union==0 | is.na(dat$chd_union)))

undetected=dat[dat$no_trigger==1,]
aggregate(undetected$ldl_most_recent, by=list(undetected$variant), FUN=mean)

groups <- c("FH+", "PRS+", "Not high risk")

# helper: binary/0-1 variables --------------------------------------------
prop_stats <- function(var, g, data = dat) {
  x <- data[[var]][data$variant == g]
  n_total     <- length(x)                # total individuals in this group
  n_missing   <- sum(is.na(x))
  n_nonmiss   <- n_total - n_missing
  k           <- sum(x, na.rm = TRUE)      # count with outcome = 1

  data.frame(
    count            = k,
    n_total          = n_total,
    n_nonmissing     = n_nonmiss,
    n_missing        = n_missing,
    prop_nonmissing  = ifelse(n_nonmiss > 0, k / n_nonmiss, NA),
    prop_total       = ifelse(n_total > 0, k / n_total, NA)
  )
}

# helper: continuous variables (e.g., LDL) ---------------------------------
mean_stats <- function(var, g, data = undetected) {
  x <- data[[var]][data$variant == g]
  n_total   <- length(x)
  n_missing <- sum(is.na(x))
  n_nonmiss <- n_total - n_missing
  m         <- mean(x, na.rm = TRUE)

  data.frame(
    count            = NA,
    n_total          = n_total,
    n_nonmissing     = n_nonmiss,
    n_missing        = n_missing,
    prop_nonmissing  = m,     # this is a mean, not technically a "proportion"
    prop_total       = NA     # doesn't make sense to average missing as 0
  )
}

build_row <- function(metric, fun, data) {
  do.call(rbind, lapply(groups, function(g) {
    stats <- fun(metric, g, data)
    cbind(variant = g, metric = metric, stats)
  }))
}

# fixed FH row: kept exactly as-is, no missingness logic here
n_total <- sapply(groups, function(g) sum(dat$variant == g))
row1 <- data.frame(
  variant         = groups,
  metric          = "FH variant",
  count           = c(n_total[1], 0, 0),
  n_total         = n_total,
  n_nonmissing    = n_total,
  n_missing       = 0,
  prop_nonmissing = c(1, 0, 0),
  prop_total      = c(1, 0, 0)
)

res <- rbind(
  row1,
  build_row("hc", prop_stats, dat),
  build_row("chd_union", prop_stats, dat),
  build_row("no_trigger", prop_stats, dat),
  build_row("ldl_most_recent", mean_stats, undetected)
)


res$metric=rep(c("FH variant", "LDL-C ≥ 160 mg/dL", "Personal/family hx", "No trigger", "No trigger mean LDL-C"), each=3)
res=as.data.frame(res)
res$Variant=rep(c("FH+ (n=11)","PRS+ (n=37)","Not-high-risk (n=1,923)"),5)

write.table(res, "../../Results/res_risk.tab", quote=FALSE, row.names=FALSE, sep="\t")

write.table(undetected, "../../Plots/ResidualRisk/undetected.tab", quote=FALSE, row.names=FALSE, sep="\t") 




