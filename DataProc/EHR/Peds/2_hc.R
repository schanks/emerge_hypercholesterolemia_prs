library(data.table)
library(ggplot2)
library(gridExtra)

colorscheme=c("#2A7F62","#344966","#B4CDED","#F06543","#BFCC94")


uniqct=function(x){
return(length(unique(x)))
}

dat=fread("../../../CDP/peds_gira.tab")

cuimc_icd=fread("../../Anvil/ICD/CUIMC_ICD_20250602.csv")
cchmc_icd=fread("../../Anvil/ICD/CCHMC_ICD_20251008_5.csv")
sinai_icd=fread("../../Anvil/ICD/MountSinai_ICD_20251006.csv")
nu_icd=fread("../../Anvil/ICD/NU_outcomes_icds_2025-10-08.csv")
uab_icd=fread("../../Anvil/ICD/UAB_ICD_20251007.csv")
uw_icd=fread("../../Anvil/ICD/UW_ICD_20250814.csv")
uw_icd=uw_icd[,-1]
vumc_icd=fread("../../Anvil/ICD/VUMC_ICD_20250527.csv")
chop_icd=fread("../../Anvil/ICD/CHOP_icd_20250606.csv")
colnames(chop_icd)[4]="ICD_FLAG"
mayo_icd=fread("../../Anvil/ICD/MAYO_ICD_20251023.csv")
colnames(mayo_icd)=c("EMERGE_ID","AGE_AT_EVENT","ICD_CODE","ICD_FLAG","ROW_ID")
mgb_icd=fread("../../Anvil/ICD/MGB_ICD_20251027.csv")

in_icd=unique(c(cuimc_icd$EMERGE_ID, cchmc_icd$EMERGE_ID, sinai_icd$EMERGE_ID, nu_icd$EMERGE_ID, uab_icd$EMERGE_ID, uw_icd$EMERGE_ID, vumc_icd$EMERGE_ID, chop_icd$EMERGE_ID, mayo_icd$EMERGE_ID, mgb_icd$EMERGE_ID))

dat$in_icd=as.numeric(is.element(dat$record_id, in_icd))

yesdat=dat[which(dat$in_icd==1),]

d=rbind(cuimc_icd, cchmc_icd, sinai_icd, nu_icd, uab_icd, uw_icd, vumc_icd, chop_icd, mayo_icd, mgb_icd)
d$AGE_AT_EVENT=as.numeric(d$AGE_AT_EVENT)

d=d[which(is.element(d$EMERGE_ID, dat$record_id)),]

## Remove all dates after GIRA generation
d=d[d$AGE_AT_EVENT < dat$age_gira_generated[match(d$EMERGE_ID, dat$record_id)], ]

## Summary statistics of data availability
d_agg=aggregate(d$ICD_CODE, by=list(d$EMERGE_ID), FUN=length)
d_agg$first_icd=aggregate(d$AGE_AT_EVENT, by=list(d$EMERGE_ID), FUN=min)$x
d_agg$last_icd=aggregate(d$AGE_AT_EVENT, by=list(d$EMERGE_ID), FUN=max)$x
colnames(d_agg)[c(1,2)]=c("EMERGE_ID","count")
d_agg$range_icd=d_agg$last_icd-d_agg$first_icd

#ICD codes for hypercholesterolemia
# Captures all observations even when not entered in standard format (accompanying other codes, extra decimal)
hyper10=d[grep("E78", d$ICD_CODE),]
hyper9=d[grepl("^272(\\.|$)",d$ICD_CODE),]

hc=rbind(hyper9, hyper10)
hc=hc[,-c("ROW_ID")]
hc=unique(hc)

colnames(hc)[1]="record_id"
write.table(hc, "../../PedsQC/hc_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Short form dataset:
dat$hcdx=as.numeric(is.element(dat$record_id, hc$record_id))


## Extract earliest age of hc dx & number of hc records pp:
ages=aggregate(hc$AGE_AT_EVENT, by=list(hc$record_id), FUN=min)
colnames(ages)=c("record_id","age_hc")
ages$hc_ct=aggregate(hc$AGE_AT_EVENT, by=list(hc$record_id), FUN=uniqct)$x
colnames(d_agg)[1:2]=c("record_id","icd_count")

dat=merge(dat, d_agg, by="record_id", all.x=TRUE)
dat=merge(dat, ages, by="record_id", all.x=TRUE)
dat$hcdx[which(dat$in_icd==0)]=NA


write.table(dat, "../../PedsQC/dat_hc.tab", quote=FALSE, row.names=FALSE, sep="\t")

hcdat=dat[which(dat$hcdx==1),c("record_id","icd_count","first_icd","last_icd","range_icd","age_hc")]

write.table(hcdat, "../../PedsQC/hc_short.tab", quote=FALSE, row.names=FALSE, sep="\t")






