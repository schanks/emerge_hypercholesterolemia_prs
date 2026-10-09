library(data.table)

uniqct=function(x){
return(length(unique(x)))
}

dat=fread("../../../CDP/dat_gira.tab")

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

d=rbind(cuimc_icd, cchmc_icd, sinai_icd, nu_icd, uab_icd, uw_icd, vumc_icd, chop_icd, mayo_icd, mgb_icd)
d$AGE_AT_EVENT=as.numeric(d$AGE_AT_EVENT)

d=d[which(is.element(d$EMERGE_ID, dat$record_id)),]

## Remove all dates after GIRA generation
d=d[d$AGE_AT_EVENT < dat$age_gira_generated[match(d$EMERGE_ID, dat$record_id)], ]

dat$in_icd=as.numeric(is.element(dat$record_id, d$EMERGE_ID))

## Summary statistics of data availability
uniqdays=unique(d[,c("EMERGE_ID","AGE_AT_EVENT")])
d_agg=aggregate(uniqdays$AGE_AT_EVENT, by=list(uniqdays$EMERGE_ID), FUN=length)
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


## Manual adjustment of non-standard codes: duplicate ICD entries
hc$ICD_CODE[which(hc$ICD_CODE=="E11.69, E78.5")]="E78.5"
hc$ICD_CODE[which(hc$ICD_CODE=="E11.69, E78.00")]="E78.00"
hc$ICD_CODE[which(hc$ICD_CODE=="D68.69, E78.41")]="E78.41"
rowdup=hc[which(hc$ICD_CODE=="E78.6, E78.1"),]
rowdup=rbind(rowdup, rowdup)
rowdup$ICD_CODE=c("E78.6","E78.1","E78.6","E78.1")
hc=hc[which(hc$ICD_CODE!="E78.6, E78.1"),]
hc=rbind(hc, rowdup)

## Manual adjustment of Sinai 3 digit codes to 2 digit codes- note low confidence in correct dx for code "E78.001" - could be E78.1 or E78.01
hc$ICD_CODE=sub("(\\.[0-9])[0-9]([0-9])$", ".\\2", hc$ICD_CODE)

colnames(hc)[1]="record_id"
write.table(hc, "../../AdultQC/hc_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

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


write.table(dat, "../../AdultQC/dat_hc.tab", quote=FALSE, row.names=FALSE, sep="\t")

hcdat=dat[which(dat$hcdx==1),c("record_id","icd_count","first_icd","last_icd","range_icd","age_hc")]

write.table(hcdat, "../../AdultQC/hc_short.tab", quote=FALSE, row.names=FALSE, sep="\t")



