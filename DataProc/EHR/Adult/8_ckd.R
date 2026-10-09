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

#ICD codes for CKD
# Captures all observations even when not entered in standard format (accompanying other codes, extra decimal)
ckd10=d[grep("N18", d$ICD_CODE),]
ckd9=d[grepl("^585(\\.|$)",d$ICD_CODE),]

ckd=rbind(ckd9, ckd10)
ckd=ckd[,-c("ROW_ID")]
ckd=unique(ckd)


## Manual adjustment of non-standard codes: duplicate ICD entries
ckd$ICD_CODE[which(ckd$ICD_CODE=="E10.22, N18.30")]="N18.30"
ckd$ICD_CODE[which(ckd$ICD_CODE=="I12.9, N18.2")]="N18.2"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.5, D63.1")]="N18.5"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.6, Z99.2")]="N18.6"
ckd$ICD_CODE[which(ckd$ICD_CODE=="I12.9, N18.30")]="N18.30"
ckd$ICD_CODE[which(ckd$ICD_CODE=="I12.9, N18.32")]="N18.32"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E11.22, N18.30")]="N18.30"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E08.22, N18.2")]="N18.2"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.9, D63.1")]="N18.9"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E10.22, N18.2")]="N18.2"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.9, Z86.2")]="N18.9"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.4, D63.1")]="N18.4"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.32, D63.1")]="N18.32"
ckd$ICD_CODE[which(ckd$ICD_CODE=="N18.30, D63.1")]="N18.30"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E11.22, I12.9, N18.30")]="N18.30"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E11.22, N18.1")]="N18.1"
ckd$ICD_CODE[which(ckd$ICD_CODE=="I13.0, N18.1")]="N18.1"
ckd$ICD_CODE[which(ckd$ICD_CODE=="E11.22, N18.32")]="N18.32"
ckd$ICD_CODE[which(ckd$ICD_CODE=="I12.9, N18.4")]="N18.4"

rowdup1=ckd[which(ckd$ICD_CODE=="N17.9, N18.9"),]
rowdup1=rbind(rowdup1, rowdup1)
rowdup1$ICD_CODE=c("N17.9","N17.9","N18.9","N18.9")
ckd=ckd[which(ckd$ICD_CODE!="N17.9, N18.9"),]
ckd=rbind(ckd, rowdup1)

rowdup2=ckd[which(ckd$ICD_CODE=="N17.9, N18.5"),]
rowdup2=rbind(rowdup2, rowdup2)
rowdup2$ICD_CODE=c("N17.9","N18.5")
ckd=ckd[which(ckd$ICD_CODE!="N17.9, N18.5"),]
ckd=rbind(ckd, rowdup2)




## Manual adjustment of Sinai 3 digit codes to 2 digit codes- ambiguous codes possible
ckd$ICD_CODE=sub("(\\.[0-9])[0-9]([0-9])$", ".\\2", ckd$ICD_CODE)

colnames(ckd)[1]="record_id"
write.table(ckd, "../../AdultQC/ckd_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Short form dataset:
dat$ckd=as.numeric(is.element(dat$record_id, ckd$record_id))

## Extract earliest age of hc dx & number of hc records pp:
ages=aggregate(ckd$AGE_AT_EVENT, by=list(ckd$record_id), FUN=min)
colnames(ages)=c("record_id","age_ckd")
ages$ckd_ct=aggregate(ckd$AGE_AT_EVENT, by=list(ckd$record_id), FUN=uniqct)$x
dat=merge(dat, ages, by="record_id", all.x=TRUE)

write.table(dat, "../../AdultQC/dat_ckd.tab", quote=FALSE, row.names=FALSE, sep="\t")




