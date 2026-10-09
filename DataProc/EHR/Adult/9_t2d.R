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

#ICD codes for T2D
# Captures all observations even when not entered in standard format (accompanying other codes, extra decimal)
t2d10=d[grep("E11", d$ICD_CODE),]
t2d9=d[grepl("^250(\\.|$)",d$ICD_CODE),]

t2d=rbind(t2d9, t2d10)
t2d=t2d[,-c("ROW_ID")]
t2d=unique(t2d)


## Manual adjustment of non-standard codes: duplicate ICD entries
library(stringr)
t2d$ICD_CODE <- sapply(strsplit(t2d$ICD_CODE, ",\\s*"), function(x) {
  e11 <- x[grepl("^E11", x)]
  if (length(x) > 1 && length(e11) == 1) e11 else paste(x, collapse = ", ")
})

rowdup1=t2d[which(t2d$ICD_CODE=="E11.29, E11.65"),]
rowdup1=rbind(rowdup1, rowdup1)
rowdup1$ICD_CODE=c(rep("E11.29",8), rep("E11.65",8))
t2d=t2d[which(t2d$ICD_CODE!="E11.29, E11.65"),]
t2d=rbind(t2d, rowdup1)

rowdup2=t2d[which(t2d$ICD_CODE=="E11.8, E11.65"),]
rowdup2=rbind(rowdup2, rowdup2)
rowdup2$ICD_CODE=c("E11.8","E11.65")
t2d=t2d[which(t2d$ICD_CODE!="E11.8, E11.65"),]
t2d=rbind(t2d, rowdup2)

rowdup3=t2d[which(t2d$ICD_CODE=="E11.69, E66.9, E11.59, I15.2"),]
rowdup3=rbind(rowdup3, rowdup3)
rowdup3$ICD_CODE=c("E11.69","E11.59")
t2d=t2d[which(t2d$ICD_CODE!="E11.69, E66.9, E11.59, I15.2"),]
t2d=rbind(t2d, rowdup3)

## Manual adjustment of Sinai 3 digit codes to 2 digit codes- ambiguous codes possible
t2d$ICD_CODE=sub("(\\.[0-9])[0-9]([0-9])$", ".\\2", t2d$ICD_CODE)

colnames(t2d)[1]="record_id"
write.table(t2d, "../../AdultQC/t2d_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Short form dataset:
dat$t2d=as.numeric(is.element(dat$record_id, t2d$record_id))

## Extract earliest age of T2D dx & number of hc records pp:
ages=aggregate(t2d$AGE_AT_EVENT, by=list(t2d$record_id), FUN=min)
colnames(ages)=c("record_id","age_t2d")
ages$t2d_ct=aggregate(t2d$AGE_AT_EVENT, by=list(t2d$record_id), FUN=uniqct)$x
dat=merge(dat, ages, by="record_id", all.x=TRUE)

write.table(dat, "../../AdultQC/dat_t2d.tab", quote=FALSE, row.names=FALSE, sep="\t")

t2ddat=dat[which(dat$t2d==1),c("record_id","age_t2d")]

write.table(t2ddat, "../../AdultQC/t2d_short.tab", quote=FALSE, row.names=FALSE, sep="\t")






