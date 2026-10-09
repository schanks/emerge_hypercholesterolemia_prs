library(data.table)

##########Need to fix family history and personal CHD history variables

dat=fread("EHR/AdultQC/dat_ldl.tab")

hc=fread("EHR/AdultQC/dat_hc.tab")
llt=fread("EHR/AdultQC/dat_llt.tab")
hdl=fread("EHR/AdultQC/dat_hdl.tab")
ckd=fread("EHR/AdultQC/dat_ckd.tab")
t2d=fread("EHR/AdultQC/dat_t2d.tab")
nonhdl=fread("EHR/AdultQC/dat_nonhdl.tab")
totchol=fread("EHR/AdultQC/dat_totchol.tab")
trigly=fread("EHR/AdultQC/dat_trigly.tab")
famhx=fread("MeTree/dat_famhx.tab")
bmi=fread("CDP/bmi.csv")
chd=fread("CDP/chd.csv")
colnames(chd)=c("record_id","has_chd")
spinks=fread("EHR/AdultQC/spinks_short.tab")

dat=merge(dat, bmi[,c("record_id","bmi")], by="record_id")
dat$bmi_cat=cut(dat$bmi, breaks=c(0,18.5,24.5,29.5,100), labels=c("<18.5","18.5-24","25-29","≥30"))

dat=merge(dat, totchol[,c("record_id","totchol_most_recent")], by="record_id")
dat=merge(dat, nonhdl[,c("record_id","nonhdl_most_recent")], by="record_id")
dat=merge(dat, hdl[,c("record_id","hdl_most_recent")], by="record_id")
dat=merge(dat, trigly[,c("record_id","trigly_most_recent")], by="record_id")

dat=merge(dat, hc[,c("in_icd","hcdx","icd_count","first_icd","last_icd","range_icd","age_hc","hc_ct","record_id")], by="record_id")
dat=merge(dat, ckd[,c("ckd","record_id")], by="record_id")
dat=merge(dat, t2d[,c("t2d","record_id")], by="record_id")

dat$ckd[dat$in_icd==0]=NA
dat$t2d[dat$in_icd==0]=NA

dat=merge(dat, llt[,c("record_id","in_drug","c10","llt","drug_count","first_drug","last_drug","range_drug","age_llt")], by="record_id")

dat=merge(dat, famhx[,c("record_id","in_famhx","famhx_hc","famhx_cad","famhx_ascvd")], by="record_id")
dat=merge(dat, chd, by="record_id", all.x=TRUE)

dat=merge(dat, spinks, by="record_id", all.x=TRUE)
dat$ascvd=as.numeric(is.element(dat$record_id, spinks$record_id))
dat$ascvd[which(dat$in_icd==0)]=NA
dat$stroke[dat$ascvd==0]=0
dat$cad[dat$ascvd==0]=0
dat$pad[dat$ascvd==0]=0

write.table(dat, "dat_adult.tab", quote=FALSE, row.names=FALSE, sep="\t")








