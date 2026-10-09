library(data.table)

dat=fread("EHR/PedsQC/peds_ldl.tab")
hc=fread("EHR/PedsQC/dat_hc.tab")
dat$in_ldl=rep(1, nrow(dat))

dat=merge(hc, dat[,c("record_id","ldlc","in_ldl")], all.x=TRUE, by="record_id")

dat$in_ldl[is.na(dat$in_ldl)]=0

write.table(dat, "peds_analytical.tab", quote=FALSE, row.names=FALSE, sep="\t")




