#################################################
## Data processing of LDL LOINC Codes          ##
#################################################

library(data.table)
library(ggplot2)
colorscheme=c("#2A7F62","#344966","#B4CDED","#F06543","#BFCC94")


uniqct=function(x){
return(length(unique(x)))
}

dat=fread("../../../CDP/dat_gira.tab")

##Individual site datasets

cuimc=fread("../../Anvil/Measurement/CUIMC_Measurement_20250602.csv")
cchmc=fread("../../Anvil/Measurement/CCHMC_Measurement_20250801.csv")
sinai=fread("../../Anvil/Measurement/MountSinai_Measurement_20250828.csv")
nu=fread("../../Anvil/Measurement/NU_Measurement_20250819.csv")
uab=fread("../../Anvil/Measurement/UAB_MEASUREMENTS_20250707.csv")
uw=fread("../../Anvil/Measurement/UW_MEASUREMENT_20250916.csv")
vumc=fread("../../Anvil/Measurement/VUMC_Measurement_20250527.csv")
mayo=fread("../../Anvil/Measurement/MAYO_MEASUREMENT_20250725.csv")
colnames(mayo)=c("EMERGE_ID","AGE_AT_EVENT","MEASUREMENT_CONCEPT_ID","VALUE_AS_NUMBER","VALUE_AS_TEXT","RANGE_LOW","RANGE_HIGH","RANGE_FLAG","UNIT_CONCEPT_ID","UNIT_CONCEPT_AS_TEXT")
mayo$ROW_ID=rep(NA, nrow(mayo))
mgb=fread("../../Anvil/Measurement/MGB_Measurement_20251027.csv")
mgb$RANGE_LOW=rep(NA, nrow(mgb))
mgb$RANGE_HIGH=rep(NA, nrow(mgb))
mgb$RANGE_FLAG=rep(NA, nrow(mgb))
mgb$UNIT_CONCEPT_ID=rep(NA, nrow(mgb))

##Combine and restrict to analytical sample

d=rbind(cuimc, cchmc, sinai, nu, uab, uw, vumc, mayo, mgb)
d$AGE_AT_EVENT=as.numeric(d$AGE_AT_EVENT)
d=d[which(is.element(d$EMERGE_ID, dat$record_id)),]


##Restrict to non-hdl cholesterol codes
nonhdl=d[which(is.element(d$MEASUREMENT_CONCEPT_ID, c("3044491"))),]

##Restrict to total cholesterol codes
totchol=d[which(is.element(d$MEASUREMENT_CONCEPT_ID, c("3027114"))),]

##Restrict to hdl cholesterol codes
hdl=d[which(is.element(d$MEASUREMENT_CONCEPT_ID, c("3007070","3005984"))),]

## Remove all dates after GIRA generation
nonhdl=nonhdl[nonhdl$AGE_AT_EVENT < dat$age_gira_generated[match(nonhdl$EMERGE_ID, dat$record_id)], ]
totchol=totchol[totchol$AGE_AT_EVENT < dat$age_gira_generated[match(totchol$EMERGE_ID, dat$record_id)], ]
hdl=hdl[hdl$AGE_AT_EVENT < dat$age_gira_generated[match(hdl$EMERGE_ID, dat$record_id)], ]

## Remove all pediatric panels
nonhdl=nonhdl[which(nonhdl$AGE_AT_EVENT>18),] 
totchol=totchol[which(totchol$AGE_AT_EVENT>18),]
hdl=hdl[which(hdl$AGE_AT_EVENT>18),]

## Remove missing values
nonhdl=nonhdl[which(!is.na(nonhdl$VALUE_AS_NUMBER)),]
totchol=totchol[which(!is.na(totchol$VALUE_AS_NUMBER)),]
hdl=hdl[which(!is.na(hdl$VALUE_AS_NUMBER)),]

## No nonstandard units observed


## Calculate non HDL from totchol and hdl
## Collapse values for a given person that are taken on the same day (with mean)
hdl2=aggregate(hdl$VALUE_AS_NUMBER, by=list(hdl$EMERGE_ID, hdl$AGE_AT_EVENT), FUN=mean)
colnames(hdl2)=c("record_id","AGE_AT_EVENT","value")
totchol2=aggregate(totchol$VALUE_AS_NUMBER, by=list(totchol$EMERGE_ID, totchol$AGE_AT_EVENT), FUN=mean)
colnames(totchol2)=c("record_id","AGE_AT_EVENT","value")

## Merge totchol and hdl by record id, AGE_AT_EVENT, row_id
calcdat=merge(totchol2[,c("record_id","AGE_AT_EVENT","value")], hdl2[,c("record_id","AGE_AT_EVENT","value")], by=c("record_id","AGE_AT_EVENT"))
calcdat$nonhdl_calc=calcdat$value.x-calcdat$value.y

## Check equivalence with recorded value when one exists
colnames(nonhdl)[1]="record_id"
#calcdat_test=merge(calcdat, nonhdl[,c("record_id","AGE_AT_EVENT","VALUE_AS_NUMBER")], by=c("record_id","AGE_AT_EVENT"))
#calcdat_test$diff=abs(calcdat_test$nonhdl_calc-calcdat_test$VALUE_AS_NUMBER)

## Add calculated values to nonhdl
nonhdl=nonhdl[,c("record_id","AGE_AT_EVENT","VALUE_AS_NUMBER")]
colnames(nonhdl)=c("record_id","AGE_AT_EVENT","value")
calcdat=calcdat[,c("record_id","AGE_AT_EVENT","nonhdl_calc")]
mergedhdl=merge(nonhdl, calcdat, by=c("record_id","AGE_AT_EVENT"), all=TRUE)
mergedhdl$nonhdl=mergedhdl$value
mergedhdl$nonhdl[which(is.na(mergedhdl$value))]=mergedhdl$nonhdl_calc[which(is.na(mergedhdl$value))]

## Collapse values for a given person that are taken on the same day (with mean)
d2=aggregate(mergedhdl$nonhdl, by=list(mergedhdl$record_id, mergedhdl$AGE_AT_EVENT), FUN=mean)
colnames(d2)=c("record_id","AGE_AT_EVENT","value")
 

######## Aggregate information by person on range and number of values
indiv=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=min)
colnames(indiv)=c("record_id","first_nonhdl")
indiv$last_nonhdl=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=max)$x
indiv$n_nonhdl=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=uniqct)$x
indiv$nonhdl_range=indiv$last_nonhdl-indiv$first_nonhdl

## Winsorization not performed
##val=mean(d2$value)-2*sd(d2$value) ##33.49
##d2$value[which(d2$value < val)]=val

## Save long form dataset
write.table(d2, "../../AdultQC/nonhdl_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Aggregate relevant values for short form dataset:
d3 <- d2[order(d2$record_id, -d2$AGE_AT_EVENT), ]
d3 <- d3[!duplicated(d3$record_id), ]
colnames(d3)[3]="nonhdl_most_recent"
dmax=d3[,c("record_id","nonhdl_most_recent")]

dmax=merge(dmax, indiv, by="record_id")

ggplot(dmax, aes(x=nonhdl_most_recent))+geom_histogram(fill=colorscheme[3], color="black",binwidth=5)+theme_bw()+xlab("Most recent non-HDL cholesterol (mg/dL)")

write.table(dmax, "../../AdultQC/nonhdl_short.tab", quote=FALSE, row.names=FALSE, sep="\t")

dat=merge(dat, dmax, by="record_id", all.x=TRUE)

write.table(dat, "../../AdultQC/dat_nonhdl.tab", quote=FALSE, row.names=FALSE, sep="\t")









