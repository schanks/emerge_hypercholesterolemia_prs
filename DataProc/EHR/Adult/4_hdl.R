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


##Restrict to total cholesterol codes
d=d[which(is.element(d$MEASUREMENT_CONCEPT_ID, c("3007070","3005984"))),]

## Remove all dates after GIRA generation
d=d[d$AGE_AT_EVENT < dat$age_gira_generated[match(d$EMERGE_ID, dat$record_id)], ]

## Remove all pediatric panels
d=d[which(d$AGE_AT_EVENT>18),] 

## Remove missing values
d=d[which(!is.na(d$VALUE_AS_NUMBER)),]

## No nonstandard units observed

## Collapse values for a given person that are taken on the same day (with mean)
d2=aggregate(d$VALUE_AS_NUMBER, by=list(d$EMERGE_ID, d$AGE_AT_EVENT), FUN=mean)
colnames(d2)=c("record_id","AGE_AT_EVENT","value")
 

######## Aggregate information by person on range and number of values
indiv=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=min)
colnames(indiv)=c("record_id","first_hdl")
indiv$last_hdl=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=max)$x
indiv$n_hdl=aggregate(d2$AGE_AT_EVENT, by=list(d2$record_id), FUN=uniqct)$x
indiv$hdl_range=indiv$last_hdl-indiv$first_hdl

## Winsorization not performed
##val=mean(d2$value)-2*sd(d2$value) ##33.49
##d2$value[which(d2$value < val)]=val

## Save long form dataset
write.table(d2, "../../AdultQC/hdl_long.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Aggregate relevant values for short form dataset:
d3 <- d2[order(d2$record_id, -d2$AGE_AT_EVENT), ]
d3 <- d3[!duplicated(d3$record_id), ]
colnames(d3)[3]="hdl_most_recent"

dmax=merge(d3, indiv, by="record_id")

ggplot(dmax, aes(x=hdl_most_recent))+geom_histogram(fill=colorscheme[4], color="black",binwidth=5)+theme_bw()+xlab("Most recent HDL (mg/dL)")

write.table(dmax, "../../AdultQC/hdl_short.tab", quote=FALSE, row.names=FALSE, sep="\t")

dat=merge(dat, dmax, by="record_id", all.x=TRUE)

write.table(dat, "../../AdultQC/dat_hdl.tab", quote=FALSE, row.names=FALSE, sep="\t")









