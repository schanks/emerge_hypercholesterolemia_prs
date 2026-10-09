library(data.table)

uniqct=function(x){
return(length(unique(x)))
}

dat=fread("../../../CDP/dat_gira.tab")

keyword_list=c("acipimox","alirocumab","atorvastatin","bempedoic","bezafibrate","cerivastatin","cholestyramine","ciprofibrate","colesevelam","colestipol","docosaenoic","docosahexaenoate","evinacumab","evolocumab","ezetimibe","fenofibrate","fenofibric","fluvastatin","gemfibrozil","icosapent","inclisiran","lerodalcibep","lomitapide","lovastatin","mipomersen","niacin","omega-3 acid","phytosterol","pitavastatin","policosanol","pravastatin","red yeast","rosuvastatin","simvastatin")

ldl_list=c("alirocumab","atorvastatin","bempedoic","cerivastatin","cholestyramine","colesevelam","colestipol","evinacumab","evolocumab","ezetimibe","fluvastatin","inclisiran","lerodalcibep","lomitapide","lovastatin","mipomersen","phytosterol","pitavastatin","policosanol","pravastatin","red yeast","rosuvastatin","simvastatin")

rxcui_prod=fread("../../Anvil/Drug/c10_rxcui_network.csv")

cuimc=fread("../../Anvil/Drug/CUIMC_Drug_Exposure_20250930.csv")
colnames(cuimc)[5]="RXCUI"
cchmc=fread("../../Anvil/Drug/CCHMC_Drug_Exposure_20250915.csv")
colnames(cchmc)[5]="RXCUI"
sinai=fread("../../Anvil/Drug/MountSinai_Drug_Exposure_20250930.csv")
sinai$RXCLASS_ID=rep(NA, nrow(sinai))
sinai$OUTCOMES_CONCEPT=rep(NA, nrow(sinai))
nu=fread("../../Anvil/Drug/NU_outcomes_drug_exposure_2025-09-30_1.csv")
nu$ROW_ID=rep(NA, nrow(nu))
uab=fread("../../Anvil/Drug/UAB_DRUG_EXPOSURE_20250915.csv")
colnames(uab)[5]="RXCUI"
uw=fread("../../Anvil/Drug/UW_DRUG_EXPOSURE_20250916.csv")
uw=uw[,-1]
vumc=fread("../../Anvil/Drug/VUMC_MEDs_20251007.csv")
colnames(vumc)=colnames(uw)
mayo=fread("../../Anvil/Drug/MAYO_DRUG_EXPOSURE_20250922.csv")
colnames(mayo)=c("EMERGE_ID","AGE_AT_EVENT","DRUG_CONCEPT_ID","DRUG_CONCEPT_NAME","RXCUI","ROW_ID")
mayo$RXCLASS_ID=rep(NA, nrow(mayo))
mayo$OUTCOMES_CONCEPT=rep(NA, nrow(mayo))
mgb=fread("../../Anvil/Drug/MGB_Drug_exposure_20251027.csv")
colnames(mgb)[5]="RXCUI"

d=rbind(cuimc, cchmc, sinai, nu, uab, uw, vumc, mayo, mgb)
d$AGE_AT_EVENT=as.numeric(d$AGE_AT_EVENT)
d=d[which(is.element(d$EMERGE_ID, dat$record_id)),]
d=d[d$AGE_AT_EVENT < dat$age_gira_generated[match(d$EMERGE_ID, dat$record_id)], ]

dat$in_drug=as.numeric(is.element(dat$record_id, d$EMERGE_ID))

####KEYWORD SEARCH VERSION

c10_mask=grepl(paste(keyword_list, collapse = "|"), d$DRUG_CONCEPT_NAME, ignore.case = TRUE) &
        !grepl("niacinamide", d$DRUG_CONCEPT_NAME, ignore.case = TRUE)
c10=d[c10_mask,]
c10_drugs=unique(c10$DRUG_CONCEPT_NAME)

llt=c10[grepl(paste(ldl_list, collapse = "|"), c10$DRUG_CONCEPT_NAME, ignore.case = TRUE), ]
llt_drugs=unique(llt$DRUG_CONCEPT_NAME)

####NETWORK RXCUI VERSION

c10_network=d[which(is.element(d$RXCUI, rxcui_prod$rxcui)),]
network_drugs=unique(c10_network$DRUG_CONCEPT_NAME)

ignore_list=c("ascorbic acid 100 MG / biotin 0.15 MG / calcium pantothenate 5 MG / folic acid 1 MG / niacin 20 MG / pyridoxine 10 MG / riboflavin 1.7 MG / thiamine mononitrate 1.5 MG / vitamin B12 0.006 MG Oral Capsule", "ascorbic acid 100 MG / biotin 0.15 MG / calcium pantothenate 5 MG / folic acid 1 MG / niacin 20 MG / pyridoxine 10 MG / riboflavin 1.7 MG / thiamine mononitrate 1.5 MG / vitamin B12 0.006 MG Oral Capsule [Nephrocaps]", "{6 (ascorbic acid 100 MG / biotin 0.3 MG / calcium carbonate 100 MG / calcium pantothenate 7 MG / folic acid 1 MG / niacin 25 MG / pyridoxine hydrochloride 30 MG / riboflavin 5 MG / succinic acid 150 MG / thiamine hydrochloride 5 MG / vitamin B12 0.01...","alpha tocopherol 30 UNT / ascorbic acid 30 MG / docosahexaenoic acid 250 MG / eicosapentaenoic acid 0.625 MG / ferrous fumarate 83 MG / folic acid 1 MG / pyridoxine 25 MG / tricalcium phosphate 100 MG Oral Capsule [Natelle]","ascorbic acid 30 MG / cholecalciferol 1000 UNT / cuprous oxide 2 MG / folic acid 1 MG / magnesium oxide 20 MG / niacin 15 MG / polysaccharide iron complex 29 MG / potassium iodide 0.15 MG / pyridoxine hydrochloride 2.5 MG / riboflavin 1.8 MG / thiamine...","ascorbic acid 113 MG / copper gluconate 0.4 MG / docosahexaenoic acid 87.5 MG / eicosapentaenoic acid 163 MG / lutein 2.5 MG / tocopherol acetate 100 UNT / zeaxanthin 0.5 MG / zinc oxide 17.4 MG Oral Capsule [Preservision 2]","fish oil (containing omega-3 acids)","heptaminol 90 MG / inositol niacinate 100 MG / rutin 50 MG Oral Tablet")

## Remove drugs from keyword-based list that do not primarily target lipids

c10=c10[which(!is.element(c10$DRUG_CONCEPT_NAME, ignore_list)),]
write.table(c10, "../../AdultQC/c10_all_dates.tab", quote=FALSE, row.names=FALSE, sep="\t")

write.table(llt, "../../AdultQC/c10_ldl_all_dates.tab", quote=FALSE, row.names=FALSE, sep="\t")

## Summary statistics of drug availability
d_agg=aggregate(d$RXCUI, by=list(d$EMERGE_ID), FUN=length)
d_agg$first_drug=aggregate(d$AGE_AT_EVENT, by=list(d$EMERGE_ID), FUN=min)$x
d_agg$last_drug=aggregate(d$AGE_AT_EVENT, by=list(d$EMERGE_ID), FUN=max)$x
colnames(d_agg)[c(1,2)]=c("record_id","drug_count")
d_agg$range_drug=d_agg$last_drug-d_agg$first_drug
 
## short form
c10_short=aggregate(c10$AGE_AT_EVENT, by=list(c10$EMERGE_ID), FUN=uniqct)
colnames(c10_short)=c("record_id","c10_ct")
c10_short$age_c10=aggregate(c10$AGE_AT_EVENT, by=list(c10$EMERGE_ID), FUN=min)$x

write.table(c10_short, "../../AdultQC/c10_short.tab", quote=FALSE, row.names=FALSE, sep="\t")

llt_short=aggregate(llt$AGE_AT_EVENT, by=list(llt$EMERGE_ID), FUN=uniqct)
colnames(llt_short)=c("record_id","llt_ct")
llt_short$age_llt=aggregate(llt$AGE_AT_EVENT, by=list(llt$EMERGE_ID), FUN=min)$x

write.table(llt_short, "../../AdultQC/llt_short.tab", quote=FALSE, row.names=FALSE, sep="\t")

#integrated dataset
dat$c10=as.numeric(is.element(dat$record_id, c10_short$record_id))
dat$llt=as.numeric(is.element(dat$record_id, llt_short$record_id))
dat=merge(dat, d_agg, by="record_id", all.x=TRUE)
dat=merge(dat, c10_short, by="record_id", all.x=TRUE)
dat=merge(dat, llt_short, by="record_id", all.x=TRUE)
dat$c10[which(dat$in_drug==0)]=NA
dat$llt[which(dat$in_drug==0)]=NA

write.table(dat, "../../AdultQC/dat_llt.tab", quote=FALSE, row.names=FALSE, sep="\t")





















