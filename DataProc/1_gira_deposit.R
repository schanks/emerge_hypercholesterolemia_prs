library(data.table)
library(ggplot2)

#Data read in and procssing
dat=fread("april25.csv")
peds=fread("PedsDemographic_15Jul25.csv")
peds_sex=fread("PedsSex_16Jul25.csv")
prs=fread("../PRS/calibrated_hcl_all.tab")
prs$record_id=as.integer(unlist(strsplit(prs$emergeID_anvilID,"_"))[seq(1,nrow(prs)*2, by=2)])

missing_broad=fread("missing_broad.csv")


missing_invitae=fread("missing_invitae.csv")

peds=peds[,c(1,2,7,9)]
colnames(peds)=c("record_id","age_deid","new_race","site_id")
peds=merge(peds, peds_sex, by="record_id")
peds=merge(peds, prs[,c("record_id","new_perc")], by="record_id")

dat$site[which(dat$site_ten==27)]="VUMC"
dat$site[which(dat$site_ten==33)]="UAB"
dat$site[which(dat$site_ten==42)]="CUIMC"
dat$site[which(dat$site_ten==48)]="Mayo"
dat$site[which(dat$site_ten==50)]="UW"
dat$site[which(dat$site_ten==52)]="NU"
dat$site[which(dat$site_ten==68)]="MGB"
dat$site[which(dat$site_ten==74)]="Sinai"
dat$site[which(dat$site_ten==81)]="CCHMC"
dat$site[which(dat$site_ten==95)]="CHOP"

peds$site[which(peds$site_id=="Children's Hospital of Philadelphia")]="CHOP"
peds$site[which(peds$site_id=="Cincinnati Children's Hospital or University of Cincinnati Health")]="CCHMC"
peds$site[which(peds$site_id=="Columbia University Medical Center")]="CUIMC"
peds$site[which(peds$site_id=="Mass General Brigham")]="MGB"
peds$site[which(peds$site_id=="Mount Sinai")]="Sinai"
peds$site[which(peds$site_id=="Northwestern")]="NU"
peds$site[which(peds$site_id=="the Mayo Clinic")]="Mayo"
peds$site[which(peds$site_id=="the University of Alabama at Birmingham")]="UAB"
peds$site[which(peds$site_id=="the University of Washington")]="UW"
peds$site[which(peds$site_id=="Vanderbilt University Medical Center")]="VUMC"

dat$variant=rep("Not high risk", nrow(dat))
dat$variant[which(dat$results_hcl==2)]="PRS+"
dat$variant[which(dat$results_hcl==3)]="FH+"
dat$variant=factor(dat$variant, levels=c("Not high risk","FH+","PRS+"))

peds$prs=as.numeric(peds$new_perc>97)

#Sample exclusions and age categorization
dat=dat[which(dat$sex_at_birth_deid!="Intersex or Unknown"),]
dat$age_cat=cut(dat$age_deid, breaks=c(17,29,39,49,59,69,75), labels=c("18-29","30-39","40-49","50-59","60-69","70-75"))

peds=peds[which(peds$sex_at_birth!="Intersex or Unknown"),]


#Genetic data missingness
dat$missing_invitae=as.numeric(is.element(dat$record_id, missing_invitae$record_id))
dat$missing_broad=as.numeric(!is.element(dat$record_id, prs$record_id) & is.element(dat$record_id, missing_broad$record_id))
dat$missing_quant=as.numeric(!is.element(dat$record_id, prs$record_id))

write.table(dat$record_id[dat$missing_invitae==1], "missing_invitae.txt", quote=FALSE, row.names=FALSE)
write.table(dat$record_id[dat$missing_broad==1], "missing_broad.txt", quote=FALSE, row.names=FALSE)
write.table(dat$record_id[dat$missing_quant==1], "missing_quant.txt", quote=FALSE, row.names=FALSE)

#Remove missing PRS
dat=dat[which(dat$missing_broad==0),]
dat=dat[,-c("missing_broad")]

#Dat race/ethnicity processing
dat$race4=dat$new_race
dat$race4[which(!is.element(dat$new_race, c("White","Asian","Black, African American, or African","Hispanic, Latino, or Spanish")))]="Other/multiple"
dat$race4[which(dat$new_race=="Prefer not to answer")]=NA

peds$race4=peds$new_race
peds$race4[which(!is.element(peds$new_race, c("White","Asian","Black, African American, or African","Hispanic, Latino, or Spanish")))]="Other/multiple"
peds$race4[which(peds$new_race=="Prefer not to answer")]=NA

#Read in GIRA dates
gira=fread("GIRA.csv")
mono=fread("Monogenic_Dates.csv")

#Get latest date generated
gira_generate=gira[,c("record_id","date_gira_generated_deid")]
gira_generate=gira_generate[which(!is.na(gira_generate$date_gira_generated_deid)),]
gira_generate=aggregate(gira_generate$date_gira_generated_deid, by=list(gira_generate$record_id), FUN=max)
colnames(gira_generate)=c("record_id","age_gira_generated")

#Get earliest date monogenic reported
mono=mono[,c("record_id","invitae_import_date_deid_2")]
mono=mono[which(!is.na(mono$invitae_import_date_deid_2)),]

#Merge
dat=merge(dat, gira_generate, by="record_id")
dat=merge(dat, mono, by="record_id", all.x=TRUE)

peds=merge(peds, gira_generate, by="record_id")

#Replace cutoff time for mono+ with monogenic date
dat$age_gira_generated[which(dat$variant=="FH+")]=dat$invitae_import_date_deid_2[which(dat$variant=="FH+")]

#Clean up and write data tables
dat=dat[,c("record_id","age_deid","sex_at_birth_deid","new_race","race4","has_hcl_m","meds_lower_cholesterol_2","site","variant","age_cat","age_gira_generated","missing_invitae")]

write.table(dat, "dat_gira.tab", quote=FALSE, row.names=FALSE, sep="\t")

peds=peds[,c("record_id","age_deid","sex_at_birth","new_race","race4","site","prs","age_gira_generated")]

write.table(peds, "peds_gira.tab", quote=FALSE, row.names=FALSE, sep="\t")



