library(data.table)
library(ggplot2)
colorscheme=c("#2A7F62","#344966","#B4CDED","#F06543","#BFCC94")


uniqct=function(x){
return(length(unique(x)))
}

dat=fread("../../../CDP/peds_gira.tab")

clin=fread("../../../CDP/PedsClinical_11Apr25.csv")
clin$totalcholest_value_most_recent=as.numeric(clin$totalcholest_value_most_recent)
clin$triglyceride_value_most_recent=as.numeric(clin$triglyceride_value_most_recent)
clin$hdl_value_most_recent=as.numeric(clin$hdl_value_most_recent)
clin[clin==-9]=NA

dat=merge(dat, clin[,-c("age_deid")], by="record_id")

complete=dat[which(!is.na(dat$totalcholest_value_most_recent)),]
complete=complete[which(!is.na(complete$triglyceride_value_most_recent)),]
complete=complete[which(!is.na(complete$hdl_value_most_recent)),]

complete=complete[which(complete$triglyceride_value_most_recent<400),]
complete$ldlc=complete$totalcholest_value_most_recent-(complete$triglyceride_value_most_recent/5)-complete$hdl_value_most_recent

min_val=mean(complete$ldlc-2*sd(complete$ldlc))
#31.80
complete$ldlc[which(complete$ldlc<min_val)]=min_val


write.table(complete, "../../PedsQC/peds_ldl.tab", quote=FALSE, row.names=FALSE, sep="\t")



