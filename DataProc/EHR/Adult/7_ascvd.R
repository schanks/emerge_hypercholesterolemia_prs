library(data.table)
library(dplyr)
library(stringr)

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
uw_long=uw_icd[rep(seq_len(nrow(uw_icd)), lengths(strsplit(uw_icd$ICD_CODE, ", "))),]
uw_long$ICD_CODE=trimws(unlist(strsplit(uw_icd$ICD_CODE, ", ")))
vumc_icd=fread("../../Anvil/ICD/VUMC_ICD_20250527.csv")
chop_icd=fread("../../Anvil/ICD/CHOP_icd_20250606.csv")
colnames(chop_icd)[4]="ICD_FLAG"
mayo_icd=fread("../../Anvil/ICD/MAYO_ICD_20251023.csv")
colnames(mayo_icd)=c("EMERGE_ID","AGE_AT_EVENT","ICD_CODE","ICD_FLAG","ROW_ID")
mgb_icd=fread("../../Anvil/ICD/MGB_ICD_20251027.csv")

d=rbind(cuimc_icd, cchmc_icd, nu_icd, uab_icd, uw_long, vumc_icd, chop_icd, mayo_icd, mgb_icd)
d$AGE_AT_EVENT=as.numeric(d$AGE_AT_EVENT)
d=d[which(is.element(d$EMERGE_ID, dat$record_id)),]
s=sinai_icd[which(is.element(sinai_icd$EMERGE_ID, dat$record_id)),]

## Remove all dates after GIRA generation
d=d[d$AGE_AT_EVENT < dat$age_gira_generated[match(d$EMERGE_ID, dat$record_id)], ]
s=s[s$AGE_AT_EVENT < dat$age_gira_generated[match(s$EMERGE_ID, dat$record_id)], ]

## In ICD
icddat=fread("../../AdultQC/dat_hc.tab")
icddat=icddat[,c("record_id","in_icd")]
icddat=icddat[icddat$in_icd==1,]

norm <- function(x) x %>% toupper() %>% str_trim() %>% str_replace_all("\\.", "")
degap_candidates <- function(root, frac) {
  n <- nchar(frac); cands <- character(0)
  for (i in 0:(n-1)) {
    if (substr(frac,1,i) == strrep("0", i)) cands <- c(cands, paste0(root, substr(frac, i+1, n)))
  }
  unique(cands)
}

match_block_d <- function(data, code_col, include, exclude = character(0)) {
  inc_n <- norm(include); exc_n <- norm(exclude)
  pattern <- paste0("^(", paste(inc_n, collapse="|"), ")")
  data %>% mutate(.tmp = norm(.data[[code_col]])) %>%
    filter(str_starts(.tmp, pattern), !(.tmp %in% exc_n)) %>% select(-.tmp)
}

match_block_s <- function(data, code_col, include, exclude = character(0)) {
  inc_n <- norm(include); exc_n <- norm(exclude); explicit_n <- c(inc_n, exc_n)
  pattern <- paste0("^(", paste(inc_n, collapse="|"), ")")
  raw <- data[[code_col]]
  parts <- str_match(raw, "^([A-Za-z0-9]+)\\.(\\d{3})$")
  root <- parts[,2]; frac <- parts[,3]
  keep <- logical(length(raw)); ambiguous <- logical(length(raw))
  for (i in seq_along(raw)) {
    if (is.na(root[i])) {
      code_n <- norm(raw[i])
      keep[i] <- str_detect(code_n, pattern) && !(code_n %in% exc_n)
      next
    }
    cands_n <- norm(degap_candidates(root[i], frac[i]))
    exact_hits <- unique(cands_n[cands_n %in% explicit_n])
    if (length(exact_hits) >= 1) {
      ambiguous[i] <- length(exact_hits) > 1
      keep[i] <- any(exact_hits %in% inc_n) && !all(exact_hits %in% exc_n)
    } else {
      keep[i] <- any(str_detect(cands_n, pattern)) && !any(cands_n %in% exc_n)
    }
  }
  data %>% mutate(.ambiguous = ambiguous) %>% filter(keep)
}

# ---- Define all blocks ----
blocks = list(
   cerebrovascular = list(
    include = c("430","431","432","433","434","435","436","437.0","437.1","438",
                "I60","I61","I62","I63","I65","I66","I67.2","I67.81","I67.82","I67.83","I67.84","I69","R29.7"),
    exclude = c("R29.700")
  ),
   ischemic_heart_disease = list(
    include = c("410","411","412","413","414","V71.7","I20","I21","I22","I23","I24","I25"),
    exclude = c("414.1","I25.3","I25.4")
  ),
   peripheral_artery_disease = list(
    include = c("440","443.8","443.9","444","445","I70","I73.9","I74","I75"),
    exclude = character(0)
  ),
   other = list(
    include = c("429.7","459.8","459.9","V12.5","V45.81","V45.82","V15.1","G45","I99",
                "Z86.7","Z95.1","Z98.6","Z95.5","Z95.8","Z95.9"),
    exclude = character(0)
  ),
  cad = list(
    include = c("410","411","412","I22","I23","I24.1","I25.2"),
    exclude = character(0)
  ),
  pad = list(
    include = c("440.0","440.2","I70.20","I70.21","I70.3","I70.4","I70.5","I70.6","I70.7"),
    exclude = character(0)
  ),
  stroke = list(
    include = c("346.6","430","431","432","433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91","436","I60","I61","I62","I63","G43.6"),
    exclude = character(0)
  )
)

d_out <- lapply(blocks, \(b) match_block_d(d, "ICD_CODE", b$include, b$exclude))
s_out <- lapply(blocks, \(b) match_block_s(s, "ICD_CODE", b$include, b$exclude))

list2env(setNames(d_out, paste0("d_", names(blocks))), envir = .GlobalEnv)
list2env(setNames(s_out, paste0("s_", names(blocks))), envir = .GlobalEnv)

#####Check/fix ambiguous Sinai codes
show_codes=function(data,prefix){
	return(summary(as.factor(data$ICD_CODE[grep(prefix, data$ICD_CODE)])))
}

show_codes(s_cerebrovascular, "437")
show_codes(s_cerebrovascular,"I67")
show_codes(s_cerebrovascular,"R29")
s_cerebrovascular=s_cerebrovascular[which(!is.element(s_cerebrovascular$ICD_CODE,c("437.002","437.003","437.007","437.008","437.009"))),]

show_codes(s_ischemic_heart_disease,"414")
show_codes(d_ischemic_heart_disease,"414")
show_codes(s_ischemic_heart_disease,"V71")
show_codes(s_ischemic_heart_disease,"I25")
show_codes(d_ischemic_heart_disease,"I25")
s_ischemic_heart_disease=s_ischemic_heart_disease[which(!is.element(s_ischemic_heart_disease$ICD_CODE, c("414.011","I25.041"))),]
d_ischemic_heart_disease=d_ischemic_heart_disease[which(!is.element(d_ischemic_heart_disease$ICD_CODE, c("414.10","414.11","414.12","414.19","I25.41","I25.42"))),]

show_codes(s_peripheral_artery_disease,"443")
show_codes(s_peripheral_artery_disease,"I73")

show_codes(s_other,"429")
show_codes(s_other,"459")
show_codes(s_other,"V12")
show_codes(s_other,"V45")
show_codes(s_other,"V15")
show_codes(s_other,"Z86")
show_codes(s_other,"Z95")
show_codes(s_other,"Z98")

show_codes(s_cad, "I24")
show_codes(s_cad,"I25")

show_codes(s_pad, "440")
show_codes(s_pad, "I70")
s_pad=s_pad[which(!is.element(s_pad$ICD_CODE, c("440.001","440.008","440.009"))),]


show_codes(s_stroke, "346")
show_codes(s_stroke, "433")
show_codes(s_stroke, "434")
show_codes(s_stroke, "G43")
s_stroke=s_stroke[which(!is.element(s_stroke$ICD_CODE, c("433.010"))),]


pantalone=rbind(d_cerebrovascular, d_ischemic_heart_disease, d_peripheral_artery_disease, d_other, s_cerebrovascular, s_ischemic_heart_disease, s_peripheral_artery_disease, s_other, fill=TRUE)
spinks=rbind(d_cad, d_pad, d_stroke, s_cad, s_pad, s_stroke, fill=TRUE)
colnames(pantalone)[1]="record_id"
colnames(spinks)[1]="record_id"
pantalone=pantalone[,-c(4:6)]
spinks=spinks[,-c(4:6)]
pantalone=unique(pantalone)
spinks=unique(spinks)

write.table(pantalone, "../../AdultQC/ascvd_pantalone_long.tab", quote=FALSE, row.names=FALSE, sep="\t")
write.table(spinks, "../../AdultQC/ascvd_spinks_long.tab", quote=FALSE, row.names=FALSE, sep="\t")


cad=rbind(d_cad, s_cad, fill=TRUE)
colnames(cad)[1]="record_id"
cad_ct=aggregate(cad$AGE_AT_EVENT, by=list(cad$record_id), FUN=min)
colnames(cad_ct)=c("record_id", "age_cad")

pad=rbind(d_pad, s_pad, fill=TRUE)
colnames(pad)[1]="record_id"
pad_ct=aggregate(pad$AGE_AT_EVENT, by=list(pad$record_id), FUN=min)
colnames(pad_ct)=c("record_id","age_pad")

stroke=rbind(d_stroke, s_stroke, fill=TRUE)
colnames(stroke)[1]="record_id"
stroke_ct=aggregate(stroke$AGE_AT_EVENT, by=list(stroke$record_id), FUN=min)
colnames(stroke_ct)=c("record_id","age_stroke")


###Individual level data
spinks_ct=aggregate(spinks$AGE_AT_EVENT, by=list(spinks$record_id), FUN=uniqct)
colnames(spinks_ct)=c("record_id","ascvd_ct")
spinks_ct$stroke=as.numeric(is.element(spinks_ct$record_id, c(d_stroke$EMERGE_ID, s_stroke$EMERGE_ID)))
spinks_ct$cad=as.numeric(is.element(spinks_ct$record_id, c(d_cad$EMERGE_ID, s_cad$EMERGE_ID)))
spinks_ct$pad=as.numeric(is.element(spinks_ct$record_id, c(d_pad$EMERGE_ID, s_pad$EMERGE_ID)))
spinks_ct$age_ascvd=aggregate(spinks$AGE_AT_EVENT, by=list(spinks$record_id), FUN=min)$x

spinks_ct=merge(spinks_ct, cad_ct, by="record_id", all.x=TRUE)
spinks_ct=merge(spinks_ct, pad_ct, by="record_id", all.x=TRUE)
spinks_ct=merge(spinks_ct, stroke_ct, by="record_id", all.x=TRUE)

write.table(spinks_ct, "../../AdultQC/spinks_short.tab", quote=FALSE, row.names=FALSE, sep="\t")


