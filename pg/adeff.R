library(haven)
library(metacore)
library(dplyr)
library(purrr)
library(stringr)
library(metatools)
library(tibble)
library(admiral)
library(admiraldev)
library(diffdf)
library(xportr)
library(admiralonco)
library(rlang)
library(lubridate)
library(readxl)

rm(list=ls())
Sys.setlocale(category = "LC_ALL",locale ='Chinese')
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/adam.R")
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/derive_param_bor.R")
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/admiralonco_environment.R")
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/signal_cr_prsd.R")
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/derive_param_confirmed_bor.R")

sdtm_path<-"D:/Users/machengcheng/Desktop/test/dev/data/sdtm"
sdtm<-read_sas_datasets(sdtm_path)
adam_path<-"D:/Users/machengcheng/Desktop/test/dev/data/adam"
adam<-read_sas_datasets(adam_path)
radam_path<-"D:/Users/machengcheng/Desktop/test/R dev/data/adam"
radam<-read_rds_datasets(radam_path)

format_aval <- function(arg) {
  case_when(
    arg == "完全缓解（CR）"  ~ 1,
    arg == "部分缓解（PR）"  ~ 2,
    arg == "疾病稳定（SD）"  ~ 3,
    arg == "疾病进展（PD）"  ~ 4,
    arg == "无法评估（NE）"  ~ 5,
    TRUE ~ NA_real_
  )
}

format_param <- function(var_input) {
  case_when(
    var_input == "Target Response for Investigator"  ~ "INVTRSP",
    var_input == "Non-target Response for Investigator"  ~ "INVNTRSP",
    var_input == "New Lesion Indicator for Investigator"  ~ "INVNLIND",
    var_input == "Overall Response for Investigator"  ~ "INVORSP",
    var_input == "Best Overall Response for Investigator"  ~ "INVBOR",
    var_input == "Best Overall Response (Unconfirmed) for Investigator"  ~ "INVBORUC",
    var_input == "Best Overall Response (Confirmed) for Investigator"  ~ "INVBORC"
  )
}

format_paramn <- function(var_input) {
  case_when(
    var_input == "Target Response for Investigator"  ~ 1,
    var_input == "Non-target Response for Investigator"  ~ 2,
    var_input == "New Lesion Indicator for Investigator"  ~ 3,
    var_input == "Overall Response for Investigator"  ~ 4,
    var_input == "Best Overall Response for Investigator"  ~ 5,
    var_input == "Best Overall Response (Unconfirmed) for Investigator"  ~ 6,
    var_input == "Best Overall Response (Confirmed) for Investigator"  ~ 7
  )
}

format_avalc <- function(arg) {
  case_when(
    arg == "完全缓解（CR）"  ~ "CR",
    arg == "部分缓解（PR）"  ~ "PR",
    arg == "疾病稳定（SD）"  ~ "SD",
    arg == "疾病进展（PD）"  ~ "PD",
    arg == "无法评估（NE）"  ~ "NE"  )
}

format_avalc_map <- function(arg) {
  case_when(
    arg == "CR"  ~ "完全缓解（CR）",
    arg == "PR"  ~ "部分缓解（PR）",
    arg == "SD"  ~ "疾病稳定（SD）",
    arg == "PD"  ~ "疾病进展（PD）",
    arg == "NE"  ~ "无法评估（NE）",
    arg == "MISSING"  ~ "不适用"
  )
}

adsl<-radam$adsl

rs<-combine_supp(sdtm$rs, sdtm$supprs)  %>% as_tibble()%>%mutate(visit_map=if_else(substr(VISIT,1,5)!="计划外访视",VISIT,RUNSVIS))
tr<-combine_supp(sdtm$tr, sdtm$supptr)  %>% as_tibble() %>%
  mutate(visit_map=if_else(substr(VISIT,1,5)!="计划外访视",VISIT,RUNSVIS))%>%
  select(USUBJID,TRDTC,visit_map)

rs1 <- rs%>%left_join(tr,by = c("USUBJID","visit_map"))

rs1 <- rs%>%left_join(adsl,by = c("STUDYID","USUBJID"))%>%
  filter(RSTESTCD!="RSALL")%>%
  mutate(AVISIT=if_else(substr(VISIT,1,5)!="计划外访视",VISIT,NULL),
         AVISITN=if_else(substr(VISIT,1,5)!="计划外访视",VISITNUM,NULL),
         PARAM=paste(RSTEST,"for Investigator",sep=" "),
         AVALC=RSSTRESC,
         SRCDOM=DOMAIN,
         SRCSEQ=RSSEQ,
         PARAMCD=format_param(PARAM)
  )

tr_<-tr%>%group_by(USUBJID,visit_map)%>%  
  mutate(first_dtc = min(var = TRDTC),
         last_dtc = max(var = TRDTC))%>%
  distinct(USUBJID,visit_map,first_dtc,last_dtc)%>%
  arrange(USUBJID,visit_map)%>%ungroup()

rs2 <- rs1%>%left_join(tr_,by = c("USUBJID","visit_map"))%>%
  mutate(IMAGDTC=if_else(PARAM=="Overall Response for Investigator",if_else(AVALC=="疾病进展（PD）",first_dtc,last_dtc),NULL))%>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = IMAGDTC,
    date_imputation = "none"
  )%>%
  mutate(
    ADY=if_else(as.numeric(ADT - TRTSDT)<0,as.numeric(ADT - TRTSDT),as.numeric(ADT - TRTSDT)+1),
             AVAL=if_else(PARAMCD=="INVORSP",format_aval(AVALC),NULL),
             AVALC=if_else(PARAMCD=="INVORSP",format_avalc(AVALC),AVALC))

adsl1<-adsl%>%mutate(mindt=coalesce(PDDT,SATSDT))%>%filter(SAFFL=="Y")

pd_date <- date_source(
  dataset_name = "adrs",
  date         = mindt)

adrs<-derive_param_bor(
  rs2,
  dataset_adsl = adsl1,
  filter_source = PARAMCD == "INVORSP" & !is.na(AVALC)&ADY>1,
  reference_date = TRTSDT,
  source_pd = pd_date,
  source_datasets = list(adrs = adsl1),
  missing_as_ne=F,
  aval_fun=format_aval,
  ref_start_window = 42,
  desc_adt=T,
  set_values_to = vars(
    PARAMCD="INVBORUC",
    PARAM = "Best Overall Response (Unconfirmed) for Investigator"
  )
)


adrs1<-derive_param_confirmed_bor(
  adrs,
  dataset_adsl = adsl1,
  filter_source = PARAMCD == "INVORSP" & !is.na(AVALC)&ADY>1,
  source_pd = pd_date,
  source_datasets = list(adrs = adsl1),
  reference_date = TRTSDT,
  ref_start_window = 42,
  ref_confirm = 28,
  max_nr_ne = 1,
  accept_sd = TRUE,
  missing_as_ne = F,
  desc_adt=T,
  set_values_to = vars(
    PARAMCD = "INVBORC",
    PARAM = "Best Overall Response (Confirmed) for Investigator"
  )
)

adrs2<-adrs1%>%
  mutate(AVALC=if_else(PARAMCD%in% c("INVORSP","INVBORUC","INVBORC"),format_avalc_map(AVALC),AVALC),
         CRIT1=if_else(AVALC %in% c("完全缓解（CR）", "部分缓解（PR）")&PARAMCD%in% c("INVBORUC","INVBORC"),"CR+PR",NULL),
         CRIT1FL=if_else(AVALC %in% c("完全缓解（CR）", "部分缓解（PR）")&PARAMCD%in% c("INVBORUC","INVBORC"),"Y",NULL),
         CRIT2=if_else(AVALC %in% c("完全缓解（CR）", "部分缓解（PR）","疾病稳定（SD）")&PARAMCD%in% c("INVBORUC","INVBORC"),"CR+PR+SD",NULL),
         CRIT2FL=if_else(AVALC %in% c("完全缓解（CR）", "部分缓解（PR）","疾病稳定（SD）")&PARAMCD%in% c("INVBORUC","INVBORC"),"Y",NULL),
         ANL01FL=if_else(!is.na(AVALC)&PARAMCD %in% c("INVBORUC","INVBORC"),"Y",NULL),
         PARAMN=format_paramn(PARAM),
         AVISIT=if_else(PARAMCD %notin% c("INVBORUC","INVBORC"),AVISIT,NULL),
         AVISITN=if_else(PARAMCD %notin% c("INVBORUC","INVBORC"),AVISITN,NULL),
         RSSTAT=if_else(PARAMCD %notin% c("INVBORUC","INVBORC"),RSSTAT,NULL),
         SRCDOM=if_else(PARAMCD %notin% c("INVBORUC","INVBORC"),SRCDOM,NULL),
         SRCSEQ=if_else(PARAMCD %notin% c("INVBORUC","INVBORC"),SRCSEQ,NULL)
         )%>%
  restrict_derivation(
    derivation = derive_var_extreme_flag,
    args = params(
      by_vars = vars(USUBJID),
      order = vars(USUBJID,PARAM,ADT,VISITNUM),
      new_var = FSTORFL,
      mode = "first"
    ),
    filter = AVALC %in% c("完全缓解（CR）", "部分缓解（PR）") & PARAMCD== "INVORSP" & !is.na(AVALC)&ADY>1
  )
adrs3<-adrs2[order(adrs2$USUBJID, adrs2$PARAMN, adrs2$ADT, adrs2$AVISITN, na.last = FALSE),]

adam_spec_file<-"D:/Users/machengcheng/Desktop/test/doc/adam/BM2L201910 ADaM Dataset Specifications v0.3.xlsx"
ADSL_com<-read_spec_sheet(adam_spec_file,"ADSL")%>%subset(`Common Variable`=="Y"|`Variable Name`%in% c("STUDYID","USUBJID","SUBJID","SITEID")) 
ADEFF_spec<-read_spec_sheet(adam_spec_file,"ADEFF")
adam_vars<-rbind(ADSL_com%>%select(-`Common Variable`),ADEFF_spec%>%filter(`Variable Name` %notin% c("STUDYID","USUBJID")))

adeff_final<-adrs3%>%out_adam(adam_vars)
adeff<-adam$adeff%>%select(-FSTCORFL)

#adeff2<-adeff[order(adeff$USUBJID, adeff$PARAMN, adeff$ADT, adeff$AVISITN),]

compare<-diffdf(adeff_final, adeff)
print(compare)

saveRDS(adeff_final, file = file.path(radam_path, "adeff.rds"))
xportr_write(adeff_final,path = file.path(radam_path, "adeff.xpt"), label = "Efficacy Analysis Dataset")
