library(haven)
library(metacore)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(metatools)
library(tibble)
library(admiral)
library(admiraldev)
library(diffdf)
library(xportr)
library(readxl)

rm(list=ls())
Sys.setlocale(category = "LC_ALL",locale ='Chinese')
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/adam.R")
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/get_summary_records.R")

adam_spec_file<-"D:/Users/machengcheng/Desktop/test/doc/adam/BM2L201910 ADaM Dataset Specifications v0.3.xlsx"
sdtm_path<-"D:/Users/machengcheng/Desktop/test/dev/data/sdtm"
sdtm<-read_sas_datasets(sdtm_path)
adam_path<-"D:/Users/machengcheng/Desktop/test/dev/data/adam"
adam<-read_sas_datasets(adam_path)
radam_path<-"D:/Users/machengcheng/Desktop/test/R dev/data/adam"
radam<-read_rds_datasets(radam_path)

adsl<-radam$adsl
tr<-combine_supp(sdtm$tr, sdtm$supptr)  %>% as_tibble()

tr1 <- tr%>%
  derive_vars_dt(
    new_vars_prefix = "A",
    dtc = TRDTC,
    date_imputation = "none"
  )%>%
  mutate(
    AVAL=TRSTRESN,
    AVALC=TRSTRESC,
    SRCDOM=DOMAIN,
    SRCSEQ=TRSEQ,
    PARAM=if_else(TRTEST=="Tumor State",TRTEST,paste0(TRTEST," (",TRSTRESU,")")),
    )

tr2<-tr1%>%derive_summary_records(
  by_vars = vars(STUDYID,USUBJID, PARAM,VISITNUM,VISIT,RUNSVIS),
  filter = PARAM == "Diameter (mm)",
  analysis_var = AVAL,
  summary_fun = sum,
  set_values_to = vars(DTYPE = "AVERAGE",
                       PARAM = "Derived Sum of Diameter (mm)"
                       )
)%>%left_join(adsl,by = c("STUDYID","USUBJID"))%>%
  mutate(
    ADY=if_else(as.numeric(ADT - TRTSDT)<0,as.numeric(ADT - TRTSDT),as.numeric(ADT - TRTSDT)+1),
    AVALC=if_else(PARAM == "Derived Sum of Diameter (mm)",as.character(AVAL),AVALC))

tr_paramcd<-read_codelist(adam_spec_file,"ADTR","TRPARAMCD")
tr_paramn<-read_codelist(adam_spec_file,"ADTR","TRPARAMN")

param_lookup<-derive_vars_merged(
  tr_paramcd,
  dataset_add = tr_paramn,
  by_vars = vars(PARAM)
)

tr3<-tr2%>%derive_vars_merged(
  dataset_add = param_lookup,
  by_vars = vars(PARAM)
)

tr4<-tr3%>%restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID, PARAMCD),
    order = vars(VISITNUM,ADT,TRLNKID),
    new_var = ABLFL,
    mode = "last"
  ),
  filter = ADT<=TRTSDT&PARAMCD%in%c("SUMDIAM","DSUMDIAM")
)%>%derive_var_base(
  by_vars = vars(USUBJID, PARAMCD),
  source_var = AVAL,
  new_var = BASE,
  filter=ABLFL == "Y"
)%>%derive_var_base(
  by_vars = vars(USUBJID, PARAMCD),
  source_var = AVALC,
  new_var = BASEC,
  filter=ABLFL == "Y"
)

tr5<-tr4%>%restrict_derivation(
  derivation = derive_var_extreme_flag,
  args = params(
    by_vars = vars(USUBJID, PARAMCD,TRLNKID),
    order = vars(VISITNUM,ADT,TRLNKID),
    new_var = BFLAG,
    mode = "last"
  ),
  filter = ADT<=TRTSDT
)%>%mutate(AVISIT=if_else(ADY<0,if_else(BFLAG=="Y","基线",NULL),
                          if_else(substr(VISIT,1,5)!="计划外访视",VISIT,NULL)),
           AVISITN=if_else(ADY<0,if_else(BFLAG=="Y",0,NULL),
                           if_else(substr(VISIT,1,5)!="计划外访视",VISITNUM,NULL)),
           ANL01FL=if_else(PARAMCD%in%c("SUMDIAM","DSUMDIAM")&
                             ((AVISIT=="基线"&ABLFL=="Y")|
                             ((!is.na(AVAL)|!is.na(AVAL))&AVISIT!="基线"&!is.na(AVISIT))),"Y",NULL),
           CHG=if_else(AVISIT!="基线",AVAL-BASE,NULL,missing = AVAL-BASE),
           PCHG=if_else(AVISIT!="基线",CHG/BASE*100,NULL,missing = CHG/BASE*100))
tr6<-tr5[order(tr5$USUBJID, tr5$PARAMN, tr5$TRLNKID, tr5$ADT, tr5$AVISITN, na.last = FALSE),]

ADSL_com<-read_spec_sheet(adam_spec_file,"ADSL")%>%subset(`Common Variable`=="Y"|`Variable Name`%in% c("STUDYID","USUBJID","SUBJID","SITEID")) 
ADTR_spec<-read_spec_sheet(adam_spec_file,"ADTR")
adam_vars<-rbind(ADSL_com%>%select(-`Common Variable`),ADTR_spec%>%filter(`Variable Name` %notin% c("STUDYID","USUBJID")))

adtr_final<-tr6%>%out_adam(adam_vars)
adtr<-adam$adtr

compare<-diffdf(adtr_final, adtr)
compare

saveRDS(adtr_final, file = file.path(radam_path, "adtr.rds"))
xportr_write(adtr_final,path = file.path(radam_path, "adtr.xpt"), label = "Tumor/Lesion Results Analysis Dataset")