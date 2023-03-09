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

rm(list=ls())
Sys.setlocale(category = "LC_ALL",locale ='Chinese')
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/adam.R")

### read sdtm datasets ###
sdtm_path<-"D:/Users/machengcheng/Desktop/test/dev/data/sdtm"
sdtm<-read_sas_datasets(sdtm_path)
adam_path<-"D:/Users/machengcheng/Desktop/test/dev/data/adam"
adam<-read_sas_datasets(adam_path)
radam_path<-"D:/Users/machengcheng/Desktop/test/R dev/data/adam"
radam<-read_rds_datasets(radam_path)

adsl<-radam$adsl

ae<-combine_supp(sdtm$ae, sdtm$suppae)  %>% as_tibble()

#Impute missing date function
miss_st_date <- function(var1,var2,var3) {
  case_when(
    nchar(var1) >= 10  ~ convert_dtc_to_dt(str_sub(var1,1,10)),
    nchar(var1) == 7 & (var1!=str_sub(var2,1,7)|is.na(var2))  ~ convert_dtc_to_dt(paste0(str_sub(var1,1,7),"-01")),
    nchar(var1) == 7 & var1==str_sub(var2,1,7)&var1!=str_sub(var3,1,7)  ~ var2,
    nchar(var1) == 7 & var1==str_sub(var2,1,7)&var1==str_sub(var3,1,7)  ~ min(var2,convert_dtc_to_dt(var3)),
    nchar(var1) == 4 & (var1!=str_sub(var2,1,4)|is.na(var2)) ~ convert_dtc_to_dt(paste0(str_sub(var1,1,4),"-01-01")),
    nchar(var1) == 4 & var1==str_sub(var2,1,4)&var1!=str_sub(var3,1,4) ~ var2,
    nchar(var1) == 4 & var1==str_sub(var2,1,4)&var1==str_sub(var3,1,4) ~ min(var2,convert_dtc_to_dt(var3)),
    nchar(var1) ==0 ~ var2
  )
}

miss_flag<- function(var1) {
  case_when(
    nchar(var1) == 10  ~ "",
    nchar(var1) == 7 ~ "D",
    nchar(var1) == 4 ~ "M",
    nchar(var1) ==0 ~ "Y"
  )
}

adae_1 <- ae%>%left_join(adsl,by = c("STUDYID","USUBJID"))%>%
  derive_vars_dtm(new_vars_prefix="AST",dtc  = AESTDTC,highest_imputation = "m",flag_imputation="none")%>%
  derive_vars_dtm(new_vars_prefix="AEN",dtc  = AEENDTC,highest_imputation = "m",flag_imputation="none")%>%
  derive_vars_dtm_to_tm(vars(ASTDTM, AENDTM))%>%
  mutate(DLTSTDTC=if_else(DLT=="Y",AESTDTC,NULL),
         AESISYFL=if_else(AESI=="是"&AESITYP%in%c("细胞因子释放综合征", "神经毒性")&AEDECOD%notin%c("细胞因子释放综合征", "免疫效应细胞相关性神经毒性综合征"),"Y",NULL),
         ASTDT=miss_st_date(AESTDTC,TRTSDT,AEENDTC),
         ASTDTF=if_else(nchar(ASTDT)>0,miss_flag(AESTDTC),NULL),
         AENDT_=if_else(nchar(AEENDTC)>0,convert_dtc_to_dt(impute_dtc_dt(dtc = AEENDTC,highest_imputation = "M",date_imputation = "last")),NULL),
         AENDT=if_else(AENDT_>DTHDT&complete.cases(DTHDT),DTHDT,AENDT_),
         AENDTF=if_else(nchar(AEENDTC)>0&nchar(AENDT)>0,miss_flag(AEENDTC),NULL),
         ASTDY=if_else(as.numeric(ASTDT - TRTSDT)<0,as.numeric(ASTDT - TRTSDT),as.numeric(ASTDT - TRTSDT)+1),
         AENDY=if_else(as.numeric(AENDT - TRTSDT)<0,as.numeric(AENDT - TRTSDT),as.numeric(AENDT - TRTSDT)+1),
         mindata=if_else(TRTEDT+90>SATSDT&complete.cases(SATSDT),SATSDT,TRTEDT+90)
         )%>%restrict_derivation(
           derivation = derive_var_extreme_flag,
           args = params(
             by_vars = vars(USUBJID, AESPID),
             order = vars(ASTDT,ASTDTM),
             new_var = ABLFL,
             mode = "last"
           ),
           filter = (is.na(ASTDTM)&ASTDT<=TRTSDT)|(!is.na(ASTDTM)&ASTDTM<=TRTSDTM)
         )%>%derive_var_base(
           by_vars = vars(USUBJID, AESPID),
           source_var = AETOXGR,
           new_var = BTOX,
           filter=ABLFL == "Y"
         )%>%
  mutate(TRTEMFL=if_else((complete.cases(ASTDTM)&complete.cases(TRTSDTM)&TRTSDTM<=ASTDTM&((ASTDT<=mindata&(AETOXGR>BTOX|is.na(BTOX)))|AERELCAR=="有关"))|
                         (!complete.cases(ASTDTM)&complete.cases(TRTSDT)&TRTSDT<=ASTDT&((ASTDT<=mindata&(AETOXGR>BTOX|is.na(BTOX)))|AERELCAR=="有关")) ,"Y",NULL))%>%
  group_by(USUBJID,AESPID) %>% 
  mutate(first_cr_vis = min_cond(var = AESTDTC, cond = AEDECOD == "细胞因子释放综合征"),
         last_pr_vis = max_cond(var = AEENDTC, cond = AEDECOD == "细胞因子释放综合征"),
         CRSDUR = as.numeric(convert_dtc_to_dt(last_pr_vis) - convert_dtc_to_dt(first_cr_vis)+1)
         )%>%ungroup()%>%arrange(USUBJID,AESEQ)

adam_spec_file<-"D:/Users/machengcheng/Desktop/test/doc/adam/BM2L201910 ADaM Dataset Specifications v0.3.xlsx"

ADSL_com<-read_spec_sheet(adam_spec_file,"ADSL")%>%subset(`Common Variable`=="Y"|`Variable Name`%in% c("STUDYID","USUBJID","SUBJID","SITEID")) 
ADAE_spec<-read_spec_sheet(adam_spec_file,"ADAE")

adam_vars<-rbind(ADSL_com%>%select(-`Common Variable`),ADAE_spec%>%filter(`Variable Name` %notin% c("STUDYID","USUBJID")))

adae_final<-adae_1%>%out_adam(adam_vars)
adae<-adam$adae
compare<-diffdf(adae_final, adae)
compare

saveRDS(adae_final, file = file.path(radam_path, "adae.rds"))
xportr_write(adae_final,path = file.path(radam_path, "adae.xpt"), label = "Adverse Events Analysis Dataset")
