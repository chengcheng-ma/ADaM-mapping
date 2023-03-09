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

Sys.setlocale(category = "LC_ALL",locale ='Chinese')
source("D:/Users/machengcheng/Desktop/test/R dev/macro/adam/adam.R")

### read sdtm datasets ###
adam_spec_file<-"D:/Users/machengcheng/Desktop/test/doc/adam/BM2L201910 ADaM Dataset Specifications v0.3.xlsx"
sdtm_path<-"D:/Users/machengcheng/Desktop/test/dev/data/sdtm"
sdtm<-read_sas_datasets(sdtm_path)
adam_path<-"D:/Users/machengcheng/Desktop/test/dev/data/adam"
adam<-read_sas_datasets(adam_path)

### define format ###
format_agegr1 <- function(var_input) {
  case_when(
    var_input < 40 ~ "< 40",
    40 <= var_input & var_input <= 49 ~ "40-49",
    50 <= var_input & var_input <= 59 ~ "50-59",
    var_input >= 60 ~ ">= 60"
  )
}

format_agegrn1 <- function(var_input) {
  case_when(
    var_input == "< 40"  ~ 1,
    var_input == "40-49"  ~ 2,
    var_input == "50-59"  ~ 3,
    var_input == ">= 60"  ~ 4
  )
}

format_trt <- function(var_input) {
  case_when(
    var_input == "CAR-T 0.5×10^6 cells/kg"  ~ 1,
    var_input == "CAR-T 1×10^6 cells/kg"  ~ 2,
    var_input == "CAR-T 2×10^6 cells/kg"  ~ 3,
    var_input == "CAR-T 3×10^6 cells/kg"  ~ 4,
    var_input == "CAR-T 6×10^6 cells/kg"  ~ 5
  )
}

### derive dm dataset message ###
dm<-sdtm$dm
adsl_dm <- dm %>%
  select(-DOMAIN)%>%
  derive_vars_dt(new_vars_prefix="BRTH",dtc  = BRTHDTC)%>%
  derive_vars_dt(new_vars_prefix="RFIC",dtc  = RFICDTC)%>%
  derive_vars_dt(new_vars_prefix="LSTALV",dtc  = RFPENDTC)%>%
  mutate(TRT01P = ARM, TRT01A = ACTARM,
         AGEGR1 = format_agegr1(AGE),
         AGEGR1N = format_agegrn1(AGEGR1),
         TRT01PN=format_trt(TRT01P),
         TRT01AN=format_trt(TRT01A))%>%
  derive_vars_dtm(new_vars_prefix="TRTS",dtc  = RFSTDTC,highest_imputation = "m",flag_imputation="none")%>%
  derive_vars_dtm(new_vars_prefix="TRTE",dtc  = RFENDTC,highest_imputation = "m",flag_imputation="none")%>%
  derive_vars_dtm_to_dt(vars(TRTSDTM, TRTEDTM))%>%
  derive_vars_dtm_to_tm(vars(TRTSDTM, TRTEDTM))

if(!("RACEOTH" %in% colnames(dm))){
  adsl_dm<-adsl_dm%>%mutate(RACEOTH="")
}
#Derive Death Variables
death_date <- function(var1,var2,var3,var4) {
  case_when(
    nchar(var1) == 10  ~ convert_dtc_to_dt(var1),
    nchar(var1) == 7 & var1==str_sub(var2,1,7) ~ var3+1,
    nchar(var1) == 7 & var1!=str_sub(var2,1,7)  ~ convert_dtc_to_dt(paste0(str_sub(var1,1,7),"-01")),
    nchar(var1) == 4 & var1==str_sub(var2,1,4) ~ var3+1,
    nchar(var1) == 4 & var1!=str_sub(var2,1,4)  ~ convert_dtc_to_dt(paste0(str_sub(var1,1,4),"-01-01")),
    nchar(var1) ==0 & var4=="Y"~ var3+1
  )
}

death_miss<- function(var1,var2) {
  case_when(
    nchar(var1) == 10  ~ "",
    nchar(var1) == 7 ~ "D",
    nchar(var1) == 4 ~ "M",
    nchar(var1) ==0 & var2=="Y"~ "Y"
  )
}

dd<-sdtm$dd%>%mutate(DDSTRESC_p=str_split(DDSTRESC,"：",simplify = T)[,1])
adsl_death <- adsl_dm %>%
  mutate(DTHDT =death_date(DTHDTC,RFPENDTC,LSTALVDT,DTHFL),
         DTHDTF=death_miss(DTHDTC,DTHFL))%>%
  derive_vars_merged(
      dataset_add = dd,
      by_vars = vars(STUDYID, USUBJID),
      new_vars = vars(DTHCAUS = DDSTRESC_p),
      filter_add = DDTESTCD=="PRCDTH"
) 

### Derive Disposition Variables###

# convert character date to numeric date without imputation
ds_ext <- derive_vars_dt(
  sdtm$ds,
  dtc = DSSTDTC,
  new_vars_prefix = "DSST"
)

#Derive EOSDT and EOSTT
adsl_ds1 <- adsl_death %>%
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOSDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSSCAT == "TRIAL"
  ) %>% 
  derive_vars_merged(
    dataset_add = ds_ext,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(EOTDT = DSSTDT),
    filter_add = DSCAT == "DISPOSITION EVENT" & DSSCAT == "TREATMENT"
  ) 

#define Reason format
format_eosstt <- function(DSDECOD) {
  case_when(
    DSDECOD %in% c("COMPLETED") ~ "COMPLETED",
    !is.na(DSDECOD) ~ "DISCONTINUED",
    TRUE ~ "ONGOING"
  )
}

#Derive EOSSTT, DCSREAS, EOTSTT, DCTREAS
adsl_ds2 <- adsl_ds1 %>%
  derive_var_disposition_status(
    dataset_ds = ds_ext,
    new_var = EOSSTT,
    status_var = DSDECOD,
    format_new_var = format_eosstt,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSSCAT == "TRIAL"
  )%>%
  derive_vars_disposition_reason(
    dataset_ds = ds_ext,
    new_var = DCSREAS,
    reason_var = DSDECOD,
    new_var_spe = DCSREASP,
    reason_var_spe = DSTERM,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSSCAT=="TRIAL" & DSDECOD!="COMPLETED" & !is.na(DSDECOD)
  ) %>%
  derive_var_disposition_status(
    dataset_ds = ds_ext,
    new_var = EOTSTT_,
    status_var = DSDECOD,
    format_new_var = format_eosstt,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSSCAT == "TREATMENT"
  ) %>% mutate(EOTSTT=if_else(!is.na(TRTSDT)|EOTSTT_!="ONGOING",EOTSTT_,NULL)) %>% 
  derive_vars_disposition_reason(
    dataset_ds = ds_ext,
    new_var = DCTREAS,
    reason_var = DSDECOD,
    new_var_spe = DCTREASP,
    reason_var_spe = DSTERM,
    filter_ds = DSCAT == "DISPOSITION EVENT" & DSSCAT=="TREATMENT" & DSDECOD!="COMPLETED" & !is.na(DSDECOD)
  )

#Derived SATSDT 
cm_pr<-rbind(sdtm$pr%>%subset(PRCAT=="后续抗肿瘤治疗")%>%mutate(CMSTDTC=PRSTDTC)%>%select(USUBJID,CMSTDTC),
             sdtm$cm%>%subset(CMCAT=="后续抗肿瘤治疗")%>%select(USUBJID,CMSTDTC))%>%
       mutate(CMDTC=str_sub(CMSTDTC, 1,10))%>%select(USUBJID,CMDTC)

cm_pr_ext <- cm_pr %>%
  derive_vars_dt(
    dtc = CMDTC,
    new_vars_prefix = "CM"
  )%>%distinct(USUBJID,CMDTC,.keep_all = T)

adsl_dt1 <- adsl_ds2 %>%
  derive_vars_merged(
    dataset_add = cm_pr_ext,
    new_vars = vars(SATSDT = CMDT),
    order = vars(CMDT),
    mode = "first",
    by_vars = vars(USUBJID)
  )


#Derived PDDT
tr<-combine_supp(sdtm$tr, sdtm$supptr)  %>% as_tibble()
rs<-combine_supp(sdtm$rs, sdtm$supprs)  %>% as_tibble()
tr1<-tr%>%subset(TRLNKID!=""& VISITNUM==floor(VISITNUM)&TRDTC!="")%>%
  group_by(USUBJID, VISITNUM, VISIT) %>%
  summarise(min_tr = min(TRDTC),.groups = "keep")
tr2<-tr%>%subset(RUNSVIS!=""&TRSTAT!="NOT DONE")%>%
  group_by(USUBJID, RUNSVIS) %>%
  summarise(min_utr = min(TRDTC),.groups = "keep")
rs_tr<-subset(rs,RSTESTCD=="OVRLRESP")%>%
  left_join(tr1,by = c("USUBJID","VISITNUM","VISIT"))%>%
  left_join(tr2,by = c("USUBJID","RUNSVIS"))%>%
  subset(RSSTRESC=="疾病进展（PD）")%>%
  mutate(mintrdtc=if_else(!is.na(min_tr), min_tr, min_utr))%>%
  subset(!is.na(mintrdtc))%>%
  derive_vars_dt(
    dtc = mintrdtc,
    new_vars_prefix = "mintr"
  )%>%distinct(USUBJID,mintrdtc,.keep_all = T)

adsl_dt2 <- adsl_dt1 %>%
  derive_vars_merged(
    dataset_add = rs_tr,
    new_vars = vars(PDDT = mintrDT),
    order = vars(mintrDT),
    mode = "first",
    by_vars = vars(USUBJID)
  )

#derive Baseline variables
adsl_bl <- adsl_dt2 %>%
  derive_vars_merged(
  dataset_add = sdtm$vs,
  by_vars = vars(STUDYID, USUBJID),
  new_vars = vars(HEIGHTBL = VSSTRESN),
  filter_add = VSTESTCD=="HEIGHT" & VSBLFL=="Y"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(WEIGHTBL = VSSTRESN),
    filter_add = VSTESTCD=="WEIGHT" & VSBLFL=="Y"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$vs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(BMIBL = VSSTRESN),
    filter_add = VSTESTCD=="BMI" & VSBLFL=="Y"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$qs,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(ECOGBL = QSSTRESN),
    filter_add = QSTESTCD=="ECOG101" & QSBLFL=="Y"
  )
#derive fa variables
adsl_fa <-adsl_bl %>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(HISTCLAS = FASTRESC),
    filter_add = FATESTCD=="HISTCLAS" & FAOBJ=="胃癌"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(RCMT = FASTRESC),
    filter_add = FATESTCD=="RCMT" & FAOBJ=="胃癌"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(CTNMSTG = FASTRESC),
    filter_add = FATESTCD=="CTNMSTG" & FAOBJ=="胃癌"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(PTNMSTG = FASTRESC),
    filter_add = FATESTCD=="PTNMSTG" & FAOBJ=="胃癌"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(YPTNMSTG = FASTRESC),
    filter_add = FATESTCD=="YPTNMSTG" & FAOBJ=="胃癌"
  )%>%
  derive_vars_merged(
    dataset_add = sdtm$fa,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(MTORG = FASTRESC),
    filter_add = FATESTCD=="MTORGAN" & FAOBJ=="胃癌"
  )%>%
  mutate(MTORGNUM=if_else(nchar(MTORG)>0,str_count(MTORG,";")+1,0))

#derive cm variables
cm<-combine_supp(sdtm$cm, sdtm$suppcm)  %>% as_tibble()%>%
    mutate(line=if_else(!is.na(str_extract(CMLINE, "\\d")),as.numeric(str_extract(CMLINE, "\\d")),0))
cm_1<-cm%>%
    group_by(STUDYID,USUBJID) %>%
    summarise(max_line = max(line),.groups = "keep")%>%ungroup()
cm_2<-distinct(subset(cm,CMRCMT=="否"),USUBJID,.keep_all = T)%>%select(STUDYID,USUBJID,CMRCMT)
adsl_cm <-adsl_fa %>%
  derive_vars_merged(
    dataset_add = cm_1,
    by_vars = vars(STUDYID,USUBJID),
    new_vars = vars(RMTLINE =max_line)
    )%>%left_join(cm_2,by = c("STUDYID","USUBJID"))%>%
    mutate(TLINE=if_else(CMRCMT=="否",RMTLINE+1,RMTLINE,RMTLINE))

#derive lb variables
adsl_lb <-adsl_cm %>%
  derive_vars_merged(
    dataset_add = sdtm$lb,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(CLAUD18 = LBSTRESC),
    filter_add = LBCAT=="CLAUDIN18.2" & LBTESTCD=="CLAUD18"
  ) %>%
  derive_vars_merged(
    dataset_add = sdtm$lb,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(CL18PER = LBSTRESC),
    filter_add = LBCAT=="CLAUDIN18.2" & LBTESTCD=="CL18PER"
  ) %>%
  derive_vars_merged(
    dataset_add = sdtm$lb,
    by_vars = vars(STUDYID, USUBJID),
    new_vars = vars(CL18DYE = LBSTRESC),
    filter_add = LBCAT=="CLAUDIN18.2" & LBTESTCD=="CL18DYE"
  )

#derive FD2FCRS variable
ae<-combine_supp(sdtm$ae, sdtm$suppae)  %>% as_tibble()
ae1<-ae%>%subset(AEDECOD=="细胞因子释放综合征")%>%
  mutate(date=convert_dtc_to_dt(str_sub(AESTDTC,1,10)))%>%
  group_by(STUDYID,USUBJID) %>%
  summarise(fcrsstdt = min(date),.groups = "keep")%>%select(STUDYID,USUBJID,fcrsstdt)
adsl_ae<-adsl_lb %>%
  left_join(ae1,by = c("STUDYID","USUBJID"))%>%
  mutate(FD2FCRS=if_else(!is.na(fcrsstdt)&!is.na(TRTSDT),as.numeric(fcrsstdt-TRTSDT+1),NULL))


###derive flag variables###

#derive FASFL SAFFL CONDRGFL APHFL PDFL
adsl_fl1 <- adsl_ae %>%
  derive_var_merged_exist_flag(
    dataset_add = sdtm$pr,
    by_vars = vars(STUDYID, USUBJID),
    new_var = FASFL,
    condition = (PRCAT=="单采成分血" & PROCCUR=="Y"),
    false_value = "N",
    missing_value = "N"
  ) %>%
  mutate(SAFFL = if_else(!is.na(TRTSDTM), "Y", "N")) %>%
  derive_var_merged_exist_flag(
    dataset_add = sdtm$ec,
    by_vars = vars(STUDYID, USUBJID),
    new_var = CONDRGFL,
    condition = (ECCAT=="化疗预处理" & ECMOOD=="PERFORMED" & ECOCCUR=="Y"),
    false_value = "N",
    missing_value = "N"
  ) %>%
  derive_var_merged_exist_flag(
    dataset_add = sdtm$pr,
    by_vars = vars(STUDYID, USUBJID),
    new_var = APHFL,
    condition = (PRCAT=="单采成分血" & PROCCUR=="Y"),
    false_value = "N",
    missing_value = "N"
  ) %>% mutate(PDFL = if_else(!is.na(PDDT), "Y", "N"))


#derive BTFL
cm1<-distinct(subset(sdtm$cm,CMINDC=="桥接治疗")%>%select(STUDYID,USUBJID, CMINDC),USUBJID,.keep_all = T)
pr1<-distinct(subset(sdtm$pr,PRINDC=="桥接治疗")%>%select(STUDYID,USUBJID, PRINDC),USUBJID,.keep_all = T)
adsl_fl2<-adsl_fl1 %>% 
  derive_var_merged_exist_flag(
    dataset_add =full_join(cm1,pr1, by = c("STUDYID","USUBJID")),
    by_vars = vars(STUDYID, USUBJID),
    new_var = BTFL,
    condition = (CMINDC=="桥接治疗" | PRINDC=="桥接治疗"),
    false_value = "N",
    missing_value = "N"
  )

#finction to get subject has baseline and at lease one post-baseline
get_b_pb_fl<- function(dataset,filter_add,dtc_var,out){
filter_add <- assert_filter_cond(enquo(filter_add), optional = TRUE)
dtc_var1<-assert_symbol(enquo(dtc_var))
base_df<-left_join(filter_if(dataset,filter_add),
                   sdtm$dm%>%select(STUDYID,USUBJID, RFSTDTC),by = c("STUDYID","USUBJID"))%>% 
  select(STUDYID,USUBJID,RFSTDTC,!!dtc_var1) %>% 
  mutate(d_var=str_sub(!!dtc_var1, 1,10)) %>% 
  subset(nchar(RFSTDTC)>=10 & expr(nchar(!!dtc_var1))>=10 &d_var<=str_sub(RFSTDTC, 1,10)) %>%
  distinct(USUBJID,.keep_all = T) %>% 
  mutate(blfl="Y")%>%
  select(STUDYID,USUBJID,blfl)

post_df<-left_join(filter_if(dataset,filter_add),
                   sdtm$dm%>%select(STUDYID,USUBJID, RFSTDTC),by = c("STUDYID","USUBJID"))%>% 
  select(STUDYID,USUBJID,RFSTDTC,!!dtc_var1) %>% 
  mutate(d_var=str_sub(!!dtc_var1, 1,10)) %>% 
  subset(nchar(RFSTDTC)>=10 & expr(nchar(!!dtc_var1))>=10 &d_var>str_sub(RFSTDTC, 1,10)) %>%
  distinct(USUBJID,.keep_all = T) %>% 
  mutate(pblfl="Y")%>%
  select(STUDYID,USUBJID,pblfl)
if (out=="base_post") {
  df<-full_join(base_df,post_df, by = c("STUDYID","USUBJID"))
}else if(out=="base") {
  df<-base_df  
}else if(out=="post") {
  df<-post_df   
}
return(df)
}

xs_final<-get_b_pb_fl(dataset=sdtm$xs,filter_add = XSCAT=="药代动力学采样-ADA样本" & XSSTRESC=="是",dtc_var=XSDTC,out="base_post")
tr_final<-get_b_pb_fl(dataset=sdtm$tr,dtc_var=TRDTC,out="base_post")
xs_final2<-get_b_pb_fl(dataset=sdtm$xs,filter_add = XSCAT %in% c("药代动力学采样-CAR阳性T细胞的PK血样", "药代动力学采样-CAR转基因水平PK血样") & XSSTRESC=="是",dtc_var=XSDTC,out="post")

rs<-distinct(subset(sdtm$rs, RSDY>1 & RSORRES!="")%>%select(STUDYID,USUBJID),USUBJID,.keep_all = T)%>%mutate(rspblfl="Y")

adsl_fl3<-adsl_fl2 %>% 
  left_join(xs_final,by = c("STUDYID","USUBJID"))%>% 
  mutate(IMMFL = if_else(SAFFL=="Y"&blfl=="Y"&pblfl=="Y", "Y", "N","N"))%>%select(-blfl,-pblfl)%>%
  left_join(xs_final2,by = c("STUDYID","USUBJID"))%>%
  mutate(PKFL = if_else(SAFFL=="Y"&pblfl=="Y", "Y", "N","N"))%>%select(-pblfl)%>%
  left_join(tr_final,by = c("STUDYID","USUBJID"))%>%
  left_join(rs,by = c("STUDYID","USUBJID"))%>%
  mutate(MEEFL = if_else(SAFFL=="Y"&blfl=="Y"&(pblfl=="Y"|rspblfl=="Y"), "Y", "N","N"),
         EEFL = if_else(SAFFL=="Y"&blfl=="Y"&(pblfl=="Y"|rspblfl=="Y"|(DTHDT>=TRTEDT & DTHDT<=TRTSDT+47)), "Y", "N","N"))%>%
  select(-rspblfl,-blfl,-pblfl)

#derive DLTFL variable
adsl_fl4<-adsl_fl3 %>%
  left_join(sdtm$ae%>%subset(AESTDY>=30)%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(ae30d='Y'),by ="USUBJID")%>%
  left_join(sdtm$lb%>%subset(!is.na(LBORRES)&LBDY>=30)%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(lb30d='Y'),by ="USUBJID")%>%
  left_join(sdtm$vs%>%subset(!is.na(VSORRES)&VSDY>=30)%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(vs30d='Y'),by ="USUBJID")%>%
  left_join(sdtm$eg%>%subset(!is.na(EGORRES)&EGDY>=30)%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(eg30d='Y'),by ="USUBJID")%>%
  left_join(sdtm$qs%>%subset(!is.na(QSORRES)&QSDY>=30)%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(qs30d='Y'),by ="USUBJID")%>%
  left_join(ae%>%subset(DLT=="Y")%>%
              distinct(USUBJID,.keep_all = F)%>%mutate(dlt='Y'),by ="USUBJID")%>%
  mutate(DLTFL=if_else(SAFFL=="Y"&(dlt=='Y'|ae30d=='Y'|lb30d=='Y'|vs30d=='Y'|eg30d=='Y'|qs30d=='Y'),"Y","N","N"))

### read spec ###
ADSL_spec <- read_spec_sheet(adam_spec_file,"ADSL")

  
adsl_final<-adsl_fl4%>%out_adam(ADSL_spec)
adsl<-adam$adsl
compare<-diffdf(adsl_final, adsl)
compare

radam_path <- "D:/Users/machengcheng/Desktop/test/R dev/data/adam"
#save adsl.sas7bdat format
write_sas(adsl_final, path = file.path(radam_path, "adsl.sas7bdat"))
#library(foreign)
#write.foreign(df = adsl, datafile = 'D:/Users/machengcheng/Desktop/test/R dev/data/adam/adsl.sas7bdat', codefile = 'adsl.txt', package = "SAS")

#save rds format
saveRDS(adsl_final, file = file.path(radam_path, "adsl.rds"))

#save xpt format
xportr_write(adsl_final,path = file.path(radam_path, "adsl.xpt"), label = "Subject-Level Analysis Dataset")