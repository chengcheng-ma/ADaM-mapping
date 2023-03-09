/****************************************************************************
Macro Name:                     out_adam.sas
Description:                    convert final dataset to standard sdtm dataset format and xpt files
Author:                         Chengcheng Ma
Creation date:                  13-SEP-2022
Parameter:                      domain - adam domain name
                                in = input dataset
Macros:                         %read_sheet,%resize_xpt,%c_nobs
Modification history:
******************************************************************************/
%macro out_adam(domain=,in=);
%c_nobs(ds=&in.);
%global in_nobs;
%let in_nobs=&nobs;
*read adam excel sheet;
%read_sheet(type=adam,sheet=&domain.,template_name=template);

*read content sheet;
proc import out = content
	 datafile = "&adam_spec."
		dbms = xlsx replace;
		getnames = yes;
		range="CONTENT$A6:"n;
run;

data order;
	 set content;
	 where dataset_name=upcase("&domain.");
	 sort_variable=compress(sort_order_variables_of_dataset,',');
run;

*get keep list and order list macro variables;
proc sql noprint;
  select variable_name into : keeplist_all separated by " "
  from spec_&domain.;
  select dataset_description into:main_label
  from order;
quit;

%put &keeplist_all.;
%put &main_label.;

data final;
	 set template &in. ;
run;

*add common variables;
%if &domain^=adsl %then %do;

%read_sheet(type=adam,sheet=adsl,template_name=template_adsl);

proc sql noprint;
  select variable_name into : keeplist_com separated by " "
  from spec_adsl
  where common_variable="Y";
quit;

%put &keeplist_com.;

proc sort data=final;
  by usubjid;
run;

data final1;
  merge final(in=a) adam.adsl(in=b);
  by usubjid;
  if a;
run;

data final;
  retain studyid usubjid subjid siteid;
	 set template_adsl(keep=&keeplist_com.) template final1;
run;

data v_adam.&domain.(label="&main_label."); 
  set final;
  keep &keeplist_com. &keeplist_all.;
run;

%end;
%else %do;

data v_adam.&domain.(label="&main_label."); 
  set final;
  keep &keeplist_all. ;
run;
%end;

%resize_xpt(libname=v_adam,data=&domain,pathname=adam);

proc datasets lib=work noprint;
	 delete fmt_: spec_ content;  
run;
%mend out_adam;
