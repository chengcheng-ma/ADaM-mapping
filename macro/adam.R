### read all sas datasets
read_sas_datasets<- function(path){
  fileNames <- dir(path) 
  filePath <- sapply(fileNames, function(x){paste(path,x,sep='/')})   
  fileNames_sas<-str_subset(filePath, ".sas7bdat")
  name <- str_remove_all(str_subset(fileNames, ".sas7bdat"), ".sas7bdat")
  data <- lapply(fileNames_sas, function(x){read_sas(x)}) 
  names(data) <- name
  return(data)
}

### read all rds datasets
read_rds_datasets<- function(path){
  fileNames <- dir(path) 
  filePath <- sapply(fileNames, function(x){paste(path,x,sep='/')})   
  fileNames_sas<-str_subset(filePath, ".rds")
  name <- str_remove_all(str_subset(fileNames, ".rds"), ".rds")
  data <- lapply(fileNames_sas, function(x){readRDS(x)}) 
  names(data) <- name
  return(data)
}

### read adam spec
read_spec_sheet <- function(filename,sheetname){
  sheet<-read_excel(filename, sheet = sheetname,col_names = T)
  if(sheetname =="CONTENT"){
    colnames(sheet)<-sheet[5,]
    spec<-sheet%>%subset(complete.cases(`Dataset Structure`)&`Dataset Structure`!="Dataset Structure")
  }else{
    colnames(sheet)<-sheet[11,]
    spec<-sheet%>%subset(complete.cases(`Variable Label`)&`Variable Label`!="Variable Label")
  }
}

### read code from spec
read_codelist<- function(filename,domain,code){
  codelist<-read_excel(filename, sheet = "Codelist",col_names = F)
  codelist1 <- codelist%>%mutate(var=if_else(!is.na(...3),...1,NULL))%>%
    fill(...3,.direction = "down")%>%
    fill(var,.direction = "down")
  
  codelist2<-codelist1%>%filter(...3==domain&var==code&!is.na(...2))
  colnames(codelist2)<-codelist2[1,]
  codelist3<-codelist2[-1, c(1,2)]
  
  vars<-str_subset(names(codelist3),"N$")
  if(length(vars)>0){
    outdata<-codelist3 %>% mutate(across(all_of(vars),as.numeric))
    outdata
  }else{
    outdata<-codelist3
    outdata
  }
}

### select variables in spec
select_spec_vars <- function(dataset, spec) {
  var_list <- spec$`Variable Name` 
  exist_vars <- names(dataset) %>%
    keep(~ . %in% var_list)
  lost_vars <-var_list%>%discard(~ . %in% exist_vars)
  if (length(lost_vars) > 0) {
    message(paste0("EORROR:The following variable(s) were lost:\n  ",
                   paste0(lost_vars, collapse = "\n  ")))
  }else{
    out <- dataset %>%
      select(all_of(var_list))
    out
  }
}

### remove label and format
remove_lab_format <- function(data) {
  # Check data
  if (!inherits(data, 'data.frame')) stop("Format must be removed from a data.frame or tibble")
  
  map_dfr(data, function(x){
    attr(x, "label") <- NULL
    attr(x, "format.sas") <- NULL
    x
  })
}

### add dataset format
add_format <- function(data, ...) {
  # Pull out ellipsis to list
  args <- rlang::list2(...)
  
  # Check params
  if (!inherits(data, 'data.frame')) stop("Formats must be applied to a data.frame or tibble")
  if (!is_named(args)) stop("Must provide variable name and format as named arguments")
  if (!all(names(args) %in% names(data))) {
    stop("All variable names supplied to format must be variables in data")
  }
  if (!all(map_lgl(args, is.character))) stop("All formats must be character")
  # Iterate the args supplied and update the variable labels in place
  walk2(names(args), args, ~ {attr(data[[.x]], "format.sas") <<- .y})
  data
}

### set variables' labels
set_variable_labels <- function(data, spec) {
  
  # Grab out the var names and labels
  var_spec <- spec %>%mutate(variable=`Variable Name` ,label=`Variable Label`)%>%
    select(variable, label)
  
  
  ns <- var_spec$variable
  labs <- var_spec$label
  dns <- names(data)
  
  # Are there any variables in data not in the metadata
  mismatch <- setdiff(dns, ns)
  in_meta <- ns[which(ns %in% mismatch)]
  in_data <- dns[which(dns %in% mismatch)]
  
  if (length(in_meta) > 0) {
    wrn <- paste0("Variables in metadata not in data:\n\t", paste0(in_meta, collapse="\n\t"))
    warning(wrn, call. = FALSE)
  }
  
  if (length(in_data) > 0) {
    wrn <- paste0("Variables in data not in metadata:\n\t", paste0(in_data, collapse="\n\t"))
    warning(wrn, call. = FALSE)
  }
  
  # Pick out only the variables which exist in both and build list
  match <- intersect(ns, dns)
  ind <- which(ns %in% match)
  
  # Subset and create a named list
  ns <- ns[ind]
  labs <- labs[ind]
  names(labs) <- ns
  labs <- as.list(labs)
  
  # Apply the labels to the data
  args = append(list(data), labs)
  do.call(add_labels, args)
}

### set variables' formats
set_variable_format <- function(data, spec) {
  
  # Grab out the var names and labels
  var_spec <- spec %>%mutate(variable=`Variable Name`,  format=str_extract(`Display Format`, "[A-Z]+"))%>%
    select(variable, format)%>%subset(!is.na(format))
  
  
  ns <- var_spec$variable
  fors <- var_spec$format
  dns <- names(data)
  
  # Pick out only the variables which exist in both and build list
  match <- intersect(ns, dns)
  ind <- which(ns %in% match)
  
  # Subset and create a named list
  ns <- ns[ind]
  fors <- fors[ind]
  names(fors) <- ns
  fors <- as.list(fors)
  
  # Apply the labels to the data
  args = append(list(data), fors)
  do.call(add_format, args)
}

na_as_empty <- function(x){
  ifelse(!is.na(x), x, "")
}

### set character variables from NA to empty
set_character_na_to_empty <- function(data){
  character<-data %>% 
    select_if(is.character)
  vars<-names(character)
  outdata<-data %>% mutate(across(all_of(vars),na_as_empty))
  outdata
}

### out the standard adam dataset
out_adam<-function(data,spec){
  out<-data%>%
    set_character_na_to_empty()%>%
    select_spec_vars(spec)%>%
    remove_lab_format()%>%
    set_variable_labels(spec)%>%
    set_variable_format(spec)
  out
}