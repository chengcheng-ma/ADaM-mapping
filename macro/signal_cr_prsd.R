signal_cr_prsd <- function(dataset,
                           order,
                           msg = "Dataset contains CR records followed by PR.",
                           subject_keys = get_admiral_option("subject_keys"),
                           check_type = "warning") {
  assert_character_scalar(msg)
  assert_vars(subject_keys)
  assert_character_scalar(check_type, values = c("message", "warning", "error"))
  
  cr_data <- filter_confirmation(
    dataset,
    by_vars = subject_keys,
    order = order,
    join_vars = vars(AVALC),
    join_type = "after",
    filter = AVALC == "CR" & AVALC.join %in% c("PR","SD") 
  )
  
  if (nrow(cr_data) > 0) {
    prsd_data <- filter_confirmation(
      dataset,
      by_vars = subject_keys,
      order = order,
      join_vars = vars(AVALC),
      join_type = "before",
      filter = AVALC %in% c("PR","SD") & AVALC.join == "CR"
    )
    
    cr_prsd_data <- bind_rows(cr_data, prsd_data) %>%
      arrange(!!!subject_keys, !!!order)
    
    admiralonco_environment$cr_prsd <- cr_prsd_data
    full_msg <- paste0(
      msg,
      "\nRun `get_crpr_dataset()` to access the CR records records followed by PR or SD"
    )
    msg_funs <- list(message = inform, warning = warn, error = abort)
    msg_funs[[check_type]](full_msg)
    
  }
}

get_crpr_dataset <- function() {
  admiralonco_environment$cr_prsd
}