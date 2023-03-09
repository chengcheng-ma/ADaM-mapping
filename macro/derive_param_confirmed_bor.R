derive_param_confirmed_bor <- function(dataset,
                                       dataset_adsl,
                                       filter_source,
                                       source_pd = NULL,
                                       source_datasets = NULL,
                                       reference_date,
                                       ref_start_window,
                                       ref_confirm,
                                       max_nr_ne = 1,
                                       accept_sd = FALSE,
                                       missing_as_ne = FALSE,
                                       desc_adt=T,
                                       aval_fun = aval_resp,
                                       set_values_to,
                                       subject_keys = get_admiral_option("subject_keys")) {
  # Check input parameters
  filter_source <- assert_filter_cond(enquo(filter_source))
  reference_date <- assert_symbol(enquo(reference_date))
  assert_integer_scalar(ref_start_window, subset = "non-negative")
  assert_integer_scalar(ref_confirm, subset = "non-negative")
  assert_integer_scalar(max_nr_ne, subset = "non-negative")
  assert_logical_scalar(accept_sd)
  assert_logical_scalar(missing_as_ne)
  assert_varval_list(set_values_to, required_elements = "PARAMCD")
  assert_vars(subject_keys)
  assert_data_frame(dataset,
                    required_vars = quo_c(subject_keys, reference_date, vars(PARAMCD, ADT, AVALC))
  )
  assert_data_frame(dataset_adsl, required_vars = subject_keys)
  assert_param_does_not_exist(dataset, quo_get_expr(set_values_to$PARAMCD))
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if (!is.null(source_pd)) {
    # Restrict input dataset
    source_data <- dataset %>%
      filter_pd(
        filter = !!filter_source,
        source_pd = source_pd,
        source_datasets = source_datasets,
        subject_keys = subject_keys
      )
  } else {
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # filter_source: Filter using filter_source argument ----
    # This would also be used to filter out records from dataset that are greater
    # than e.g. ADSL.TRTSDT
    # Not filtering data after progressive disease with filter_pd
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    source_data <- dataset %>%
      filter(!!filter_source)
  }
  
  # Check for invalid AVALC values
  resp_vals <- source_data$AVALC
  valid_vals <- c("CR", "PR", "SD", "NON-CR/NON-PD", "PD", "NE", "ND")
  invalid_vals <- unique(resp_vals[!resp_vals %in% valid_vals])
  if (length(invalid_vals) > 0) {
    abort(
      paste0(
        "The function is considering only the following response values:\n",
        enumerate(valid_vals),
        "\nThe following invalid values were found:\n",
        enumerate(invalid_vals)
      )
    )
  }
  
  # Check for CR followed by PR or SD (this might data issue)
  
  signal_cr_prsd(
    source_data,
    order = vars(ADT),
    subject_keys = subject_keys
  )
  
  cr_prsd_data<-get_crpr_dataset()
  
  
  # Create observations for potential responses
  cr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC, ADT),
    join_type = "after",
    order = vars(ADT),
    first_cond = AVALC.join == "CR" &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "CR" &
      all(AVALC.join %in% c("CR", "NE")) &
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne
  ) %>%
    mutate(tmp_order = 1)
  
  if (accept_sd) {
    max_nr_sd <- 1
  } else {
    max_nr_sd <- 0
  }
  pr_data <- filter_confirmation(
    source_data,
    by_vars = subject_keys,
    join_vars = vars(AVALC, ADT),
    join_type = "after",
    order = vars(ADT),
    first_cond = AVALC.join %in% c("CR", "PR") &
      ADT.join >= ADT + days(ref_confirm),
    filter = AVALC == "PR" &
      all(AVALC.join %in% c("CR", "PR", "SD", "NE")) &
      count_vals(var = AVALC.join, val = "NE") <= max_nr_ne &
      count_vals(var = AVALC.join, val = "SD") <= max_nr_sd &
      (
        min_cond(
          var = ADT.join,
          cond = AVALC.join == "CR"
        ) > max_cond(var = ADT.join, cond = AVALC.join == "PR") |
          count_vals(var = AVALC.join, val = "CR") == 0 |
          count_vals(var = AVALC.join, val = "PR") == 0
      )
  ) %>%
    mutate(tmp_order = 2)
  
  sd_data <- filter(
    source_data,
    AVALC %in% c("CR", "PR", "SD") & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "SD",
      tmp_order = 3
    )
  
  non_data <- filter(
    source_data,
    AVALC == "NON-CR/NON-PD" & ADT >= !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      tmp_order = 4
    )
  
  pd_data <- filter(source_data, AVALC == "PD") %>%
    mutate(tmp_order = 5)
  
  ne_data <- filter(
    source_data,
    AVALC == "NE" | AVALC %in% c("CR", "PR", "SD", "NON-CR/NON-PD") &
      ADT < !!reference_date + days(ref_start_window)
  ) %>%
    mutate(
      AVALC = "NE",
      tmp_order = 6
    )
  
  nd_data <- filter(
    source_data,
    AVALC == "ND"
  ) %>%
    mutate(
      AVALC = "ND",
      tmp_order = 7
    )
  
  if (missing_as_ne) {
    missing_val <- "NE"
  } else {
    missing_val <- "MISSING"
  }
  
  source_vars <- colnames(source_data)
  adsl_vars <- colnames(dataset_adsl)
  
  missing_data <- dataset_adsl %>%
    select(!!!subject_keys, intersect(source_vars, adsl_vars)) %>%
    mutate(
      AVALC = missing_val,
      tmp_order = 8
    )
  
  if (!is_null(cr_prsd_data)) {
    cr_pd_data <- filter(
      cr_prsd_data,
      AVALC %in% c("PR", "SD") &
        ADT < !!reference_date + days(ref_start_window)
    ) %>%
      mutate(
        AVALC = "PD",
        tmp_order = 5
      )
    bor <- bind_rows(cr_data, pr_data, sd_data, pd_data, cr_pd_data,non_data, ne_data, nd_data, missing_data) 
  }else{
    bor <- bind_rows(cr_data, pr_data, sd_data, pd_data,non_data, ne_data, nd_data, missing_data) 
  }
  
  # Select best response
  if (isTRUE(desc_adt)){
    bor_filter <- bor %>%
      filter_extreme(
        by_vars = subject_keys,
        order = vars(tmp_order, desc(ADT)),
        mode = "first"
      ) %>%
      select(-tmp_order) %>%
      mutate(
        !!!set_values_to
      ) %>%
      call_aval_fun(
        aval_fun
      )
  }else{
    bor_filter <- bor %>%
      filter_extreme(
        by_vars = subject_keys,
        order = vars(tmp_order, ADT),
        mode = "first"
      ) %>%
      select(-tmp_order) %>%
      mutate(
        !!!set_values_to
      ) %>%
      call_aval_fun(
        aval_fun
      )     
  }
  
  
  # Add to input dataset
  bind_rows(dataset, bor_filter)
}