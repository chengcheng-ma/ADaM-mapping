derive_param_bor <- function(dataset,
                             dataset_adsl,
                             filter_source,
                             source_pd = NULL,
                             source_datasets = NULL,
                             reference_date,
                             ref_start_window,
                             missing_as_ne = FALSE,
                             aval_fun = aval_resp,
                             desc_adt=FALSE,
                             set_values_to,
                             subject_keys = get_admiral_option("subject_keys")) {
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Assert statements (checked in order of signature) ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  reference_date <- assert_symbol(arg = enquo(reference_date))
  
  assert_vars(arg = subject_keys)
  
  assert_data_frame(
    arg = dataset,
    required_vars = quo_c(
      subject_keys,
      reference_date,
      vars(PARAMCD, ADT, AVALC)
    )
  )
  
  assert_data_frame(
    arg           = dataset_adsl,
    required_vars = quo_c(subject_keys)
  )
  
  filter_source <- assert_filter_cond(arg = enquo(filter_source))
  
  assert_integer_scalar(
    arg      = ref_start_window,
    subset   = "non-negative"
  )
  
  assert_logical_scalar(arg = missing_as_ne)
  
  assert_varval_list(
    arg               = set_values_to,
    required_elements = c("PARAMCD")
  )
  
  assert_param_does_not_exist(
    dataset = dataset,
    param   = quo_get_expr(set_values_to$PARAMCD)
  )
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # filter_pd and filter_source: Filter source dataset using filter_source----
  # argument and also filter data after progressive disease with filter_pd
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  if (!is.null(source_pd)) {
    dataset_filter <- dataset %>%
      filter_pd(
        filter          = !!filter_source,
        source_pd       = source_pd,
        source_datasets = source_datasets,
        subject_keys    = subject_keys
      )
  } else {
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    # filter_source: Filter using filter_source argument ----
    # This would also be used to filter out records from dataset that are greater
    # than e.g. ADSL.TRTSDT
    # Not filtering data after progressive disease with filter_pd
    #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    
    dataset_filter <- dataset %>%
      filter(!!filter_source)
  }
  
  # Error if filter results in 0 records
  if (nrow(dataset_filter) == 0) {
    err_msg <- sprintf(
      "dataframe passed into %s argument with the filter %s has 0 records",
      "dataset",
      deparse(quo_get_expr(filter_source))
    )
    
    abort(err_msg)
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Create Sort Order for Selection of Minimum Later ----
  #
  #   Note:
  #   1. All `CR`, `PR` and `PD` response records are considered for Best Overall
  #      Response.
  #
  #   2. All `SD` or `NON-CR/NON-PD` records where `ADT` >= `reference_date` +
  #     `ref_start_window` are also considered for Best Overall Response.
  #
  #  3. Subjects with **ONLY** `SD` or `NON-CR/NON-PD` records where `ADT` <
  #     `reference_date` + `ref_start_window` would give Best Overall Response
  #     of `NE`.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  dataset_ordered <- dataset_filter %>%
    mutate(
      AVALC = if_else(
        AVALC %in% c("SD","NON-CR/NON-PD") & ADT < !!reference_date + days(ref_start_window),
        "NE",
        AVALC
      ),
      tmp_order = case_when(
        AVALC %in% c("CR") ~ 1,
        AVALC %in% c("PR") ~ 2,
        AVALC %in% c("SD") ~ 3,
        AVALC %in% c("NON-CR/NON-PD") ~ 4,
        AVALC %in% c("PD") ~ 5,
        AVALC %in% c("NE") ~ 6,
        is.null(AVALC) ~ 7
      )
    )
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # adsl only subjects ----
  # Note Requirement: For subjects without observations in the input dataset
  # after the filter is applied, we keep all columns from ADSL which
  # are also in the input dataset.
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  adsl_data <- dataset_adsl %>%
    select(intersect(
      colnames(dataset_adsl),
      colnames(dataset)
    )) %>%
    mutate(
      AVALC = case_when(
        isTRUE(missing_as_ne) ~ "NE",
        TRUE ~ "MISSING"
      ),
      tmp_order = 999
    )
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Bind two types of dataframes and select lowest value as BOR
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if (isTRUE(desc_adt)){
    param_bor <- bind_rows(
      dataset_ordered,
      adsl_data
    ) %>%
      filter_extreme(
        by_vars = subject_keys,
        order = vars(tmp_order, desc(ADT)),
        mode = "first"
      ) %>%
      select(-tmp_order)
  }else{
    param_bor <- bind_rows(
      dataset_ordered,
      adsl_data
    ) %>%
      filter_extreme(
        by_vars = subject_keys,
        order = vars(tmp_order, ADT),
        mode = "first"
      ) %>%
      select(-tmp_order)
  }
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # set_values_to: Execute set_values_to ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  tryCatch(
    param_bor_values_set <- param_bor %>%
      mutate(
        !!!set_values_to
      ),
    error = function(cnd) {
      abort(
        paste0(
          "Assigning new columns with set_values_to has failed:\n",
          "set_values_to = (\n",
          paste(
            " ",
            names(set_values_to),
            "=",
            lapply(set_values_to, quo_get_expr),
            collapse = "\n"
          ),
          "\n)\nError message:\n  ",
          cnd
        )
      )
    }
  )
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # aval_fun(AVALC): Execute aval_fun ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  param_bor_aval_fun <- call_aval_fun(
    param_bor_values_set,
    aval_fun
  )
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  # Bind back to passed dataframe in dataset argument and return ----
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  
  bind_rows(
    dataset,
    param_bor_aval_fun
  )
}