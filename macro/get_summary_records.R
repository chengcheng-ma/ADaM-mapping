get_summary_records <- function(dataset,
                                by_vars,
                                filter = NULL,
                                analysis_var,
                                summary_fun,
                                set_values_to = NULL) {
  assert_vars(by_vars)
  analysis_var <- assert_symbol(enquo(analysis_var))
  filter <- assert_filter_cond(enquo(filter), optional = TRUE)
  assert_s3_class(summary_fun, "function")
  assert_data_frame(
    dataset,
    required_vars = quo_c(by_vars, analysis_var),
    check_is_grouped = F
  )
  if (!is.null(set_values_to)) {
    assert_varval_list(set_values_to, optional = TRUE)
  }
  
  # Summarise the analysis value
  dataset %>%
    group_by(!!!by_vars) %>%
    filter_if(filter) %>%
    summarise(ADT=min(ADT),!!analysis_var := summary_fun(!!analysis_var)) %>%
    mutate(!!!set_values_to) %>%
    ungroup()
}

derive_summary_records <- function(dataset,
                                   by_vars,
                                   filter = NULL,
                                   analysis_var,
                                   summary_fun,
                                   set_values_to = NULL) {
  assert_vars(by_vars)
  analysis_var <- assert_symbol(enquo(analysis_var))
  filter <- assert_filter_cond(enquo(filter), optional = TRUE)
  assert_s3_class(summary_fun, "function")
  assert_data_frame(
    dataset,
    required_vars = quo_c(by_vars, analysis_var)
  )
  if (!is.null(set_values_to)) {
    assert_varval_list(set_values_to, optional = TRUE)
  }
  
  # Summarise the analysis value and bind to the original dataset
  bind_rows(
    dataset,
    get_summary_records(
      dataset,
      by_vars = by_vars,
      filter = !!filter,
      analysis_var = !!analysis_var,
      summary_fun = summary_fun,
      set_values_to = set_values_to
    )
  )
}