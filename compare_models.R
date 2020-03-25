lda_errors
log_error
lasso_errors

one <- full_join(lda_errors, log_error)

all_models <- full_join(one, lasso_errors) %>%
  arrange(test_error)
