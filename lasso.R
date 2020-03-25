library(tidyverse)
library(modelr)
library(janitor)
library(skimr)
library(leaps) # best subset selection
library(glmnet) # ridge & lasso
library(glmnetUtils) # improves working with glmnet

set.seed(1234)

lambda_grid <- 10^seq(-2, 10, length = 200)

train_data <- clean_data_split$train[[1]]


test_data <- clean_data_split$test[[1]]

# lasso: 10-fold cv
lasso_cv <- train_data %>% 
  cv.glmnet(
    formula = party ~ ., 
    data = ., 
    alpha = 1, 
    nfolds = 10,
    family = "binomial"
  )


plot(lasso_cv)

lasso_lambda_1se <- lasso_cv$lambda.1se
lasso_lambda_min <- lasso_cv$lambda.min

lasso_glmnet <- tibble(
  train = train_data %>% list(),
  test  = test_data %>% list()
) %>%
  mutate(
    lasso_min = map(train, ~ glmnet(party ~ ., data = .x,
                                    alpha = 1, lambda = lasso_lambda_min, family = "binomial")),
    lasso_1se = map(train, ~ glmnet(party ~ ., data = .x,
                                    alpha = 1, lambda = lasso_lambda_1se, family = "binomial"))
  ) %>% 
  pivot_longer(cols = c(-test, -train), names_to = "model_name", values_to = "fit")

# Inspect/compare model coefficients 
lasso_glmnet %>% 
  pluck("fit") %>% 
  map( ~ coef(.x) %>% 
         as.matrix() %>% 
         as.data.frame() %>% 
         rownames_to_column("name")) %>%
  reduce(full_join, by = "name") %>% 
  mutate_if(is.double, ~ if_else(. == 0, NA_real_, .)) %>% 
  rename(lasso_min = s0.x,
         lasso_1se = s0.y) %>% 
  knitr::kable(digits = 3)


error_rate_lasso <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_party = if_else(pred_prob > 0.5, "Other", "Democrat"),
           error = pred_party != party) %>% 
    pull(error) %>% 
    mean()
}

lasso_errors <- lasso_glmnet %>% 
  mutate(test_error  = map2_dbl(test, fit, error_rate_glm)) %>%
  dplyr::select(model_name, test_error)

lasso_errors





