
# Load Packages -----------------------------------------------------------

library(MASS)
library(tidyverse)
library(modelr)
library(janitor)
library(skimr)
library(broom)
library(corrplot)
library(class)


# Linear Discriminant Analysis ----------------------------------------------------------------
lda_fits <- clean_data_split %>% 
  mutate(lda_mod_01 = map(train, ~ lda(formula = party ~.,
                                   data = .x)),
         lda_mod_02 = map(train, ~ lda(formula = party ~ Q6_1A + Q6_5_1 + Q6_5_2 + Q6_5_3 + Q6_5_7,
                                   data = .x)), 
         lda_mod_03 = map(train, ~ lda(formula = party ~ Q6_1A,
                                   data = .x))) %>% 
  pivot_longer(cols = contains("mod_"), names_to = "model_name", values_to = "model_fit")

# Function to calculate lda error rate 
error_rate_lda <- function(data, model){
  data %>% 
    mutate(pred_party = predict(model, newdata = data) %>% 
             pluck("class"),
           error = pred_party != party) %>% 
    pull(error) %>% 
    mean()
}

# Function to form lda confusion matrix
confusion_mat_lda <- function(data, model){
  data %>% 
    mutate(pred_party = predict(model, newdata = data) %>% 
             pluck("class")) %>% 
    count(party, pred_party) %>% 
    mutate(prop = n / sum(n))
}

# update lda_fits with error and confusion info
lda_fits <- lda_fits %>% 
  mutate(train_error = map2_dbl(train, model_fit, error_rate_lda),
         test_error  = map2_dbl(test, model_fit, error_rate_lda),
         test_confusion = map2(test, model_fit, confusion_mat_lda))  

# Compare models by test_error
lda_errors <- lda_fits %>% 
  dplyr::select(model_name, test_error) %>% 
  arrange(test_error)

# mod 1 confusion
lda_fits %>%
  unnest(test_confusion) %>%
  dplyr::select(model_name, party, pred_party, prop) %>%
  dplyr::filter(model_name == "lda_mod_01") %>%
  dplyr::select(-model_name) %>% 
  pivot_wider(names_from = pred_party, values_from = prop)

# mod 2 confusion
lda_fits %>%
  unnest(test_confusion) %>%
  dplyr::select(model_name, party, pred_party, prop) %>%
  dplyr::filter(model_name == "lda_mod_02") %>%
  dplyr::select(-model_name) %>% 
  pivot_wider(names_from = pred_party, values_from = prop)

lda_fits1 <- clean_data_split %>% 
  mutate(mod_01 = map(train, ~ lda(formula = party ~.,
                                   data = .x)))

lda_fits1 %>%
  pluck("mod_01", 1)
