#logistic 
glm_fits <- clean_data_split %>% 
  mutate(mod_01 = map(train, glm, 
                      formula = party ~.,
                      family = binomial("logit")), 
         mod_02 = map(train, glm, 
                      formula = party ~ Q6_1A + Q6_5_1 + Q6_5_2 + Q6_5_3 + Q6_5_4 + Q6_5_5 + Q6_5_6 + Q6_5_7,
                      family = binomial("logit"))) %>%
  pivot_longer(cols = contains("mod_"), names_to = "model_name", values_to =  "model_fit")


#now, for all the models 
#Function for error rate
error_rate_glm <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_party = if_else(pred_prob > 0.5, "Other", "Democrat"),
           error = pred_party != party) %>% 
    pull(error) %>% 
    mean()
}

# Function to form confusion matrix
confusion_mat_glm <- function(data, model){
  data %>% 
    mutate(pred_prob = predict(model, newdata = data, type = "response"),
           pred_party = if_else(pred_prob > 0.5, "Other", "Democrat")) %>% 
    count(party, pred_party) %>% 
    mutate(prop = n / sum(n))
}

# Calculate model error
glm_fits <- glm_fits %>% 
  mutate(train_error = map2_dbl(train, model_fit, error_rate_glm),
         test_error  = map2_dbl(test, model_fit, error_rate_glm),
         test_confusion = map2(test, model_fit, confusion_mat_glm))  

glm_fits %>% 
  dplyr::select(model_name, train_error, test_error) %>% 
  # select_if(~ !is_list(.)) %>% 
  arrange(test_error)











glm_fits %>% 
  pluck("mod_02", 1) %>% 
  tidy()

glm_fits %>% 
  pluck("mod_02", 1) %>% 
  predict(type = "response") %>% 
  skim_without_charts()
#no significant p vals 

clean_data_split %>% 
  # pull(): extract a column from a tibble - replacement for $
  unnest(train) %>%
  pull(party) %>% 
  contrasts()
#Other is encoded as 1. So, we will assign >0.5 to other, <0.5 to Democrat

#use separate tibble so as not to alter glm_fits, we can add more models to it if we want
demo_tib <- glm_fits %>%
  mutate(train_prob = map(mod_01, predict, type = "response"),
         train_direction = map(train_prob, ~ if_else(.x > 0.5, "Other", "Democrat")))

#add predictions on training set 
demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(train_direction) %>% 
  mutate(prop = n / sum(n))

#confusion matrix 
demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  count(party, train_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  dplyr::select(-n) %>% 
  pivot_wider(names_from = train_direction, values_from = prop)
#type 1 error: 9%, type 2 error: 20%, predict democrat accurately 46%, predict other accurately 24%

#assess model. do this with cross validation!
# Model assessment (accuracy/error) for train dataset
demo_tib %>% 
  unnest(cols = c(train, train_direction)) %>% 
  mutate(correct = if_else(train_direction == party, 1, 0)) %>% 
  summarise(train_accuracy = mean(correct),
            train_error    = 1 - train_accuracy)


#Predict and assess on test dataset 
# Model predictions for test dataset
demo_tib <- demo_tib %>%
  mutate(test_prob = map2(mod_01, test, predict, type = "response"),
         test_direction = map(test_prob, ~ if_else(.x > 0.5, "Other", "Democrat")))

# Model assessment (accuracy/error) for test dataset
demo_tib %>% 
  unnest(cols = c(test, test_direction)) %>% 
  mutate(correct = if_else(test_direction == party, 1, 0)) %>% 
  summarise(test_accuracy = mean(correct),
            test_error    = 1 - test_accuracy)

#confusion matrix for test data 
demo_tib %>% 
  unnest(cols = c(test, test_direction)) %>% 
  count(party, test_direction) %>% 
  mutate(prop = n / sum(n)) %>% 
  dplyr::select(-n) %>% 
  pivot_wider(names_from = test_direction, values_from = prop)
