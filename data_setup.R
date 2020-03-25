
# Load Packages -----------------------------------------------------------

library(tidyverse)
library(skimr)
library(tidyr)
library(janitor)
library(broom)
library(MASS)
library(tidyverse)
library(modelr)
library(janitor)
library(skimr)
library(broom)
library(corrplot)
library(class)
# Import ------------------------------------------------------------------
set.seed(1234)
load(file = "data/data.rda")

raw_data <- da37024.0001

clean_data <- raw_data %>%
  dplyr::select(
    RESPID,
    #response: party affiliation
    Q7_1,
    #demographics
    # RETHNIC, 
    # RSTATE, 
    # EDUC3, 
    # S7, #gender
    # FORBORN, 
    # YOUTH, 
    #CITIZEN,
    #values
    # Q2_1A, #success
    # Q2_1B, #religion
    # Q2_1C, #free time
    # Q2_1D, #helping others
    # Q2_1E, #owning home
    # Q2_2B, #ethnic identity important
    # Q2_2C, #gender identity important
    # Q2_2D, #american identity important
    #sources of information
    # Q3_1,
    # Q3_2_0,
    # Q3_2_1,
    # Q3_2_2,
    # Q3_2_3,
    # Q3_2_4,
    # Q3_2_5,
    # Q3_2_6,
    # Q3A_2_1,
    # Q3A_2_2,
    # Q3A_2_3,
    #voting
    #REGISTERED, 
    # Q4_2, 
    # Q4_6, 
    # #favorability of candidates
    # Q4_11C, 
    # Q4_11D, 
    # Q4_11E, 
    # Q4_11F, 
    #civic behavior
    # Q5_1_01,
    # Q5_1_02,
    # Q5_1_03,
    # Q5_1_04,
    # Q5_1_05,
    # Q5_1_06,
    # Q5_1_07,
    # Q5_1_08,
    # Q5_1_09,
    # #contact
    # Q5_5, 
    # #religion
    # RELIGIOUS, 
    # ALLCHRISTIAN, 
    #issues
    Q6_1A, 
    #Q6_1D, 
    Q6_5_1, 
    Q6_5_2, 
    Q6_5_3, 
    Q6_5_4, 
    Q6_5_5, 
    Q6_5_6, 
    Q6_5_7, 
    # Q6_6A, 
    # Q6_6B, 
    #home and family
    # Q8_1, 
    # Q8_2, 
    # Q8_6, 
    # Q8_8, 
    # EMPLOYED, 
    # Q8_20

  )

#general cleaning
clean_data %>% skim()

clean_data <- clean_data %>% 
  mutate_if(is_character, factor)

clean_data %>% skim_without_charts()

#recode missingness according to codebook
clean_data_ <- clean_data %>%
  mutate(Q7_1  = fct_recode(Q7_1, 
                           "(05) Don't think in terms of parties" = "(05) DO NOT READ  Do not think in terms of political parties", 
                           "(04) Other party" = "(04) Other party  SPECIFY")) 


#recode for question frame. dems are biggest single group, what makes someone a dem? 
clean_data <- clean_data %>%
  mutate(party = fct_recode(Q7_1,
                            "Democrat" = "(01) Democrat", 
                            "Other" = "(02) Republican", 
                            "Other" = "(03) Independent", 
                            "Other" = "(04) Other party  SPECIFY", 
                            "Other" = "(05) DO NOT READ  Do not think in terms of political parties", 
                            "Other" = "(98) UNDOCUMENTED CODE"
                            )) 


#recode missingness
clean_data <- clean_data %>%
  mutate(Q6_1A = as.character(Q6_1A)) %>%
  mutate(Q6_5_1 = as.character(Q6_5_1)) %>%
  mutate(Q6_5_2 = as.character(Q6_5_2)) %>%
  mutate(Q6_5_3 = as.character(Q6_5_3)) %>%
  mutate(Q6_5_4 = as.character(Q6_5_4)) %>%
  mutate(Q6_5_5 = as.character(Q6_5_5)) %>%
  mutate(Q6_5_6 = as.character(Q6_5_6)) %>%
  mutate(Q6_5_7 = as.character(Q6_5_7)) %>%
  replace_na(list(Q6_1A = "Don't Know/Refused",
                  Q6_5_1 = "Don't Know/Refused",
                  Q6_5_2 = "Don't Know/Refused",
                  Q6_5_3 = "Don't Know/Refused",
                  Q6_5_4 = "Don't Know/Refused",
                  Q6_5_5 = "Don't Know/Refused",
                  Q6_5_6 = "Don't Know/Refused",
                  Q6_5_7 = "Don't Know/Refused")) %>%
  mutate(Q6_1A = as.factor(Q6_1A))%>%
  mutate(Q6_5_1 = as.factor(Q6_5_1))%>%
  mutate(Q6_5_2 = as.factor(Q6_5_2))%>%
  mutate(Q6_5_3 = as.factor(Q6_5_3))%>%
  mutate(Q6_5_4 = as.factor(Q6_5_4))%>%
  mutate(Q6_5_5 = as.factor(Q6_5_5))%>%
  mutate(Q6_5_6 = as.factor(Q6_5_6))%>%
  mutate(Q6_5_7 = as.factor(Q6_5_7))

clean_data <- clean_data %>%
  mutate(Q6_1A = fct_recode(Q6_1A, "Don't Know/Refused" = "(15) UNDOCUMENTED CODE" ))

#drop replicated var
clean_data <- clean_data %>%
  dplyr::select(-Q7_1)

#split into testing/training data 
tibble(clean_data)

eda_data <- clean_data %>% sample_frac(0.10)

clean_data <- setdiff(clean_data, eda_data)

#drop missing
clean_data <- clean_data %>%
  drop_na()

#drop uneeded unique identifier
clean_data <- clean_data %>%
  dplyr::select(-RESPID)

eda_data <- eda_data %>%
  dplyr::select(-RESPID)


clean_data_split <- tibble(train = list(clean_data %>% sample_frac(0.80)),
                  test  = list(clean_data %>% setdiff(train)))

clean_data_split %>%
  unnest(test) %>%
  skim_without_charts()


