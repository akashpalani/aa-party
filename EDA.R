library(GGally)


#visualize distributions
viz_dis <- function(variable, title) {
  ggplot(eda_data, aes(x = variable)) + geom_bar() + ggtitle(title)
}

#response
viz_dis(eda_data$party, "Party Affiliation")

#predictors
#most important issue facing US
viz_dis(eda_data$Q6_1A, "Most Important Issue")

#obamacare
viz_dis(eda_data$Q6_5_1, "Obamacare")

#free public college
viz_dis(eda_data$Q6_5_2, "Free Public College")

#accepting syrian refugees
viz_dis(eda_data$Q6_5_3, "Accept Syrian Refugees")

#marijuana
viz_dis(eda_data$Q6_5_4, "Legalize Marijuana Posession (Small Amounts)")

#muslim ban
viz_dis(eda_data$Q6_5_5, "Muslim Ban")

#emissions
viz_dis(eda_data$Q6_5_6, "Stricter Emissions Limits")

#equal rights
viz_dis(eda_data$Q6_5_7, "Government Action on African American Rights")

#missingness 
skim_without_charts(eda_data)

#covariation
ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_1)) + ggtitle("obamacare")

ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_2)) + ggtitle("free college")

ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_3)) + ggtitle("syrian refugees")

#exclude
ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_4)) + ggtitle("marijuana legalization")

#exclude
ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_5)) + ggtitle("muslim ban")

#exclude
ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_6)) + ggtitle("emission limits")

ggplot(data = eda_data) +
  geom_count(mapping = aes(x = party, y = Q6_5_7)) + ggtitle("equal rights")

# 
# eda_data <- eda_data %>%
#   mutate(Q6_1A = as.character(Q6_1A)) %>%
#   mutate(Q6_5_1 = as.character(Q6_5_1)) %>%
#   mutate(Q6_5_2 = as.character(Q6_5_2)) %>%
#   mutate(Q6_5_3 = as.character(Q6_5_3)) %>%
#   mutate(Q6_5_4 = as.character(Q6_5_4)) %>%
#   mutate(Q6_5_5 = as.character(Q6_5_5)) %>%
#   mutate(Q6_5_6 = as.character(Q6_5_6)) %>%
#   mutate(Q6_5_7 = as.character(Q6_5_7)) %>%
#   replace_na(list(Q6_1A = "Don't Know/Refused", 
#                   Q6_5_1 = "Don't Know/Refused", 
#                   Q6_5_2 = "Don't Know/Refused",
#                   Q6_5_3 = "Don't Know/Refused",
#                   Q6_5_4 = "Don't Know/Refused",
#                   Q6_5_5 = "Don't Know/Refused",
#                   Q6_5_6 = "Don't Know/Refused", 
#                   Q6_5_7 = "Don't Know/Refused")) %>%
#   mutate(Q6_1A = as.factor(Q6_1A))%>%
#   mutate(Q6_5_1 = as.factor(Q6_5_1))%>%
#   mutate(Q6_5_2 = as.factor(Q6_5_2))%>%
#   mutate(Q6_5_3 = as.factor(Q6_5_3))%>%
#   mutate(Q6_5_4 = as.factor(Q6_5_4))%>%
#   mutate(Q6_5_5 = as.factor(Q6_5_5))%>%
#   mutate(Q6_5_6 = as.factor(Q6_5_6))%>%
#   mutate(Q6_5_7 = as.factor(Q6_5_7))
# #refugees and muslim ban both had high don't know/NA. notable, not really sure why
# 
# 
# 
# 
# 
# 
