library(tidyverse)
library(lme4)
library(lmerTest)

#Load data
data = read.csv("Data/full_data.csv", row.names = 1, colClasses = "character")

data$trial = as.numeric(data$trial)
data$points = as.numeric(data$points)
data$correct = as.numeric(data$correct)

train_data = subset(data, block == "training")
test_data = subset(data, block == "test")

#test data with non-reseting trial numbers
test_data2 = test_data
for (id in unique(test_data2$unique_id)){
  for (rule in 2:11){
    max = max(subset(test_data2, unique_id == id & rule_num == rule)[,"trial"])
    test_data2[test_data2$unique_id == id & test_data2$rule_num == rule+1,"trial"] = test_data2[test_data2$unique_id == id & test_data2$rule_num == rule+1,"trial"] + max + 1
  }
}


test_data$condition = factor(test_data$condition, levels = c("2_ND", "3_D", "1_I"))
m1 = glmer(correct ~ condition + (1 + trial|pair_id) + (1 + trial|rule_num), test_data, family = "binomial")
summary(m1)



ggplot(test_data2, aes(x = trial, y = points)) +
  geom_point(alpha = 0.5, aes(color = rule_num)) +
  facet_wrap(~ condition) +
  theme_minimal()

ggplot(test_data2, aes(x = trial, y = points)) +
  geom_smooth() +
  facet_wrap(~ condition)

  
