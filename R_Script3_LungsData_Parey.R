getwd()

#Aditya Parey
#ALY 6010 Summer 2024

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)


#Read the data
lungs <- read.csv("LungCapDataCSV.csv")
lungs


# Standardize column names using clean_names
lungs <- clean_names(lungs)
names(lungs)

#Handling Missing Values
lungs <- lungs %>%
  mutate(lung_cap = ifelse(is.na(lung_cap), median(lung_cap, na.rm = TRUE), lung_cap))


#Recoding Variables
lungs <- lungs %>%
  mutate(smoke_status = ifelse(smoke == "yes", 1, 0))

#Change Data Types
lungs <- lungs %>%
  mutate(smoke_status = as.factor(smoke_status),
         age = as.integer(age),
         lung_capacity = as.numeric(lung_cap))

#Frequency Table
freq_table <- table(lungs$smoke_status)
freq_table

#Cross Tab
cross_tab <- table(lungs$smoke_status, lungs$gender)
cross_tab

#Histogram

ggplot(lungs, aes(x = lung_capacity, fill = smoke_status)) +
  geom_histogram(binwidth = 0.5, color = "black", position = "dodge") +
  labs(title = "Lung Capacity Distribution by Smoking Status",
       x = "Lung Capacity",
       y = "Frequency") +
  theme_minimal()


#Histogram of Lung Capacity Distribution by Gender and Smoking Status

ggplot(lungs, aes(x = lung_capacity, fill = smoke_status)) +
  geom_histogram(binwidth = 0.5, color = "black") +
  facet_wrap(~ gender) +
  labs(title = "Lung Capacity Distribution by Gender and Smoking Status",
       x = "Lung Capacity",
       y = "Frequency") +
  theme_minimal()


#Hypothesis Testing

#Question 1: Does the average lung capacity significantly differ from a known population average (e.g., 8 liters)?
#Hypothesis Test: One-sample t-test
#Null Hypothesis (H0): The average lung capacity of the sample is equal to 8 liters.
#Alternative Hypothesis (H1): The average lung capacity of the sample is not equal to 8 liters.

# example population mean
population_mean <- 10

# one-sample t-test
t_test_one_sample <- t.test(lungs$lung_capacity, mu = population_mean)
t_test_one_sample



#Question 2: Is there a significant difference in lung capacity between smokers and non-smokers?
#Hypothesis Test: Two-sample t-test (independent)
#Null Hypothesis (H0): The average lung capacity of smokers is equal to that of non-smokers.
#Alternative Hypothesis (H1): The average lung capacity of smokers is different from that of non-smokers.

#two-sample t-test

t_test_two_sample <- t.test(lung_capacity ~ smoke_status, data = lungs)
t_test_two_sample


#Question 3: Is the proportion of smokers among males different from the proportion of smokers among females?
#Hypothesis Test: Two-proportion z-test
#Null Hypothesis (H0): The proportion of smokers is the same among males and females.
#Alternative Hypothesis (H1): The proportion of smokers is different among males and females.

# create a table
smoking_gender_table <- table(lungs$smoke_status, lungs$gender)

#the two-proportion z-test
prop_test <- prop.test(smoking_gender_table)
prop_test

#Question 1: Is there a significant relationship between lung capacity and age?
#Independent Variable (X): Age
#Dependent Variable (Y): Lung Capacity

#Scatterplot
ggplot(lungs, aes(x = age, y = lung_capacity)) +
  geom_point(color = "blue") +
  labs(title = "Scatterplot of Lung Capacity vs. Age",
       x = "Age",
       y = "Lung Capacity") +
  theme_minimal()

model_age_lungcap <- lm(lung_capacity ~ age, data = lungs)
summary(model_age_lungcap)


#Question 2: Does height predict lung capacity, and is the relationship significant?
#Independent Variable (X): Height
#Dependent Variable (Y): Lung Capacity

ggplot(lungs, aes(x = height, y = lung_capacity)) +
  geom_point(color = "green") +
  labs(title = "Scatterplot of Lung Capacity vs. Height",
       x = "Height",
       y = "Lung Capacity") +
  theme_minimal()

model_height_lungcap <- lm(lung_capacity ~ height, data = lungs)
summary(model_height_lungcap)


#Question 3: Is there a significant interaction between smoking status and age in predicting lung capacity?
#Independent Variables (X): Age, Smoking Status, and their Interaction (Age * Smoking Status)
#Dependent Variable (Y): Lung Capacity

ggplot(lungs, aes(x = age, y = lung_capacity, color = smoke_status)) +
  geom_point() +
  facet_wrap(~smoke_status) +
  labs(title = "Scatterplot of Lung Capacity vs. Age by Smoking Status",
       x = "Age",
       y = "Lung Capacity") +
  theme_minimal()


model_interaction <- lm(lung_capacity ~ age * smoke_status, data = lungs)
summary(model_interaction)















