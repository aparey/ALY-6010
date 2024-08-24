getwd()

#Aditya Parey
#ALY 6010 Summer 2024

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)



lungs <- read.csv("LungCapDataCSV.csv")
lungs


# Data Cleaning
lungs <- clean_names(lungs)
names(lungs)

lungs <- lungs %>%
  mutate(lung_cap = ifelse(is.na(lung_cap), median(lung_cap, na.rm = TRUE), lung_cap))


lungs <- lungs %>%
  mutate(smoke_status = ifelse(smoke == "yes", 1, 0))

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