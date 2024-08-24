getwd()

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(gmodels)
library(psych)


#Read the data
lungs <- read.csv("LungCapDataCSV.csv")
lungs


# Standardize column names using clean_names
lungs <- clean_names(lungs)
names(lungs)


# Convert appropriate columns to factors or numeric types
lungs$gender <- as.factor(lungs$gender)
lungs$age <- as.integer(lungs$age)


str(lungs)

#Summary stats
summary_stats <- summary(lungs)
summary_stats


#one-sample t-test
#for example purposes we have taken mean as 5

t_test_result <- t.test(lungs$lung_cap, mu = 5)
t_test_result

#Proportion test
#for example purposes we have taken p=0.33 or 33%

num_smokers <- sum(lungs$smoke == "yes")
n <- nrow(lungs)

prop_test_result <- prop.test(num_smokers, n, p = 0.33, alternative = "two.sided")
prop_test_result

