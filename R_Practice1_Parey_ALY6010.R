getwd()

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(gmodels)



lungs <- read.csv("LungCapDataCSV.csv")
lungs

#Data Cleaning
lungs <- clean_names(lungs)
names(lungs)


lungs$gender <- as.factor(lungs$gender)
lungs$age <- as.integer(lungs$age)


str(lungs)


#Frequency Tables:
# Frequency table for Gender
gender_freq <- table(lungs$gender)
print(gender_freq)

# Frequency table for Smoking Status
smoke_freq <- table(lungs$smoke)
print(smoke_freq)



# Cross Tab
CrossTable(lungs$gender, lungs$smoke, prop.chisq = FALSE)


#Histograms:
# Histogram of Lung Capacity
ggplot(lungs, aes(x = lung_cap)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Lung Capacity", x = "Lung Capacity", y = "Frequency") +
  theme_minimal()

# Histogram of Age
ggplot(lungs, aes(x = age)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Age", x = "Age", y = "Frequency") +
  theme_minimal()

#Comparison across classes

# histogram of Lung Capacity by Gender
ggplot(lungs, aes(x = lung_cap, fill = gender)) +
  geom_histogram(binwidth = 1, color = "black", position = "dodge") +
  facet_wrap(~ gender) +
  labs(title = "Lung Capacity Distribution by Gender", x = "Lung Capacity", y = "Frequency") +
  theme_minimal()

# bar plot of Smoking Status by Gender
ggplot(lungs, aes(x = gender, fill = smoke)) +
  geom_bar(position = "dodge", color = "black") +
  labs(title = "Smoking Status by Gender", x = "Gender", y = "Count") +
  theme_minimal()

#date()




