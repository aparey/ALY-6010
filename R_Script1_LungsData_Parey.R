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
















