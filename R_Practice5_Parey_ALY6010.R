getwd()

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(gmodels)
library(psych)
library(MASS)
library(corrplot)

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

#Correlation 

selected_vars <- lungs %>%
  dplyr::select(lung_cap, age, height)


cor_matrix <- cor(selected_vars, use = "complete.obs")
cor_matrix

corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black", 
         tl.srt = 45, title = "Correlation Matrix of Selected Variables")

regression_model <- lm(lung_cap ~ height, data = lungs)
summary(regression_model)


