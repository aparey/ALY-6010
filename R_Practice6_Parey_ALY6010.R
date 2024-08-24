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


#Dummy variables

lungs <- lungs %>%
  mutate(gender_male = ifelse(gender == "male", 1, 0),
         gender_female = ifelse(gender == "female", 1, 0))

head(lungs)

#LRM
regression_model <- lm(lung_cap ~ age + height + gender_male, data = lungs)
summary(regression_model)

# MLR Plot
ggplot(lungs, aes(x = height, y = lung_cap, color = gender)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Lung Capacity vs Height by Gender",
       x = "Height",
       y = "Lung Capacity") +
  theme_minimal()

# regression line for males
regression_male <- lm(lung_cap ~ age + height, data = lungs %>% filter(gender_male == 1))
regression_male

ggplot(lungs %>% filter(gender_male == 1), aes(x = height, y = lung_cap)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "lightpink") +
  labs(title = "Lung Capacity vs Height for Males",
       x = "Height",
       y = "Lung Capacity") +
  theme_minimal()

# regression line for females
regression_female <- lm(lung_cap ~ age + height, data = lungs %>% filter(gender_female == 1))
regression_female
# Scatter plot with regression lines for females
ggplot(lungs %>% filter(gender_female == 1), aes(x = height, y = lung_cap)) +
  geom_point(color = "lightpink") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Lung Capacity vs Height for Females",
       x = "Height",
       y = "Lung Capacity") +
  theme_minimal()



