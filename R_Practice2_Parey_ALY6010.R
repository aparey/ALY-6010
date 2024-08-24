getwd()

library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(gmodels)
library(psych)
library(knitr)


lungs <- read.csv("LungCapDataCSV.csv")
lungs


#Data Cleaning
lungs <- clean_names(lungs)
names(lungs)


lungs$gender <- as.factor(lungs$gender)
lungs$age <- as.integer(lungs$age)


str(lungs)

#Summary stats
summary_stats <- summary(lungs)
summary_stats

#Stats by group
group_stats <- lungs %>%
  group_by() %>%
  summarise(
    Mean_LungCapacity = mean(lung_cap, na.rm = TRUE),
    SD_LungCapacity = sd(lung_cap, na.rm = TRUE),
    Min_LungCapacity = min(lung_cap, na.rm = TRUE),
    Max_LungCapacity = max(lung_cap, na.rm = TRUE),
    N = n()
  )

group_stats



kable(summary_stats, format = "markdown", col.names = c("Variable", "Mean", "SD", "Min", "Max", "N"), 
      align = "c", caption = "Descriptive Statistics for the Sample")

#Scatter Plot
plot(lungs$age, lungs$lung_capacity, 
     main = "Scatter Plot of Age vs Lung Capacity", 
     xlab = "Age", 
     ylab = "Lung Capacity", 
     col = "orange", 
     pch = 19)

#Jitter Plot

plot(jitter(lungs$age), lungs$lung_capacity, 
     main = "Jitter Plot of Age vs Lung Capacity", 
     xlab = "Age", 
     ylab = "Lung Capacity", 
     col = "red", 
     pch = 19)

#Box Plot
boxplot(lung_cap ~ gender, data = lungs, 
        main = "Boxplot of Lung Capacity by Gender", 
        xlab = "Gender", 
        ylab = "Lung Capacity", 
        col = c("lightblue", "lightpink"))

abline(h = median(lungs$lung_capacity), col = "red", lty = 2)






