getwd()

#Aditya Parey ALY 6010 Summer 2024
date()



library(ggplot2)
library(dplyr)
library(janitor)
library(lubridate)
library(readr)
library(tidyr)
library(testthat)



# Read the CSV file into a data frame
books <- read.csv("books.csv")
books

# Standardize column names using clean_names
books <- clean_names(books)
names(books)


# Convert the first_publish_date column to date type using mdy
books$first_publish_date <- mdy(books$first_publish_date)
books$first_publish_date
summary(books$first_publish_date)




# Extract the year from first_publish_date and place it in a new column named year
books <- books %>% mutate(year = year(first_publish_date))
books$first_publish_date



books <- books %>% filter(year >= 1990 & year <= 2020)
summary(books)



books <- books %>% select(-c(publish_date, edition, characters, price, genres, setting, isbn))
names(books)



# Keep only books that are fewer than 700 pages
books_filtered <- books %>% filter(pages < 700)
book_titles <- books_filtered %>% select(title)
nrow(book_titles)




# Remove any rows that contain NAs
books <- books %>% drop_na()
nrow(books)



glimpse(books)


summary(books)



ggplot(books, aes(x = rating)) +
  geom_histogram(binwidth = 0.25, fill = "red", color = "yellow") +
  labs(title = "Books Histogram",
       x = "Rating",
       y = "Number of Books")



ggplot(books, aes(x = pages)) +
  geom_boxplot( fill = "red", color = "yellow") +
  labs(title = "Box Plot of Page Counts",
       x = "Frequency",
       y = "Pages") 



#Hypothesis Testing 1

#Does the Average Rating of Books Differ Significantly from a Benchmark Value?
  
#Question: Is the average rating of books significantly different from a benchmark rating (e.g., 3.5)?
#Test: One-sample t-test.
#Null Hypothesis (H0): The mean rating of books is equal to 3.5.
#Alternative Hypothesis (H1): The mean rating of books is not equal to 3.5.

# Calculate the mean and standard deviation of ratings
mean_rating <- mean(books$rating)
sd_rating <- sd(books$rating)

# Perform a one-sample t-test
t_test_rating <- t.test(books$rating, mu = 3.5)
t_test_rating


#Hypothesis Testing 2 
#Is There a Significant Difference in the Average Ratings of Books Published Before and After 2005?
  
#Question: Is there a significant difference in the average ratings of books published before 2005 compared to those published in or after 2005?
#Test: Two-sample t-test (independent samples).
#Null Hypothesis (H0): The mean rating of books published before 2005 is equal to the mean rating of books published after 2005.
#Alternative Hypothesis (H1): The mean rating of books published before 2005 is not equal to the mean rating of books published after 2005.

# Split the data into two groups: before 2005 and 2005 or later
before_2005 <- books %>% filter(year < 2005)
after_2005 <- books %>% filter(year >= 2005)
# Calculate the means and standard deviations for both groups
mean_before_2005 <- mean(before_2005$rating)
mean_after_2005 <- mean(after_2005$rating)

sd_before_2005 <- sd(before_2005$rating)
sd_after_2005 <- sd(after_2005$rating)
# Perform a two-sample t-test
t_test_ratings_by_year <- t.test(before_2005$rating, after_2005$rating)
t_test_ratings_by_year


#Hypothesis Testing 3
#Is the Average Page Count of Books Less Than a Specified Number (e.g., 400 Pages)?
#Question: Is the average page count of books less than 400 pages?
#Test: One-sample t-test.
#Null Hypothesis (H0): The mean page count of books is equal to or greater than 400 pages.
#Alternative Hypothesis (H1): The mean page count of books is less than 400 pages.

# Calculate the mean and standard deviation of page counts
mean_pages <- mean(books$pages)
sd_pages <- sd(books$pages)

# Perform a one-sample t-test with 400 pages as the benchmark
t_test_pages <- t.test(books$pages, mu = 400, alternative = "less")
t_test_pages























