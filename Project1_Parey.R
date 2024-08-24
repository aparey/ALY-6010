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


nrow(books)
ncol(books)
data.frame(books)










