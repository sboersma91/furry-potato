# This first set of code is the prompt code to start analyzing.
# R version 4.0.2 was used in analysis.
library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)

#above will be deleted
# prompt-------------------
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Additional Libraries Used --------------------
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")


library(lubridate)
library(ggplot2)
library(ggthemes)
library(scales)


# Creating edx_train, edx_test  -------------------------------

# Creating a train and test set from the initial edx test set for model building
set.seed(31, sample.kind = "Rounding")
test_index2 <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
edx_train <- edx[-test_index2]
temp2 <- edx[test_index2]

#ensure userId and movieId are in both train and test sets
edx_test <- temp2 %>% 
  semi_join(edx_train, by = "movieId") %>% 
  semi_join(edx_train, by = "userId") 

removed2 <- anti_join(temp2, edx_test)
edx_train <- rbind(edx_train, removed2)

rm(test_index2, temp2, removed2)

# Initial Data exploration-----------------------
head(edx)

edx %>%
  summarize(n_users = n_distinct(userId), 
            n_movies = n_distinct(movieId))

# Number of years ratings were given
year(as_datetime(max(edx$timestamp))) - year(as_datetime(min(edx$timestamp)))

# Summary of genres
edx %>% 
  group_by(genres) %>% 
  summarise(n=n()) %>% 
  head()

# movie distribution
edx %>% 
  group_by(movieId) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(color = "white")+
  scale_x_log10()+
  ggtitle("Distribution of Movies")+
  xlab("Number of Ratings")+
  ylab("Number of Movies")+
  theme_economist()

# user distribution
edx %>% 
  group_by(userId) %>% 
  summarize(n=n()) %>% 
  ggplot(aes(n)) +
  geom_histogram(color = "white")+
  scale_x_log10()+
  ggtitle("Distrobution of Users")+
  xlab("Number of Ratings")+
  ylab("Number of Users") +
  theme_economist()

# User Rating Distribution
edx %>% 
  group_by(userId) %>% 
  summarize(user_b = mean(rating)) %>% 
  filter(n() >= 100) %>% 
  ggplot(aes(user_b)) +
  geom_histogram(color = "black") +
  ggtitle("User Bias Distribution")+
  xlab("user_bias")+
  ylab("Count") +
  scale_y_continuous(labels = comma) +
  theme_economist()

# Year Distribution
edx %>% 
  mutate(year = year(as_datetime(timestamp))) %>%
  ggplot(aes(x = year))+
  geom_histogram() +
  ggtitle("Rating Distribution")+
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma)+
  theme_economist()





# Mutate Movie Year & Timestamp as Date & day of week ---------------
edx2 <- edx %>% 
  mutate(year = str_extract((str_extract(edx$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx$timestamp)),
         day_of_week = weekdays(as_datetime((edx$timestamp)))) %>% 
  group_by(userId) %>% 
  mutate(start_date = min(date)) %>% 
  ungroup() %>% 
  mutate(age = date - start_date)

validation2 <- validation %>% 
  mutate(year = str_extract((str_extract(validation$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(validation$timestamp)),
         day_of_week = weekdays(as_datetime((validation$timestamp)))) %>% 
  group_by(userId) %>% 
  mutate(start_date = min(date)) %>% 
  ungroup() %>% 
  mutate(age = date - start_date)

edx_train2 <- edx_train %>% 
  mutate(year = str_extract((str_extract(edx_train$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx_train$timestamp)),
         day_of_week = weekdays(as_datetime((edx_train$timestamp)))) %>% 
  group_by(userId) %>% 
  mutate(start_date = min(date)) %>% 
  ungroup() %>% 
  mutate(age = date - start_date)

edx_test2 <- edx_test %>% 
  mutate(year = str_extract((str_extract(edx_test$title, "\\(\\d{4}\\)$")), "\\d{4}"), 
         date = date(as_datetime(edx_test$timestamp)),
         day_of_week = weekdays(as_datetime((edx_test$timestamp)))) %>% 
  group_by(userId) %>% 
  mutate(start_date = min(date)) %>% 
  ungroup() %>% 
  mutate(age = date - start_date)

# Ensure all age days are in (reword needed) ---------
edx_test2 <- edx_test2 %>% 
  semi_join(edx_train2, by = "age")

validation2 <- validation2 %>% 
  semi_join(edx2, by = "age")


# Error calculated with RMSE (also found in caret package)
RMSE1 <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2)
  )}

# Mean   -------------------

# first predict by the average rating
mu <- mean(edx_train2$rating)
# create a table to check progress
result <- tibble(Method = "Avg", RMSE = RMSE(mu, edx_test2$rating))

# Movie Bias -----------------
m_bias <- edx_train2 %>% 
  group_by(movieId) %>% 
  summarize(m_bias = mean(rating - mu))


m_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  mutate(pred = mu + m_bias) %>% 
  pull(pred)

result <- bind_rows(result, tibble(Method = "Movie bias", 
                                   RMSE = RMSE(m_bias_pred, edx_test2$rating)))



# User Bias ---------------------
u_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(u_bias = mean(rating - mu - m_bias))

u_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId") %>% 
  mutate(pred = mu + m_bias + u_bias) %>% 
  pull(pred)

result <- bind_rows(result, tibble(Method = "User bias",
                                   RMSE = RMSE(u_bias_pred, edx_test2$rating)))

# Genre Bias ------------------
g_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias,  by = "userId") %>%
  group_by(genres) %>%
  summarise(g_bias = mean(rating - mu - m_bias - u_bias))

g_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  mutate(pred = mu + m_bias + u_bias + g_bias) %>%
  pull(pred)

result <- bind_rows(result, tibble(Method = "Genre bias",
                                   RMSE = RMSE(g_bias_pred, edx_test2$rating)))

# year released -----------------
# proxie for age? - should be able to say
y_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>%
  left_join(g_bias, by = "genres")  %>%
  group_by(year) %>%
  summarise(y_bias = mean(rating - mu - m_bias - u_bias - g_bias))


y_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year") %>% 
  mutate(pred = mu + m_bias + u_bias + g_bias + y_bias) %>%
  pull(pred)

result <- bind_rows(result, tibble(Method = "Year bias",
                                   RMSE = RMSE(y_bias_pred, edx_test2$rating)))


# Date Rated Bias ---------------------

d_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  group_by(date) %>% 
  summarise(d_bias = mean(rating - mu - m_bias - u_bias - g_bias - y_bias))

d_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>% 
  mutate(pred = mu + m_bias + u_bias + g_bias + y_bias + d_bias) %>%
  pull(pred)


result <- bind_rows(result, tibble(Method = "Date bias",
                                   RMSE = RMSE(d_bias_pred, edx_test2$rating)))

# Time (days) age first rating ---------------------

t_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>%
  group_by(age) %>% 
  summarise(t_bias = mean(rating - mu - m_bias - u_bias - g_bias - y_bias - d_bias))

t_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>%
  left_join(t_bias, by = "age")   %>% 
  mutate(pred = mu + m_bias + u_bias + g_bias + y_bias + d_bias + t_bias) %>%
  pull(pred)

result <- bind_rows(result, tibble(Method = "Time bias",
                                   RMSE = RMSE(t_bias_pred, edx_test2$rating)))


# Day of week bias ---------------
w_bias <- edx_train2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>%
  left_join(t_bias, by = "age")   %>% 
  group_by(day_of_week) %>% 
  summarise(w_bias = mean(rating - mu - m_bias - u_bias - g_bias - y_bias - d_bias - t_bias))

w_bias_pred <- edx_test2 %>% 
  left_join(m_bias, by = "movieId") %>% 
  left_join(u_bias, by = "userId")  %>% 
  left_join(g_bias, by = "genres")  %>%
  left_join(y_bias, by = "year")    %>%
  left_join(d_bias, by = "date")    %>%
  left_join(t_bias, by = "age")   %>%
  left_join(w_bias, by = "day_of_week") %>% 
  mutate(pred = mu + m_bias + u_bias + g_bias + y_bias + d_bias + t_bias + w_bias) %>%
  pull(pred)

result <- bind_rows(result, tibble(Method = "Week bias",
                                   RMSE = RMSE(w_bias_pred, edx_test2$rating)))



result 

# Accuracy Check -------------------
# See where the error is the most with just movie bias
titles <- edx_train2 %>% 
  select(movieId, title) %>% 
  distinct()

# best movies with n ratings
edx_train2 %>%
  count(movieId) %>% 
  left_join(titles) %>% 
  left_join(m_bias, by ="movieId") %>% 
  arrange(-m_bias) %>% 
  select(title, m_bias, n) %>% 
  slice(1:10) %>% 
  knitr::kable()


# 10 best ranked by movie bias
m_bias %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(-m_bias) %>% 
  select(title, m_bias) %>% 
  slice(1:10) %>% 
  knitr::kable()

# worst 10
m_bias %>% 
  inner_join(titles, by = "movieId") %>% 
  arrange(m_bias) %>% 
  select(title, m_bias) %>% 
  slice(1:10) %>% 
  knitr::kable()

# number of ratings for the worst
edx_train2 %>% 
  left_join(m_bias, by ="movieId") %>% 
  arrange(m_bias) %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  slice(1:10) %>% 
  knitr::kable()

# number of ratings for the best
edx_train2 %>% 
  left_join(m_bias, by ="movieId") %>% 
  arrange(-m_bias) %>% 
  group_by(title) %>% 
  summarise(n = n()) %>% 
  slice(1:10) %>% 
  knitr::kable()





# regularization Function----------------------
# lambda is a variable and this function is made to find the best value.
lambda_values <- seq(1, 10, 0.25)
regularization <- sapply(lambda_values, function(lambda){
  
  mean_rating <- mean(edx_train2$rating)
  
  movie_bias <- edx_train2 %>% 
    group_by(movieId) %>% 
    summarize(movie_bias = sum(rating - mean_rating)/(n()+lambda))
  
  user_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    group_by(userId) %>% 
    summarize(user_bias = sum(rating - mean_rating - movie_bias)/(n()+lambda))
  
  genre_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId") %>%
    group_by(genres) %>% 
    summarise(genre_bias = sum(rating - mean_rating - movie_bias - user_bias)/(n()+lambda))
  
  year_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias,  by = "userId")  %>%
    left_join(genre_bias, by = "genres")  %>%
    group_by(year) %>%
    summarise(year_bias = sum(rating - mean_rating - movie_bias - 
                                user_bias - genre_bias)/(n()+lambda))
  
  date_bias <- edx_train2 %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId")  %>%
    left_join(genre_bias, by = "genres")  %>%
    left_join(year_bias, by = "year")    %>%
    group_by(date)  %>%
    summarise(date_bias = sum(rating - mean_rating - movie_bias - 
                                user_bias - genre_bias - year_bias)/(n()+lambda))
 
  time_bias <- edx_train2 %>% 
    left_join(movie_bias, by = "movieId") %>% 
    left_join(user_bias, by = "userId")  %>% 
    left_join(genre_bias, by = "genres")  %>%
    left_join(year_bias, by = "year")    %>%
    left_join(date_bias, by = "date")    %>%
    group_by(age) %>% 
    summarise(time_bias = sum(rating - mean_rating - movie_bias - user_bias - 
                                genre_bias - year_bias - date_bias)/(n()+lambda))
  
  week_bias <- edx_train2 %>%
    left_join(movie_bias, by = "movieId") %>%
    left_join(user_bias, by = "userId")   %>%
    left_join(genre_bias, by = "genres")  %>%
    left_join(year_bias, by = "year")     %>%
    left_join(date_bias, by = "date")     %>% 
    left_join(time_bias, by = "age")    %>% 
    group_by(day_of_week)  %>%
    summarise(week_bias = sum(rating - mean_rating - movie_bias - user_bias - 
                                genre_bias - year_bias - date_bias - time_bias)/(n()+lambda))
   
  predict_ratings_edx <- edx_test2 %>% 
    left_join(movie_bias, by = "movieId")%>% 
    left_join(user_bias,  by = "userId") %>%
    left_join(genre_bias, by = "genres") %>% 
    left_join(year_bias,  by = "year")   %>% 
    left_join(date_bias,  by = "date")   %>% 
    left_join(time_bias, by = "age")    %>% 
    left_join(week_bias, by = "day_of_week") %>% 
    mutate(pred = mean_rating + movie_bias + user_bias + genre_bias + year_bias + date_bias + time_bias + week_bias) %>% 
    pull(pred)
  
  RMSE(predict_ratings_edx, edx_test2$rating)
})

# Check range if have definite minimum
tibble(lambda = lambda_values, RMSE = regularization) %>% 
  ggplot(aes(x = lambda, y = RMSE))+
  geom_point()

# Keep the lambda with lowest error for final test
lambda_best <- lambda_values[which.min(regularization)]

result <- bind_rows(result, tibble(Method = "Regularization + Bias",
                                   RMSE = min(regularization)))

result %>% knitr::kable()

# Accuracy Check Regularization --------
# Checking error with the lambda value

# keeping it simple checking to see if the regularization worked
reg <- edx_train2 %>% 
  group_by(movieId) %>% 
  summarize(m_bias = sum(rating - mu)/(n()+lambda_best), n = n())

edx_train2 %>% 
  count(movieId) %>% 
  left_join(reg) %>% 
  left_join(titles, by = "movieId") %>% 
  arrange(-m_bias) %>% 
  select(title, m_bias, n) %>% 
  slice(1:10) %>% 
  knitr::kable()



# Final Regularization Model --------------------

lambda_best

mean_edx <- mean(edx2$rating)

movie_bias <- edx2 %>% 
  group_by(movieId) %>% 
  summarize(movie_bias = sum(rating - mean_edx)/(n()+lambda_best))

user_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  group_by(userId) %>% 
  summarize(user_bias = sum(rating - mean_edx - movie_bias)/(n()+lambda_best))

genre_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias,  by = "userId") %>%
  group_by(genres) %>% 
  summarise(genre_bias = sum(rating - mean_edx - movie_bias - 
                               user_bias)/(n()+lambda_best))

year_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias,  by = "userId")  %>%
  left_join(genre_bias, by = "genres")  %>%
  group_by(year) %>%
  summarise(year_bias = sum(rating - mean_edx - movie_bias - 
                              user_bias - genre_bias)/(n()+lambda_best))

date_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId")  %>% 
  left_join(genre_bias, by = "genres")  %>%
  left_join(year_bias, by = "year")    %>%
  group_by(date) %>% 
  summarise(date_bias = sum(rating - mean_edx - movie_bias - 
                              user_bias - genre_bias - 
                              year_bias)/(n()+lambda_best))

time_bias <- edx2 %>% 
  left_join(movie_bias, by = "movieId") %>% 
  left_join(user_bias, by = "userId")  %>% 
  left_join(genre_bias, by = "genres")  %>%
  left_join(year_bias, by = "year")    %>%
  left_join(date_bias, by = "date")    %>%
  group_by(age) %>% 
  summarise(time_bias = sum(rating - mean_edx - movie_bias - 
                              user_bias - genre_bias - 
                              year_bias - date_bias)/(n()+lambda_best))

week_bias <- edx2 %>%
  left_join(movie_bias, by = "movieId") %>%
  left_join(user_bias, by = "userId")   %>%
  left_join(genre_bias, by = "genres")  %>%
  left_join(year_bias, by = "year")     %>%
  left_join(date_bias, by = "date")     %>% 
  left_join(time_bias, by = "age")    %>% 
  group_by(day_of_week)  %>%
  summarise(week_bias = sum(rating - mean_edx - movie_bias - 
                              user_bias - genre_bias - 
                              year_bias - date_bias - 
                              time_bias)/(n()+lambda_best))

predict_ratings_edx <- validation2 %>% 
  left_join(movie_bias, by = "movieId")%>% 
  left_join(user_bias,  by = "userId") %>%
  left_join(genre_bias, by = "genres") %>% 
  left_join(year_bias,  by = "year")   %>% 
  left_join(date_bias,  by = "date")   %>% 
  left_join(time_bias, by = "age")    %>% 
  left_join(week_bias, by = "day_of_week") %>% 
  mutate(pred = mean_edx + movie_bias + user_bias + genre_bias + year_bias + date_bias + time_bias + week_bias) %>% 
  pull(pred)

RMSE(predict_ratings_edx, validation2$rating)

result <- bind_rows(result, tibble(Method = "Final Error: Regularization + Bias",
                                   RMSE = RMSE(predict_ratings_edx, validation2$rating)))
result %>% knitr::kable()

# Check Final RMSE meets desired goal
RMSE(predict_ratings_edx, validation2$rating) < 0.86490

# Reco systems-----------------
# first is the train and test sets

# WARNING this section of code involving the recosystem took ~ 2 hours to run each

if(!require(recosystem))
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

# Set seed because it is a random function
set.seed(2020, sample.kind = "Rounding")
reco_train_data <- with(edx_train2, data_memory(user_index = userId,
                                               item_index = movieId,
                                               rating = rating))
reco_test_data <- with(edx_test2, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating = rating))
reco <- Reco()
tune <- reco$tune(reco_train_data)
reco_train <- reco$train(reco_train_data, opts = tune$min)
reco_predict <- reco$predict(reco_test_data, out_memory())
reco_rmse <-RMSE(reco_predict, edx_test2$rating)
result <- bind_rows(result, tibble(Method = "Matrix Factorization, RecoSystem",
                                   RMSE = reco_rmse))


# Final RecoSystem - Matrix Factorization ------------------------
# Uses the orginial, unmodified, test and train set as it does not use any mutated columns
set.seed(2020, sample.kind = "Rounding")
reco_train_data_edx <- with(edx, data_memory(user_index = userId,
                                             item_index = movieId,
                                             rating = rating))
reco_test_data_edx <- with(validation, data_memory(user_index = userId,
                                                   item_index = movieId,
                                                   rating = rating))

reco_edx <- Reco()
tune_edx <- reco_edx$tune(reco_train_data_edx)
reco_edx_train <- reco_edx$train(reco_train_data_edx, opts = tune_edx$min)
reco_predict_edx <- reco_edx$predict(reco_test_data_edx, out_memory())
reco_rmse_edx <- RMSE(reco_predict_edx, validation$rating)
result <- bind_rows(result, tibble(Method = "Final Error: RecoSystem",
                                   RMSE = reco_rmse_edx))

result %>% knitr::kable()
# Results ------------------------------
# Make sure results appear in proper order in graphs
result$Method <- factor(result$Method, levels = result$Method)

# Table with all results
result %>% knitr::kable()

# Graph with All results
result %>% 
  ggplot(aes(x = Method, y = RMSE)) +
  geom_point() +
  ggtitle("All Results") +
  xlab("Method") +
  ylab("RMSE") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Plot of all results with error less than 0.9 and greater than 0.8
result %>% 
  filter(RMSE < 0.94 & RMSE > 0.8) %>% 
  ggplot(aes(x = Method, y = RMSE)) +
  geom_point() +
  ggtitle("All Results") +
  xlab("Method") +
  ylab("RMSE") +
  theme_economist()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# end space ------------------------------










