##########################################################
# Create edx and final_holdout_test sets 
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


###########################################
#Begin edx dataset analysis
###########################################

library(tidyverse)
library(caret)
library(lubridate)
library(gam)

#making the RMSE function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#Purpose: To predict a user's rating for a film given the date it was made

#add a "date" column to the edx function. Format dates and round them to the day
edx <- mutate(edx, date = as_datetime(timestamp))
#remove the times from the dates and format it Year-Month-Date
edx <- mutate(edx, date = round_date(date, unit = "day"))

#create the train and test sets using the indexes
set.seed(468)
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE) # create a 20% test set
test_set <- edx[test_index,]
train_set <- edx[-test_index,]

#make sure that test set doesn't include users, dates, and movies that don't appear in training
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = "date")

#d_ui = day of rating of movie i

#finding which lambda has the least RMSE
lambdas <- lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l) {
  mu <- mean(train_set$rating)
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  d_ui <- train_set %>% left_join(b_u, by=c("userId")) %>%
    group_by(date) %>%
    summarize(d_ui = mean(rating)/(n()+l))
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(d_ui, by = "date") %>%
    mutate(pred = (mu + b_i + b_u + d_ui)) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})

lambdas[which.min(rmses)]

#running the code with the lamda creating the least RMSE
l <- 4.75
mu <- mean(train_set$rating)
b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))
b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))
d_ui <- train_set %>% left_join(b_u, by=c("userId")) %>%
  group_by(date) %>%
  summarize(d_ui = mean(rating)/(n()+l))
predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(d_ui, by = "date") %>%
  mutate(pred = (mu + b_i + b_u + d_ui)) %>%
  pull(pred)

#testing model RMSE with final_holdout_test
RMSE(predicted_ratings, final_holdout_test$rating)
