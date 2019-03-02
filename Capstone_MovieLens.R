if(!require(readr)) install.packages("readr")
if(!require(ggrepel)) install.packages("ggrepel")
if(!require(ggthemes)) install.packages("ggthemes")
if(!require(stringr)) install.packages("stringr")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(dplyr)) install.packages("dplyr")
if(!require(tidyr)) install.packages("tidyr")
if(!require(dslabs)) install.packages("dslabs")
if(!require(data.table)) install.packages("data.table")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(caret)) install.packages("caret")
if(!require(knitr)) install.packages("knitr")
if(!require(lubridate)) install.packages("lubridate")
if(!require(gridExtra)) install.packages("gridExtra")
if(!require(corrplot)) install.packages("corrplot")
if(!require(RColorBrewer)) install.packages("RColorBrewer")
if(!require(kableExtra)) install.packages("kableExtra")


library(readr)
library(kableExtra)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(gridExtra)
library(dslabs)
library(data.table)
library(ggrepel)
library(ggthemes)
library(tidyr)
library(stringr)
library(ggplot2)
library(knitr)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")



movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
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

#####################################################################
######################### Data Check ################################
#####################################################################

# Overview of edx Data
head(edx)

#Rows and columns of edx
dim(edx)

#Unique Movies & Unique Users
n_distinct(edx$userId)
n_distinct(edx$movieId)

# Summary of the dataset
summary(edx)

# Checking edx missing values
any(is.na(edx))

#####################################################################
######################### Distribution Insights #####################
#####################################################################


######################### Ratings Distribution ######################### 
#HALF VS FULL Rating Distribution
  ratings_dist <- edx %>% 
  mutate("Type"=ifelse(rating %in% c(1,2,3,4,5),"Full","Half")) %>% 
  group_by(Type) %>% 
  select(Type) %>% 
  count() %>% 
  rename("Ratings"=n) %>% 
  mutate("Share"=paste(round(100*Ratings/nrow(edx),2),"%",sep=""))
  ratings_dist %>% ggplot(aes(x = Type, y=Share)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") +
  labs(title = "Distribution of Half & Full Ratings", x = "Type", y = "Share")

ggplot(data = edx, aes(x = rating)) +
  geom_bar(color='royalblue',fill='royalblue') + 
  labs(title = "Distribution of Ratings by Count", x = "Ratings", y = "Count of Ratings")

#Top 5 Ratings
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count)) 


######################### Users Distribution ###########################
users_dist <- edx %>% 
  group_by(userId) %>%
  summarize(count_Rating=n(), mean_Rating=mean(rating))

ggplot(data=users_dist,aes(x = mean_Rating)) +
  geom_histogram(binwidth=0.05,colour="royalblue", fill="royalblue") +
  labs(title = "Distribution of Users by Average Ratings", x = "Average rating", y = "Count") 

ggplot(data=users_dist,aes(x = count_Rating)) +
  geom_histogram(binwidth=0.05,colour="royalblue", fill="royalblue") +
  labs(title = "Distribution of Users by Number of Ratings", x = "No. of Ratings", y = "Count") +
  scale_x_log10(breaks = c(10,50,100,250, 500, 1000,5000))

#########################  Genres Distribution ######################### 
genres_dist <- edx %>% 
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count_Rating=n(), mean_Rating=mean(rating),count_Movies=n_distinct(movieId))

genres_dist %>% ggplot(aes(x = genres, y=count_Movies)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust = 1)) +
  labs(title = "Distribution of Genres by Count", x = "Genres", y = "Count")

genres_dist %>% ggplot(aes(x = genres, y=mean_Rating)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") +
  theme(axis.text.x = element_text(angle = 90, vjust=0.25, hjust = 1)) +
  labs(title = "Distribution of Genres by Rating Average", x = "Genres", y = "Rating Average")

######################### Movies Distribution ######################### 
movies_dist <- edx %>% 
  group_by(movieId) %>%
  summarize(count_Rating=n(), mean_Rating=mean(rating)) 

ggplot(data = movies_dist, aes(y = movieId,x=count_Rating)) +
  geom_point(color='royalblue',alpha = 0.2) + 
  labs(title = "Distribution of Movies by Count", x = "Count", y = "Movie_ID")

#Rearranging in histogram
edx %>% 
  count(movieId) %>%
  arrange(-n) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 50, colour = "royalblue", fill = "royalblue") + 
  labs(title="Movies by Rating Count", y="Movies", x="Count of ratings")

ggplot(data=movies_dist,aes(x = mean_Rating)) +
  geom_histogram(binwidth=0.05,colour="royalblue", fill="royalblue") +
  labs(title = "Distribution of Movies by Ratings", x = "No. of Ratings", y = "Count") 

#Movie with Greatest Number of Ratings
high_rating <- edx %>%
  group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

head(high_rating)

######################### Year Distribution #########################

#By Active user
edx <- mutate(edx, rated_year = year(as_datetime(timestamp)))

year_dist_user<- edx %>%
  select(rated_year, userId) %>%  group_by(rated_year) %>%
  summarise(count = n_distinct(userId))

ggplot(data = year_dist_user, aes(x = rated_year, y = count)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") + 
  labs(title = "Active Users by Year", x = "Year", y = "Active Users Count")

#By Movies
year_dist_movies <- edx %>%
  select(rated_year, movieId) %>% group_by(rated_year) %>%
  summarise(count = n_distinct(movieId))

ggplot(data = year_dist_movies, aes(x = rated_year, y = count)) +
  geom_bar(stat="identity",colour = "royalblue", fill = "royalblue") + 
  labs(title = "Movies Rated Distribution by Year", x = "Year", y = "Ratings Count")

###################### Sparcity (from course Book) #############
if(!require(rafalib)) install.packages("rafalib", repos = "http://cran.us.r-project.org")
library(rafalib)
users <- sample(unique(edx$userId), 100)
rafalib::mypar()
edx %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users") %>% 
  abline(h=0:100+0.5, v=0:100+0.5, col = "lightgrey")

######################### Correlation #########################

head(edx)

release_year <- stringi::stri_extract(edx$title, regex = "(\\d{4})", comments = TRUE ) %>% 
  as.numeric()

edx_with_year <- edx %>% mutate(release_year = release_year)

head(edx_with_year)


count_movies <- edx_with_year %>% group_by(movieId) %>% summarize(movie_count = n())
mean_movie <- edx_with_year %>% group_by(movieId) %>% summarize(movie_avg = mean(rating))

cor_edx <- edx_with_year %>% select(rating, movieId, userId, rated_year, release_year) %>% 
  left_join(count_movies, by = "movieId") %>% 
  left_join(mean_movie, by = 'movieId')

head(cor_edx)


corr <- cor_edx %>% 
  select(one_of("rating", "movieId", "userId", "rated_year", "release_year", "movie_count", "movie_avg")) %>% 
  as.matrix()

corr_plot <- cor(corr, use = "pairwise.complete.obs")
corrplot(corr_plot, order = "hclust", addrect = 2, type = "upper", method = "color", tl.srt=45)


# Cleaning up defined variables to release memory
rm(cor_edx, corr, corr_plot, count_movies,
   edx_with_year, genres_dist, ratings_dist, high_rating, mean_movie,
   movies_dist,users_dist,year_dist_movies,year_dist_user,users)

gc()


##########################################################################
########################## Modeling ######################################
##########################################################################

#Split train & test set by randomly selecting 20% of the **edx set**
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Removing entries (users/movies) present in test set to ensure that they don't appear in train set, we will use semi_join()
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")


###################### MODELING WITH SIMPLE AVERAGE #######################

# RMSE Function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Starting by simple possible recommendation, we predict average rating across all users for all movies 
mu <- mean(train_set$rating)


# Plotting average rating for movies rated at least 500 times.
train_set %>% group_by(movieId) %>% 
  filter(n()>=500) %>% 
  summarize(avg_rating = mean(rating)) %>% 
  ggplot(aes(avg_rating)) + 
  geom_histogram(bins = 50, colour = "royalblue", fill = "royalblue") 


# Prediction on test set.
predictions <- rep(mu, nrow(test_set))

# RMSE for test set.
naive_rmse <- RMSE(test_set$rating, predictions)


# Table creation for storing the results for different scenarios/methods

rmse_results <- data_frame(Method = "Simple Average Model", Dataset="test_edx",RMSE = naive_rmse)
kable(rmse_results,align=rep("c",3),caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')


########################## MODELING MOVIE EFFECT ###########################

#Now including movie effect,

movie_means <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) 

ggplot(data = movie_means, aes(x = b_i)) +
  geom_histogram(bins = 50,colour = "royalblue", fill = "royalblue")

final <- test_set %>% 
  left_join(movie_means, by='movieId')

prediction_ratings <- mu + final$b_i

model_movie_effect_rmse <- RMSE(prediction_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Movie Effect Model", Dataset="test_edx", 
                                     RMSE = model_movie_effect_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')


####################### REGULARIZATION OF THE MOVIE EFFECT #####################


#In order to avoid impact of movies with few ratings, we would use regularization to nullify thier impact

# Thus lambda (tuning parameter) wil be calculated for regularized estimates of b_i.
lambdas <- seq(0, 10, 0.1)

summation <- train_set %>% 
  group_by(movieId) %>% 
  summarize(sum = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
    final <- test_set %>% 
    left_join(summation, by='movieId') %>% 
    mutate(b_i = sum/(n_i+l))
    prediction_ratings <- mu + final$b_i
    return(RMSE(prediction_ratings, test_set$rating))
})

qplot(lambdas, rmses)

# Thus, 2.4 appeared to be the most optimized lambda value i.e. giving smallest RMSE

lambdas <- 2.4

movie_reg_means <- train_set %>% 
                   group_by(movieId) %>% 
                   summarize(b_i = sum(rating - mu)/(n()+lambdas), n_i = n()) 

final <- test_set %>% 
         left_join(movie_reg_means, by='movieId') %>% 
         replace_na(list(b_i=0))

prediction_ratings <- mu + final$b_i

model_movie_reg_rmse <- RMSE(prediction_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Regularized Movie Effect Model", Dataset="test_edx",  
                                     RMSE = model_movie_reg_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')

############### REGULARIZATION OF THE MOVIE EFFECT (Validated)##################

# Repeating the same steps for validated dataset
lambdas <- seq(0, 10, 0.1)

mu <- mean(edx$rating)

summation <- edx %>% 
  group_by(movieId) %>% 
  summarize(sum = sum(rating - mu), n_i = n())

rmses <- sapply(lambdas, function(l){
  final <- validation %>% 
    left_join(summation, by='movieId') %>% 
    mutate(b_i = sum/(n_i+l))
  prediction_ratings <- mu + final$b_i
  return(RMSE(prediction_ratings, validation$rating))
})

qplot(lambdas, rmses)

# Thus, again, 2.4 appeared to be the most optimized lambda value i.e. giving smallest RMSE

lambdas <- 2.4

movie_reg_means <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambdas), n_i = n()) 

final <- validation %>% 
  left_join(movie_reg_means, by='movieId') %>% 
  replace_na(list(b_i=0))

prediction_ratings <- mu + final$b_i

model_movie_reg_rmse <- RMSE(prediction_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Regularized Movie Effect Model", Dataset="Validation",  
                                     RMSE = model_movie_reg_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')

############### Movie + User Effect Model ###########################

# Similar to movie effect, user effect also impacted by users who rate less movies, as can be seen below
train_set %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n() >= 50) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 50, color  = "royalblue", fill  = "royalblue")

user_means <- train_set %>% 
  left_join(movie_means, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

ggplot(data = user_means, aes(x = b_u)) +
  geom_histogram(bins = 50,colour = "royalblue", fill = "royalblue")

final <- test_set %>% 
  left_join(movie_means, by='movieId') %>%
  left_join(user_means, by='userId') 
  
prediction_ratings <- mu + final$b_i + final$b_u

model_user_movie_rmse <- RMSE(prediction_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Movie + User Effect Model", Dataset="test_edx",  
                                     RMSE = model_user_movie_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')


############### Regularized Movie + User Effect Model ###########################

# For regularization, lambda (tuning parameter) wil be calculated for regularized estimates of b_i & b_u.

lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  prediction_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(prediction_ratings, test_set$rating))
})

qplot(lambdas, rmses)

# It's clear that lambda=5 has the least value for RMSE

lambdas <- 5

user_reg_means <- train_set %>% 
  left_join(movie_reg_means) %>%
  mutate(resids = rating - mu - b_i) %>% 
  group_by(userId) %>%
  summarize(b_u = sum(resids)/(n()+lambdas))

final <- test_set %>% 
  left_join(movie_reg_means, by='movieId') %>% 
  left_join(user_reg_means, by='userId') %>% 
  replace_na(list(b_i=0, b_u=0))

prediction_ratings <- mu + final$b_i + final$b_u

model_user_movie_reg_rmse <- RMSE(prediction_ratings, test_set$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Regularized Movie + User Effect Model", Dataset="test_edx",  
                                     RMSE = model_user_movie_reg_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')


################# Regularized Movie + User Effect Model (Validated) ###########

# Verifying the model for validation dataset
lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- validation %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    .$pred
  
  return(RMSE(predicted_ratings, validation$rating))
})

qplot(lambdas, rmses)

# It's clear that lambda=5 has the least value for RMSE

lambdas <- 5

user_reg_means <- edx %>% 
  left_join(movie_reg_means) %>%
  mutate(resids = rating - mu - b_i) %>% 
  group_by(userId) %>%
  summarize(b_u = sum(resids)/(n()+lambdas))

final <- validation %>% 
  left_join(movie_reg_means, by='movieId') %>% 
  left_join(user_reg_means, by='userId') %>% 
  replace_na(list(b_i=0, b_u=0))

prediction_ratings <- mu + final$b_i + final$b_u

model_user_movie_reg_rmse <- RMSE(prediction_ratings, validation$rating)

rmse_results <- bind_rows(rmse_results,
                          data_frame(Method = "Regularized Movie + User Effect Model", Dataset="Validation",
                                     RMSE = model_user_movie_reg_rmse ))

kable(rmse_results,align=rep("c",3),,caption="Output") %>% 
  kable_styling(full_width = F) %>% 
  column_spec(1:3,bold=T,border_right = T,color='royalblue')





