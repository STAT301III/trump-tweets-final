# Modeling Methods - script 2
# Sentiment analysis


# Read-in packages
library(tidyverse)
library(randomForest)

# Packages for sentiment analysis
library(tidytext)
library(tm)
library(knitr)
library(lubridate)


# Read-in data
train_trump <- readRDS("data/processed/train_trump.rds")
test_trump <- readRDS("data/processed/test_trump.rds")
final_test_trump <- readRDS("data/processed/final_test_trump.rds")

# Prepare data
# Based on sentiment analysis tutorial on Datacamp
# https://www.datacamp.com/community/tutorials/R-nlp-machine-learning

#### Training data
trump_text <- train_trump %>% 
  dplyr::select(text, created_at, favorite_count, retweet_count, tweet_id, is_retweet) %>% 
  mutate(year = year(created_at)) 

# Function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# Fix (expand) contractions
# e.g. won't = will not
trump_text$text <- sapply(trump_text$text, fix.contractions)

# Function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# Remove special characters
trump_text$text <- sapply(trump_text$text, removeSpecialChars)

# Convert text to lower case
trump_text$text <- sapply(trump_text$text, tolower)

# Undesirable words
# e.g. https means a link was included, but does not give much information about a tweet otherwise
filter_out <- c("realdonaldtrump", "https", "http")

# Remove stopwords and unnest
# stopwords such as "has" or "this"
trump_text_filtered <- trump_text %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  distinct() %>% 
  filter(!word %in% filter_out)


#### On test data from training set
trump_text_test <- test_trump %>% 
  dplyr::select(text, created_at, favorite_count, retweet_count, tweet_id, is_retweet) %>% 
  mutate(year = year(created_at)) 

# Function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# Fix (expand) contractions
# e.g. won't = will not
trump_text_test$text <- sapply(trump_text_test$text, fix.contractions)

# Function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# Remove special characters
trump_text_test$text <- sapply(trump_text_test$text, removeSpecialChars)

# Convert text to lower case
trump_text_test$text <- sapply(trump_text_test$text, tolower)

# Undesirable words
# e.g. https means a link was included, but does not give much information about a tweet otherwise
filter_out <- c("realdonaldtrump", "https", "http")

# Remove stopwords and unnest
# stopwords such as "has" or "this"
trump_text_filtered_test <- trump_text_test %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  distinct() %>% 
  filter(!word %in% filter_out)




#### On final test data
trump_text_test_final <- final_test_trump %>% 
  dplyr::select(text, created_at, favorite_count, retweet_count, tweet_id, is_retweet) %>% 
  mutate(year = year(created_at)) 

# Function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  doc <- gsub("won't", "will not", doc)
  doc <- gsub("can't", "can not", doc)
  doc <- gsub("n't", " not", doc)
  doc <- gsub("'ll", " will", doc)
  doc <- gsub("'re", " are", doc)
  doc <- gsub("'ve", " have", doc)
  doc <- gsub("'m", " am", doc)
  doc <- gsub("'d", " would", doc)
  # 's could be 'is' or could be possessive: it has no expansion
  doc <- gsub("'s", "", doc)
  return(doc)
}

# Fix (expand) contractions
# e.g. won't = will not
trump_text_test_final$text <- sapply(trump_text_test_final$text, fix.contractions)

# Function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)

# Remove special characters
trump_text_test_final$text <- sapply(trump_text_test_final$text, removeSpecialChars)

# Convert text to lower case
trump_text_test_final$text <- sapply(trump_text_test_final$text, tolower)

# Undesirable words
# e.g. https means a link was included, but does not give much information about a tweet otherwise
filter_out <- c("realdonaldtrump", "https", "http")

# Remove stopwords and unnest
# stopwords such as "has" or "this"
trump_text_filtered_test_final <- trump_text_test_final %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  distinct() %>% 
  filter(!word %in% filter_out)


#### ------------------- Training set analysis ------------------- ####

# Look at highest word counts
highest_word_count <- trump_text_filtered %>% 
  count(word, sort = TRUE) %>% 
  top_n(20) %>% 
  ungroup() %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot() +
  geom_col(aes(word, n)) +
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_blank()) +
  xlab("") + 
  ylab("Tweet Count") +
  ggtitle("Most Frequently Used Words in Trump Tweets") +
  coord_flip()

# What's up with amp? Cutoff of campaign? part of a link?
trump_text_filtered %>% 
  filter(word == "amp")

trump_text_filtered %>% 
  filter(word == "campaign") 


# Word count distribution across tweets
full_word_count <- trump_text %>% 
  unnest_tokens(word, text) %>% 
  # chose these variables so each tweet gets its own value
  group_by(tweet_id) %>% 
  summarise(num_words = n()) %>% 
  arrange(desc(num_words))

# Trump does have a varied range of tweet length...from as little as one word to using 266 (must be after Twitter expanded character limits)
full_word_count %>% summary()

# Word length? Trump is known for his perhaps limited vocabulary
# Unsurprisingly, many short words.
trump_word_lengths <- trump_text %>% 
  unnest_tokens(word, text) %>% 
  group_by(tweet_id) %>% 
  distinct() %>% 
  filter(!word %in% filter_out) %>% 
  mutate(word_length = nchar(word))

trump_word_length_plot <- trump_word_lengths %>% 
  count(word_length, sort = TRUE) %>% 
  ggplot(aes(word_length),
         binwidth = 10) +
  geom_histogram(aes(fill = ..count..),
                 breaks = seq(1, 25, by = 2),
                 show.legend = FALSE) +
  xlab("Word Length") +
  ylab("Word Count") +
  ggtitle("Word Length Distribution") + 
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank())

# What are the ones longer than 15 words?
# Long hashtags of words strung together or people's usernames. 
trump_word_lengths %>% 
  group_by(tweet_id) %>% 
  filter(word_length >= 15) %>% 
  dplyr::select(word_length, word)

# important words, as defined by IDF
idf_method <- trump_text %>% 
  unnest_tokens(word, text) %>% 
  distinct() %>% 
  filter(!word %in% filter_out) %>% 
  count(year, word, sort = TRUE) %>% 
  ungroup() %>% 
  bind_tf_idf(word, year, n)

# see most important words
# Suggests that other words are more important. 
# some of these seem web related, others really don't make sense.
# A lot of these seem related to either the election or sweepstakes. #priorities
idf <- idf_method %>% 
  arrange(desc(tf_idf)) %>% head(20)


#### ------------------- Create topic sets ------------------- ####

## Create dataset based on top ten most frequently used words
# BC the question is - are the most frequently used topics also the most engaged-with tweets?
# removed most common word (trump)
# amp 
# president
# donald
# people
# obama
# america
# country
# time
# run
# trump2016

# Function to pull indices with a given word
word_index <- function(topic) {
  trump_text_filtered %>% 
    group_by(tweet_id) %>% 
    mutate(topic = ifelse(word %in% c(topic), 1, 0)) %>% 
    filter(topic == 1) %>% 
    dplyr::select(tweet_id)
}

amp_index <- word_index("amp")
president_index <- word_index("president")
donald_index <- word_index("donald")
people_index <- word_index("people")
obama_index <- word_index("obama")
america_index <- word_index("america")
country_index <- word_index("country")
time_index <- word_index("time")
run_index <- word_index("run")
trump2016_index <- word_index("trump2016")


# Create tibble with topics coded as columns 
# Select features that characterize tweets (all_caps, exclamation point count)
# No need to include created_at for modeling purposes

# Training set
topic_trump_train <- train_trump %>%  
  dplyr::select(tweet_id, retweet_count, favorite_count, exclam_count, all_caps_count) %>% 
  mutate(amp = ifelse(tweet_id %in% amp_index$tweet_id, 1, 0),
         president = ifelse(tweet_id %in% president_index$tweet_id, 1, 0),
         donald = ifelse(tweet_id %in% donald_index$tweet_id, 1, 0),
         people = ifelse(tweet_id %in% people_index$tweet_id, 1, 0),
         obama = ifelse(tweet_id %in% obama_index$tweet_id, 1, 0),
         america = ifelse(tweet_id %in% america_index$tweet_id, 1, 0),
         country = ifelse(tweet_id %in% country_index$tweet_id, 1, 0),
         time = ifelse(tweet_id %in% time_index$tweet_id, 1, 0),
         run = ifelse(tweet_id %in% run_index$tweet_id, 1, 0),
         trump2016 = ifelse(tweet_id %in% trump2016_index$tweet_id, 1, 0)) %>% 
  dplyr::select(-tweet_id) %>% 
  filter(!is.na(retweet_count))

# Test set (from training set)
topic_trump_test <- test_trump %>%  
  dplyr::select(tweet_id, retweet_count, favorite_count, exclam_count, all_caps_count) %>% 
  mutate(amp = ifelse(tweet_id %in% amp_index$tweet_id, 1, 0),
         president = ifelse(tweet_id %in% president_index$tweet_id, 1, 0),
         donald = ifelse(tweet_id %in% donald_index$tweet_id, 1, 0),
         people = ifelse(tweet_id %in% people_index$tweet_id, 1, 0),
         obama = ifelse(tweet_id %in% obama_index$tweet_id, 1, 0),
         america = ifelse(tweet_id %in% america_index$tweet_id, 1, 0),
         country = ifelse(tweet_id %in% country_index$tweet_id, 1, 0),
         time = ifelse(tweet_id %in% time_index$tweet_id, 1, 0),
         run = ifelse(tweet_id %in% run_index$tweet_id, 1, 0),
         trump2016 = ifelse(tweet_id %in% trump2016_index$tweet_id, 1, 0)) %>% 
  dplyr::select(-tweet_id) %>% 
  filter(!is.na(retweet_count))

# Final test set
topic_trump_test_final <- final_test_trump %>%  
  dplyr::select(tweet_id, retweet_count, favorite_count, exclam_count, all_caps_count) %>% 
  mutate(amp = ifelse(tweet_id %in% amp_index$tweet_id, 1, 0),
         president = ifelse(tweet_id %in% president_index$tweet_id, 1, 0),
         donald = ifelse(tweet_id %in% donald_index$tweet_id, 1, 0),
         people = ifelse(tweet_id %in% people_index$tweet_id, 1, 0),
         obama = ifelse(tweet_id %in% obama_index$tweet_id, 1, 0),
         america = ifelse(tweet_id %in% america_index$tweet_id, 1, 0),
         country = ifelse(tweet_id %in% country_index$tweet_id, 1, 0),
         time = ifelse(tweet_id %in% time_index$tweet_id, 1, 0),
         run = ifelse(tweet_id %in% run_index$tweet_id, 1, 0),
         trump2016 = ifelse(tweet_id %in% trump2016_index$tweet_id, 1, 0)) %>% 
  dplyr::select(-tweet_id) %>% 
  filter(!is.na(retweet_count))


#### ------------------- Modeling using topic sets ------------------- ####

# Based on initial modeling, it seems that random forests and polynomial regression work the best

names(topic_trump_train)
# - Random forests - #
rf.trumptopics <- randomForest(retweet_count ~ ., data = topic_trump_train, importance = TRUE)

# to see training mse
# MSE = 11455656
rf.trumptopics
rf.trumptopics$mse[500]


# Evaluate on test data
yhat_rf_topics <- predict(rf.trumptopics, newdata = topic_trump_test)
rf_topics_mse <- mean((yhat_rf_topics - topic_trump_test$retweet_count)^2)

# influences still lies in favorite count
# but also in all_caps_count?
# of the topics, obama came out on top for highest %IncMSE
importance(rf.trumptopics)
sent_rf_varimp <- varImpPlot(rf.trumptopics)


# - Polynomial regression - #

# Degree = 6
poly_fit_topic <- lm(retweet_count ~ poly(favorite_count, 6) + exclam_count + all_caps_count + amp + president + donald + people + obama + america + country + time + run + trump2016, data = topic_trump_train)


# RSE = 2800
poly_sum_topic <- summary(poly_fit_topic)

# Check training error
poly_train_topic_mse <- mean(poly_sum_topic$residuals^2)

poly_pred_topic <- predict(poly_fit_topic, topic_trump_test)
poly_test_topic_mse <- mean((poly_pred_topic - topic_trump_test$retweet_count)^2)

# Degree = 4
poly_fit_topic2 <- lm(retweet_count ~ poly(favorite_count, 4) + exclam_count + all_caps_count + amp + president + donald + people + obama + america + country + time + run + trump2016, data = topic_trump_train)


# RSE = 2800
poly_sum_topic2 <- summary(poly_fit_topic2)

# Check training error
poly_train_topic_mse2 <- mean(poly_sum_topic2$residuals^2)

poly_pred_topic2 <- predict(poly_fit_topic2, topic_trump_test)
poly_test_topic_mse2 <- mean((poly_pred_topic2 - topic_trump_test$retweet_count)^2)


# Compare error
# random forests still performs better. Possible that polynomail regression is overfitting?
topics_errors <- tibble(Method = c("random forest", "polynomial regression: degree = 6", "polynomial regression: degree = 4"),
       Train_MSE = c(rf.trumptopics$mse[500], poly_train_topic_mse, poly_train_topic_mse2), Test_MSE = c(rf_topics_mse, poly_test_topic_mse, poly_test_topic_mse2)) %>% 
  arrange(Test_MSE)

write_rds(topics_errors, path = "results/topics_errors.rds")
