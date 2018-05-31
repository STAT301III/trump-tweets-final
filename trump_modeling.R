
# read-in packages
library(tidyverse)
library(tree)
library(MASS)
library(randomForest)
library(gbm)

library(tidytext)
library(tm)

# read-in dataset
trump <- readRDS("data/processed/train_trump.rds")
final_test_trump <- readRDS("data/processed/test_trump.rds")

# Set aside 15 percent of training set as "test" set in an 80/20 split
set.seed(117)

# Select random indices
train_index <- sample(1:nrow(trump), 0.8 * nrow(trump))
test_index <- setdiff(1:nrow(trump), train_index)

# Pull observations of selected indices
train_trump <- trump[train_index,]
test_trump <- trump[test_index,]


# retweet_count -----------------------------------------------------------

# Multiple Linear Regression
train_trump_num <- train_trump %>%
  dplyr::select(is_retweet, favorite_count, exclam_count, all_caps_count, uppercase_first_count)

lm_fit <- lm(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump)

# Very high MSE
mlm_sm <- summary(lm_fit)
mean(mlm_sm$residuals^2)

# Decision trees

# Remove values with missing retweet_count values
# 804 values from training set
train_trump_rt <- train_trump %>%
  filter(!is.na(retweet_count))

# Removes 226 values from test set
test_trump_rt <- test_trump %>%
  filter(!is.na(retweet_count))

# Regression tree
tree_trump <- tree(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump)

# Initial tree seems to heavily rely on favorite count
summary(tree_trump)
plot(tree_trump)
text(tree_trump)

# Calculate test MSE
# Better than linear model, but definitely not great
yhat_1 <- predict(tree_trump, newdata = test_trump)
mean((yhat_1 - test_trump$retweet_count)^2, na.rm = TRUE)

# Cross-validate to determine optimal tree complexity
cv.trumptree <- cv.tree(tree_trump)

# CV selects most complex tree (6) as best in this case
trump_sizedev <- tibble(x = cv.trumptree$size, y = cv.trumptree$dev)
ggplot(trump_sizedev) +
  geom_point(aes(x = x, y = y)) + 
  geom_path(aes(x = x, y = y))

### Boosting?

# Need to change is_retweet from logical to factor for use in gbm
train_trump_rt <- train_trump_rt %>% 
  mutate(is_retweet = factor(is_retweet))

test_trump_rt <- test_trump_rt %>% 
  mutate(is_retweet = factor(is_retweet))

# Fit boosting
boost.trump <- gbm(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, distribution = "gaussian", n.trees = 5000)

# Unsurprisingly, favorite count seems to be very related to favorite count
summary(boost.trump)

yhat.boost <- predict(boost.trump, newdata = test_trump_rt, n.trees = 5000)
boosted1_mse <- mean((yhat.boost - test_trump$retweet_count)^2, na.rm = TRUE)

# What if we take out favorite count, since it seems overwhelmingly influential?
boost.trump2 <- gbm(retweet_count ~ is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, distribution = "gaussian", n.trees = 5000)

# Without favorite count, use of Exclamation points and all caps seem to be the most influential
summary(boost.trump2)

# However, it's a terrible predictor of user engagement (rewtweet + favorite counts)
yhat.boost2 <- predict(boost.trump2, newdata = test_trump, n.trees = 5000)
boosted2_mse <- mean((yhat.boost2 - test_trump$retweet_count)^2, na.rm = TRUE)

### Random forests?
rf.trump <- randomForest(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, mtry = 2, importance = TRUE)

# Random forests are an improvement, but the MSE is still terrible. Perhaps the current predictors are just not effective.
yhat_rf <- predict(rf.trump, newdata = test_trump_rt)
rf1_mse <- mean((yhat_rf - test_trump_rt$retweet_count)^2)

# Like before, favorite count is very influential. Let's take it out.
importance(rf.trump)
varImpPlot(rf.trump)

# Random forest without favorite count
rf.trump2 <- randomForest(retweet_count ~ is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, mtry = 2, importance = TRUE)

# Random forests are an improvement, but the MSE is still terrible. Perhaps the current predictors are just not effective.
# Without favorite count, prediction is again, terrible.
yhat_rf2 <- predict(rf.trump2, newdata = test_trump_rt)
rf2_mse <- mean((yhat_rf2 - test_trump_rt$retweet_count)^2)

errors <- tribble(~boosted1, ~boosted2, ~rf1, ~rf2, boosted1_mse, boosted2_mse, rf1_mse, rf2_mse)

knitr::kable(errors)
# try standard deviations as well since it's nicer to compare

# It seems that the current predictors are not great indicators of which tweets will be successful. Let's try looking at the actual content of the tweets itself?


# NNet
# one-hot encoding of text to predict retweet count

# USE NEURAL NETS WITH THE ACTUAL WORDS TO TRY AND PREDICT

#  classification ---------------------------------------------------------

## predict whether trump will use all-caps or not




# sentiment analysis ------------------------------------------------------

trump_text <- train_trump %>% 
  dplyr::select(text, created_at, favorite_count, retweet_count)

# FROM DATACAMP
# https://www.datacamp.com/community/tutorials/R-nlp-machine-learning

# function to expand contractions in an English-language source
fix.contractions <- function(doc) {
  # "won't" is a special case as it does not expand to "wo not"
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

# fix (expand) contractions
trump_text$text <- sapply(trump_text$text, fix.contractions)

# function to remove special characters
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]", " ", x)
# remove special characters
trump_text$text <- sapply(trump_text$text, removeSpecialChars)

# convert everything to lower case
trump_text$text <- sapply(trump_text$text, tolower)

# remove stopwords and unnest 
trump_text_filtered <- trump_text %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)



