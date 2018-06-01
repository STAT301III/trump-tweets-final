
# read-in packages
library(tidyverse)
library(tree)
library(MASS)
library(randomForest)
library(gbm)
library(ISLR) # for polynomail regression


## Adjusted because of script issues
trump_all <- readRDS("data/processed/trump_all.rds")

set.seed(10)

# Of 36242 obs, select random indices to split into test and training
train_index <- sample(1:nrow(trump_all), 0.8 * nrow(trump_all))
test_index <- setdiff(1:nrow(trump_all), train_index)

# Pull observations of selected indices 
# Final test data (7249 observations) not touched until final model evaluation
train_trump <- trump_all[train_index,]
final_test_trump <- trump_all[test_index,]

# Of 28993 obs of training data, set aside 80% for training and 20% for testing during modeling.
train_index <- sample(1:nrow(train_trump), 0.8 * nrow(train_trump))
test_index <- setdiff(1:nrow(train_trump), train_index)

# Pull observations of selected indices
# Leaves 23194 obs in training
# Leaves 5799 obs in testing
train_trump <- train_trump[train_index,] 
test_trump <- train_trump[test_index,]

# Add tweet id column to each
train_trump <- train_trump %>%
  rowid_to_column("tweet_id")

test_trump <- test_trump %>%
  rowid_to_column("tweet_id")

final_test_trump <- final_test_trump %>% 
  rowid_to_column("tweet_id")

range(trump_all$created_at, na.rm = TRUE)


# predicting retweet_count (regression) -----------------------------------------------------------

# Note range of retweet_count when considering MSE
# from 0 to 370245
range(train_trump$retweet_count, na.rm = TRUE)

# Multiple Linear Regression
train_trump_num <- train_trump %>%
  dplyr::select(is_retweet, favorite_count, exclam_count, all_caps_count, uppercase_first_count, retweet_count) %>% 
  filter(!is.na(favorite_count) & !is.na(retweet_count))

lm_fit <- lm(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump)

# Very high MSE
mlm_sm <- summary(lm_fit)
mlm_MSE <- mean(mlm_sm$residuals^2)


# Remove missing values of favorite for prediction
poly_fit1 <- lm(retweet_count ~ poly(favorite_count, 3) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)
# store model fit summary
# RSE = 3412
poly_sum <- summary(poly_fit)

# Different polynomial?
poly_fit2 <- lm(retweet_count ~ poly(favorite_count, 4) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)
# RSE = 3388
summary(poly_fit2)

anova(poly_fit1, poly_fit2)

# RMSE
#sqrt(sum(residuals(poly_sum)^2) / df(poly_sum))
# check page 291? 



### -- Decision trees -- ###

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
boosted1_mse <- mean((yhat.boost - test_trump_rt$retweet_count)^2, na.rm = TRUE)

# What if we take out favorite count, since it seems overwhelmingly influential?
boost.trump2 <- gbm(retweet_count ~ is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, distribution = "gaussian", n.trees = 5000)

# Without favorite count, use of Exclamation points and all caps seem to be the most influential
summary(boost.trump2)

# However, it's a terrible predictor of user engagement (rewtweet + favorite counts)
yhat.boost2 <- predict(boost.trump2, newdata = test_trump_rt, n.trees = 5000)
boosted2_mse <- mean((yhat.boost2 - test_trump_rt$retweet_count)^2, na.rm = TRUE)

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

errors <- tribble(~boosted1, ~boosted2, ~rf1, ~rf2, ~mlm, boosted1_mse, boosted2_mse, rf1_mse, rf2_mse, mlm_MSE)

knitr::kable(errors)
# try standard deviations as well since it's nicer to compare

# It seems that the current predictors are not great indicators of which tweets will be successful. Let's try looking at the actual content of the tweets itself?


# NNet
# one-hot encoding of text to predict retweet count

# USE NEURAL NETS WITH THE ACTUAL WORDS TO TRY AND PREDICT






 



