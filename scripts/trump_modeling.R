# Modeling Methods - script 1
# Read-in packages
library(tidyverse)
library(tree)
library(MASS)
library(randomForest)
library(gbm)
library(ISLR)
library(knitr)

# Read-in data

# Some data cleaning included here due to current Twitter API issues (as of 5/30/2018)
trump_all <- readRDS("data/processed/trump_all.rds")

# Set seed for reproducibility
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

# Predicting variable, retweet_count (regression) ----------------------------------------------------------

# Note range of retweet_count when considering MSE
# from 0 to 370245
range(train_trump$retweet_count, na.rm = TRUE)

# Linear Regression
train_trump_num <- train_trump %>%
  dplyr::select(is_retweet, favorite_count, exclam_count, all_caps_count, uppercase_first_count, retweet_count) %>% 
  filter(!is.na(favorite_count) & !is.na(retweet_count))

test_trump_num <- test_trump %>% 
  dplyr::select(is_retweet, favorite_count, exclam_count, all_caps_count, uppercase_first_count, retweet_count) %>% 
  filter(!is.na(favorite_count) & !is.na(retweet_count))

final_test_trump_num <- final_test_trump %>% 
  dplyr::select(is_retweet, favorite_count, exclam_count, all_caps_count, uppercase_first_count, retweet_count) %>% 
  filter(!is.na(favorite_count) & !is.na(retweet_count))

# - LINEAR MODEL - #
lm_fit <- lm(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)

# Very high training MSE
mlm_sm <- summary(lm_fit)
train_lm_MSE <- mean(mlm_sm$residuals^2)

# On test set? (from training data, NOT final test set)
lm_pred_y <- predict(lm_fit, newdata = test_trump_num)

# Calculate residuals/MSE
lm_test_mse <- mean((lm_pred_y - test_trump_num$retweet_count)^2)


# - WITH INTERACTION TERMS - #
lm_fit_interact <- lm(retweet_count ~ is_retweet + favorite_count + exclam_count * all_caps_count + uppercase_first_count, data = train_trump_num)

# Very high training MSE
mlm_sm_interact <- summary(lm_fit_interact)
train_lm_interact_MSE <- mean(mlm_sm$residuals^2)

# On test set? (from training data, NOT final test set)
lm_pred_y_int <- predict(lm_fit_interact, newdata = test_trump_num)

# Calculate residuals/MSE
# This particular interaction doesn't help by much?
lm_test_mse_interact <- mean((lm_pred_y_int - test_trump_num$retweet_count)^2)

# Interaction terms - 2
lm_fit_interact2 <- lm(retweet_count ~ is_retweet + favorite_count + exclam_count * all_caps_count * uppercase_first_count, data = train_trump_num)

# Very high training MSE
mlm_sm_interact2 <- summary(lm_fit_interact2)
train_lm_interact_MSE2 <- mean(mlm_sm$residuals^2)

# On test set? (from training data, NOT final test set)
lm_pred_y_int2 <- predict(lm_fit_interact2, newdata = test_trump_num)

# Calculate residuals/MSE
# Helps a bit more
lm_test_mse_interact2 <- mean((lm_pred_y_int2 - test_trump_num$retweet_count)^2)

# Compare error
linear_fit_error <- tibble(Method = c("Linear model", "Linear - interaction 1", "Linear - interaction 2"),
       Train_MSE = c(train_lm_MSE, train_lm_interact_MSE, train_lm_interact_MSE2), Test_MSE = c(lm_test_mse, lm_test_mse_interact, lm_test_mse_interact2)) %>% 
  arrange(Test_MSE)

write_rds(linear_fit_error, path = "results/linear_fit_error.rds")

# - POLYNOMIAL FIT - #
# Although increasing the degree can lower test MSE, must be wary of overfitting.

# Degree = 3
poly_fit1 <- lm(retweet_count ~ poly(favorite_count, 3) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)

# store model fit summary
# RSE = 3412
poly_sum1 <- summary(poly_fit1)

# Degree = 4
poly_fit2 <- lm(retweet_count ~ poly(favorite_count, 4) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)

# RSE = 3388
poly_sum2 <- summary(poly_fit2)

# Degree = 5
poly_fit3 <- lm(retweet_count ~ poly(favorite_count, 5) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)

# RSE = 2949
poly_sum3 <- summary(poly_fit3)

# Degree = 6
poly_fit4 <- lm(retweet_count ~ poly(favorite_count, 6) + is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_num)

# RSE = 2800
poly_sum4 <- summary(poly_fit4)

# Check training error
anova(poly_fit1, poly_fit2, poly_fit3, poly_fit4)

poly_trainmse1 <- mean(poly_sum1$residuals^2)
poly_trainmse2 <- mean(poly_sum2$residuals^2)
poly_trainmse3 <- mean(poly_sum3$residuals^2)
poly_trainmse4 <- mean(poly_sum4$residuals^2)

# Test error (from training set)
# Still seems that higher degree remains effective in test set
poly_pred1 <- predict(poly_fit1, test_trump_num)
poly_testmse1 <- mean((poly_pred1 - test_trump_num$retweet_count)^2)

poly_pred2 <- predict(poly_fit2, test_trump_num)
poly_testmse2 <- mean((poly_pred2 - test_trump_num$retweet_count)^2)

poly_pred3 <- predict(poly_fit3, test_trump_num)
poly_testmse3 <- mean((poly_pred3 - test_trump_num$retweet_count)^2)

poly_pred4 <- predict(poly_fit4, test_trump_num)
poly_testmse4 <- mean((poly_pred4 - test_trump_num$retweet_count)^2)

# Compare error
polynomial_fit_error <- tibble(Method = c("polynomial regression: degree = 3", "polynomial regression: degree = 4", "polynomial regression: degree = 5", "polynomial regression: degree = 6"),
       Train_MSE = c(poly_trainmse1, poly_trainmse2, poly_trainmse3, poly_trainmse4), Test_MSE = c(poly_testmse1, poly_testmse2, poly_testmse3, poly_testmse4)) %>% 
  arrange(Test_MSE)

write_rds(polynomial_fit_error, path = "results/polynomial_fit_error.rds")

# -- DECISION TREES -- #

# Remove values with missing retweet_count values
# 804 values from training set
train_trump_rt <- train_trump %>%
  filter(!is.na(retweet_count))

# Removes 226 values from test set
test_trump_rt <- test_trump %>%
  filter(!is.na(retweet_count))

# Remove missing from final test data
final_test_trump_rt <- final_test_trump %>%
  filter(!is.na(retweet_count))


# Regression tree
tree_trump <- tree(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump)

# Initial tree seems to heavily rely on favorite count
# Only used favorite count and is_retweet...linguistic options didn't come up.
tree_sum1 <- summary(tree_trump)
plot(tree_trump)
text(tree_trump)

reg_treetrain_mse1 <- mean(tree_sum1$residuals^2)

# Calculate test MSE
# Better than linear model, but definitely not great
yhat_1 <- predict(tree_trump, newdata = test_trump)
reg_treetest_mse1 <- mean((yhat_1 - test_trump$retweet_count)^2, na.rm = TRUE)

# Cross-validate to determine optimal tree complexity
cv.trumptree <- cv.tree(tree_trump)

# CV selects most complex tree (8) as best in this case, which was used by the model
trump_sizedev <- tibble(x = cv.trumptree$size, y = cv.trumptree$dev)
ggplot(trump_sizedev) +
  geom_point(aes(x = x, y = y)) + 
  geom_path(aes(x = x, y = y))

# -- BOOSTING -- #

# Need to change is_retweet from logical to factor for use in gbm
train_trump_rt <- train_trump_rt %>% 
  mutate(is_retweet = factor(is_retweet))

test_trump_rt <- test_trump_rt %>% 
  mutate(is_retweet = factor(is_retweet))

final_test_trump_rt <- final_test_trump_rt %>% 
  mutate(is_retweet = factor(is_retweet))

# Fit boosting
boost.trump1 <- gbm(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, distribution = "gaussian", n.trees = 5000)

# Unsurprisingly, favorite count seems to be very related to favorite count
boost_sum1 <- summary(boost.trump1)
boost_sum1 <- boost.trump1
boosted1_train_MSE <- boost_sum1$train.error[5000]

yhat.boost1 <- predict(boost.trump1, newdata = test_trump_rt, n.trees = 5000)
boosted1_mse <- mean((yhat.boost1 - test_trump_rt$retweet_count)^2, na.rm = TRUE)
boosted1_mse

# What if we take out favorite count, since it seems overwhelmingly influential?
boost.trump2 <- gbm(retweet_count ~ is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, distribution = "gaussian", n.trees = 5000)

# Without favorite count, use of exclamation points and all caps seem to be the most influential
summary(boost.trump2)
boost_sum2 <- boost.trump2
boosted2_train_MSE <- boost_sum2$train.error[5000]
  

# However, it's a terrible predictor of user engagement (rewtweet + favorite counts)
yhat.boost2 <- predict(boost.trump2, newdata = test_trump_rt, n.trees = 5000)
boosted2_mse <- mean((yhat.boost2 - test_trump_rt$retweet_count)^2, na.rm = TRUE)
boosted2_mse


# - RANDOM FORESTS - #
rf.trump <- randomForest(retweet_count ~ is_retweet + favorite_count + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, mtry = 2, importance = TRUE)

# to see training mse
rf.trump

# Evaluate on test data
yhat_rf <- predict(rf.trump, newdata = test_trump_rt)
rf1_mse <- mean((yhat_rf - test_trump_rt$retweet_count)^2)

# Like before, favorite count is very influential.
# is_retweet is second most influential
importance(rf.trump)
varImpPlot(rf.trump)

write_rds(importance(rf.trump), path = "results/rf.trump_importance.rds")

# Random forest without favorite count
rf.trump2 <- randomForest(retweet_count ~ is_retweet + exclam_count + all_caps_count + uppercase_first_count, data = train_trump_rt, mtry = 2, importance = TRUE)

# Without favorite count, prediction is again, terrible.
yhat_rf2 <- predict(rf.trump2, newdata = test_trump_rt)
rf2_mse <- mean((yhat_rf2 - test_trump_rt$retweet_count)^2)

varImpPlot(rf.trump2)
write_rds(importance(rf.trump2), path = "results/rf.trump_importance2.rds")

# Compare error
tree_fit_error <- tibble(Method = c("regression_tree", "boosting - 1", "boosting - 2", "random forests"),
       Train_MSE = c(reg_treetrain_mse1, boosted1_train_MSE, boosted2_train_MSE, 11687195), Test_MSE = c(reg_treetest_mse1, boosted1_mse, boosted2_mse, rf1_mse)) %>% 
  arrange(Test_MSE)

write_rds(tree_fit_error, path = "results/tree_fit_error.rds")

all_errors <- tibble(Method = c("Linear model", "Linear - interaction 1", "Linear - interaction 2", "polynomial regression: degree = 3", "polynomial regression: degree = 4", "polynomial regression: degree = 5", "polynomial regression: degree = 6", "regression_tree", "boosting - 1", "boosting - 2", "random forests"),
                           Train_MSE = c(train_lm_MSE, train_lm_interact_MSE, train_lm_interact_MSE2, poly_trainmse1, poly_trainmse2, poly_trainmse3, poly_trainmse4, reg_treetrain_mse1, boosted1_train_MSE, boosted2_train_MSE, 11687195), Test_MSE = c(lm_test_mse, lm_test_mse_interact, lm_test_mse_interact2, poly_testmse1, poly_testmse2, poly_testmse3, poly_testmse4, reg_treetest_mse1, boosted1_mse, boosted2_mse, rf1_mse)) %>% 
  arrange(Test_MSE)

all_errors %>% kable()

write_rds(all_errors, path = "results/all_errors.rds")

# It seems that the current predictors are not great indicators of which tweets will be successful. Let's try looking at the actual content of the tweets itself? (see sentiment analysis script)

 



