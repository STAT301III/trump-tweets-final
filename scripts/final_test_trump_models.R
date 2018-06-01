# Final held-back data evaluation (final_test_trump) #

source(file = "trump_modeling.R")
source(file = "sentiment_analysis.R")

# Evaluate models using final test data

# From the non-topics data, random forests, and polynomial regression worked the best.
# Out of the linear models, the forumla with higher interaction performed surprisingly well
all_errors %>% kable()
# From topic data, random forests continued to perform well. A polynomial regression with degree 6 is likely to overfit the data - perhaps try a smaller degree in final evaluations?
topics_errors %>% kable()

# Final models - evaluate with non-topic data, then topic data
# 1) random forests
# 2) polynomial regression (degree = 4)
# 3) linear model - two interaction terms

# - Random forests - #

# Evaluate non-topic model on final test data
yhat_rf_final <- predict(rf.trump, newdata = final_test_trump_rt)
rf_final_mse <- mean((yhat_rf_final - final_test_trump_rt$retweet_count)^2)

# Evaluate non-topic model on final test data
yhat_rf_topics_final <- predict(rf.trumptopics, newdata = topic_trump_test_final)
rf_topics_mse <- mean((yhat_rf_topics_final - topic_trump_test_final$retweet_count)^2)

# - Polynomial regression - #

# Non-topic data
poly_pred_final <- predict(poly_fit4, final_test_trump_num)
poly_testmse_final <- mean((poly_pred_final - final_test_trump_num$retweet_count)^2)

# Degree = 6 (topic data)
poly_pred_topic <- predict(poly_fit_topic, topic_trump_test_final)
poly_test_topic_mse <- mean((poly_pred_topic - topic_trump_test_final$retweet_count)^2)


# - Linear with two interaction terms - #

# On non-topic set
lm_pred_int2_final <- predict(lm_fit_interact2, newdata = final_test_trump_num)

# Calculate residuals/MSE
lm_testmse_interact2_final <- mean((lm_pred_int2_final - final_test_trump_num$retweet_count)^2)


# Compare error
final_test_err <- tibble(Method = c("dataset 1: random forest", "dataset 2: random forest", "dataset 1: polynomial degree = 6", "dataset 2: polyomial degree = 6", "dataset 1: 2 Interaction terms"),
                         Test_MSE = c(rf_final_mse, rf_topics_mse, poly_testmse_final, poly_test_topic_mse, lm_testmse_interact2_final)) %>% 
  arrange(Test_MSE) %>% 
  kable()

# Performed quite...badly! Far more than the actual test dataset, which indicates a lot of overfitting.
final_test_err

write_rds(final_test_err, path = "results/final_test_err.rds")



