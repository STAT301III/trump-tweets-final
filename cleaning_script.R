## -- Processing raw data -- ##


# Load packages
library(tidyverse)
library(rtweet)
library(lubridate)


## -- Read-in data -- ##

# Original Trump tweets dataset, compiled by Michael W. Kearney
trump_tweets <- read_csv(file = "data/unprocessed/trump_tweets.csv")

# Remove extra variables and change time to POSIXct object
trump_tweets <- trump_tweets %>%
  select(-status_id, -user_id, -reply_to_status_id, -reply_to_user_id, -is_quote, -symbols, -media_t.co, -media_expanded_url, -ext_media_url, -ext_media_t.co, -ext_media_expanded_url, -ext_media_type, -mentions_user_id, -lang, -quoted_status_id, -retweet_status_id, -retweet_text, -place_url, -place_type, -country, -bbox_coords, -coords_coords, -source, -geo_coords) %>% 
  mutate(created_at = mdy_hm(created_at))

# Tweet time range
# 2009-05-04 18:54:00
# 2017-08-10, 10:54:00
trump_tweets %>% 
  arrange(created_at)


## -- Setup with Twitter API using rtweet -- ##

# Reference name of twitter app
app_name <- "tj_twitter_app"

consumer_key <- "wk3xlo3pKWTJLVepvkQHPtUUg"
consumer_secret <- "70CPaHormUdvb1kgHxufaXV3YNL7ejXlDiFQbAq9tDumJmRcFB"

token <- create_token(app_name, consumer_key, consumer_secret)

# Save token to home directory
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)

# Create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)

# Save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"),
    fill = TRUE, append = TRUE)


## -- Acquire Tweet data using rtweet -- ##

# Acquire most recent 3200 statuses posted
trump_newest <- get_timeline("realDonaldTrump", n = 3200)

# Remove extra variables
trump_newest <- trump_newest %>%
  select(-status_id, -user_id, -reply_to_status_id, -reply_to_user_id, -is_quote, -symbols, -media_t.co, -media_expanded_url, -ext_media_url, -ext_media_t.co, -ext_media_expanded_url, -ext_media_type, -mentions_user_id, -lang, -quoted_status_id, -retweet_status_id, -retweet_text, -place_url, -place_type, -country, -bbox_coords, -coords_coords, -source, -urls_url, -urls_t.co, -urls_expanded_url, -place_full_name, -geo_coords)

# Change time to POSIXct object
trump_newest <- trump_newest %>% 
  mutate(created_at = ymd_hms(created_at))

# Time range of trump_newest
# 2017-01-20, 17:54:36
# 2018-05-05, 00:31:30
trump_newest %>% 
  arrange(desc(created_at))

# Write to csv for future reference (as get_timeline will get different tweets every time, and eventually the trump_newest dataset will no longer overlap with the original dataset)
write_as_csv(trump_newest, "data/unprocessed/trump_may4.csv")

# Check if variables match
names(trump_newest)
names(trump_tweets)

# Bind old and new tweets together
trump_all <- rbind(trump_newest, trump_tweets)

# Remove duplicates
trump_all <- unique(trump_all) 


## -- Add columns -- ##

# Current variables
names(trump_all)

# Exclamation points
trump_all <- trump_all %>% 
  mutate(exclam = str_detect(text, "!"),
         exlam_count = str_count(text, "!"))

# Url embedded?
trump_all <- trump_all %>% 
  mutate(url_embeded = str_detect(text, "http"))

# Extract all-caps phrases
trump_all <- trump_all %>% 
  mutate(all_caps = str_extract_all(text, "[A-Z]{2,}"))

# First letter uppercase words
trump_all <- trump_all %>% 
  mutate(uppercase_first = str_extract_all(text, "[A-Z][a-z]+"))


## -- Save Trump tweet dataset -- ##

# Set seed for reproducibility
set.seed(5)

# Set aside performance set (20 percent)
train_trump <- trump_all %>% sample_frac(0.8)
test_trump <- trump_all %>% setdiff(train_trump)

# Read tidied trump datasets for use
saveRDS(trump_all, "data/processed/trump_all.rds")
write_as_csv(trump_all, "data/processed/trump_all.csv")

saveRDS(train_trump, "data/processed/train_trump.rds")
write_as_csv(train_trump, "data/processed/train_trump.csv")

saveRDS(test_trump, "data/processed/test_trump.rds")
write_as_csv(test_trump, "data/processed/test_trump.csv")


## -- Fox and Friends Tweets -- ##

# Retrieve tweets using rtweet

foxandfriends <- get_timeline("foxandfriends", n = 3200)

# Remove extra variables
foxandfriends <- foxandfriends %>% 
  select(-status_id, -user_id, -reply_to_status_id, -reply_to_user_id, -is_quote, -symbols, -media_t.co, -media_expanded_url, -ext_media_url, -ext_media_t.co, -ext_media_expanded_url, -ext_media_type, -mentions_user_id, -lang, -quoted_status_id, -retweet_status_id, -retweet_text, -place_url, -place_type, -country, -bbox_coords, -coords_coords, -source, -urls_url, -urls_t.co, -urls_expanded_url, -place_full_name, -geo_coords)

# Change time to POSIXct object
# 2018-03-12, 11:30:00
# 2018-05-04 11:20:06
foxandfriends <- foxandfriends %>% 
  mutate(created_at = ymd_hms(created_at))

foxandfriends %>% 
  arrange(created_at)

## -- Add relevant variables -- ##

# Exclamation points
foxandfriends <- foxandfriends %>% 
  mutate(exclam = str_detect(text, "!"),
         exlam_count = str_count(text, "!"))

# Url embedded?
foxandfriends <- foxandfriends %>% 
  mutate(url_embeded = str_detect(text, "http"))

# Extract all-caps phrases
foxandfriends <- foxandfriends %>% 
  mutate(all_caps = str_extract_all(text, "[A-Z]{2,}"))

# First letter uppercase words
foxandfriends <- foxandfriends %>% 
  mutate(uppercase_first = str_extract_all(text, "[A-Z][a-z]+"))

## -- Store Fox and Friends tweets for EDA -- ##

# Set seed for reproducibility
set.seed(5)

# Set aside performance set (20 percent)
train_fox <- foxandfriends %>% sample_frac(0.8)
test_fox <- foxandfriends %>% setdiff(train_fox)

# Read tidied trump_all dataset for use in EDA
saveRDS(foxandfriends, "data/processed/foxandfriends.rds")
write_as_csv(foxandfriends, "data/processed/foxandfriends.csv")


# Read tidied trump datasets for use
saveRDS(trump_all, "data/processed/trump_all.rds")
write_as_csv(trump_all, "data/processed/trump_all.csv")

saveRDS(train_fox, "data/processed/train_fox.rds")
write_as_csv(train_fox, "data/processed/train_fox.csv")

saveRDS(test_fox, "data/processed/test_fox.rds")
write_as_csv(test_fox, "data/processed/test_fox.csv")
