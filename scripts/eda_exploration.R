## -- EDA Exploration -- ##

# Load packages
library(tidyverse)
library(lubridate)
library(rtweet)
library(cluster)
library(gridExtra)

# Load data
train_trump <- readRDS(file = "data/processed/train_trump.rds")
train_fox <- readRDS(file = "data/processed/train_fox.rds")

## -- REALDONALDTRUMP -- ##

## Context
# Trump announced candidacy for president June 16, 2015
# Trump became president on Jan 20, 2017

## -- Interactions -- ##

# Most retweets 
rt <- train_trump %>% 
  arrange(desc(retweet_count)) %>% 
  select(retweet_count, created_at, text, favorite_count)

# 1070 missing retweet/favorite values
sum(is.na(rt$retweet_count))
sum(is.na(rt$favorite_count))

# Most of them are from before Trump's candidacy or presidency (time of lower engagement)
rt %>% 
  filter(is.na(rt$retweet_count)) %>% 
  arrange(desc(created_at))

# Remove missing retweets counts
rt <- rt %>% 
  filter(!is.na(retweet_count) & !is.na(favorite_count))

# Select only tweets post presidency
rt_pres <- rt %>% 
  filter(created_at > "2017-01-20")

# Select pre-presidency tweets
rt_pre_pres <- rt %>% 
  filter(created_at < "2017-01-20")

# Select post-candidacy announcement tweets
rt_candidate <- rt %>% 
  filter(created_at > "2015-06-16")

# Average retweets before and after
round(mean(rt_pre_pres$retweet_count), 0)
round(mean(rt_pres$retweet_count), 0)

# Average favorites before and after
round(mean(rt_pre_pres$favorite_count), 0)
round(mean(rt_pres$favorite_count), 0)

# Retweets over time
ggplot(rt, aes(created_at, retweet_count)) +
  geom_point(alpha = 0.2) +
  ggtitle("Trump's Retweeted Counts over Time") +
  scale_y_continuous(name = "Retweets", labels = scales::comma) +
  scale_x_datetime(name = "Year") +
  geom_vline(xintercept = as.numeric()) +
  theme(plot.title = element_text(hjust = 0.5))

# Retweets during presidency
ggplot(rt_pres, aes(created_at, retweet_count)) +
  geom_point(alpha = 0.2) +
  scale_y_continuous(name = "Retweets", labels = scales::comma) +
  scale_x_datetime(name = "Year-Month") +
  ggtitle("Trump's Retweeted Counts from Presidency On") +
  theme(plot.title = element_text(hjust = 0.5))

### dodged bar graphs per month from candidacy through presidency

rt_candidate <- rt_candidate %>% 
  mutate(month = month(created_at),
         year = year(created_at)) %>% 
  group_by(month, year) %>%
  summarise(rt_avg = mean(retweet_count),
            fav_avg = mean(favorite_count)) %>% 
  #ungroup(month, year) %>% 
  mutate(date = parse_date((paste0(year, "-", month)), "%Y-%m"))

# Retweet and favorite progression since candidacy by month
avg_rt_since_cand <- ggplot(rt_candidate, aes(x = date)) +
  geom_bar(aes(y = rt_avg), stat = "identity") +
  scale_x_date(name = "Date by Month", date_breaks = "3 months", date_labels = "%Y-%b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Average Retweets", labels = scales::comma) +
  ggtitle("Average Retweets per Month since Candidacy Announcement") +
  theme(plot.title = element_text(hjust = 0.5))

avg_fav_since_cand <- ggplot(rt_candidate, aes(x = date)) +
  geom_bar(aes(y = fav_avg), stat = "identity") +
  scale_x_date(name = "Date by Month", date_breaks = "3 months", date_labels = "%Y-%b") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(name = "Average Favorites", labels = scales::comma) +
  ggtitle("Average Favorites per Month since Candidacy Announcement")

grid.arrange(avg_rt_since_cand, avg_fav_since_cand)
## -- Mentions -- ##
# Examine most common interactions via mentions_screen_names and hashtags.

names(train_trump)

mentions_tr <- train_trump %>% 
  select(created_at, hashtags, mentions_screen_name)

# Does he use hashtags?

# Let's look at how he uses hashtags and at signs.
hashtags_use <- mentions_tr %>% 
  filter(!is.na(hashtags) & !is.na(mentions_screen_name))

# 4267 of 24703 tweets use hashtags, which is ~ 17 percent of his tweets.
mentions_tr %>% 
  count(is.na(hashtags))
4267 / 24703

# If he uses a hashtag, will he also tag (@) someone?

# Both present - 2645 tweets
mentions_tr %>% 
  count(!is.na(hashtags) & !is.na(mentions_screen_name))
2645/24704

# Only hashtag - 1622 tweets
mentions_tr %>% 
  count(!is.na(hashtags) & is.na(mentions_screen_name))
1622/24704

# Only @ tag - 14486
# Seems trump has a much higher propensity to tag others than use hashtags, although he does sometimes use them in tandem
mentions_tr %>% 
  count(is.na(hashtags) & !is.na(mentions_screen_name))
14486/24704

# What does he usually use # for? 
hashtag_list <- mentions_tr %>% 
  filter(!is.na(hashtags)) %>% 
  select(hashtags) %>% 
  unlist()

# There are 384 hashtags Trump uses more than once, and only 145 that he uses more than 3 times. Many are related to his presidential campaign, which is unsurprising.
freq_hashtags <- as.tibble(sort(table(hashtag_list), decreasing = TRUE)) %>% 
  filter(n > 30)

ggplot(freq_hashtags) + 
  geom_bar(aes(x = reorder(hashtag_list, -n), y = n), stat = "identity") +
  scale_x_discrete(name = "Hashtag") +
  scale_y_continuous(name = "Number of Uses") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Top Hashtags Used by Trump")

# Who does he typically mention?
mention_list <- mentions_tr %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  select(mentions_screen_name) %>% 
  unlist() 


# Trump actively uses the @ sign: 
# Discard first value, since that seems to be other people tagging Trump, not Trump using @ tags.
freq_mentions <- as.tibble(sort(table(mention_list), decreasing = TRUE)) %>% 
  filter(n > 50)
freq_mentions <- freq_mentions[-1, ]

freq_mentions

# Check why mention of trump screenname so frequent.
# train_trump %>% 
#   filter(mentions_screen_name == "realDonaldTrump") %>% 
#   select(text) 

ggplot(freq_mentions) + 
  geom_bar(aes(x = reorder(mention_list, -n), y = n), stat = "identity") +
  scale_x_discrete(name = "Screenname Mentioned") +
  scale_y_continuous(name = "Number of Uses") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Top @tags Used by Trump")

## -- Timeline -- ##

# Use rtweet plot function to get a basic overview
ts_plot(train_trump, "week") +
  scale_x_datetime(name = "Year") +
  scale_y_continuous(name = "Number of Tweets") +
  ggtitle("Frequency of Trump Tweets by Week") 

# Add sections based on candidacy and presidency.
# Trump announced candidacy for president June 16, 2015
# Trump became president on Jan 20, 2017

# What time of day does Trump usually tweet?
train_trump <- train_trump %>% 
  mutate(hour = hour(created_at))

train_trump %>%
  ggplot() +
  geom_bar(aes(hour)) +
  scale_x_continuous("Hour of the Day") +
  scale_y_continuous("Tweet Count") +
  ggtitle("Trump Tweets by the Hour") + 
  theme(plot.title = element_text(hjust = 0.5))

# What day of the week does he tweet most?
dow <- train_trump %>% 
  mutate(wday = wday(created_at, label = TRUE),
         year = year(created_at)) %>% 
  group_by(wday, year) %>% 
  summarise(n = n())
dow

ggplot(dow, aes(wday, n)) +
  geom_bar(stat = "identity", aes(fill = year)) + 
  scale_x_discrete(name = "Number of Tweets") +
  scale_y_continuous(name = "Days of the Week") +
  ggtitle("Trump's Tweets by Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5))

## -- Exclamations -- ##

# on average, 0.5741 exclamation points
# maximum: 15 used
# total number across all tweets: 16631
train_trump$exlam_count %>% summary()
sum(train_trump$exlam_count)

# Exclamation point frequency?
top_exclam <- train_trump %>% 
  arrange(desc(exlam_count)) %>% 
  select(created_at, text, exlam_count) %>% 
  filter(exlam_count > 5) 

# Top exclamation point useage above 5 uses
ggplot(top_exclam, aes(created_at, exlam_count)) +
  geom_line() +
  geom_point() +
  scale_y_continuous("Exclamation Points Used", breaks = seq(5, 18, 1), limits = c(6, 18))
  
# Does he tend to use multiple or single exclamation points?
ggplot(train_trump, aes(exlam_count)) +
  geom_histogram(bins = 15) +
  scale_x_continuous(breaks = c(0:15), limits = c(0,15)) +
  scale_y_continuous(limits = c(0,11000), breaks = seq(0, 11000, 1000)) +
  theme(panel.grid.minor = element_blank()) +
  ggtitle("Frequency of Exclamation Point Usage in Succession")

  
# Zoom in on higher exclamation point usage
ggplot(top_exclam, aes(exlam_count)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(5, 16), breaks = c(5:16)) +
  theme(panel.grid.minor = element_blank()) +
  ggtitle("Frequency of Top Exclamation Point Usage in Succession")

## -- Capitalization -- ##
names(train_trump)

# All-caps? 
# How frequently does he use all-caps?
allcaps_use <- train_trump %>% 
  select(created_at, all_caps, uppercase_first)

all_cap_list <- unlist(allcaps_use$all_caps) 

# What does he all-caps the most?
freq_allcaps <- as.tibble(sort(table(all_cap_list), decreasing = TRUE))  %>% 
  filter(n > 225)

# Plot of most commonly mentioned words
ggplot(freq_allcaps) +
  geom_bar(aes(x = reorder(all_cap_list, -n), y = n), stat = "identity") + 
  scale_x_discrete(name = "Top words in all-caps") +
  scale_y_continuous(name = "Use Count") +
  ggtitle("Top words used by Trump in All-Caps") +
  theme(plot.title = element_text(hjust = 0.5))

# Words capitalized (first letter)
# What words does he capitalize the first letter of most often?
uppercap_list <- unlist(allcaps_use$uppercase_first) 

# What does he uppercase the first letter of the most?
freq_upperfirst <- as.tibble(sort(table(uppercap_list), decreasing = TRUE)) %>% 
  filter(n > 400)

# Plot
# Most of these words are unsurprising, since they're people or words that could be used to begin sentences. "Great" is quite high - presumably to match his catchphrase, "Make America Great Again". "News" is also quite high, potentially because of his references to "fake news".
ggplot(freq_upperfirst) +
  geom_bar(aes(x = reorder(uppercap_list, -n), y = n), stat = "identity") + 
  scale_x_discrete(name = "Top words with First Letter Capitalized") +
  scale_y_continuous(name = "Use Count") +
  ggtitle("Top words with First Letter Capitalized used by Trump") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 40, hjust = 1))

## -- Fake News -- ##
# Fake news count - 184 times.
sum(str_count(train_trump$text, "fake news | FAKE NEWS | Fake News"))

# plot fake news tweets.

# Filter for fake news tweets only
fakenews <- train_trump %>% 
  filter(str_detect(text, "fake news | FAKE NEWS | Fake News") == TRUE)

# Plot of fake news tweets by week
ts_plot(fakenews, "weeks") +
  scale_x_datetime(name = "Date", date_breaks = "month", date_labels = "%b %Y") +
  scale_y_continuous(name = "Count", breaks = seq(0, 13, 1)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), plot.title = element_text(hjust = 0.5)) +
  ggtitle("Mentions of Fake News in Trump Tweets")

## -- FOX AND FRIENDS -- ##

## -- Timeline -- ##

# FoxandFriends tweets over time
ts_plot(train_fox) +
  scale_x_datetime(name = "Date", date_breaks = "week", date_labels = "%b %d %Y") +
  scale_y_continuous(name = "Tweet Count") +
  ggtitle("Fox and Friends Tweets over Time") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 30, hjust = 1))

# What time of day do foxandfriends usually tweet?
train_fox <- train_fox %>% 
  mutate(hour = hour(created_at))

train_fox %>%
  ggplot() +
  geom_bar(aes(hour)) +
  scale_x_continuous("Hour of the Day") +
  scale_y_continuous("Tweet Count") +
  ggtitle("Foxandfriends Tweets by the Hour") + 
  theme(plot.title = element_text(hjust = 0.5))

# What day of the week does he tweet most?
dow_fox <- train_fox %>% 
  mutate(wday = wday(created_at, label = TRUE)) %>% 
  group_by(wday) %>% 
  summarise(n = n())

ggplot(dow_fox, aes(wday, n)) +
  geom_bar(stat = "identity") + 
  scale_x_discrete(name = "Number of Tweets") +
  scale_y_continuous(name = "Days of the Week") +
  ggtitle("Foxandfriend's Tweets by Day of the Week") +
  theme(plot.title = element_text(hjust = 0.5))

## -- Mentions -- ##

# What proportion of foxandfriend's tweets use hashtags?
# About 8 percent
train_fox %>% 
  count(is.na(hashtags))
207 / 2591

# What hashtags are most frequently used?
hashtag_foxlist <- train_fox %>% 
  filter(!is.na(hashtags)) %>% 
  select(hashtags) %>% 
  unlist()

freq_hashtags_fox <- as.tibble(sort(table(hashtag_foxlist), decreasing = TRUE)) %>% 
  head()

# Plot of most frequently used hashtags
ggplot(freq_hashtags_fox) + 
  geom_bar(aes(x = reorder(hashtag_foxlist, -n), y = n), stat = "identity") +
  scale_x_discrete(name = "Hashtag") +
  scale_y_continuous(name = "Number of Uses") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Top Hashtags Used by Foxandfriends")


# Most frequently tagged twitter users
# Who does foxandfriends typically mention?
mention_list_fox <- train_fox %>% 
  filter(!is.na(mentions_screen_name)) %>% 
  select(mentions_screen_name) %>% 
  unlist()

# Discard first value, since that seems to be other people tagging fox, not fox using @ tags.
freq_mentions_fox <- as.tibble(sort(table(mention_list_fox), decreasing = TRUE)) 
freq_mentions_fox <- freq_mentions_fox[-1,] %>% head()

# Plot of most commonly used @ tags 
ggplot(freq_mentions_fox) + 
  geom_bar(aes(x = reorder(mention_list_fox, -n), y = n), stat = "identity") +
  scale_x_discrete(name = "Screenname Mentioned") +
  scale_y_continuous(name = "Number of Uses") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  ggtitle("Top @tags Used by foxandfriends")




