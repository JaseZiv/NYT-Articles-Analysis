rm(list = ls())
gc()


# load libraries
library(dplyr)
library(tidytext)
library(tidyr)
library(ggplot2)
library(lubridate)

#---------- Read in files ----------#

# Set location for all of the files
articles_data_location <- "/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data"

# create a vector with the neames of all the files to import
articles_data_files <- list.files(path = articles_data_location, full.names = TRUE)

# loop to grab all monthly article files
articles_df <- data.frame()

for(each in 1:length(articles_data_files)) {
  
  articles_df <- bind_rows(articles_df, read.csv(articles_data_files[each], stringsAsFactors = FALSE))
}

print(colSums(is.na(articles_df)))

#---------- Script to read in files for Kaggle purposes... ----------#

jan17 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesJan2017.csv', stringsAsFactors = F)
feb17 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesFeb2017.csv', stringsAsFactors = F)
mar17 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesMarch2017.csv', stringsAsFactors = F)
apr17 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesApril2017.csv', stringsAsFactors = F)
may17 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesMay2017.csv', stringsAsFactors = F)
jan18 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesJan2018.csv', stringsAsFactors = F)
feb18 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesFeb2018.csv', stringsAsFactors = F)
mar18 <-read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/NYTArticlesMarch2018.csv', stringsAsFactors = F)
apr18 <- read.csv('/Users/jasonzivkovic/Documents/NewYorkTimesTextAnalysis/NYT Articles Data/ArticlesApril2018.csv', stringsAsFactors = F)


articles_df <- bind_rows(jan17, feb17, mar17, apr17, may17, jan18, feb18, mar18, apr18)

articles_df <- articles_df %>%
  select(-abstract)

articles_df$pubDate <- ymd_hms(articles_df$pubDate)
articles_df$day_of_week <- wday(articles_df$pubDate, label = TRUE)
articles_df$year <- year(articles_df$pubDate)


# have words increased over time?
articles_df %>%
  mutate(pubDate = as_date(ymd_hms(pubDate))) %>%
  group_by(pubDate, year) %>%
  summarise(median_articleWordCount = median(articleWordCount)) %>%
  ggplot(aes(x=pubDate, y=median_articleWordCount)) +
  geom_line() +
  geom_smooth(method = "lm", se=F, col = "red") +
  facet_wrap(~factor(year), scales = "free", ncol = 1) + 
  labs(x = "Published Date", y = "Median article word count") +
  theme_bw()

#----------- Sentiment Analysis on headlines ----------#
# headlines_analysis <- articles_df %>%
#   select(articleID, headline) %>%
#   unnest_tokens(word, headline) %>%
#   anti_join(stop_words, by = "word") %>%
#   inner_join(get_sentiments("afinn"), by = "word") %>%
#   filter(word != "trump", word != "unknown")
# 
# 
# wide_headlines_analysis <- headlines_analysis %>%
#   count(articleID, sentiment) %>%
#   spread(sentiment, n, fill = 0) %>%
#   mutate(sentiment = positive - negative) %>%
#   select(articleID, sentiment) %>%
#   mutate(Headline_sentiment = ifelse(sentiment == 0,"Neutral",ifelse(sentiment >0,"Positive", "Negative"))) %>%
#   select(-sentiment)


# bing sentiments
headlines_analysis <- articles_df %>%
  select(articleID, headline) %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(word != "unknown") %>%
  count(articleID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  mutate(Headline_sentiment = ifelse(sentiment == 0,"Neutral",ifelse(sentiment >0,"Positive", "Negative"))) %>%
  select(articleID, Headline_sentiment)



articles_df <- articles_df %>%
  left_join(headlines_analysis, by = "articleID")


articles_df$Headline_sentiment[is.na(articles_df$Headline_sentiment)] <- "Neutral"


# are positive headlines futher towards the front of the paper than negative comments
articles_df %>%
  filter(Headline_sentiment == "Positive" | Headline_sentiment == "Negative") %>%
  ggplot(aes(x=Headline_sentiment, y=printPage)) +
  geom_boxplot(col = "darkblue", fill = "lightgrey", alpha = 0.4) +
  coord_flip() +
  labs(x= "Headline Sentiment", y= "Print Page") +
  theme_bw() +
  facet_wrap(~factor(year), scales = "free", ncol = 1)



#---------- Article snippet sentiment ----------#

##### bing #####
snippet_analysis_bing <- articles_df %>%
  select(articleID, snippet) %>%
  unnest_tokens(word, snippet) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word")

snippet_analysis_bing_count <- snippet_analysis_bing %>%
  count(articleID, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative) %>%
  select(articleID, sentiment) %>%
  mutate(snippet_sentiment = ifelse(sentiment == 0,"Neutral",ifelse(sentiment >0,"Positive", "Negative"))) %>%
  select(-sentiment)


articles_df <- articles_df %>%
  left_join(snippet_analysis_bing_count, by = "articleID")

articles_df$snippet_sentiment[is.na(articles_df$snippet_sentiment)] <- "Neutral"


### how have the article snippets changed sentiment over time
articles_df %>%
  mutate(pubDate = as_date(ymd_hms(pubDate))) %>%
  group_by(pubDate, year, snippet_sentiment) %>%
  summarise(n = n()) %>%
  mutate(percent = n/sum(n)) %>%
  filter(snippet_sentiment == "Positive" | snippet_sentiment == "Negative") %>%
  ggplot(aes(x=pubDate, y=percent, colour = snippet_sentiment)) +
  geom_line() +
  scale_color_manual(values = c("red", "darkblue")) +
  geom_smooth(method = "lm", col = "black", linetype =4, se=F) +
  facet_wrap(snippet_sentiment ~ factor(year), scales = "free") +
  labs(x = "Published Date", y= "Percent Article Sentiment") +
  theme_bw() +
  theme(legend.position="none")


# Is Trump in the headlines more or less now?

trump_headlines_analysis <- articles_df %>%
  select(articleID, headline) %>%
  unnest_tokens(word, headline) %>%
  anti_join(stop_words, by = "word") %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  filter(word != "unknown")


trump_headlines_analysis$word[trump_headlines_analysis$word == "trump’s"] <- "trump"


# create a data frame to use
trump_articles <- trump_headlines_analysis %>%
  mutate(trump_yes_no = factor(ifelse(word == "trump", "yes", "no"))) %>%
  filter(trump_yes_no == "yes") %>%
  select(articleID, trump_yes_no) %>%
  right_join(articles_df, by = "articleID")

# replace NAs to "no"
trump_articles$trump_yes_no[is.na(trump_articles$trump_yes_no)] <- "no"

trump_articles <-  trump_articles[!duplicated(trump_articles[c('articleID', 'trump_yes_no')]),]

trump_articles %>%
  mutate(pubDate = as_date(ymd_hms(pubDate))) %>%
  count(pubDate, year, trump_yes_no) %>%
  mutate(frequency = n/sum(n)) %>%
  filter(trump_yes_no == "yes") %>%
  ggplot(aes(x=pubDate, y=frequency, colour = trump_yes_no)) +
  geom_line(col = "darkred") +
  geom_smooth(method = "lm", linetype = 4, col = "black", se=F) +
  facet_wrap(~ factor(year), scales = "free", ncol = 1) +
  labs(x="Published Date", y= "Percent Trump in Headline") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()



