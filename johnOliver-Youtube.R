library(tuber)
library(purrr)
library(tibble)
library(dplyr)
library(purrr)
library(magrittr)

source("functions.R")
# load("lwt_image.Rdata")

app_id <- "729826126898-be7jmfb8ueqikop349iii7769u2c9d1i.apps.googleusercontent.com"
app_password <- "1mNxVLjwHIh_C0nFNWg7xqC8"

# yt_oauth(app_id, app_password)

# --------- preparing Youtube data -----------

search_channel <- yt_search("lastweektonight")

lwt_channel <- search_channel %>% 
  slice(1) %>% 
  select(channelId) %>% 
  .$channelId %>% 
  as.character

channel_resources <- list_channel_resources(filter = c(channel_id = lwt_channel),
                                            part =  "contentDetails")

playlist_id <- channel_resources$items[[1]]$contentDetails$relatedPlaylists$uploads

# get videos
videos <- get_videos(playlist_id) %>%
  mutate(short_title = str_match(title, "^([^:]+).+")[,2],
         hbo_web =  str_match(title, ".+\\((.+)\\)$")[,2],
         short_desc = str_match(description, "^([^\n]+).+")[,2])

# check how many videos I have
nrow(videos)

# get comments from videos
comments <- get_comments_dani(videos$id, n = 400)

# --------- processing Youtube data -----------

comments <- comments %>%
  left_join(videos, by = c("video_id" = "id"))


# -------------- wordcloud -------------

library(wordcloud)
library(stringr)
library(viridis)
library(tm)

# by title
# words <- comments %>%
#   filter(video_id == "Tt-mpuR_QHQ") %>%
#   select(com_text) %>%
#   toString

words <- toString(comments$com_text)
words <- str_split(words, pattern = " ", simplify = TRUE)
# wordcloud <- wordcloud(words, colors = viridis::viridis_pal(end = 0.8)(10),
#                        min.freq = 1500, random.color = TRUE, max.words = 150,
#                        scale = c(4,.1))
wordcloud


# -------------- tidytext: wordcount and n-grams ---------------
 
library(tidytext)

comments_words <- comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words, by = "word") %>%
  count(word, sort = TRUE) %>%
  top_n(50)

comments_3grams <- comments %>%
  tidytext::unnest_tokens(three_gram, com_text, token = "ngrams", n = 3) %>%
  count(three_gram, sort = TRUE) %>%
  top_n(20)

comments_4grams <- comments %>%
  tidytext::unnest_tokens(four_gram, com_text, token = "ngrams", n = 4) %>%
  count(four_gram, sort = TRUE) %>%
  top_n(20)

comments_5grams <- comments %>%
  tidytext::unnest_tokens(five_gram, com_text, token = "ngrams", n = 5) %>%
  count(five_gram, sort = TRUE) %>%
  top_n(20)

comments_6grams <- comments %>%
  tidytext::unnest_tokens(six_gram, com_text, token = "ngrams", n = 6) %>%
  count(six_gram, sort = TRUE) %>%
  top_n(20)


# ------------------ tidytext: sentiment analysis --------------

bing <- sentiments %>%
  filter(lexicon == "bing") %>%
  select(-score)

afinn <- sentiments %>%
  filter(lexicon == "AFINN")

lwt_wordcount <- comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words) %>%
  count(short_title)

library(tidyr)

lwt_words <- comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words, by = "word") 

lwt_sentiment_bing <- lwt_words %>%
  inner_join(bing, by = "word") %>%
  count(short_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  left_join(lwt_wordcount) %>%
  mutate(sentiment = (positive - negative)/n)

lwt_sentiment_afinn <- lwt_words %>%
  inner_join(afinn, by = "word") %>%
  group_by(short_title) %>%
  summarize(sentiment = mean(score))

most_positive_bing <- lwt_sentiment_bing %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) %>%
  top_n(15)

most_negative_bing <- lwt_sentiment_bing %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) %>%
  top_n(-15)

most_positive_afinn <- lwt_sentiment_afinn %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) %>%
  top_n(15)

most_negative_afinn <- lwt_sentiment_afinn %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) %>%
  top_n(-15)

library(ggplot2)
most_positive_bing %>%
  mutate(short_title = reorder(short_title, -sentiment)) %>%
  ggplot(aes(short_title, sentiment)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()