
library(tidytext)
library(dplyr)
load("data.Rdata")

# ------------------ tidytext: sentiment analysis --------------

tidy_yt_comments <- yt_comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words, by = "word") 

# -------------- tidytext: wordcount and n-grams ---------------

library(tidytext)

comments_words_yt <- tidy_yt_comments %>%
  count(word, sort = TRUE) %>%
  top_n(50)

comments_2grams_yt <- yt_comments %>%
  tidytext::unnest_tokens(two_gram, com_text, token = "ngrams", n = 2) %>%
  count(two_gram, sort = TRUE) %>%
  top_n(20)

comments_3grams_yt <- yt_comments %>%
  tidytext::unnest_tokens(three_gram, com_text, token = "ngrams", n = 3) %>%
  count(three_gram, sort = TRUE) %>%
  top_n(20)

comments_4grams_yt <- yt_comments %>%
  tidytext::unnest_tokens(four_gram, com_text, token = "ngrams", n = 4) %>%
  count(four_gram, sort = TRUE) %>%
  top_n(20)

comments_5grams_yt <- yt_comments %>%
  tidytext::unnest_tokens(five_gram, com_text, token = "ngrams", n = 5) %>%
  count(five_gram, sort = TRUE) %>%
  top_n(20)

comments_6grams_yt <- yt_comments %>%
  tidytext::unnest_tokens(six_gram, com_text, token = "ngrams", n = 6) %>%
  count(six_gram, sort = TRUE) %>%
  top_n(20)




# ---------- sentiment by comment ------------

library(tidyr)

yt_comment_sent_bing <- tidy_yt_comments  %>%
  inner_join(get_sentiments("bing"), by = "word") %>% 
  count(com_id, sentiment) %>%
  spread(sentiment, nn, fill = 0) %>%
  mutate(sentiment_bing = positive - negative) %>% 
  ungroup() %>% 
  left_join(yt_comments, by = "com_id")

yt_comment_sent_afinn <- tidy_yt_comments  %>%
  count(com_id, word) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn"), by = "word") %>% 
  group_by(com_id) %>% 
  summarize(score_afinn  = sum(score * nn / sum(nn))) %>% 
  left_join(yt_comments, by = "com_id")





# sentiment youtube total

yt_comment_sent_bing %>% 
  summarise(positive   = sum(positive),
            negative   = sum(negative),
            sent_mean  = mean(sentiment_bing),
            sentiment  = positive - negative)

yt_comment_sent_afinn %>% 
  summarize(score_afinn_mean = mean(score_afinn),
            score_afinn      = sum(score_afinn))




# sentiment youtube total -     BY MONTH 

yt_month_sent_bing <- yt_comment_sent_bing %>% 
  group_by(month = format(com_created, "%Y-%m")) %>% 
  filter(n() >= 100) %>% 
  summarise(pos_bing           = sum(positive),
            neg_bing           = sum(negative),
            sent_mean          = mean(sentiment_bing),
            sent_pos_menos_neg = pos_bing - neg_bing)

yt_month_sent_afinn <- yt_comment_sent_afinn %>% 
  group_by(month = format(com_created, "%Y-%m")) %>% 
  filter(n() >= 100) %>% 
  summarise(score_afinn  = mean(score_afinn))

yt_month_sent <- full_join(yt_month_sent_bing, yt_month_sent_afinn, by = "month")
# rm(yt_month_sent_bing, yt_month_sent_afinn)

library(ggplot2)
yt_month_sent %>% 
  ggplot(aes(x = month)) +
  geom_line(aes(y = sent_mean, group = 1), color = "red") +
  geom_line(aes(y = score_afinn, group = 1), color = "green") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "none",
        panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))





# sentiment youtube by title 

yt_title_sent_bing <- yt_comment_sent_bing %>% 
  group_by(short_title, vid_created) %>% 
  summarise(pos_bing           = sum(positive),
            neg_bing           = sum(negative),
            sent_mean          = mean(sentiment_bing),
            sent_pos_menos_neg = pos_bing - neg_bing) %>% 
  ungroup() %>% 
  arrange(-sent_pos_menos_neg)

yt_title_sent_afinn <- yt_comment_sent_afinn %>% 
  group_by(short_title, vid_created) %>% 
  filter(n() >= 100) %>% 
  summarise(score_afinn  = mean(score_afinn))

yt_title_sent <- full_join(yt_title_sent_bing, yt_title_sent_afinn, by = "short_title")
# rm(yt_title_sent_bing, yt_title_sent_afinn)

library(ggplot2)
yt_title_sent %>% 
  ggplot(aes(x = reorder(short_title, vid_created.x))) +
  geom_line(aes(y = sent_mean, group = 1), color = "red") +
  geom_line(aes(y = score_afinn, group = 1), color = "green") +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "none",
        panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))




# --------- most common words ------

most_used <- tidy_yt_comments %>%  
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
  
most_used %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# I see trump as a positive word!

custom_stop_words <- bind_rows(data_frame(word = c("trump"), 
                                          lexicon = c("custom")), 
                               stop_words)


tidy_yt_comments_2 <- yt_comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(custom_stop_words, by = "word") 

most_used_2 <- tidy_yt_comments_2 %>%  
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

most_used_2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()




# --------- sentiments with nrc -------

tidy_yt_comments  %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# john is bad and trump is surprise!!

custom_stop_words <- bind_rows(data_frame(word = c("john"), 
                                          lexicon = c("custom")),
                               custom_stop_words)

tidy_yt_comments_3 <- yt_comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(custom_stop_words, by = "word") 

tidy_yt_comments_3  %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, nn)) %>%
  ggplot(aes(word, nn, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()

# comment on money!

tidy_yt_comments_3  %>%
  inner_join(get_sentiments("nrc"), by = "word") %>% 
  count(word, sentiment, sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(sentiment = reorder(sentiment, nn),
         pos_neg = ifelse(sentiment %in% c("positive", "anticipation", "joy", "trust", "surprise"), "Positive", "Negative")) %>%
  ggplot(aes(sentiment, nn)) +
  geom_col(aes(fill = pos_neg), show.legend = FALSE) +
  # facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Contribution to sentiment",
       x = NULL) +
  coord_flip()


# ---------- sentiments extreme --------------

library(ggplot2)
extremes_sentiment_yt <- head(yt_title_sent_bing, 10) %>% 
  bind_rows(tail(yt_title_sent_bing, 10)) %>% 
  ungroup() %>% 
  mutate(sentiment   = sent_pos_menos_neg,
         short_title = reorder(short_title, -sentiment)) %>%
  ggplot(aes(reorder(short_title, -sentiment), sentiment, fill = sentiment > 0)) +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "none",
        panel.grid.major = element_blank(), panel.border = element_blank())


save.image("data.Rdata")
