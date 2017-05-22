library(Rfacebook)

fb_token <- "EAACEdEose0cBAJCRNjmpZAwZCcJ5TVZAJd0MC1vz6VZCkc2gbxGqOgnWrans4cJV3JKZCPIOMIzP7zmRS935CZCZCO2S9V3CFIjhsejdBsMMAlSbAjdOTgs32LFrBYrz2d4rAcNSG2rxbJDMIG7MIZCXGwn9vkvKYzF3D03HO367DGwfe8CEWCqHs1hyXfKC1G0ZD"

page <- getPage("LastWeekTonight", fb_token, n = 5000)

library(stringr)
videos_fb <- page %>% 
  filter(type == "video" &
           link == str_match(link, "^https://www.youtube.com/watch\\?v=.+")) %>% 
  mutate(ids = str_match(link, "^https://www.youtube.com/watch\\?v=([^&]+)")[,2])%>% 
  left_join(videos, by = c("ids" = "id")) %>% 
  filter(!is.na(short_title))

# --------- comments on FB -------------

fb_com <- pmap(list(videos_fb$id, fb_token, 400), getPost)

fb_comments <- {}
for (i in 1:length(fb_com)) {
  video_id     <- fb_com[[i]]$post$id
  com_id       <- fb_com[[i]]$comments$id
  com_text     <- fb_com[[i]]$comments$message
  
  list(video_id = video_id, com_id = com_id, com_text = com_text) %>%  tibble
  
  tibble(com_id    = com_id,
         com_text  = com_text)
  
  fb_comments   <- fb_comments %>% bind_rows(data.frame(video_id, com_id, com_text))
}

fb_comments <- fb_comments %>% 
  filter(com_text != "") %>% 
  left_join(videos_fb, by = c("video_id" = "id")) %>% 
  select(short_title, ids, com_text)

# ----- comments fb and youtube ----

#####    FACEBOOK    ####

# bing <- sentiments %>%
#   filter(lexicon == "bing") %>%
#   select(-score)
# 
# # afinn <- sentiments %>%
# #   filter(lexicon == "AFINN")

lwt_words_fb <- fb_comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words, by = "word") 

lwt_sentiment_bing_fb <- lwt_words_fb %>%
  inner_join(bing, by = "word") %>%
  count(short_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))

sentiment_fb <- lwt_words_fb %>%
  inner_join(bing, by = "word") %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))

most_positive_bing_fb <- lwt_sentiment_bing_fb %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment)


most_negative_bing_fb <- lwt_sentiment_bing_fb %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment)

library(ggplot2)
most_positive_bing_fb %>%
  mutate(short_title = reorder(short_title, -sentiment)) %>%
  ggplot(aes(short_title, sentiment)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()


#####    YOUTUBE    ####

yt_comments <- videos_fb %>% 
  select(ids) %>% 
  right_join(comments, by = c("ids" = "video_id"))


lwt_words_yt <- yt_comments %>%
  tidytext::unnest_tokens(word, com_text) %>%
  anti_join(stop_words, by = "word") 

lwt_sentiment_bing_yt <- lwt_words_yt %>%
  inner_join(bing, by = "word") %>%
  count(short_title, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))

sentiment_yt <- lwt_words_yt %>%
  inner_join(bing, by = "word") %>%
  count(sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = (positive - negative)/(positive + negative))


most_positive_bing_yt <- lwt_sentiment_bing_yt %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) 

most_negative_bing_yt <- lwt_sentiment_bing_yt %>%
  ungroup() %>%
  arrange(desc(sentiment)) %>%
  select(short_title, sentiment) 

library(ggplot2)
most_positive_bing_yt %>%
  mutate(short_title = reorder(short_title, -sentiment)) %>%
  ggplot(aes(short_title, sentiment)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

lwt_sentiment_bing_both <- lwt_sentiment_bing_yt %>% 
  inner_join(lwt_sentiment_bing_fb, by = "short_title") %>% 
  select(short_title, sentiment_fb = sentiment.y, sentiment_yt = sentiment.x)

lwt_sentiment_bing_both %>%
  # mutate(short_title = reorder(short_title, sentiment_yt)) %>%
  top_n(-10) %>% 
  ggplot() +
  geom_col(aes(short_title, sentiment_fb)) +
  geom_col(aes(short_title, sentiment_yt)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

save.image("lwt_fb.Rdata")
