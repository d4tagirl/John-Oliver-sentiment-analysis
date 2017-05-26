
# raw_yt_comments <- all videos with all comments
# yt_comments     <- all videos with more than 100 comments

library(dplyr)
library(stringr)
load("data.Rdata")

# -------------- wordcloud -------------

# -- comparison cloud ---

library(reshape2)

tidy_yt_comments %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "nn", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)



# -- wordcloud --

library(wordcloud)
library(stringr)
library(viridis)
library(tm)

words <- toString(yt_comments$com_text) %>%
  str_split(pattern = " ", simplify = TRUE)

set.seed(1645)
# wordcloud <- wordcloud(words, colors = viridis::viridis_pal(end = 0.8)(10),
#                        min.freq = 800, random.color = TRUE, max.words = 100,
#                        scale = c(3.5,.03))

wordcloud

save.image("data.Rdata")