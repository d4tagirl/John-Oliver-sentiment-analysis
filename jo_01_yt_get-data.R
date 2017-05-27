library(tibble)
library(dplyr)
library(purrr)
library(magrittr)
library(stringr)

library(tuber)

# load("data.Rdata")
source("functions.R")

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

lwt_channel_resources <- list_channel_resources(filter = c(channel_id = lwt_channel),
                                                part =  "contentDetails")

lwt_playlist_id <- lwt_channel_resources$items[[1]]$contentDetails$relatedPlaylists$uploads

# get videos
videos <- get_videos(lwt_playlist_id) %>%
  mutate(short_title = str_match(title, "^([^:]+).+")[,2],
         hbo_web     = str_match(title, ".+\\((.+)\\)$")[,2],
         short_desc  = str_match(description, "^([^\n]+).+")[,2],
         vid_created = as.Date(created)) %>% 
  select(-created)

# # --- extract .csv for blog post ----
# 
# write.csv(videos, 'videos.csv')
# 
# # --------------------------------

# check how many videos I have
nrow(videos)

# get comments from videos
raw_yt_comments <- get_comments_dani(videos$id, n = 300)

# --------- processing Youtube data -----------

# join comments with videos, and filter videos with less than 100 comments

yt_comments <- raw_yt_comments %>%
  mutate(com_created = as.Date(com_created)) %>%
  filter(com_text != "")
  left_join(videos, by = c("video_id" = "id")) %>%
  group_by(short_title) %>% 
  mutate(n = n(),
         com_created = as.Date(com_created)) %>% 
  ungroup() %>% 
  filter(n >= 100)
  
  # # --- extract .csv for blog post ----
  # 
  # write.csv(yt_comments, 'yt_comments.csv')
  # 
  # # --------------------------------


# save all downloaded data!
# save.image("data.Rdata")
