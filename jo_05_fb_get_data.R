load("data.Rdata")

library(Rfacebook)
library(dplyr)

fb_token <- "EAACEdEose0cBACBxPZAHT4ZApzZCzOZAch4HGTEnXTblzeXLfZAiZClE6SfU6CFEbdb6XZCVTPbkEOwpIhGZBZBdK8k5RWpZAJzF1iXSAT6fgAu1DAjSS0QG65iOshZAd6K5bK4NF2IjvxH5aFqmYnMrmHoGi2yO3Y3KHPQ6zJ1C4oeVl9z4Su66yhUxnvFuGNDwZBAZD"

fb_page <- getPage("LastWeekTonight", fb_token, n = 5000)

library(stringr)
videos_fb <- fb_page %>% 
  filter(type == "video" &
           link == str_match(link, "^https://www.youtube.com/watch\\?v=.+")) %>% 
  mutate(ids = str_match(link, "^https://www.youtube.com/watch\\?v=([^&]+)")[,2]) %>% 
  left_join(videos, by = c("ids" = "id")) %>% 
  filter(!is.na(short_title))

# --------- get comments on FB -------------

library(purrr)
fb_com <- lapply(videos_fb$id, getPost, token = fb_token, n = 300)

fb_comments <- {}
for (i in 1:length(fb_com)) {
  post_id_fb   <- fb_com[[i]]$post$id
  com_id       <- fb_com[[i]]$comments$id
  com_text     <- fb_com[[i]]$comments$message
  com_created  <- fb_com[[i]]$comments$created_time
  fb_comments  <- fb_comments %>% 
    bind_rows(data.frame(post_id_fb, com_id, com_text, com_created))
}

fb_comments <- fb_comments %>% 
  filter(com_text != "") %>%
  left_join(videos_fb, by = c("post_id_fb" = "id")) %>% 
  group_by(short_title) %>% 
  mutate(n = n(),
         com_created = as.Date(com_created)) %>% 
  ungroup() %>% 
  filter(n >= 100) %>% 
  select(short_title, video_id = ids, post_id_fb, com_text, com_id, com_created)


save.image("data.Rdata")
