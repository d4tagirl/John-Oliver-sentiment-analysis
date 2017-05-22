# load("lwt_image.Rdata")

library(rvest)
library(xml2)

url <- "https://en.wikipedia.org/wiki/List_of_Last_Week_Tonight_with_John_Oliver_episodes#Episodes"

# reference! http://blog.corynissen.com/2015/01/using-rvest-to-scrape-html-table.html

for (i in 2:5) {
  title <- url %>%
    read_html() %>%
    html_nodes(xpath = paste0('//*[@id="mw-content-text"]/table[', i, ']')) %>%
    html_table()
  title <- title[[1]]  
  title <- title %>% 
    bind_cols(tibble(season  = rep(i - 1, each = nrow(title))))
  colnames(title) <- c("abs_episode", "episode_ish","main_segment", "air_date", "viewers", "season")
  assign(paste0("lwt_s0", i - 1), title)
}

episodes_wiki <- bind_rows(lwt_s01, lwt_s02, lwt_s03, lwt_s04) %>% 
  filter(main_segment != "TBA") %>% 
  mutate(episode = ifelse(nchar(episode_ish) > 3, lag(episode_ish), episode_ish))

episodes_wiki <- episodes_wiki %>% 
  left_join(episodes_wiki, by = c("season" = "season", "episode" = "episode")) %>% 
  filter(episode_ish.x != episode_ish.y,
         episode_ish.x != episode) %>% 
  select(season, episode, 
         abs_episode = abs_episode.y, 
         main_segment = main_segment.y,
         air_date = air_date.y,
         viewers = viewers.y,
         segments = main_segment.x)

rm(lwt_s01, lwt_s02, lwt_s03, lwt_s04)

library(fuzzyjoin)

youtube_names <-  videos %>% 
  select(short_title, short_desc) %>% 
  stringdist_left_join(episodes_wiki, by = c("short_title" = "main_segment"), method = "soundex") %>%
  select(short_title, short_desc, main_segment) %>% 
  stringdist_left_join(episodes_wiki, by = c("short_desc" = "main_segment"), method = "soundex") %>% 
  select(short_title, short_desc, main_segment.x, main_segment.y) #%>% 
  # stringdist_left_join(episodes_wiki, by = c("short_desc" = "segments"), method = "soundex") %>% 
  # select(short_title, short_desc, main_segment.x, main_segment.y, segments) 
  
  
  
  
  tbl_df %>%  
  # stringdist_full_join(episodes_wiki, by = c("value" = "main_segment"), max_dist = 3) %>% 
  stringdist_left_join(episodes_wiki, by = c("value" = "main_segment"), method = "soundex") %>% 
  select(value, main_segment) %>%
  stringdist_left_join(episodes_wiki, by = c("value" = "main_segment"), max_dist = 3) %>% 
  select(value, main_segment.x, main_segment.y) 
  

  left_join(episodes_wiki, by = c("value" = "main_segment"))



save.image("lwt_wiki")