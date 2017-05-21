library(Rfacebook)

fb_token <- "EAACEdEose0cBAAlbANrfVMZBK5oyEV7qnu9eUUBhAH7ZA7ZBY5v5rZCyeYbM8ZA9wj2bUZBRbebr5tr3GM01T5xGk98dDfxJb7J08lXpFcFLIruseKV6lDvNZAjeE0i41JSbBXlfi6Gn7pHZA2mvM3lL7wV95f5NhJE9x0ZAMwCNa8lRZAJC8HZA4EVvN2qFGb1JqwZD"

page <- getPage("LastWeekTonight", fb_token, n = 5000)

library(stringr)
videos <- page %>% 
  filter(type == "video" &
           link == str_match(link, "^https://www.youtube.com/watch\\?v=.+")) %>% 
  mutate(ids = str_match(link, "^https://www.youtube.com/watch\\?v=([^&]+)")[,2])

