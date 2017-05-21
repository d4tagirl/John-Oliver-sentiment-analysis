fb_yt <- videos %>% 
  inner_join(vid_info)

posts <- pmap(list(fb_yt$id, fb_token, 100, TRUE, FALSE), getPost) %>% 
  map("comments") %>% 
  map("message") 

str_match_all(test, "\\\.{1})(.+)")

library(htmltools) 
htmlEscape(test, attribute = FALSE)
