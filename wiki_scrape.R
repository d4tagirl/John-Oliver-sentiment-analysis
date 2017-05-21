library(rvest)
librray(xml2)

url <- "https://en.wikipedia.org/wiki/List_of_Last_Week_Tonight_with_John_Oliver_episodes#Episodes"

for (i in 2:5) {
  title <- url %>%
    read_html() %>%
    html_nodes(xpath=paste0('//*[@id="mw-content-text"]/table[', i, ']')) %>%
    html_table()
  title <- title[[1]]  
  assign(paste0("lwt_s0", i-1), title)
}


