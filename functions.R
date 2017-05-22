#---------------------------- 
# get videos from playlist

get_videos <- function(playlist) {
  # pass NA as next page to get first page
  nextPageToken <- NA
  videos <- {}
  
  # Loop over every available page
  repeat {
    vid      <- get_playlist_items(filter = c(playlist_id = playlist),
                                   page_token = nextPageToken)
    vid_id   <- map(vid$items, "contentDetails") %>% map_chr("videoId")
    
    titles   <- lapply(vid_id, get_video_details) %>%
      map("localized") %>%
      map_df(magrittr::extract, c("title", "description"))
    
    videos   <- videos %>% bind_rows(tibble(id          = vid_id,
                                            title       = titles$title,
                                            description = titles$description))
    
    # get the token for the next page
    nextPageToken <- ifelse(!is.null(vid$nextPageToken), vid$nextPageToken, NA)
    
    # if no more pages then done
    if (is.na(nextPageToken)) {
      break
    }
  }
  return(videos)
}




#--------------------------
# get comments from 1 video

get_1_video_comments <- function(video_id, n = 5) {
  nextPageToken <- NA
  comments <- {}
  
  repeat {
    com <- get_comment_threads(c(video_id = video_id),
                               part = "id, snippet",
                               page_token = nextPageToken,
                               text_format = "plainText")
    
    for (i in 1:length(com$items)) {
      com_id     <- com$items[[i]]$snippet$topLevelComment$id
      com_text   <- com$items[[i]]$snippet$topLevelComment$snippet$textDisplay
      com_video  <- com$items[[i]]$snippet$topLevelComment$snippet$videoId
      
      comments   <- comments %>% bind_rows(tibble(video_id  = com_video,
                                                  com_id    = com_id,
                                                  com_text  = com_text))
      
      nextPageToken <- ifelse(!is.null(com$nextPageToken), com$nextPageToken, NA)
      
      if (nrow(comments) == n) {
        break
      }
    }
    
    if (is.na(nextPageToken) | nrow(comments) == n) {
      break
    }
  }
  return(comments)
}




#--------------------------
# get comments from several videos

get_comments_dani <- function(videos, n = 10){
  comments <- pmap_df(list(videos, n), get_1_video_comments)
}













