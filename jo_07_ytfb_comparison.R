
comments_by_title <- yt_title_sent %>% 
  inner_join(fb_title_sent, by = c("short_title" = "short_title.x")) %>% 
  select(video_created = vid_created.x.x, 
         short_title, 
         sent_yt = sent_pos_menos_neg.x,
         sent_fb = sent_pos_menos_neg.y,
         mean_sent_yt = sent_mean.x,
         mean_sent_fb = sent_mean.y) %>% 
  ungroup() %>% 
  mutate(diff = sent_fb - sent_yt,
         diff_mean = mean_sent_fb - mean_sent_yt,
         abs_diff = abs(sent_fb - sent_yt),
         abs_diff_mean = abs(mean_sent_fb - mean_sent_yt),
         short_title = reorder(short_title, -diff_mean)) %>% 
  arrange(desc(diff_mean))


######## diff in sentiment between fb and youtube #########



# diff graph
comments_by_title %>%
  ggplot(aes(x = reorder(short_title, diff_mean))) +
  geom_col(aes(y = diff_mean)) +
  coord_flip()

# diff in sentiment by chapter ordered by video_created

library(plotly)
ggplotly(comments_by_title %>%
  mutate(short_title = reorder(short_title, video_created)) %>%
  
  ggplot(aes(x = short_title, text = paste(short_title, "<br />",  video_created))) +
  geom_line(aes(y = mean_sent_fb, group = 1), color = "blue") +
  geom_line(aes(y = mean_sent_yt, group = 1), color = "red") +
  # coord_flip() +
  geom_hline(yintercept = 0) +
  xlab(NULL) +
  ylab(NULL) +
  theme_minimal() +
  theme(axis.text.x = element_blank()),
tooltip = "text")


# difference in sentiment (bars) between both ordered by short_title

comments_by_title %>%
  select(diff_mean, short_title, mean_sent_yt, mean_sent_fb) %>%
  mutate(mean_sent_yt_order = mean_sent_yt) %>% 
  gather(variable, value, 
        -short_title, -diff_mean, -mean_sent_yt_order) %>%
  mutate(short_title = reorder(short_title, -mean_sent_yt_order)) %>%
   
  ggplot() +
  geom_col(aes(x = short_title, y = value, fill = variable), position = 'dodge')  +
  scale_fill_manual(values = alpha(c("blue", "red"))#,
                    # breaks = c("0", "1"), labels = c("Clinton", "Trump")
                    ) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "none",
        panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()
   




########### compare evolution of comments ###########


fb_month_sent %>% 
  inner_join(yt_month_sent, by = "month") %>%
  mutate(month = as.Date(paste(month, 1, sep = "-"), "%Y-%m-%d")) %>% 
  select(month, 
         mean_sent_fb = sent_mean.x, 
         mean_sent_yt = sent_mean.y) %>% 
  ungroup() %>% 
  mutate(diff_mean = mean_sent_fb - mean_sent_yt) %>% 
  
  ggplot() +
  geom_line(aes(x = month , y = mean_sent_fb, group = 1), color = "blue") +
  geom_line(aes(x = month , y = mean_sent_yt, group = 1), color = "red") +
  geom_hline(yintercept = 0) +
  # geom_col(aes(value, short_title, fill = variable), position = 'stack') +
  # geom_col(aes( short_title, sentiment_yt)) +
  theme_bw() +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
        axis.line = element_line(colour = "grey"), legend.position = "none",
        panel.grid.major = element_blank(), panel.border = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1))


save.image("data.Rdata")

