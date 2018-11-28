# libraries
pkgs <- c("rstudioapi", "tidyverse", "scales", "gridExtra", "grid", "cowplot",
          "utf8")

load <- sapply(pkgs, function(x) {
  suppressPackageStartupMessages(
    require(x, character.only = TRUE))
})

rm(load, pkgs)

## Set custom theme
theme_plex <- function(base_size = 11,
                       strip_text_size = 12,
                       strip_text_margin = 5,
                       subtitle_size = 13,
                       subtitle_margin = 10,
                       plot_title_size = 18,
                       plot_title_margin = 10,
                       ...) {
  ret <- ggplot2::theme_minimal(
    base_size = base_size, ...)
  ret$strip.text <- ggplot2::element_text(hjust = 0, size=strip_text_size,
                                          margin=margin(b=strip_text_margin))
  ret$plot.subtitle <- ggplot2::element_text(hjust = 0, size=subtitle_size,
                                             margin=margin(b=subtitle_margin))
  ret$plot.title <- ggplot2::element_text(hjust = 0, size = plot_title_size,
                                          margin=margin(b=plot_title_margin))
  ret
}

## Define party colours
cols <- c("Left" = "#960E66",
          "Greens" = "#00A646",
          "AfD" = "#009FE1",
          "CDU" = "#000000",
          "CSU" = "#0080c8",
          "FDP" = "#ffed00",
          "SPD" = "#DF0029")

#lading data
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

load("complete_tweets_by_politicians.RData") 
tweets_by_politicians <- all.pol 
rm(all.pol) 

##getting the (updated) everypolitican data
everypol_bundestag <- read.csv("../Auxiliary datasets/unified_politicians_file_NOV2018.csv", 
                               stringsAsFactors = F, encoding="UTF-8") 

everypol_bundestag$twitter <- utf8_normalize(everypol_bundestag$twitter,
                                             remove_ignorable = T)
everypol_bundestag$twitter <- tolower(everypol_bundestag$twitter)
everypol_bundestag$twitter <- trimws(everypol_bundestag$twitter)

tweets_by_politicians$screen_name <- utf8_normalize(tweets_by_politicians$screen_name,
                                                    remove_ignorable = T)
tweets_by_politicians$screen_name <- tolower(tweets_by_politicians$screen_name)

## Rename and join political parties
everypol_bundestag$group <- as.factor(everypol_bundestag$group) 

everypol_bundestag$group <- plyr::revalue(everypol_bundestag$group, 
                                    c("Alliance '90/The Greens" = "Greens",
                                      "Alternative for Germany" = "AfD",
                                      "Christian Democratic Union" = 
                                        "CDU",
                                      "Christian Social Union of Bavaria" = 
                                        "CSU",
                                      "Die Linke" = "Left",
                                      "Free Democratic Party" = "FDP",
                                      "Social Democratic Party of Germany" = 
                                        "SPD"))

## Reorder factor levels
everypol_bundestag$group <- factor(everypol_bundestag$group, 
                                      levels = c("CDU","CSU", "SPD", "AfD", 
                                                 "FDP", "Greens", "Left"))

# merging data
tweets_by_politicians <- tweets_by_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata",  "twitter")], 
            by=c("screen_name"="twitter")) 


## load tweets at politicians
load("../AtPolit/final_de.RData")
tweets_at_politicians <- final_de
remove(final_de)

tweets_at_politicians$atId <- utf8_normalize(tweets_at_politicians$atId,
                                             remove_ignorable = T)
tweets_at_politicians$atId <- tolower(tweets_at_politicians$atId)
tweets_at_politicians$atId=gsub("\"","",tweets_at_politicians$atId)

#merging data (info about politican that is mentioned)
tweets_at_politicians <- tweets_at_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata", "twitter")], 
                               by=c("atId"="twitter")) 

###################
# Exploring data
####################

# Filter for all relevant tweets (made by politicians)
## number of politicans by gender in general 
gender_bt <- everypol_bundestag %>% 
  filter(!is.na(gender) & gender != "") %>%
  group_by(gender) %>% 
  summarise(n_bt = n()) %>% 
  mutate(rel_freq = n_bt/sum(n_bt)) 

## number of tweeting politicians by gender
gender_twitter <- tweets_by_politicians %>% 
  filter(!is.na(gender) & gender != "") %>%
  group_by(gender) %>% 
  summarise(n_twitter = n_distinct(name.y),
            rel_freq=(n_distinct(name.y)*100)/
              length(unique(tweets_by_politicians$name.y)))

## number of tweets by gender
gender_tweets <- tweets_by_politicians %>% 
  filter(!is.na(gender) & gender != "") %>%
  group_by(gender) %>% 
  summarise(n_tweets = n(),
            perc_bt=(n()*100)/nrow(tweets_by_politicians))

## Plot representation ratio by gender 
gender_representation <- gender_bt %>% 
  left_join(gender_twitter, by = "gender") %>% 
  left_join(gender_tweets, by = "gender") %>% 
  gather("n_bt", "n_twitter", "n_tweets",
         key = "type", value = "n") %>% 
  mutate(type = as.factor(type),
         gender = as.factor(gender)) %>% 
  ggplot(aes(type, n,fill = gender)) + 
  geom_bar(position = position_fill(reverse = TRUE),stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  scale_x_discrete(limits = c("n_bt", "n_twitter", "n_tweets"),
                   labels = c("Bundestag", "Twitter", "Number of tweets")) +
  scale_fill_manual(name = "Gender",
                     values = c("#EB811B", "#14B03D")) +
  xlab("") + ylab("") + 
  ggtitle("Representation by Gender") +
  theme_plex()

## save plot
pdf("../Graphs/gender_representation.pdf", width = 7, height = 5)
gender_representation
dev.off()

## Descriptive Plots
## Gender representation by politial party
gender_bt_groups <- everypol_bundestag %>% 
  filter(!is.na(gender) & !is.na(group)) %>%
  group_by(group) %>% 
  summarise(n_bt = n())

## Number of politicans by gender in general 
gender_bt_party <- everypol_bundestag %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n_bt = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_bt_party <- gender_bt_party %>% 
  left_join(gender_bt_groups, by = "group") %>% 
  mutate(ratio = n_bt.x / n_bt.y,
         gender = as.factor(gender),
         group = as.factor(group))

## plot gender ratios by party 
gender_bt_plot <- gender_bt_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, group, group), 
             n_bt.x,fill = gender)) + 
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  scale_y_continuous(labels = percent_format()) +
  xlab("") + ylab("") + 
  labs(title = "Ratio of politicians by gender and party",
       subtitle = "Member of the German Bundestag") +
  scale_fill_manual(name = "Gender",
                    values = c("#EB811B", "#14B03D")) +
  coord_flip() +
  theme_plex() +
  NULL

pdf("../Graphs/gender_bt_plot.pdf", width = 7, height = 5)
gender_bt_plot
dev.off()

## Tweets
## Gender representation by politial party
gender_tw_groups <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

## Number of politicans by gender in general 
gender_tw_party <- tweets_by_politicians %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_tw_party <- gender_tw_party %>% 
  left_join(gender_tw_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of tweets by party and gender 
gender_tweets_plot <- gender_tw_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, group, group), 
             n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
  xlab("") + ylab("") + 
  labs(title = "Total number of tweets by party and gender",
       subtitle = "July 6, 2017 to September 29, 2017") +
  scale_fill_manual(name = "Gender",
                    values = c("#EB811B", "#14B03D")) +
  coord_flip() +
  theme_plex() +
  NULL

## Save plot
pdf("../Graphs/gender_tweets_plott.pdf", width = 7, height = 5)
gender_tweets_plot
dev.off()

## Twitter
## Gender representation by politial party
gender_twitter_groups <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n_distinct(name.y))

## number of politicans by gender in general 
gender_twitter_party <- tweets_by_politicians %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n = n_distinct(name.y)) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_twitter_party <- gender_twitter_party %>% 
  left_join(gender_twitter_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of twitter account by party and gender 
gender_twitter_plot <- gender_twitter_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, group, group), 
             n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
  xlab("") + ylab("") + 
  labs(title = "Total number of twitter accounts by party and gender",
       subtitle = "July 6, 2017 to September 29, 2017") +
  scale_fill_manual(name = "Gender",
                    values = c("#EB811B", "#14B03D")) +
  coord_flip() +
  theme_plex() +
  NULL

## Save plot
pdf("../Graphs/gender_twitter_plot.pdf", width = 7, height = 5)
gender_twitter_plot
dev.off()

## Descriptive statistics of reply to politicans' tweets
gender_reply_groups <- tweets_at_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

## number of politicans by gender in general 
gender_reply_party <- tweets_at_politicians %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

## Join both datasets and create groupwise ratio
gender_reply_party <- gender_reply_party %>% 
  left_join(gender_reply_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

## Plot number of twitter account by party and gender 
twitter_replys_plot <- gender_reply_party %>% 
  ggplot(aes(forcats::fct_reorder2(group, group, group), n.x,fill = gender)) + 
  geom_bar(stat = "identity") +
#  scale_y_continuous(labels = percent_format()) +
  xlab("") + ylab("") + 
  labs(title = "Total number of tweets at politicians by party and gender",
       subtitle = "July 6, 2017 to September 29, 2017") +
  scale_fill_manual(name = "Gender",
                    values = c("#EB811B", "#14B03D")) +
  coord_flip() +
  theme_plex() +
  NULL

## Save plot
pdf("../Graphs/twitter_replys_plot.pdf", width = 7, height = 5)
twitter_replys_plot
dev.off()

## Ratio of favourites to total tweets by gender 
fav_plot <- tweets_by_politicians %>% 
  filter(is_retweet == FALSE & !is.na(gender) & !is.na(group) & 
           gender != "") %>% 
  group_by(gender) %>% 
  summarise(n = n(),
            fav = sum(favorite_count),
            ratio = fav / n) %>% 
  ggplot(aes(gender, ratio, fill = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Total number of favourites by gender",
       subtitle = "July 6, 2017 to September 29, 2017") +
  scale_fill_manual(name = "Gender",
                    values = c("#EB811B", "#14B03D")) +
  theme_plex() +
  NULL

## Save plot
pdf("../Graphs/fav_plot.pdf", width = 7, height = 5)
fav_plot
dev.off()

## Plot all descriptive plots together
## grid.arrange(gender_representation, gender_bt_plot, gender_twitter_plot, 
##              gender_tweets_plot, twitter_replys_plot, fav_plot, 
##              ncol = 2, nrow = 3,
##              top=textGrob("First descriptive statistics of GESIS data",
##                          gp=gpar(fontsize=20 ))) 


## Alternative tweet plot with party colours
gender_tw_plot <- gender_tw_party %>% 
  ggplot(aes(gender, n.x,fill = group)) + 
  geom_bar(stat = "identity") +
  facet_grid(group ~ .) +
  scale_fill_manual(values=cols) +
  xlab("") + ylab("") + 
  labs(title = "Number of tweets by politicians",
       subtitle = "July 6, 2017 to September 29, 2017") + 
  coord_flip() +
  theme_plex() +
  theme(strip.background = element_blank(),
  strip.text.y = element_blank())+
  guides(fill=guide_legend(title="Party")) +
  NULL

## Save plot
pdf("../Graphs/gender_tw_plot.pdf", width = 7, height = 5)
gender_tw_plot
dev.off()

## Alternative reply plot with party colours
gender_reply_plot <- gender_reply_party %>% 
  ggplot(aes(gender, n.x,fill = group)) + 
  geom_bar(stat = "identity") +
  facet_grid(group ~ .) +
  scale_fill_manual(values=cols) +
  xlab("") + ylab("") + 
  labs(title = "Number of tweets at politicians",
       subtitle = "July 6, 2017 to September 29, 2017") + 
  coord_flip() +
  theme_plex() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  guides(fill=guide_legend(title="Party")) +
  NULL

## Save plot
pdf("../Graphs/gender_reply_plot.pdf", width = 7, height = 5)
gender_reply_plot
dev.off()

## get common legend
# legend_tw <- get_legend(gender_tw_plot)

## create base plot
## prow <- plot_grid(gender_tw_plot + theme(legend.position="none"),
##                gender_reply_plot + theme(legend.position="none"),
##                align = 'vh',
##                hjust = -1,
##                nrow = 1)

## plot final tweet / reply plot with common legend 
## plot_grid( prow, legend_tw, rel_widths = c(3, .3))
