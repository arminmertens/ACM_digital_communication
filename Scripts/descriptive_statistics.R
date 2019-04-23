# ----------------------------
# Replication code for Mertens et al. (2019) "As the tweet, so the reply? Gender 
# bias in digital communicationwith politicians", in 11th ACM Conference on Web 
# Science (WebSci ’19),June 30–July 3, 2019, Boston, MA, USA.

# Replication code for Section 5 - Descriptive statistics
# ----------------------------

# ----------------------------
# load libraries
pkgs <- c("rstudioapi", "tidyverse", "scales", "gridExtra", "grid", "cowplot",
          "utf8", "viridis", "tikzDevice")

load <- sapply(pkgs, function(x) {
  suppressPackageStartupMessages(
    require(x, character.only = TRUE))
})
load
rm(load, pkgs)

# ----------------------------
# load data
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path))

load("complete_tweets_by_politicians.RData") 
tweets_by_politicians <- all.pol 
rm(all.pol) 

# getting the eeverypolitican data
everypol_bundestag <- read.csv("../Auxiliary datasets/unified_politicians_file_NOV2018.csv", 
                               stringsAsFactors = F, encoding="UTF-8") 

everypol_bundestag$twitter <- utf8_normalize(everypol_bundestag$twitter,
                                             remove_ignorable = T)
everypol_bundestag$twitter <- tolower(everypol_bundestag$twitter)
everypol_bundestag$twitter <- trimws(everypol_bundestag$twitter)

tweets_by_politicians$screen_name <- utf8_normalize(tweets_by_politicians$screen_name,
                                                    remove_ignorable = T)
tweets_by_politicians$screen_name <- tolower(tweets_by_politicians$screen_name)

# rename and join political parties
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

# reorder factor levels
everypol_bundestag$group <- factor(everypol_bundestag$group, 
                                   levels = c("AfD","FDP", "CSU", "CDU", 
                                              "SPD", "Greens", "Left"))

# merging data
tweets_by_politicians <- tweets_by_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata",  "twitter")], 
            by=c("screen_name"="twitter")) 

# load tweets at politicians
load("../AtPolit/final_de.RData")
tweets_at_politicians <- final_de
remove(final_de)

tweets_at_politicians$atId <- utf8_normalize(tweets_at_politicians$atId,
                                             remove_ignorable = T)
tweets_at_politicians$atId <- tolower(tweets_at_politicians$atId)
tweets_at_politicians$atId=gsub("\"","",tweets_at_politicians$atId)

# merging data (info about politican that is mentioned)
tweets_at_politicians <- tweets_at_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata", "twitter")], 
                               by=c("atId"="twitter")) 

# ----------------------------
# calculate summary statistics for descriptive plots

# 1. tweets by politicians
# gender representation by politial party
gender_tw_groups <- tweets_by_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

# number of politicans by gender in general 
gender_tw_party <- tweets_by_politicians %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

# join both datasets and create groupwise ratio
gender_tw_party <- gender_tw_party %>% 
  left_join(gender_tw_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

# relabel gender variable
gender_tw_party$gender <- factor(gender_tw_party$gender,
                                 levels = c("male", "female"))

# relable party variable
gender_tw_party$group <- factor(gender_tw_party$group,
                                levels = c("Left", "Greens", "FDP", "AfD",
                                           "SPD", "CSU", "CDU"))
# add male row for AfD
tw_adf <- data.frame(group = "AfD",
                     gender = "male",
                     n.x = 0,
                     n.y = 0,
                     ratio = 0)

gender_tw_party <- rbind(gender_tw_party, tw_adf)


# 2. tweets at politicians
# descriptive statistics of reply to politicans' tweets
gender_reply_groups <- tweets_at_politicians %>% 
  filter(!is.na(gender)) %>%
  group_by(group) %>% 
  summarise(n = n())

# number of politicans by gender in general 
gender_reply_party <- tweets_at_politicians %>% 
  filter(!is.na(gender) & !is.na(group) & gender != "") %>%
  group_by(group, gender) %>% 
  summarise(n = n()) %>% 
  ungroup() 

# join both datasets and create groupwise ratio
gender_reply_party <- gender_reply_party %>% 
  left_join(gender_reply_groups, by = "group") %>% 
  mutate(ratio = n.x / n.y,
         gender = as.factor(gender),
         group = as.factor(group))

# relabel gender variable 
gender_reply_party$gender <- factor(gender_reply_party$gender,
                                 levels = c("male", "female"))

# relabel party variable
gender_reply_party$group <- factor(gender_reply_party$group,
                                levels = c("Left", "Greens", "FDP", "AfD",
                                           "SPD", "CSU", "CDU"))
# add male row for AfD
gender_reply_party <- rbind(gender_reply_party, tw_adf)


# ----------------------------
# Plots for ACM Paper (output in .tex)
# Plot Figure 1
dev.off()
tikz(file = "plot_descriptive_1.tex", width = 5, height = 4)
gender_tw_party %>% 
  ggplot(aes(group, n.x,fill = gender, width=.75)) + 
  geom_bar(stat = "identity",position=position_dodge()) +
 # facet_grid(group ~ .) +
  theme_bw() +
  scale_fill_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  xlab("") + ylab("") + 
#  labs(title = "Number of tweets by politicians",
#       subtitle = "July 6, 2017 to September 29, 2017") + 
  coord_flip() +
  ylab("number of tweets") +
  theme(strip.background = element_rect(fill = "white")) +
  geom_vline(xintercept = 4.5,lty=2) +
# match default line size of theme_classic# hide facet o
  NULL
endoffile <- dev.off() 

# Plot Figure 2
tikz(file = "plot_descriptive_2.tex", width = 5, height = 4)
gender_reply_party %>% 
  ggplot(aes(group, n.x,fill = gender, width = .75)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("") +
  #facet_grid(group ~ .) +
  scale_fill_manual(name = "gender",
                    values = c("#440154FF", "#20A387FF")) +
  ylab("number of tweets") + 
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  guides(fill=guide_legend(title="Party")) +
  geom_vline(xintercept = 4.5,lty=2) +
  NULL
endoffile <- dev.off() 
