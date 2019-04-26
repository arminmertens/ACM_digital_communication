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

# anonomyzed tweets by politicians
load("bypol_data.RData") 
tweets_by_politicians <- bypol_data
rm(bypol_data) 

# anonomyzed tweets at politicans
load("atpol_data.Rdata")
tweets_at_politicians <- atpol_data
remove(atpol_data)


# ----------------------------
# clean data

# create gender and party variable for tweets by politicians
tweets_by_politicians <- tweets_by_politicians %>% 
  separate(labels, c("gender", "group"), "\\+") 

# remove whitespace and recode gender as factor
tweets_by_politicians$gender <- as.factor(str_trim(tweets_by_politicians$gender, 
                                                   "both"))

# recode and reorder party variable
tweets_by_politicians$group <- as.factor(tweets_by_politicians$group) 
tweets_by_politicians$group <- plyr::revalue(tweets_by_politicians$group, 
                                    c(" Alliance '90/The Greens" = "Greens",
                                      " Alternative for Germany" = "AfD",
                                      " Christian Democratic Union" = 
                                        "CDU",
                                      " Christian Social Union of Bavaria" = 
                                        "CSU",
                                      " Die Linke" = "Left",
                                      " Free Democratic Party" = "FDP",
                                      " Social Democratic Party of Germany" = 
                                        "SPD"))

tweets_by_politicians$group <- factor(tweets_by_politicians$group, 
                                   levels = c("AfD","FDP", "CSU", "CDU", 
                                              "SPD", "Greens", "Left"))

# create gender and party variable for tweets by politicians
tweets_at_politicians <- tweets_at_politicians %>% 
  separate(labels, c("gender", "group"), "\\+")

# remove whitespace and recode gender as factor
tweets_at_politicians$gender <- as.factor(str_trim(tweets_at_politicians$gender, 
                                                   "both"))

# recode and reorder party variable
tweets_at_politicians$group <- as.factor(tweets_at_politicians$group) 
tweets_at_politicians$group <- plyr::revalue(tweets_at_politicians$group, 
                                             c(" Alliance '90/The Greens" = "Greens",
                                               " Alternative for Germany" = "AfD",
                                               " Christian Democratic Union" = 
                                                 "CDU",
                                               " Christian Social Union of Bavaria" = 
                                                 "CSU",
                                               " Die Linke" = "Left",
                                               " Free Democratic Party" = "FDP",
                                               " Social Democratic Party of Germany" = 
                                                 "SPD"))

tweets_at_politicians$group <- factor(tweets_at_politicians$group, 
                                      levels = c("AfD","FDP", "CSU", "CDU", 
                                                 "SPD", "Greens", "Left"))

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
# use tikz and dev.off() to render in latex enironment
dev.off()
tikz(file = "plot_descriptive_1.tex", width = 5, height = 4)
gender_tw_party %>% 
  ggplot(aes(group, n.x,fill = gender, width=.75)) + 
  geom_bar(stat = "identity",position=position_dodge()) +
  theme_bw() +
  scale_fill_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  xlab("") + ylab("") + 
  coord_flip() +
  ylab("\nnumber of tweets") +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.background = element_rect(fill = "white")) +
  geom_vline(xintercept = 4.5,lty=2) +
  NULL
endoffile <- dev.off() 

# Plot Figure 2
tikz(file = "plot_descriptive_2.tex", width = 5, height = 4)
gender_reply_party %>% 
  ggplot(aes(group, n.x,fill = gender, width = .75)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  xlab("") +
  scale_fill_manual(name = "",
                    values = c("#440154FF", "#20A387FF")) +
  ylab("\nnumber of tweets") + 
  coord_flip() +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text.y = element_blank())+
  guides(fill=guide_legend(title="")) +
  theme(axis.text = element_text(size = 12)) +
  geom_vline(xintercept = 4.5,lty=2) +
  NULL
endoffile <- dev.off() 
