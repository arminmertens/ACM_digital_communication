##########################################################################
# Personal- vs. job-related Analysis:                                    #
#                                                                        #
# ANALYZING THE WORD USAGE OF PERSONAL- COMPARED TO JOB-RELATED          # 
# WORDS OF POLITICANS' TWEETS AND TWEETS TO THEM                         #
#                                                                        #
##########################################################################

#load library
library(tidyverse)
library(scales)
library(gridExtra)
library(grid)
library(quanteda)
library(utf8)
library(lme4)
library(rstudioapi)
library(dotwhisker)
library(broom)
library(dplyr)
library("effects")

current_path <- getActiveDocumentContext()$path #setting path
setwd(dirname(current_path))
######################################################################################
# CUSTOM THEME
######################################################################################


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

######################################################################################
# LOADING DATA
######################################################################################

#lading data
load("complete_tweets_by_politicians.RData") 

#getting all tweets by politicans
tweets_by_politicians <- all.pol 

#getting the (updated) everypolitican data
everypol_bundestag <- read.csv("../Auxiliary datasets/unified_politicians_file_NOV2018.csv", 
                               stringsAsFactors = F, encoding="UTF-8") 
everypol_bundestag$twitter <- utf8_normalize(everypol_bundestag$twitter,
                                             remove_ignorable = T)
everypol_bundestag$twitter <- tolower(everypol_bundestag$twitter)
everypol_bundestag$twitter <- trimws(everypol_bundestag$twitter)
tweets_by_politicians$screen_name <- utf8_normalize(tweets_by_politicians$screen_name,
                                                    remove_ignorable = T)
tweets_by_politicians$screen_name <- tolower(tweets_by_politicians$screen_name)

#merging data
tweets_by_politicians <- tweets_by_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata",  "twitter")], 
            by=c("screen_name"="twitter")) 
tweets_by_politicians$gender[which(tweets_by_politicians$screen_name=="martinschulz")] <- "male" #adding gender information for Martin Schulz


######################################################################################
# GETTING REPLY DATA
######################################################################################

load("responses_tweets_bypol.RData")
tweet_response <- responses_tweets_bypol
remove(responses_tweets_bypol)

tweet_response$reply_to_screen_name <- utf8_normalize(tweet_response$reply_to_screen_name,
                                                      remove_ignorable = T)
tweet_response$reply_to_screen_name <- trimws(tweet_response$reply_to_screen_name)
tweet_response$reply_to_screen_name <- tolower(tweet_response$reply_to_screen_name)
tweet_response$reply_to_screen_name <- gsub("\"","",tweet_response$reply_to_screen_name)

# Join reply data and tweets by politicians
tweet_response <- tweet_response %>% 
  left_join(all.pol[ , c("user_id", "text", "name", "status_id")], 
            by=c("reply_to_status_id"="status_id")) 

tweet_response  <- tweet_response %>% 
  rename(text_reply = text.x,
         name_reply = name.x,
         user_id_reply = user_id.x,
         user_id_pol = user_id.y,
         text_pol = text.y,
         name_pol = name.y)

#merging data (info about politican that is mentioned)
tweet_response <- tweet_response %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata", "twitter")], 
            by=c("reply_to_screen_name"="twitter")) 



######################################################################################
# GETTING AT POLITICIANS DATASET
######################################################################################

load("../final_de.RData")
tweets_at_politicians <- final_de
remove(final_de)

tweets_at_politicians$atId <- utf8_normalize(tweets_at_politicians$atId,
                                             remove_ignorable = T)

tweets_at_politicians$atId <- trimws(tweets_at_politicians$atId)
tweets_at_politicians$atId <- tolower(tweets_at_politicians$atId)
tweets_at_politicians$atId <- gsub("\"","",tweets_at_politicians$atId)

#merging data (info about politican that is mentioned)
tweets_at_politicians <- tweets_at_politicians %>% 
  left_join(everypol_bundestag[ , c("name", "gender", "group", "facebook", 
                                    "wikidata", "twitter")], 
            by=c("atId"="twitter"))


# missing values
tweets_at_politicians$gender[which(tweets_at_politicians$atId=="martinschulz")]="male"
tweets_at_politicians$gender[which(tweets_at_politicians$atId=="achimkessler")]="male"
tweets_at_politicians$group[which(tweets_at_politicians$atId=="achimkessler")]="Die Linke"


######################################################################################
# DICTIONARY, CORPUS
######################################################################################

#dictionary LIWC (personal and job)
mydict <- dictionary(file = "../Auxiliary datasets/LWIC Sentiment Dictionary/Dictionaries/LIWC2001_German.dic", 
                            format = "LIWC") ###loading the "personal" dictionary; personal and professional info

mydict.de <- dictionary(list(family=mydict$Family,friends=mydict$Friends, ##creating the categories for the dictionary
                             job=mydict$Job, leisure=mydict$Leisure, 
                             body=mydict$Body, sexual=mydict$Sexual, 
                             home=mydict$Home)) 

##BY POLITICAN 
c_tweets_by_politicians <- tweets_by_politicians %>% #keep only relevant vars and creating a corpus
  corpus() 
##AT POLITICAN
c_tweets_at_politicians=tweets_at_politicians %>% #keep only relevant vars and creating a corpus
  corpus() 

##############################
#Assigning dictionary to corpus 
##############################

##BY POLITICAN
senti <- dfm(c_tweets_by_politicians,
             remove=stopwords("de"),
             remove_punct=T,
             dictionary = mydict.de) %>%
  convert(to = "data.frame") 
tweets_by_politicians <- cbind(tweets_by_politicians,senti)

# personal info in word usage 
tweets_by_politicians$personalinfo <- tweets_by_politicians$family + tweets_by_politicians$friends + tweets_by_politicians$leisure 
# log. ratio of personal- vs. job-related words 
tweets_by_politicians$personal_job <- log((0.5+(tweets_by_politicians$personalinfo) / 
                                           0.5+(tweets_by_politicians$job))) 
##AT POLITICAN
senti <- dfm(c_tweets_at_politicians,
          remove=stopwords("de"),
          remove_punct=T,
          dictionary = mydict.de) %>%
         convert(to = "data.frame") 
tweets_at_politicians <- cbind(tweets_at_politicians,senti)

# creating var.: personal info in word usage a new variable (sum of friends-, family and leisture-related word occurrences)
tweets_at_politicians$personalinfo <- tweets_at_politicians$family +  tweets_at_politicians$friends + tweets_at_politicians$leisure 
## creating var.: log. ratio of personal.info and job-related words 
tweets_at_politicians$personal_job <- log((0.5 + (tweets_at_politicians$personalinfo) / 
                                           0.5 + (tweets_at_politicians$job))) 



######################################################################################
# PLOTTING REGRESSION RESULTS, PERSONAL/JOB- word-usage ratio
######################################################################################
######################################################################################
# MULTILEVEL REGRESSION, BY POLITICANS
######################################################################################


tweets_by_politicians$labels=factor(paste0(tweets_by_politicians$gender," + ",tweets_by_politicians$group))
mod.b_robust <- lmer(personal_job~ labels + (1|user_id), data=tweets_by_politicians)


effects.robust.by=Effect(c("labels"), mod.b_robust,
                         xlevels=list(labels=unique(tweets_by_politicians$labels)))
preds.bypol.robust=data.frame(labels=effects.robust.by$x$labels,
                              estimate=effects.robust.by$fit,
                              lower=effects.robust.by$lower,
                              upper=effects.robust.by$upper,
                              std.error=effects.robust.by$se)

info=do.call("rbind",strsplit(as.character(preds.bypol.robust$labels), "\\+"))
preds.bypol.robust$model=info[,1]
preds.bypol.robust$term=info[,2]
preds.bypol.robust$term=preds.bypol.robust$term%>%
  trimws()%>%
  recode_factor('Christian Democratic Union'="CDU",
                'Christian Social Union of Bavaria'= "CSU",
                'Social Democratic Party of Germany'="SPD",
                'Alternative for Germany' = "AfD",
                'Free Democratic Party' = "FDP",
                "Alliance '90/The Greens" = "The Greens",
                'Die Linke' = "The Left")

preds.bypol.robust=preds.bypol.robust[order(preds.bypol.robust$model,decreasing = T),]
p=dwplot(preds.bypol.robust,
         dot_args = list(aes(shape=model, colour=model)),
         whisker_args = list(aes(linetype = model,colour=model)))  +
 geom_hline(yintercept = 4.5,lty=2)+
 xlab("job vs. personal") + ylab("") +
  labs(title = "Predicting job- vs. personal-related communication",
       subtitle = "Tweets by politicians") +
 scale_color_manual(name = "Gender",
                     values = c("#14B03D", "#EB811B")) +
  scale_shape_discrete(name = "Gender")+
  theme_plex()
p
pdf("../Graphs/Personal_job_by_politican.pdf", width = 7, height = 5) # Open a new pdf file
p
dev.off()

######################################################



######################################################################################
# PLOTTING REGRESSION RESULTS, PERSONAL/JOB- word-usage ratio
######################################################################################
######################################################################################
# REGRESSION, AT POLITICANS
######################################################################################



table(tweets_at_politicians$gender,exclude=NULL)
tweets_at_politicians=tweets_at_politicians%>%filter(!is.na(gender))
tweets_at_politicians$labels=factor(paste0(tweets_at_politicians$gender," + ",tweets_at_politicians$group))


mod.d <- lm(personal_job~labels,data=tweets_at_politicians)
summary(mod.d)
nd_at <- with(tweets_at_politicians,
              expand.grid(labels = unique(tweets_at_politicians$labels)))

pred_list2_at <- predict(mod.d, nd_at, se.fit = TRUE)
preds_at <- with(pred_list2_at,
                 data.frame(nd_at, prob = fit,  std.error=se.fit,
                            lower = fit - 1.96 * se.fit,
                            upper = fit + 1.96 * se.fit))

info=do.call("rbind",strsplit(as.character(preds_at$labels), "\\+"))
preds_at$model=info[,1]
preds_at$term=info[,2]
colnames(preds_at)[colnames(preds_at) == 'prob'] <- 'estimate'

preds_at$term=preds_at$term%>%
  trimws()%>%
  recode_factor('Christian Democratic Union'="CDU",
                'Christian Social Union of Bavaria'= "CSU",
                'Social Democratic Party of Germany'="SPD",
                'Alternative for Germany' = "AfD",
                'Free Democratic Party' = "FDP",
                "Alliance '90/The Greens" = "The Greens",
                'Die Linke' = "The Left")
preds_at=preds_at[order(preds_at$model,decreasing = T),]#

p_at=dwplot(preds_at,
         dot_args = list(aes(shape=model, colour=model)),
         whisker_args = list(aes(linetype = model,colour=model))) +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("job vs. personal") + ylab("")   +
  labs(title = "Predicting job- vs. personal-related communication",
       subtitle = "Tweets at politicians") +
  scale_color_manual(name = "Gender",
                     values = c("#14B03D", "#EB811B")) +
  scale_shape_discrete(name = "Gender")+
  theme_plex() 
p_at
pdf("../Graphs/Personal_job_at_politican.pdf", width = 7, height = 5) # Open a new pdf file
p_at
dev.off()



######################################################################################
# FINAL EDITED PLOTS FOR THE PUBLICATION
######################################################################################

library(tikzDevice)

tikz(file = "pers_job_2.tex", width = 5, height = 4)
dwplot(preds_at,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model))) +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("job vs. personal") + ylab("")   +
  scale_color_manual(name = "gender",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "gender")+
  theme_bw() 
endoffile <- dev.off() 

tikz(file = "pers_job_1.tex", width = 5, height = 4)
dwplot(preds.bypol.robust,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model)))  +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("job vs. personal") + ylab("") +
  scale_color_manual(name = "gender",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "gender")+
  theme_bw() 

endoffile <- dev.off() 


