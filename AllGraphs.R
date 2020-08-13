#libraries
rm(list= ls())
library(dplyr)
library(readxl)
library(ggplot2)

#Failed plotting attempts -----
ggplot(tweets_at_politicians, aes(x= group)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent") 



ggplot(tweets_at_politicians, aes(x= group)) + 
  geom_bar(aes(y = ..prop.., stat="count")) +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..prop.. ), stat= "count", vjust = -.5) +
  labs(y = "Percent", fill="Party") +
  facet_grid(~group) +
  scale_y_continuous(labels = scales::percent)


ggplot()


#Transforming the data
atpolit <- tweets_at_politicians %>% 
  group_by(group) %>%
  summarise(number = n())

#sorting the data
atpolit

#Transforming the bypolit
bypolit <- tweets_by_politicians %>% 
  group_by(group) %>%
  summarise(number = n())

#Now grapging
ggplot(data=atpolit, aes(x = group, y=number)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=group), vjust=-0.3, size=3.5)+
  theme_minimal()

#graphing the bypoliticians
ggplot(data=bypolit, aes(x = group, y=number)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=group), vjust=-0.3, size=3.5)+
  theme_minimal()



#Bar chart of the followers -----

FollowerCount <- read_excel("C:/Users/wmc855/Desktop/FollowerCount.xlsx")
names(FollowerCount) <- c("Party", "Followers")

FollowerCount$Party <- factor(FollowerCount$Party, levels = c("Die Grünen", "SPD", "FDP", "CDU", "Die Linke", "CSU", "AfD"))
options(scipen = 10000)
ggplot(FollowerCount, aes(x = Party, y = Followers, fill = Party)) +
                  geom_bar(stat="identity") + 
  theme_classic() +
  scale_fill_manual(name = "",
                    values = c("green2", "red3", "yellow2", "royalblue3", "purple3", "black", "turquoise2")) +
  xlab("") + ylab("") + 
  ylab("\nNumber of followers") +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.background = element_rect(fill = "white")) 


 #So, the colour needs to change

#Bar chart of the hashtags -----

Hashtags <- read_excel("C:/Users/wmc855/Desktop/Hashtags.xlsx")
names(Hashtags) <- c("Party", "Hashtags")

Hashtags$Party <- factor(Hashtags$Party, levels = c("Die Grünen", "CDU", "Die Linke", "FDP", "CSU", "SPD", "AfD"))
options(scipen = 10000)
ggplot(Hashtags, aes(x = Party, y = Hashtags, fill = Party)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_fill_manual(name = "",
                    values = c("green2", "red3", "yellow2", "royalblue3", "purple3", "black", "turquoise2")) +
  xlab("") + ylab("") + 
  ylab("\nProportion of party hashtags") +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.background = element_rect(fill = "white")) 


#So, the colour needs to change


#Bar chart of the Retweets -----

Retweets <- read_excel("C:/Users/wmc855/Desktop/Retweets.xlsx")
names(Retweets) <- c("Party", "Retweets")

Retweets$Party <- factor(Retweets$Party, levels = c( "CDU",  "FDP",  "Die Linke", "CSU", "SPD", "Die Grünen", "AfD"))
options(scipen = 10000)
ggplot(Retweets, aes(x = Party, y = Retweets, fill = Party)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_fill_manual(name = "",
                    values = c("royalblue3", "yellow2", "purple3", "black", "red3", "green2", "turquoise2")) +
  xlab("") + ylab("") + 
  ylab("\nProportion of party retweets") +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.background = element_rect(fill = "white")) 


#The messages charts ----

Messages <- read_excel("C:/Users/wmc855/Desktop/messages.xlsx")
names(Messages) <- c("Party", "Messages")

Messages$Party <- factor(Messages$Party, levels = c( "CSU",  "SPD",  "Die Linke", "Die Grünen", "AfD", "FDP", "CDU"))
options(scipen = 10000)
ggplot(Messages, aes(x = Party, y = Messages, fill = Party)) +
  geom_bar(stat="identity") + 
  theme_classic() +
  scale_fill_manual(name = "",
                    values = c("black", "red3", "purple3", "green2", "turquoise2", "yellow2", "royalblue3")) +
  xlab("") + ylab("") + 
  ylab("\nProportion of messages to party accounts") +
  theme(axis.text = element_text(size = 12)) +
  theme(strip.background = element_rect(fill = "white")) 






