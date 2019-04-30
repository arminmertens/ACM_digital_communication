#The stm for at politicians ----
#Preliminaries
rm(list=ls())
#libraries
library(tidyverse)
library(stm)

library(Rtsne)
library(rsvd)
library(magic)
library(abind)
library(geometry)
library(tm)


#The functions ----
clean.text <- function(some_txt)
{
  #this can help me to replace the umlauts into an stm readable text
  some_txt = gsub(pattern = '[?]', replacement = "ae",some_txt)
  some_txt = gsub(pattern = '[?]', replacement = "ue",some_txt)
  some_txt = gsub(pattern = '[?]', replacement = "oe",some_txt)
  
  #the rest of cleaning
  some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
  some_txt = gsub("@\\w+", "", some_txt)
  some_txt = gsub("[[:punct:]]", "", some_txt)
  some_txt = gsub("[[:digit:]]", "", some_txt)
  some_txt = gsub("http\\w+", "", some_txt)
  some_txt = gsub("[ \t]{2,}", "", some_txt)
  some_txt = gsub("^\\s+|\\s+$", "", some_txt)
  some_txt = gsub("amp", "", some_txt)
  
  #the following is to remove the chinese, japanese and korean characters
  some_txt = iconv(some_txt, "latin1", "ASCII", sub="")
  
  # define "tolower error handling" function
  try.tolower = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, "error"))
      y = tolower(x)
    return(y)
  }
  
  some_txt = sapply(some_txt, try.tolower)
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

#the basic clean
clean.text2 <- function(some_txt) {
  #this can help me to replace the umlauts into an stm readable text
  some_txt = gsub(pattern = '[?]', replacement = "ae",some_txt)
  some_txt = gsub(pattern = '[?]', replacement = "ue",some_txt)
  some_txt = gsub(pattern = '[?]', replacement = "oe",some_txt)
  some_txt = gsub(pattern = '[?]', replacement = "e",some_txt) 
  some_txt = gsub(pattern = '[?]', replacement = "Oe",some_txt) 
  some_txt = gsub(pattern = '[?]', replacement = "e",some_txt)
  
  some_txt = some_txt[some_txt != ""]
  names(some_txt) = NULL
  return(some_txt)
}

#load the datasets called ----
#final_de1
load("C:/Users/wmc855/Desktop/stms_current/stm_atPoliticians/Final_At_Pol.RData")
#covariates data
load("C:/Users/wmc855/Desktop/stms_current/stm_atPoliticians/CovariatesAll.RData")
names(final_dta)

#cleaning the names in covariates data
final_dta$names.full <- clean.text2(final_dta$names.full)


#Counting the number of words per text and then
#subsetting by the count
final_de1$wordcount <- sapply(final_de1$ctext, function(x) length(unlist(strsplit(as.character(x), "\\W+"))))

#Subsetting and selecting
stm_de <- final_de1 %>% 
  filter(wordcount > 3) %>% 
  select(-c(text, First, Last, party,
            education, names, 
            friends_count, wordcount, 
            atId))


#Stopwords
stopwords("german")

#transforming the stopwords
stpwords_corrected <- clean.text2(stopwords("german"))

#removing the stopwords
stm_de$ctext <- removeWords(stm_de$ctext, stpwords_corrected)


#the stms ----
#Running the model hopefully
processed <- textProcessor(stm_de$ctext, metadata = stm_de, 
                           removestopwords = TRUE, removenumbers = TRUE, 
                           removepunctuation = TRUE, verbose = TRUE, language = "german")
rm(stm_de, final_de1)

#The stm object generation
out <- prepDocuments(processed$documents, processed$vocab, processed$meta)


#Saving the output object details
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#inspecting the removed data
par(mar=c(1,1,1,1))
plotRemoved(processed$documents, lower.thresh = seq(1, 200, by = 100))

#Fitting the model
TwitterFit <- stm(out$documents, out$vocab, K=0, 
                  control = c("favorite_count", "retweet_count", 
                              "followers_count", "gender", "group", 
                              "age"), 
                  max.em.its=75, data=out$meta, init.type="Spectral", 
                  seed=8458159)

# Labelling 
labelTopics(Twitt)


#Covariate analysis 1: indep variable is GENDER ----
prep_gender_at <- estimateEffect(1:20 ~ gender, TwitterFit, 
                                 metadata = out$meta, uncertainty = "Global")

#the stats
summary(prep_gender)

#the chart
plot(prep_gender, covariate="gender", topics=c(1:60), model=TwitterFit, 
     method="difference", cov.value1="male", cov.value2="female",
     xlab="More femals ... More males", main="Effect of Male vs. Female",
     xlim=c(-.15,.15), labeltype ="custom")


#Covariate analysis 2: indep variable is Party ----
prep_party <- estimateEffect(1:20 ~ group, TwitterFit, 
                             metadata = out$meta, uncertainty = "Global")
summary(prep_party)
plot(prep_party, covariate="group", topics=c(1:60), model=TwitterFit, 
     method="difference", cov.value1="SPD", cov.value2="Die Gruenen",
     cov.value3="FDP", cov.value4="CDU", cov.value5="Die Linke",
     cov.value6="AfD", cov.value7="CSU",
     xlab="More femals ... More males", main="Effect of Male vs. Female",
     xlim=c(-.15,.15), labeltype ="custom")

#Covariate analysis 3: indep variable is Party and Gender ----
prep_pg <- estimateEffect(1:60 ~ group + gender + , TwitterFit, 
                          metadata = out$meta, uncertainty = "Global")

summary(prep_pg)


#Covariate analysis 4: Party*Gender ----
out$meta$gender <- as.factor(out$meta$gender)
out$meta$party <- as.factor(out$meta$group)
prep_p*g <- estimateEffect(1:20 ~ group*gender, TwitterFit, 
                           metadata = out$meta, uncertainty = "Global")

summary(prep*pg)

#Interactions: requires a separate running of the entire code!
TwitterInteraction <- stm(out$documents, out$vocab, K = 60, prevalence = ~group + gender + group*gender, 
                          max.em.its = 50, data = out$meta, seed = 8458159)

prep_Ineraction <- estimateEffect(1:60 ~ group*gender, TwitterInteraction, 
                                  metadata = out$meta, uncertainty = "Global")

summary(prep_Ineraction)

#labeling the topics
labelTopics(TwitterFit)
summary(prep_pg)
plot(poliblogPrevFit, type = "perspectives", topics = c(12, 20))

#some evaluation techniques
topicQuality(model=TwitterFit, documents=docs)

#correlations of topics
mod.out.corr <- topicCorr(TwitterFit)
plot(mod.out.corr)

#Looking at the topics
TwitterContent <- stm(out$documents, out$vocab, K = 60, prevalence = ~ group + gender, content = ~ gender,
                      max.em.its = 50, data = out$meta, init.type = "Spectral")

plot(prep_gender, type = "perspectives", topics = 11)


#Presentation of the topics
labelTopics(TwitterFit)
summary(prep_pg)
plot(TwitterContent, type = "perspectives", topics = c(6, 7))

