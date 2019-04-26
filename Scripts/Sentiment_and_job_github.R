####################################################################################################################
# Replication Data for: Mertens, Pradel, Rozyjumayeva, Wäckerle, Rozyjumayeva (2019):  As the tweet, so the reply? #
#                                                                                                                  #
####################################################################################################################

####################################################################################################################
# Personal-job                                                                                                     #
#                                                                                                                  #
####################################################################################################################


#packages
library(tidyverse)
library(dotwhisker)
library(lme4)
library(effects)
library(tikzDevice)



######################################################################################
# BY Politicians
######################################################################################
load("../Generated_data_for_github/bypol_data.Rdata")

mod.b_robust <- lmer(personal_job~ labels + (1|id_person), data=bypol_data)


effects.robust.by=Effect(c("labels"), mod.b_robust,
                         xlevels=list(labels=unique(bypol_data$labels)))
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

######################################################################################
# AT Politicians
######################################################################################

load("../Generated_data_for_github/atpol_data.Rdata")

mod.d <- lm(personal_job~labels,data=atpol_data)
summary(mod.d)
nd_at <- with(atpol_data,
              expand.grid(labels = unique(atpol_data$labels)))

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



######################################################################################
# PLOTS
######################################################################################


#tikz(file = "pers_job_2.tex", width = 5, height = 4)
dwplot(preds_at,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model))) +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("job vs. personal") + ylab("")   +
  scale_color_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "")+
  theme_bw() 
#endoffile <- dev.off() 

#tikz(file = "pers_job_1.tex", width = 5, height = 4)
dwplot(preds.bypol.robust,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model)))  +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("job vs. personal") + ylab("") +
  scale_color_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "")+
  theme_bw() 

#endoffile <- dev.off() 


####################################################################################################################
# Sentiment                                                                                                  #
#                                                                                                                  #
####################################################################################################################

#################
# By politicians

mod.b_robust <- lmer(Sentiment~ labels +(1|id_person), data=bypol_data)
summary(mod.b_robust)

effects.robust.by=Effect(c("labels"), mod.b_robust,
                         xlevels=list(labels=unique(bypol_data$labels)))
preds.bypol.robust=data.frame(labels=effects.robust.by$x$labels,
                              estimate=effects.robust.by$fit,
                              lower=effects.robust.by$lower,
                              upper=effects.robust.by$upper,
                              std.error=effects.robust.by$se)
info=do.call("rbind",strsplit(as.character(preds.bypol.robust$labels), "\\+"))
preds.bypol.robust$model=info[,1]
preds.bypol.robust$term=info[,2]


preds.bypol.robust=preds.bypol.robust[order(preds.bypol.robust$model,decreasing = T),]



#################
# At politicians

mod.d <- lm(Sentiment~labels,data=atpol_data)
nd <- with(atpol_data,
           expand.grid(labels = unique(atpol_data$labels)))
pred_list <- predict(mod.d, nd, se.fit = TRUE)
preds.atpol <- with(pred_list,
                    data.frame(nd, prob = fit, std.error=se.fit,
                               lower = fit - 1.96 * se.fit,
                               upper = fit + 1.96 * se.fit))

info=do.call("rbind",strsplit(as.character(preds.atpol$labels), "\\+"))
preds.atpol$model=info[,1]
preds.atpol$term=info[,2]

colnames(preds.atpol)[colnames(preds.atpol) == 'prob'] <- 'estimate'
str(preds.atpol)

######## Plots
preds.bypol.robust$term=preds.bypol.robust$term%>%
  trimws()%>%
  recode_factor('Christian Democratic Union'="CDU",
                'Christian Social Union of Bavaria'= "CSU",
                'Social Democratic Party of Germany'="SPD",
                'Alternative for Germany' = "AfD",
                'Free Democratic Party' = "FDP",
                "Alliance '90/The Greens" = "The Greens",
                'Die Linke' = "The Left")
preds.atpol$term=preds.atpol$term%>%
  trimws()%>%
  recode_factor('Christian Democratic Union'="CDU",
                'Christian Social Union of Bavaria'= "CSU",
                'Social Democratic Party of Germany'="SPD",
                'Alternative for Germany' = "AfD",
                'Free Democratic Party' = "FDP",
                "Alliance '90/The Greens" = "The Greens",
                'Die Linke' = "The Left")

# --------------
# plot for ACM paper

#tikz(file = "sentiment_1.tex", width = 5, height = 4)
dwplot(preds.bypol.robust,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model)))  +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("sentiment") + ylab("") +
  # labs(title = "Predicting Sentiment",
  #     subtitle = "Tweets by politicians") +
  scale_color_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "")+
  theme_bw() 
#endoffile <- dev.off() 

#tikz(file = "sentiment_2.tex", width = 5, height = 4)
dwplot(preds.atpol,
       dot_args = list(aes(shape=model, colour=model)),
       whisker_args = list(aes(linetype = model,colour=model))) +
  geom_hline(yintercept = 4.5,lty=2)+
  xlab("sentiment") + ylab("")   +
  scale_color_manual(name = "",
                     values = c("#440154FF", "#20A387FF")) +
  scale_shape_discrete(name = "")+
  theme_bw() 
#endoffile <- dev.off() 


