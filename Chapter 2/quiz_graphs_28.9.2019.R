#########################################################
# ESSEXLab Experiments 2018 (Round 2)
# 28 September 2019
# Part 2 -- Data Visualization
#########################################################

rm(list = ls()) 
setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/2.SeedcornProject/Data/Round2/") 
library(foreign) # for read.csv 
# library(plyr) 
library(dplyr) 
library(tidyr) 
# library(car) # for recode
library(stargazer) 
library(ggplot2) 
library(scales) 
library(ggthemes) 
library(ggThemeAssist) 

load("df") 
load("df_long") 


#########################################################
# Define helper functions 
#########################################################

## SummarySE
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
## http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/#Helper functions

summarySE <- function(data=NULL, 
                      measurevar, 
                      groupvars=NULL, 
                      na.rm=FALSE,
                      conf.interval=.95, 
                      .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, 
                 groupvars, 
                 .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

#########################################################
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color="black"))
#########################################################


## Create new variables

### paid ################################################

df$paid <- ifelse(df$participant.payoff < 5, 5, 
                  df$participant.payoff)

mean(df$paid) # 7.07

print(payoffs_hist)
ggsave("images/payoffs_hist.png")



#########################################################
# Tables
#########################################################

# How many participants?
nrow(df) # 277

# female
prop.table(table(df$gender)) # 61.7% female

# at Essex
prop.table(table(df$atEssex)) # 84% at Essex --84

# Student
prop.table(table(df$positionAtEssex)) # 48% undergrad

# Age
table(df$born)
df$born <- as.numeric(df$born)
df$age <- 2018 - df$born
min(df$age) # 19
max(df$age) # 80
mean(df$age) # 31.4
sd(df$age) # 14.3
hist(df$age)

# timing
hist(df$participant.time_started, 
     breaks = "weeks")

timing <- 
ggplot(df, aes(participant.time_started, ..count..)) + 
  geom_histogram() +
  theme_bw() + 
  xlab(NULL) +
  scale_x_datetime(breaks = date_breaks("weeks"),
                  # labels = date_format("%b-%d")) +
                  labels = date_format("%b")) +
  cleanup

# https://stackoverflow.com/questions/10770698/understanding-dates-and-plotting-a-histogram-with-ggplot2-in-r

print(timing)
ggsave("images/timing.png")

# British
prop.table(table(df$british))

# mean payoffs
str(df$participant.payoff)
mean(df$participant.payoff)
sd(df$participant.payoff)

# Team A
mean(subset(df, treatment=="A")$participant.payoff) # 8.88
sd(subset(df, treatment=="A")$participant.payoff) # 1.35
hist(subset(df, treatment=="A")$participant.payoff) 
min(subset(df, treatment=="A")$participant.payoff) # 5.75
max(subset(df, treatment=="A")$participant.payoff) # 11.80

# Team B
mean(subset(df, treatment=="B")$participant.payoff) # 4.83
sd(subset(df, treatment=="B")$participant.payoff) # 1.24
hist(subset(df, treatment=="B")$participant.payoff) 
min(subset(df, treatment=="B")$participant.payoff) # 2.25
max(subset(df, treatment=="B")$participant.payoff) # 8.40

age_dis <- summarySE(df, 
                     measurevar="age",
                     groupvars=c("positionAtEssex"))

# In total, how many A/B?
table(df$treatment) # 133 A, 144 B

# In total, how many fairplay / unfair?
table(df$falsefeedback) # 152 fairplay 125 unfair! 

# How many people per cell?
table(df$treatment, df$falsefeedback)
# 61 unfair / B

# dplyr equivalent
df %>%
  group_by(treatment, falsefeedback) %>%
  summarise(
    n = length(unique(participant.code))
  )

# How many people / cell / session?
df %>%
  group_by(session.code, treatment, falsefeedback) %>%
  summarise(
    n = length(unique(participant.code))
  )

# How many people in each session?
table(df$session.code)
table(df$session)

temp <- 
  df %>%
  group_by(session) %>%
  summarise(
    n = length(unique(participant.code))
  )

# How many team A/B in each session?
temp <- 
df %>%
  group_by(session.code, treatment) %>%
  summarise(
    n = length(unique(participant.code))
  )

# Payoffs Histogram
payoffs_hist <- 
  ggplot(data=df, aes(df$participant.payoff)) +
  geom_histogram(breaks=seq(0, 12, by=.5),
                 aes(fill=treatment)) +
  scale_fill_manual(name="Team",
                    labels=c("A", "B"),
                    # values=c("#91cf60", "#fc8d59")) +
                    values=c("#7fbf7b", "#1f78b4")) +
  labs(title="Participants' Payoffs", 
       x="Payoffs in GBP, rounded to 50p",
       y="") +
  cleanup

print(payoffs_hist)
ggsave("images/payoffs_hist.png")
# Bimodal distribution

# How many people got less than GBP 5.00? 
nrow(df[ which(df$participant.payoff < 5) , 
         c("treatment", "participant.payoff") ]) # 78

# Who? -- ALL on Team B
temp <- df[ which(df$participant.payoff < 5) , 
            c("treatment", "participant.payoff") ]

# Who got more than 8? -- All but 2 Team A
temp <- df[ which(df$participant.payoff > 8) , 
            c("treatment", "participant.payoff") ]


### manipulation check ##################################

mean(df$difPayoffs)
sd(df$difPayoffs)

table(df$difPayoffs)
hist(df$difPayoffs)

table(df$whoGotHigherPayoff)

hist(df$unfair_manipulation_check)
hist(df$fairplay_manipulation_check)

df$failMP_fairplay
df$failMP_unfair

table(df$failMP)


#########################################################
# Check what they thought this was about
#########################################################

# df$whatAbout

# People who guessed it
# id 77
# 'Tricking into the sense that pub quizzes can be rigged, how showing fake 
# feedback can make people have a placebo effect, and will start believing 
# that they deserved more of a payment. Even I wanted to say that I deserve 
# more of a payment, but sadly, I would a absolute idiot to think like this.'

# id 151 
# I think the point is probably to see if the follow-up questions convinced 
# us that one of the questions was wrong (fake place). If not, then it's to 
# see how people feel about distinctions between random chance and fairness 

#########################################################
# Check how many believed in the false claim
#########################################################

### rigged ##############################################

rigged_hist <- 
  ggplot(data=subset(df, !is.na(df$rigged)),
         aes(x=rigged, 
             fill=falsefeedback)) +
  geom_bar(position="dodge") +
  scale_fill_manual(name="Who said it",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#7fbf7b", "#1f78b4")) +
  facet_grid(. ~ treatment) + 
  ggtitle("How likely do you think it is that any of the places in that quiz do not exist?", 
          subtitle = "NB: Question was added after 99 observations") +
  labs(# title="How likely do you think it is that any of the places in that quiz do not exist?", 
    x="",
    y="") +
  cleanup

print(rigged_hist)
ggsave("images/rigged_hist.png")

# See if the people who thought the false claim was true 
# and the people who thought the game was rigged are the
# same people
table(df$rigged, df$false_fact_too_hard2)

# Table -- belief in false
round(prop.table(table(df$treatment, df$false_fact_too_hard2),1),2)
round(prop.table(table(df$treatment, df$false_fact_too_hard2, 
                       df$falsefeedback),1),2)

round(prop.table(table(df$treatment, df$believe),1),2)





#########################################################
# Graphs for paper 
#########################################################

# NEW NEW NEW
# Create 2 graphs; one with a subset of players who got false 
# claims in the fairplay feedback; one with a subset of players 
# who got false claims in the un fair feedback
# Then putting them next to each other.
# (Doing this rather than one graph for all with facet_grid 
# because I can't figure out how to give each bar a different 
# label with facet_grid)

# https://www.datanovia.com/en/blog/how-to-change-ggplot-facet-labels/

#########################################################
# GoodRep
#########################################################

# New facet label names: 
labels_treatment <- c(A = "Advantaged", 
                      B = "Disadvantaged")

labels_falsefeedback <- c(fairplay = "Players who saw false claims in the 'fair play' feedback \n(and no false claims in the 'unfair' feedback)",
                          unfair = "Players who saw false claims in the 'unfair' feedback' \n(and no false claims in the 'fair play' feedback)")


# Part 1: subset of players who got false claims in the fairplay feedback

false_fairplay <-
  ggplot(subset(df_long, falsefeedback == "fairplay"), 
         aes(x=feedback ,
             y=representationRating,
             fill=falsefeedback
         )) + 
  # facet_grid(treatment ~ .,
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (false) \n'fair play' feedback", 
                            "unfair" = "rating the (correct) \n'unfair' feedback")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size=10 #, face="bold"
                                    ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_fairplay)
ggsave("images/false_fairplay.png")

# Part 2: subset of players who got false claims in the unfair feedback

false_unfair <-
  ggplot(subset(df_long, falsefeedback == "unfair"), 
         aes(x=feedback ,
             y=representationRating,
             fill=falsefeedback
         )) + 
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (correct) \n'fair play' feedback ", 
                            "unfair" = "rating the (false) \n'unfair' feedback")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_text(size=10),
        strip.text.x = element_text(size=10 #, face="bold"
        ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_unfair)
ggsave("images/false_unfair.png")

# Part 3: putting the 2 next to each other 

require(gridExtra)
goodRep <- grid.arrange(
  false_fairplay, 
  false_unfair, 
  ncol=2) 

# NB: Screenshotted this and saved it as goodRep.png
# NB: Find out how to save this using ggsave
# https://cran.r-project.org/web/packages/gridExtra/vignettes/arrangeGrob.html 


#########################################################
# ONE graph using facet_grid
#########################################################

# Problem with this: cannot find a way to relabel the x-axis labels 
# so that each bar gets a different label

caption <- "This question was part of three slider questions 
in which respondents were asked to rate how educated, 
how accurate, and how good a team representative both 
feedback authors were. Opinions were measured on a scale from 
0 (not at all) to 100 (very). The dotted line represents the mid-point 
of the scale (50)."

lying_not_lying_goodRep <-
  ggplot(subset(df_long), 
         aes(x=feedback ,
             y=representationRating,
             fill=falsefeedback
         )) + 
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black", 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd") #"steelblue"
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the \n 'fair play' person", 
                            "unfair" = "rating the \n 'unfair' person")) +
  scale_fill_discrete(name = "Rating", 
                      labels = c("'fair play person'", 
                                 "'unfair' person")) +
  theme(legend.position="top") + # top none
  theme(
    plot.title = element_text(hjust = 0.5), # center title
    plot.subtitle = element_text(hjust = 0.5), # size=8, 
    plot.caption=element_text(size=8, 
                              hjust=0, # position caption on the plot left side
                              margin=margin(t=15))) +
  theme(strip.text.x = element_text(size=10, face="bold"), 
        strip.text.y = element_text(size=10, face="bold", angle=0),
        strip.background = element_rect(colour="black", fill="WhiteSmoke"
  )) +
  labs(x="", 
       y="",
       subtitle = "'This person is a good representative of my team.' (0=no, 100=yes) \n",
       caption =  label_wrap_gen(170)(caption)) +
  ggtitle("Representation ratings by team and factual accuracy") + 
  theme(panel.spacing = unit(1.5, "lines")) + # more white space
  theme_hc() 

print(lying_not_lying_goodRep)
# ggsave("images/lying_not_lying_goodRep.png", 
#        dpi = 600, width = 9, height = 7)


#########################################################
# Accurate
#########################################################

# Part 1: subset of players who got false claims in the fairplay feedback

false_fairplay_accurate <-
  ggplot(subset(df_long, falsefeedback == "fairplay"), 
         aes(x=feedback ,
             y=accuracyRating,
             fill=falsefeedback
         )) + 
  # facet_grid(treatment ~ .,
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (false) \n'fair play' feedback", 
                            "unfair" = "rating the (correct) \n'unfair' feedback")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size=10 #, face="bold"
        ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_fairplay_accurate)
# ggsave("images/false_fairplay_accurate.png")

# Part 2: subset of players who got false claims in the unfair feedback

false_unfair_accurate <-
  ggplot(subset(df_long, falsefeedback == "unfair"), 
         aes(x=feedback ,
             y=accuracyRating,
             fill=falsefeedback
         )) + 
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (correct) \n'fair play' feedback", 
                            "unfair" = "rating the (false) \n'unfair' feedback")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_text(size=10),
        strip.text.x = element_text(size=10 #, face="bold"
        ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_unfair_accurate)
# ggsave("images/false_unfair_accurate.png")

# Part 3: putting the 2 next to each other 

require(gridExtra)
accurate <- grid.arrange(
  false_fairplay_accurate, 
  false_unfair_accurate, 
  ncol=2) 

# saved as accurate.png


#########################################################
# Accurate -- ONE graph using facet_grid
#########################################################

lying_not_lying_accurate <-
  ggplot(subset(df_long), 
         aes(x=feedback ,
             y=accuracyRating,
             fill=falsefeedback
         )) + 
  facet_grid(falsefeedback ~ treatment, 
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +
  coord_cartesian(ylim=c(0,100)) +
  geom_abline(slope=0, intercept=50,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black", 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  # scale_fill_manual(name="false feedback",
  #                   labels=c("'fair play' feedback", "'unfair' feedback"),
  #                   values=c("#eeccaa", "#8fb8bd")) +
  scale_x_discrete(labels=c("rating the (false) \n 'fair play' person", 
                            "rating the (correct) \n 'unfair' person",
                            "rating the (correct) \n 'fair play' person", 
                            "rating the (false) \n 'unfair' person")) +
  theme(legend.position="none") + # top
  labs(x="", # Feedback rated
       y="",
       caption =  label_wrap_gen(160)(caption),
       col = "Reference") +
   ggtitle("Accuracy ratings by team and factual accuracy",
           subtitle = "'The points this person makes are factually accurate.' (0=no, 100=yes) \n") + 
  # theme(strip.text.x = element_text(size=12, angle=90),
  #       strip.text.y = element_text(size=12, face="bold") ,
  #       strip.background = element_rect(colour="red", fill="#CCCCFF")
  # ) +
  theme_hc()+
  theme(
    plot.title = element_text(hjust = 0.5), # center title
    plot.subtitle = element_text(hjust = 0.5), # size=8, 
    plot.caption=element_text(size=8, 
                              hjust=0, # position caption on the plot left side
                              margin=margin(t=15))) +
  theme(strip.text.x = element_text(size=10), # face="bold"
        strip.text.y = element_text(size=10), # face="bold", angle=270
        strip.background = element_rect(fill="WhiteSmoke")) # colour="black", 

# print(lying_not_lying_accurate)
# ggsave("images/lying_not_lying_accurate.png", 
#        dpi = 600, width = 9, height = 7)




#########################################################
# Agreed 
#########################################################

# Part 1: subset of players who got false claims in the fairplay feedback

false_fairplay_agreed <-
  ggplot(subset(df_long, falsefeedback == "fairplay"), 
         aes(x=feedback ,
             y=agreeFeedback_num,
             fill=falsefeedback
         )) + 
  # facet_grid(treatment ~ .,
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,6)) +
  geom_abline(slope=0, intercept=3.5,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (false) \n 'fair play' person", 
                            "unfair" = "rating the (correct) \n 'unfair' person")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_blank(),
        strip.text.x = element_text(size=10 #, face="bold"
        ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_fairplay_agreed)
# ggsave("images/false_fairplay_agreed.png")

# Part 2: subset of players who got false claims in the unfair feedback

false_unfair_agreed <-
  ggplot(subset(df_long, falsefeedback == "unfair"), 
         aes(x=feedback ,
             y=agreeFeedback_num,
             fill=falsefeedback
         )) + 
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +  
  coord_cartesian(ylim=c(0,6)) +
  geom_abline(slope=0, intercept=3.5,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black" , 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd")) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the (correct) \n 'fair play' person", 
                            "unfair" = "rating the (false) \n 'unfair' person")) +
  labs(x="", 
       y="") +
  ggtitle("") + 
  theme_hc() +
  theme(strip.text.y = element_text(size=10),
        strip.text.x = element_text(size=10 #, face="bold"
        ), 
        strip.background = element_rect(fill="WhiteSmoke")) + # colour="black", 
  theme(panel.spacing = unit(1.5, "lines")) # more white space

print(false_unfair_agreed)
# ggsave("images/false_unfair_agreed.png")

# Part 3: putting the 2 next to each other 

require(gridExtra)
accurate <- grid.arrange(
  false_fairplay_agreed, 
  false_unfair_agreed, 
  ncol=2) 


#########################################################
# Agreed -- ONE graph using facet_grid
#########################################################

caption <- "Graph shows average answers to the first question  
respondents were asked about each feedback. 
Opinions were measured on a scale from 1 (strongly disagree) 
to 6 (strongly agree). 
The dotted line represents the mid-point of the scale (3.5)."

lying_not_lying_agreed <-
  ggplot(subset(df_long), 
         aes(x=feedback ,
             y=agreeFeedback_num,
             fill=falsefeedback
         )) + 
  facet_grid(treatment ~ falsefeedback,
             labeller = labeller(treatment = labels_treatment,
                                 falsefeedback = labels_falsefeedback)
  ) +
  coord_cartesian(ylim=c(0,6)) +
  geom_abline(slope=0, intercept=3.5,  col = "black", lty=2) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black", 
               fill=c("#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd",
                      "#eeccaa", "#8fb8bd") #"steelblue"
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "rating the \n 'fair play' person", 
                            "unfair" = "rating the \n 'unfair' person")) +
  scale_fill_discrete(name = "Rating", 
                      labels = c("'fair play person'", 
                                 "'unfair' person")) +
  theme(legend.position="top") + # top none
  theme(
    plot.title = element_text(hjust = 0.5), # center title
    plot.subtitle = element_text(hjust = 0.5), # size=8, 
    plot.caption=element_text(size=8, 
                              hjust=0, # position caption on the plot left side
                              margin=margin(t=15))) +
  theme(strip.text.x = element_text(size=10, face="bold"), 
        strip.text.y = element_text(size=10, face="bold", angle=0),
        strip.background = element_rect(colour="black", fill="WhiteSmoke"
        )) +
  ggtitle("General agreement by team and factual accuracy") + 
  labs(x="", 
       y="",
       subtitle = "'Generally speaking, do you agree with the author of this feedback?' (1='Strongly Disagree', 2='Disagree', 3='Slightly Disagree', 4='Slightly Agree', 5='Agree', 6='Strongly Agree) \n",
       caption =  label_wrap_gen(170)(caption)) +
  theme(panel.spacing = unit(1.5, "lines")) + # more white space
  theme_hc() 

print(lying_not_lying_agreed)
ggsave("images/lying_not_lying_agreed.png",
       dpi = 600, width = 9, height = 7)



#########################################################
# Belief in false claims
#########################################################

caption <- "To measure belief in the false claim that 'some 
of these places don't exist' respondents were asked to rate this
claim (along with three other claims) on a scale from 1 
(definitely false) to 4 (definitely false). 
The dotted line represents the mid-point of the scale (2.5). 
(Note though that a 'don't know' option was not provided.)
The top row represents the advantaged team (Team A); 
the bottom row represents the disadvantaged team (Team B). 
The left-hand column shows respondents who saw  
a 'fair play' feedback containing false claims (and a factually
accurate 'unfair' feedback). The right-hand column shows 
respondents who saw an 'unfair' feedback containing false claims
(and a factually accurate 'fair play' feedback)." 

#########################################################
# Belief in false claim 
#########################################################

# Is team B more likely to believe in the false claim when 
# it appears as part of the 'unfair' feedback? --> NO!

df %>%
  group_by(treatment, falsefeedback) %>%
  summarise(
    mean = mean(false_fact_too_hard2_num, na.rm=T))

# Have a look at the df_long dataset
names(df_long)
temp <- df_long[ , c(
  "treatment", 
  "feedback", 
  "falsefeedback", 
  
  "agreeFeedback", 
  "accuracyRating", 
  "representationRating",
  
  "true_fact_too_easy_num",
  "true_fact_too_hard_num",
  "true_fact_too_hard2_num",
  "true_fact_30sec",
  
  "false_fact_too_easy",
  "false_fact_too_hard_num",
  "false_fact_too_hard2_num",
  "grossExag30sec_num", 
  # "false_fact_30sec_unfair_num",
  # "false_fact_30sec_fairplay", 
  "id")  ]


H4_AB_rating_false_claim <-
  ggplot(df, # subset(df, 
  #               complete.cases(df$false_fact_too_hard2_num)), 
         aes(x=treatment,
             y=false_fact_too_hard2_num,
             fill=falsefeedback)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge", 
               colour="black") +
  scale_fill_manual(name = "Saw false claims in",
                    labels = c("'Fair play' feedback", 
                               "'Unfair' feedback"),
                    values=c('#eeccaa','#8fb8bd')) + 
  coord_cartesian(ylim=c(0,4)) +
  # geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Advantaged team", 
                            "B" = "Disadvantaged team")) +
  theme(legend.position="top") + # top
  labs(x="",
       y="") +
  ggtitle("'Some of these places don't even exist.'", 
          subtitle = "(1='Definitely False' 2='Probably False' 3='Probably True' 4='Definitely True')") +
  theme_hc() 

print(H4_AB_rating_false_claim)
ggsave("images/H4_AB_rating_false_claim.png",
       dpi = 600, width = 9, height = 7)



#########################################################
# Multiple plot function
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
#########################################################


#########################################################
# Graphs for Paper 
#########################################################

#########################################################
# NEW Hypothesis 1 (= OLD H2)
#########################################################

# Hypothesis 1 considers the hypothetical scenario in which 
# a person  who sides with your team either makes false 
# claims or doesn't: It is hypothesized that factual accuracy 
# has no effect on perceived suitability as a team 
# representative. 

#########################################################
# Team B - Rating the (correct & incorrect) 'unfair' feedback
#########################################################

H2_B_rating_unfair_agreed <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=agreeFeedback_unfair_num ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Agree.'") +
  cleanup

print(H2_B_rating_unfair_agreed)
# ggsave("images/H2_B_rating_unfair_agreed.png")


H2_B_rating_unfair_educated <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=educationRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Educated.'") +
  cleanup

print(H2_B_rating_unfair_educated)
# ggsave("images/H2_B_rating_unfair_educated.png")

H2_B_rating_unfair_accurate <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=accuracyRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Factually Accurate.'") +
  cleanup

print(H2_B_rating_unfair_accurate)
# ggsave("images/H2_B_rating_unfair_accurate.png")


H2_B_rating_unfair_goodRep <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=representationRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Good Representative.'") +
  cleanup

print(H2_B_rating_unfair_goodRep)
# ggsave("images/H2_B_rating_unfair_goodRep.png")

require(gridExtra)
H2_B_rating_unfair <- grid.arrange(
  H2_B_rating_unfair_agreed,
  H2_B_rating_unfair_educated,
  H2_B_rating_unfair_accurate,
  H2_B_rating_unfair_goodRep, 
  ncol=4) 

require(gridExtra)
H2_B_rating_unfair <- grid.arrange(
  H2_B_rating_unfair_agreed,
 # H2_B_rating_unfair_educated,
  H2_B_rating_unfair_accurate,
  H2_B_rating_unfair_goodRep, 
  ncol=3) 

#########################################################
# Team A - Rating the (correct & incorrect) 'fair play' feedback
#########################################################

H2_A_rating_fairplay_agreed <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=agreeFeedback_fairplay_num ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Agree.'") +
  cleanup

print(H2_A_rating_fairplay_agreed)
# ggsave("images/H2_A_rating_fairplay_agreed.png")

H2_A_rating_fairplay_educated <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=educationRating_fairplay ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Educated.'") +
  cleanup

print(H2_A_rating_fairplay_educated)
# ggsave("images/H2_A_rating_fairplay_educated.png")

H2_A_rating_fairplay_accurate <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=accuracyRating_fairplay ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Factually Accurate.'") +
  cleanup

print(H2_A_rating_fairplay_accurate)
# ggsave("images/H2_A_rating_fairplay_accurate.png")

H2_A_rating_fairplay_goodRep <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=representationRating_fairplay,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Good Representative.'") +
  cleanup

print(H2_A_rating_fairplay_goodRep)
# ggsave("images/H2_A_rating_fairplay_goodRep.png")

require(gridExtra)
H2_A_rating_fairplay <- grid.arrange(H2_A_rating_fairplay_agreed,
                                     H2_A_rating_fairplay_educated, 
                                     H2_A_rating_fairplay_accurate,
                                     H2_A_rating_fairplay_goodRep, 
                                     ncol=4) 

require(gridExtra)
H2_A_rating_fairplay <- grid.arrange(H2_A_rating_fairplay_agreed,
                                     # H2_A_rating_fairplay_educated, 
                                     H2_A_rating_fairplay_accurate,
                                     H2_A_rating_fairplay_goodRep, 
                                     ncol=3) 

#########################################################
# NEW Hypothesis 2 -- People DO notice false claims in the out-group
#########################################################

#########################################################
# Team B - Rating the (correct & incorrect) 'fair play' feedback
#########################################################

H2_B_rating_fairplay_agreed <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=agreeFeedback_fairplay_num ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Agree.'") +
  cleanup

print(H2_B_rating_fairplay_agreed)
# ggsave("images/H2_B_rating_fairplay_agreed.png")

H2_B_rating_fairplay_educated <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=educationRating_fairplay ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Educated.'") +
  cleanup

print(H2_B_rating_fairplay_educated)
# ggsave("images/H2_B_rating_fairplay_educated.png")

H2_B_rating_fairplay_accurate <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=accuracyRating_fairplay ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Factually Accurate.'") +
  cleanup

print(H2_B_rating_fairplay_accurate)
# ggsave("images/H2_B_rating_fairplay_accurate.png")


H2_B_rating_fairplay_goodRep <-
  ggplot(subset(df, treatment == "B"),
         aes(x=falsefeedback,
             y=representationRating_fairplay,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#eeccaa") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false claims", 
                            "unfair" = "no false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Good Representative.'") +
  cleanup

print(H2_B_rating_fairplay_goodRep)
# ggsave("images/H2_B_rating_fairplay_goodRep.png")

require(gridExtra)
H2_B_rating_fairplay <- grid.arrange(H2_B_rating_fairplay_agreed,
                                     H2_B_rating_fairplay_educated,
                                     H2_B_rating_fairplay_accurate,
                                     H2_B_rating_fairplay_goodRep,
                                     ncol=4) 

H2_B_rating_fairplay <- grid.arrange(H2_B_rating_fairplay_agreed,
                                     # H2_B_rating_fairplay_educated,
                                     H2_B_rating_fairplay_accurate,
                                     H2_B_rating_fairplay_goodRep,
                                     ncol=3) 

#########################################################
#  Team A - Rating the (correct & incorrect) 'unfair' feedback
#########################################################

H2_A_rating_unfair_agreed <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=agreeFeedback_unfair_num ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Agree.'") +
  cleanup

print(H2_A_rating_unfair_agreed)
# ggsave("images/H2_A_rating_unfair_agreed.png")

H2_A_rating_unfair_educated <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=educationRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Educated.'") +
  cleanup

print(H2_A_rating_unfair_educated)
# ggsave("images/H2_A_rating_unfair_educated.png")

H2_A_rating_unfair_accurate <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=accuracyRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Factually Accurate.'") +
  cleanup

print(H2_A_rating_unfair_accurate)
# ggsave("images/H2_A_rating_unfair_accurate.png")

H2_A_rating_unfair_goodRep <-
  ggplot(subset(df, treatment == "A"),
         aes(x=falsefeedback,
             y=representationRating_unfair ,
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#8fb8bd") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                            "unfair" = "false claims")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'Good Representative.'") +
  cleanup

print(H2_A_rating_unfair_goodRep)
# ggsave("images/H2_A_rating_unfair_goodRep.png")

require(gridExtra)
H2_A_rating_unfair <- grid.arrange(H2_A_rating_unfair_agreed, 
                                   H2_A_rating_unfair_educated, 
                                   H2_A_rating_unfair_accurate,
                                   H2_A_rating_unfair_goodRep, 
                                   ncol=4)


#########################################################
# NEW Hypothesis 3
#########################################################

# Team B members who are exposed to factually incorrect 
# claims in the 'unfair' feedback and to factually correct 
# claims in the 'fair play' feedback (group 1) are more 
# supportive of the (factually inaccurate) 'unfair' feedback 
# as compared with the (factually accurate) 'fair play' 
# feedback.

#########################################################
# Team B -- false unfair -- Agree? 
#########################################################

H3_B_false_unfair_agreed <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=agreeFeedback_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Agree.'") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_unfair_agreed) 
# ggsave("images/H3_B_false_unfair_agreed.png") 


#########################################################
# Team B -- false fair play -- Agree? 
#########################################################

H3_B_false_fairplay_agreed <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=agreeFeedback_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Generally speaking, do you agree with the author of this feedback?'",
          subtitle = "(1='Strongly Disagree', 2='Disagree', 3='Slightly Disagree', 4='Slightly Agree', 5='Agree', 6='Strongly Agree')") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_agreed) 
# ggsave("images/H3_B_false_fairplay_agreed.png") 


#########################################################
# Team B -- false unfair -- educated, accurate, good rep
#########################################################

H3_B_false_unfair_educated <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=educationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Educated.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Well educated.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_unfair_educated)
# ggsave("images/H3_B_false_unfair_educated.png")


H3_B_false_unfair_accurate <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  #coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Factually accurate.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Factually accurate.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_unfair_accurate)
# ggsave("images/H3_B_false_unfair_accurate.png")

H3_B_false_unfair_goodRep <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Good representative.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Good representative.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_unfair_goodRep)
# ggsave("images/H3_B_false_unfair_goodRep.png")


#########################################################
# Team B -- false unfair -- false claim
#########################################################

H3_B_false_unfair_falseclaim <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=false_fact_too_hard2_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Some of these places don't even exist.'", # \n
          subtitle = "(1='Definitely False' 2='Probably False' 3='Probably True' 4='Definitely True')") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_unfair_falseclaim)
# ggsave("images/H3_B_false_unfair_falseclaim.png")


#########################################################
# H3 -- Slider Qs 3 in 1 -- SCREENSHOT -- USE THIS
#########################################################

# Team B Players who saw an 'unfair' feedback that contained 
# false claims and a 'fair play' feedback that was factually 
# accurate

require(gridExtra)
H3_B_false_unfair <- grid.arrange(H3_B_false_unfair_accurate,
                                  H3_B_false_unfair_goodRep,
                                  ncol=2) 

require(gridExtra)
H3_B_false_unfair <- grid.arrange(H3_B_false_unfair_educated, 
                                  H3_B_false_unfair_accurate,
                                  H3_B_false_unfair_goodRep,
                                  ncol=3) 

H3_B_false_unfair <- grid.arrange(H3_B_false_unfair_agreed,
                                  H3_B_false_unfair_educated, 
                                  H3_B_false_unfair_accurate,
                                  H3_B_false_unfair_goodRep,
                                  ncol=4) 

# print(TeamB_unfair_sliderQs)
# ggsave("images/TeamB_unfair_sliderQs.png") # NOT WORKING; SCREENSHOT!!

# multiplot(TeamB_unfair_educated, TeamB_unfair_accurate, TeamB_unfair_goodRep, 
#           cols=3)







#########################################################
# EXETER SLIDES -- ZOOMING IN ON GROUPS 3 & 4
#########################################################

#########################################################
# ACCURATE
#########################################################

H3_B_false_unfair_accurate <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("(correct) 'fair play'", 
                             "(false) 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") + 
  ggtitle("") + 
  theme(legend.position = "none") +
  ggtitle("False claims in 'unfair' feedback\n") + #, # Disadvantaged // 
         # subtitle = "'The points this person makes are factually accurate.'") +
  cleanup

print(H3_B_false_unfair_accurate)
ggsave("images/H3_B_false_unfair_accurate.png")


H3_B_false_fairplay_accurate <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("(false) 'fair play'", 
                             "(correct) 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") + 
  ggtitle("False claims in 'fair play' feedback\n") + #Disadvantaged //
          # subtitle = "'The points this person makes are factually accurate.'") + 
  # ggtitle("") + 
  cleanup

print(H3_B_false_fairplay_accurate)
ggsave("images/H3_B_false_fairplay_accurate.png")

require(gridExtra)
groups3_4_accurate <- grid.arrange(H3_B_false_fairplay_accurate,
                                   H3_B_false_unfair_accurate,
                                  ncol=2) 

#########################################################
# GOOD REP
#########################################################

H3_B_false_unfair_goodRep <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("(correct) 'fair play'", 
                             "(false) 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="none") +
  labs(x="",
       y="") + 
  ggtitle("False claims in 'unfair' feedback\n") + # #, #Disadvantaged //
          #subtitle = "'This person is a good representative of my team.'"
  #) +
  cleanup

print(H3_B_false_unfair_goodRep)
ggsave("images/H3_B_false_unfair_goodRep.png")


H3_B_false_fairplay_goodRep <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("(false) 'fair play'", 
                             "(correct) 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="none") +
  labs(x="",
       y="") + 
  ggtitle("False claims in 'fair play' feedback\n") + # #, #Disadvantaged //
          # subtitle = "'This person is a good representative of my team.'"
  # ) +
  cleanup

print(H3_B_false_fairplay_goodRep)
ggsave("images/H3_B_false_fairplay_goodRep.png")

require(gridExtra)
groups3_4_goodRep <- grid.arrange(H3_B_false_fairplay_goodRep,
                                  H3_B_false_unfair_goodRep,
                                  ncol=2) 


#########################################################
# Belief in false claims -- BAR CHARTS
#########################################################

groups3_4_belief <-
  ggplot(subset(df_long, 
                 treatment == "B" & 
                  complete.cases(df_long$false_fact_too_hard2)),
         aes(x=falsefeedback,
             y=false_fact_too_hard2_num
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black", 
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  labs(x="",
       y="") +
  ggtitle("'Some of these places don't even exist'", # \n
          subtitle = "(1=Definitely False; 2=Probably False; 2=Probably True; 4=Definitely True)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(groups3_4_belief)
ggsave("images/groups3_4_belief.png")


#########################################################
# Belief in false claims -- HISTOGRAMS
#########################################################

df_long$false_fact_too_hard2 <- factor(df$false_fact_too_hard2, 
                                  levels = c("Definitely False",
                                             "Probably False",
                                             "Probably True", 
                                             "Definitely True"))


distribution_belief_false_claim_B <- 
  ggplot(subset(df, treatment == "B" &
                complete.cases(df$false_fact_too_hard2)),
         aes(false_fact_too_hard2,
             fill = falsefeedback)) +
  geom_bar() + 
  # geom_histogram() + 
  labs(x="",
       y="") + 
  ggtitle("Disadvantaged team: 'Some of these places don't even exist'",
          subtitle = "0=no; 100=yes") +
  # facet_grid(falsefeedback ~ .) + 
  facet_grid(. ~ falsefeedback ) + 
  scale_fill_manual(name="False claims in",
                    labels=c("Fair play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
                    # values=c("#7fbf7b", "#1f78b4")) + # green blue
  # values=c("#39ac73", "#e6004c")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  cleanup

print(distribution_belief_false_claim_B)
ggsave("images/distribution_belief_false_claim_B.png")


distribution_belief_false_claim_A <- 
  ggplot(subset(df, treatment == "A" &
                  complete.cases(df$false_fact_too_hard2)),
         aes(false_fact_too_hard2,
             fill = falsefeedback)) +
  geom_bar() + 
  # geom_histogram() + 
  labs(x="",
       y="") + 
  ggtitle("Advantaged team: 'Some of these places don't even exist'",
          subtitle = "0=no; 100=yes") +
  # facet_grid(falsefeedback ~ .) + 
  facet_grid(. ~ falsefeedback ) + 
  scale_fill_manual(name="False claims in",
                    labels=c("Fair play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # values=c("#7fbf7b", "#1f78b4")) + # green blue
  # values=c("#39ac73", "#e6004c")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  cleanup

print(distribution_belief_false_claim_A)
ggsave("images/distribution_belief_false_claim_A.png")


#########################################################
# Belief in gross exxaggeration -- HISTOGRAMS
#########################################################

distribution_belief_false_claim_B <- 
  ggplot(subset(df, treatment == "B" &
                  complete.cases(df$false_fact_30sec_unfair)),
         aes(false_fact_30sec_unfair,
             fill = falsefeedback)) +
  geom_bar() + 
  # geom_histogram() + 
  labs(x="",
       y="") + 
  ggtitle("Disadvantaged team: 'And the 30 seconds we had was barely enough time to read the questions.'",
          subtitle = "0=no; 100=yes") +
  # facet_grid(falsefeedback ~ .) + 
  facet_grid(. ~ falsefeedback ) + 
  scale_fill_manual(name="False claims in",
                    labels=c("Fair play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # values=c("#7fbf7b", "#1f78b4")) + # green blue
  # values=c("#39ac73", "#e6004c")) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  cleanup

print(distribution_30secNoTimeToRead_B)
ggsave("images/distribution_30secNoTimeToRead_B.png")


distribution_30secNoTimeToRead <- 
  ggplot(df, #subset(df, complete.cases(df$false_fact_30sec_unfair)), 
         aes(
           x=false_fact_30sec_unfair,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           # width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6)) + 
  scale_fill_manual(name="False Feedback in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#b1cbbb", "#c94c4c")) +
  xlab("") +
  ylab("") +
  ggtitle("How True Or False?", # \n
          subtitle = "'And the 30 seconds we had was barely enough time to read the questions.'") +
  cleanup

print(distribution_30secNoTimeToRead)
ggsave("images/distribution_30secNoTimeToRead.png")


### BEGIN NOT NEEDED GRAPHS #############################

#########################################################
# MORE HYPOTHESES -- Team B -- false fair play -- agreed
#########################################################

# How those on Team B who see false facts on the other side 
# rate the (incorrect) fair play and the (correct) 'unfair' feedback?

H3_B_false_fairplay_agreed_short <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=agreeFeedback_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Agree.'") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_agreed_short) 
# ggsave("images/H3_B_false_fairplay_agreed_short.png") 

#########################################################
# Team B -- false fairplay -- educated, accurate, good rep
#########################################################

H3_B_false_fairplay_educated <- 
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=educationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Educated.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Well educated.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_educated)
# ggsave("images/H3_B_false_fairplay_educated.png")

H3_B_false_fairplay_accurate <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Factually accurate.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Factually accurate.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_accurate)
# ggsave("images/H3_B_false_fairplay_accurate.png")

H3_B_false_fairplay_goodRep <- 
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Good representative.'") +
  # ggtitle("Team B // false 'unfair' feedback", # \n
  #         subtitle = "'Good representative.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_goodRep)
# ggsave("images/H3_B_false_fairplay_goodRep.png")

H3_B_false_fairplay_falseclaim <-
  ggplot(subset(df_long, treatment == "B" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=false_fact_too_hard2_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Team B // false 'unfair' feedback", # \n
          subtitle = "'Some of these places don't even exist.' \n (1='Definitely False' 2='Probably False' 3='Probably True' 4='Definitely True')") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_B_false_fairplay_falseclaim)
# ggsave("images/H3_B_false_fairplay_falseclaim.png")

# print(H3_falsefact_FP)
# ggsave("images/H3_falsefact_FP.png")

# grossExag30sec_num
# 30 sec was not even enough time to read the question.

#########################################################
# Slider Qs 3 in 1 -- SCREENSHOT -- USE THIS
#########################################################

# Team B Players who saw an 'unfair' feedback that contained 
# false claims and a 'fair play' feedback that was factually 
# accurate

require(gridExtra)
H3_B_false_fairplay <- grid.arrange(H3_B_false_fairplay_educated, 
                                    H3_B_false_fairplay_accurate,
                                    H3_B_false_fairplay_goodRep, 
                                    ncol=3)

H3_B_false_fairplay <- grid.arrange(H3_B_false_fairplay_agreed_short,
                                  H3_B_false_fairplay_educated, 
                                  H3_B_false_fairplay_accurate,
                                  H3_B_false_fairplay_goodRep,
                                  # H3_B_false_unfair_falseclaim_short,
                                  ncol=4)

#########################################################
# Team A -- false fairplay
#########################################################

H3_A_false_fairplay_agreed <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=agreeFeedback_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Agree.'") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_fairplay_agreed) 
# ggsave("images/H3_A_false_fairplay_agreed.png") 


#########################################################
# Team A -- false fairplay -- educated, accurate, good rep
#########################################################

H3_A_false_fairplay_educated <- 
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=educationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Educated.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Well educated.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_fairplay_educated)
# ggsave("images/H3_A_false_fairplay_educated.png")

H3_A_false_fairplay_accurate <- 
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Factually accurate.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Factually accurate.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_fairplay_accurate)
# ggsave("images/H3_A_false_fairplay_accurate.png")

H3_A_false_fairplay_goodRep <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Good representative.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Food representative.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_fairplay_goodRep)
# ggsave("images/H3_A_false_fairplay_goodRep.png")

H3_A_false_fairplay_falseclaim <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "fairplay"), 
         aes(x=feedback, 
             y=false_fact_too_hard2_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Team A // false 'fair play' feedback", # \n
          subtitle = "'Some of these places don't even exist.' \n (1='Definitely False' 2='Probably False' 3='Probably True' 4='Definitely True')") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_fairplay_falseclaim)
# ggsave("images/H3_A_false_fairplay_falseclaim.png")



#########################################################
# Slider Qs 3 in 1
#########################################################

# Team A Players who saw a 'fair play' feedback that contained 
# false claims and an 'unfair' feedback that was factually 
# accurate

require(gridExtra)
H3_A_false_fairplay <- grid.arrange(H3_A_false_fairplay_agreed,
                                  H3_A_false_fairplay_educated, 
                                  H3_A_false_fairplay_accurate,
                                  H3_A_false_fairplay_goodRep,
                                  ncol=4) 



#########################################################
# Team A -- false unfair
#########################################################

H3_A_false_unfair_agreed <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=agreeFeedback_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Agree.'") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_unfair_agreed) 
# ggsave("images/H3_A_false_unfair_agreed.png") 

H3_A_false_unfair_educated <- 
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=educationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Educated.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Well educated.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_unfair_educated)
# ggsave("images/H3_A_false_unfair_educated.png")


H3_A_false_unfair_accurate <- 
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=accuracyRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Factually accurate.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Factually accurate.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_unfair_accurate)
# ggsave("images/H3_A_false_unfair_accurate.png")


H3_A_false_unfair_goodRep <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=representationRating , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("'Good representative.'") +
  # ggtitle("Team A // false 'fair play' feedback", # \n
  #         subtitle = "'Food representative.' (0=no, 100=yes)") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_unfair_goodRep)
# ggsave("images/H3_A_false_unfair_goodRep.png")


H3_A_false_unfair_falseclaim <-
  ggplot(subset(df_long, treatment == "A" & falsefeedback == "unfair"), 
         aes(x=feedback, 
             y=false_fact_too_hard2_num , 
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("rating 'fair play'", 
                             "rating 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Team A // false 'unfair' feedback", # \n
          subtitle = "'Some of these places don't even exist.' \n (1='Definitely False' 2='Probably False' 3='Probably True' 4='Definitely True')") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H3_A_false_unfair_falseclaim)
# ggsave("images/H3_A_false_unfair_falseclaim.png")


#########################################################
# Slider Qs 3 in 1
#########################################################

# Team A Players who saw a 'fair play' feedback that contained 
# false claims and an 'unfair' feedback that was factually 
# accurate

require(gridExtra)
H3_A_false_unfair <- grid.arrange(H3_A_false_unfair_agreed,
                                  H3_A_false_unfair_educated, 
                                  H3_A_false_unfair_accurate,
                                  H3_A_false_unfair_goodRep,
                                  ncol=4) 


### BEGIN NOT NEEDED GRAPHS ###############################

#########################################################
# OLD H1 -- NEW H4
#########################################################

# Hypothesis 4 considers a scenario in which false claims 
# circulate on both sides.  

# ONE GRAPH ONLY -- USE TO SUMMARIZE DATA FOR ISPP

# Looking at cases in which the feedback == falsefeedback,
# i.e. only one row per person: the one where they rate the 
# false feedback

H4_B_falsefeedback_accurate_only <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                treatment == "B"
  ),
  aes(x=feedback,
      y=accuracyRating
  )) +
  # facet_grid(. ~ treatment) +
  # facet_grid(treatment ~ .) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")
  ) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'The points this person makes are factually accurate.' (0=No; 100=Yes)") +
  cleanup

print(H4_B_falsefeedback_accurate_only)
ggsave("images/H4_B_falsefeedback_accurate_only.png")

labels <- c(A = "Advantaged", B = "Disadvantaged")

H4_AB_falsefeedback_accurate_only <-
  ggplot(subset(df_long, 
                feedback == falsefeedback # & 
                # treatment == "B"
                  ),
         aes(x=feedback,
             y=accuracyRating
             )) +
  facet_grid(. ~ treatment,
             labeller=labeller(treatment = labels)) +
  # facet_grid(treatment ~ .) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd", "#eeccaa", "#8fb8bd")
               ) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("'The points this person makes are factually accurate.' \n(0=No; 100=Yes)") +
  cleanup

# USE THIS FOR ISPP

print(H4_AB_falsefeedback_accurate_only)
ggsave("images/H4_AB_falsefeedback_accurate_only.png")



#########################################################
# Team B - false facts on both sides
#########################################################

H4_B_falsefeedback_agreed <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "B" ),
         aes(x=feedback,
             y=agreeFeedback_num)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Agree") +
  cleanup

print(H4_B_falsefeedback_agreed)
# ggsave("images/H4_B_falsefeedback_agreed.png")

H4_B_falsefeedback_educated <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "B"),
         aes(x=feedback,
             y=educationRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Educated") +
  cleanup

print(H4_B_falsefeedback_educated)
# ggsave("images/H4_B_falsefeedback_educated.png")


H4_B_falsefeedback_accurate <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "B"),
         aes(x=feedback,
             y=accuracyRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Accurate") +
  cleanup

print(H4_B_falsefeedback_accurate)
# ggsave("images/H4_B_falsefeedback_accurate.png")

H4_B_falsefeedback_goodRep <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "B"),
         aes(x=feedback,
             y=representationRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Good Representative") +
  cleanup

print(H4_B_falsefeedback_goodRep)
# ggsave("images/H4_B_falsefeedback_goodRep.png")

H4_B_falsefeedback_noExist <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "B"),
         aes(x=feedback,
             y=false_fact_too_hard2_num)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H4_B_falsefeedback_noExist)
# ggsave("images/H4_B_falsefeedback_noExist.png")

require(gridExtra)
H4_B_falsefeedback <- grid.arrange(
  H4_B_falsefeedback_agreed,
  H4_B_falsefeedback_accurate,
  H4_B_falsefeedback_goodRep,
  H4_B_falsefeedback_noExist, 
  ncol=4) 

# Screenshotted and saved as H4_B_falsefeedback.png

require(gridExtra)
H4_B_falsefeedback <- grid.arrange(
  # H4_B_falsefeedback_agreed,
  H4_B_falsefeedback_accurate,
  H4_B_falsefeedback_goodRep,
  H4_B_falsefeedback_noExist, 
  ncol=3) 


#########################################################
# Team A - false facts on both sides
#########################################################

H4_A_falsefeedback_agreed <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "A"),
         aes(x=feedback,
             y=agreeFeedback_num)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Agree") +
  cleanup

print(H4_A_falsefeedback_agreed)
# ggsave("images/H4_A_falsefeedback_agreed.png")

H4_A_falsefeedback_educated <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "A"),
         aes(x=feedback,
             y=educationRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Educated") +
  cleanup

print(H4_A_falsefeedback_educated)
# ggsave("images/H4_A_falsefeedback_educated.png")


H4_A_falsefeedback_accurate <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "A"),
         aes(x=feedback,
             y=accuracyRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Accurate") +
  cleanup

print(H4_A_falsefeedback_accurate)
# ggsave("images/H4_A_falsefeedback_accurate.png")

H4_A_falsefeedback_goodRep <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "A"),
         aes(x=feedback,
             y=representationRating)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Good Representative") +
  cleanup

print(H4_A_falsefeedback_goodRep)
# ggsave("images/H4_A_falsefeedback_goodRep.png")

H4_A_falsefeedback_noExist <-
  ggplot(subset(df_long, 
                feedback == falsefeedback & 
                  treatment == "A"),
         aes(x=feedback,
             y=false_fact_too_hard2_num)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=c("#eeccaa", "#8fb8bd")) +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "false fair play", 
                            "unfair" = "false unfair")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H4_A_falsefeedback_noExist)
# ggsave("images/H4_A_falsefeedback_noExist.png")

require(gridExtra)
H4_A_falsefeedback <- grid.arrange(
  H4_A_falsefeedback_agreed,
  H4_A_falsefeedback_accurate,
  H4_A_falsefeedback_goodRep,
  H4_A_falsefeedback_noExist, 
  ncol=4) 

# Screenshotted and saved as H1_A_falsefeedback.png

#########################################################
# BOTH TEAMS -- false facts on both sides
#########################################################

### agreement with false claim ##########################

H4_falsefeedback_false_claim <-
  ggplot(subset(df_long, feedback == falsefeedback), # treatment == "B" &
         aes(x=treatment,
             y=false_fact_too_hard2_num,
             fill=feedback
         )) + # which feedback they are rating
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(1,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="",
                    labels=c("inaccurate fair play feedback",
                             "inaccurate unfair feedback"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="Team",
       y="") +
  ggtitle("Rating the feedback that contained false claims", # \n
          subtitle = "'Some of these places don't exist") +
  # facet_grid(. ~ treatment) +  # vertical ~ horizontal
  cleanup

print(H4_falsefeedback_false_claim)
ggsave("images/H4_falsefeedback_false_claim.png")


#########################################################
# Slider Qs 3 in 1 
#########################################################

H4_falsefeedback_educated_short <-
  ggplot(subset(df_long, feedback == falsefeedback), # treatment == "B" &
         aes(x=treatment, 
             y=educationRating, 
             fill=feedback
         )) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("false 'fair play'", 
                             "false 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Educated.") +
  cleanup

print(H4_falsefeedback_educated_short)
# ggsave("images/H4_falsefeedback_educated_short.png")

H4_falsefeedback_accurate_short <-
  ggplot(subset(df_long, feedback == falsefeedback), # treatment == "B" &
         aes(x=treatment, 
             y=accuracyRating, 
             fill=feedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("false 'fair play'", 
                             "false 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Factually Accurate.") + 
  cleanup

print(H4_falsefeedback_accurate_short)
# ggsave("images/H4_falsefeedback_accurate_short.png")


H4_falsefeedback_goodRep_short <-
  ggplot(subset(df_long, feedback == falsefeedback), 
         aes(x=treatment, 
             y=representationRating, 
             fill=feedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="",
                    labels=c("false 'fair play'", 
                             "false 'unfair'"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") +
  labs(x="",
       y="") + 
  ggtitle("Good Representative.") +
  cleanup

print(H4_falsefeedback_goodRep_short)
# ggsave("images/H4_falsefeedback_goodRep_short.png")

require(gridExtra)
H4_AB_rating_false_feedback <- 
  grid.arrange(H4_falsefeedback_educated_short, 
               H4_falsefeedback_accurate_short, 
               H4_falsefeedback_goodRep_short, 
               ncol=3) 


#########################################################
# OLD H4 -- NEW H5 -- Team A and Team B rating the false unfair feedback
#########################################################

# H5: False facts in the ‘unfair’ treatment are more likely to 
# go unnoticed among the disadvantaged team than the advantaged team.
# [→ The disadvantaged Team (B) (group 1) is more likely than 
# the advantaged Team (A) (group 2) to overlook false facts in 
# the ‘unfair’ treatment. ]

# Subset data with only ratings of false feedback 
# -- NO NEED, just subset using feedback == falsefeedback
df_false <- df_long[ which(df_long$treatment=='A' 
                           & df_long$feedback == 'fairplay' | 
                             df_long$treatment=='B' 
                           & df_long$feedback == 'unfair'
), ]


H5_rating_false_unfair_agreed <-
  ggplot(subset(df, falsefeedback == "unfair"),
         aes(x=treatment,
             y=agreeFeedback_unfair_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Agree") +
  cleanup

print(H5_rating_false_unfair_agreed)
# ggsave("images/H5_rating_false_unfair_agreed.png")

H5_rating_false_unfair_educated <-
  ggplot(subset(df, falsefeedback == "unfair"),
         aes(x=treatment,
             y=educationRating_unfair)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Educated") +
  cleanup

print(H5_rating_false_unfair_educated)
# ggsave("images/H5_rating_false_unfair_educated.png")

H5_rating_false_unfair_accurate <-
  ggplot(subset(df, falsefeedback == "unfair"),
         aes(x=treatment,
             y=accuracyRating_unfair)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Accurate") +
  cleanup

print(H5_rating_false_unfair_accurate)
# ggsave("images/H5_rating_false_unfair_accurate.png")


H5_rating_false_unfair_goodRep <-
  ggplot(subset(df, falsefeedback == "unfair"),
         aes(x=treatment,
             y=representationRating_unfair)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Good Representative") +
  cleanup

print(H5_rating_false_unfair_goodRep)
# ggsave("images/H5_rating_false_unfair_goodRep.png")

H5_rating_false_unfair_noExist <-
  ggplot(subset(df, falsefeedback == "unfair"),
         aes(x=treatment,
             y=false_fact_too_hard2_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H5_rating_false_unfair_noExist)
# ggsave("images/H5_rating_false_unfair_noExist.png")

# same thing using df_long
H5_rating_false_unfair_noExist <-
  ggplot(subset(df_long, feedback == "unfair" & 
                  falsefeedback == "unfair"),
         aes(x=treatment,
             y=false_fact_too_hard2_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H5_rating_false_unfair_noExist)
# ggsave("images/H5_rating_false_unfair_noExist.png")


require(gridExtra)
H5_AB_rating_unfair <- 
  grid.arrange(H5_rating_false_unfair_agreed,
               H5_rating_false_unfair_educated, 
               H5_rating_false_unfair_accurate, 
               H5_rating_false_unfair_goodRep, 
               ncol=4) 

# Screenshotted and saved as H5_AB_rating_unfair.png

#########################################################
# Hypothesis 5b -- Team A and Team B rating the false fairplay feedback
#########################################################

# H5b: False facts in the ‘fair play’ treatment are more likely to 
# go unnoticed among the advantaged team than the disadvantaged team

# [→ The advantaged Team (A) (group 4) is more likely than 
# the disadvantaged Team (B) (group 3) to overlook false facts in 
# the ‘fair play’ treatment. ] -- H9 in the pre-registration

H5_rating_false_fairplay_agreed <-
  ggplot(subset(df, falsefeedback == "fairplay"),
         aes(x=treatment,
             y=agreeFeedback_fairplay_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,6)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Agree") +
  cleanup

print(H5_rating_false_fairplay_agreed)
# ggsave("images/H5_rating_false_fairplay_agreed.png")

H5_rating_false_fairplay_educated <-
  ggplot(subset(df, falsefeedback == "fairplay"),
         aes(x=treatment,
             y=educationRating_fairplay)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Educated") +
  cleanup

print(H5_rating_false_fairplay_educated)
# ggsave("images/H5_rating_false_fairplay_educated.png")

H5_rating_false_fairplay_accurate <-
  ggplot(subset(df, falsefeedback == "fairplay"),
         aes(x=treatment,
             y=accuracyRating_fairplay)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Accurate") +
  cleanup

print(H5_rating_false_fairplay_accurate)
# ggsave("images/H5_rating_false_fairplay_accurate.png")


H5_rating_false_fairplay_goodRep <-
  ggplot(subset(df, falsefeedback == "fairplay"),
         aes(x=treatment,
             y=representationRating_fairplay)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Good Representative") +
  cleanup

print(H5_rating_false_fairplay_goodRep)
# ggsave("images/H5_rating_false_fairplay_goodRep.png")


H5_rating_false_fairplay_noExist <-
  ggplot(subset(df, falsefeedback == "fairplay"),
         aes(x=treatment,
             y=false_fact_too_hard2_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H5_rating_false_fairplay_noExist)
# ggsave("images/H5_rating_false_fairplay_noExist.png")

H5_rating_false_fairplay_noExist <-
  ggplot(subset(df_long, feedback == "fairplay" & 
                  falsefeedback == "fairplay"),
         aes(x=treatment,
             y=false_fact_too_hard2_num)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill="#DCDCDC") +
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("A" = "Team A", 
                            "B" = "Team B")) +
  theme(legend.position="none") + # top
  labs(x="",
       y="") +
  ggtitle("Don't Exist") +
  cleanup

print(H5_rating_false_fairplay_noExist)
# ggsave("images/H5_rating_false_fairplay_noExist.png")


require(gridExtra)
H5_AB_rating_fairplay <- 
  grid.arrange(H5_rating_false_fairplay_agreed,
               # H5_rating_false_fairplay_educated, 
               H5_rating_false_fairplay_accurate, 
               H5_rating_false_fairplay_goodRep,
               H5_rating_false_fairplay_noExist,
               ncol=4) 

# Screenshotted and saved as H5_AB_rating_fairplay.png

# NB: Kicked out H4 and H5.

### END NOT NEEDED GRAPHS ###############################



#########################################################
# Social Identity stuff
#########################################################

# How likely do people think it is that Team B will get their
# payoffs topped up? 
table(df$feedbackMakesDifference, df$treatment) # Somewhat Likely # Very Likely

round(prop.table(table(df$treatment, df$feedbackMakesDifference),1),2)

# 50+ ratings
plot(df$fair)

# the outraged
# df$angry # booh

# Social Identity Theory: Those on team B who thought their status
# differences were illegitimate & could be changed

# LOOK AT -- those who thought payoff differences were illegitimate
# AND status differences unstable (or at least not totally unlikely)

outraged <-
  ggplot(subset(df, df$legitTeamAhigherPayoffs == "No" # & 
                  # df$feedbackMakesDifference != "Very Unlikely"
  ),
  aes(x=treatment,
      y=false_fact_too_hard2_num,
      fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Some of the places in that quiz don’t even exist.'", # \n
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  # ggtitle("How True Or False?", # \n
  #         subtitle = "'Some of the places in that quiz don’t even exist. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  cleanup



#########################################################
# DISTRIBUTION -- 4 Bullet points 
#########################################################

# bullet_points_true_facts = [
#   'The questions about European capitals were quite easy.', # 1) TRUE
#   'Some of the questions about state capitals or provincial capitals were too difficult.', # 2) TRUE
#   '''For example, ‘Concord is the capital of which U.S. state?’ People are very unlikely to know that unless they are American.''', # 3) TRUE
#   'And the 30 seconds were not a whole lot of time but enough to read the questions and answer them if you knew the answer.', # 4) TRUE
#   ]

# bullet_points_false_facts_unfair = [
#   '''One of the first questions was 'What is the capital city of Germany'. That was pretty easy.''', # 1) TRUE
#   'Too hard: How to get from one place you have never heard of to another place you have never heard of.', # 2) TRUE
#   '''Some of the places in that quiz don’t even exist.''', # 3) FALSE
#   'And the 30 seconds we had was barely enough time to read the questions.' # GROSS EXXAGGERATION
#   ]
# 'And 30 seconds was more than enough time to answer these questions: 10 seconds would have been plenty.' # GROSS EXXAGGERATION


#########################################################
# Distribution -- false feedback
#########################################################

# 1 -- Any questions too easy?
distribution_false_fact_too_easy <- 
  ggplot(subset(df, 
                complete.cases(df$false_fact_too_easy)), 
         aes(
           x=false_fact_too_easy,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           # width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=35, vjust=0.6)) + 
  scale_fill_manual(name="Who said it",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#b1cbbb", "#c94c4c")) +
  xlab("") +
  ylab("") +
  ggtitle("'One of the first questions was 'What is the capital city of Germany'. That was pretty easy.'") +
  cleanup

print(distribution_false_fact_too_easy)
ggsave("images/distribution_false_fact_too_easy.png")


# 2 -- Any questions too hard? 
distribution_false_fact_too_hard <- 
  ggplot(subset(df, 
                complete.cases(df$false_fact_too_hard)), 
         aes(
           x=false_fact_too_hard,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           # width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=35, vjust=0.6)) + 
  scale_fill_manual(name="Who said it",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#b1cbbb", "#c94c4c")) +
  xlab("") +
  ylab("") +
  ggtitle("'Too hard: How to get from one place you have never heard of to another place you have never heard of.'") +
  cleanup

print(distribution_false_fact_too_hard)
ggsave("images/distribution_false_fact_too_hard.png")

# 2 -- Example too hard
distribution_dontExist <- 
  ggplot(subset(df, 
                complete.cases(df$false_fact_too_hard2)), 
         aes(
           x=false_fact_too_hard2,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           # width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=35, vjust=0.6)) + 
  scale_fill_manual(name="Who said it",
                    labels=c("Fair Play", "Unfair"),
                    # values=c("#b1cbbb", "#c94c4c")) +
                    values=c("#eeccaa", "#8fb8bd")) +
  
  xlab("") +
  ylab("") +
  ggtitle("'Some of these places don't even exist'") +
  cleanup

print(distribution_dontExist)
ggsave("images/distribution_dontExist.png")


# 3 -- What did you think of the time limit? 
distribution_10secPlenty <- 
  ggplot(subset(df, complete.cases(df$false_fact_30sec_fairplay)), 
         aes(
           x=false_fact_30sec_fairplay,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           #width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6)) + 
  scale_fill_manual(name="False Feedback in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#b1cbbb", "#c94c4c")) +
  xlab("") +
  ylab("") +
  ggtitle("'How True Or False?", # \n
          subtitle = "'And 30 seconds was more than enough time to answer these questions: 10 seconds would have been plenty.'") +
  cleanup

print(distribution_10secPlenty)
ggsave("images/distribution_10secPlenty.png")

# Something in here doesn't make sense. Why doesn't it
# only show the 'unfair' feedback? If ff are assigned to 
# fair play then players shouldn't be getting this claim. 
# If the false facts were assigned to the 'fair play' feedback
# then this one is a missing -- the one they got was '10 sec 
# would have been plenty'. 

# USE THIS FOR ISPP
distribution_30secNoTimeToRead <- 
  ggplot(df, #subset(df, complete.cases(df$false_fact_30sec_unfair)), 
         aes(
           x=false_fact_30sec_unfair,
           fill=falsefeedback)) + 
  geom_bar(stat="count",
           # width = 0.5,
           position="dodge") +
  facet_grid(treatment ~ .) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6)) + 
  scale_fill_manual(name="False Feedback in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#b1cbbb", "#c94c4c")) +
  xlab("") +
  ylab("") +
  ggtitle("How True Or False?", # \n
          subtitle = "'And the 30 seconds we had was barely enough time to read the questions.'") +
  cleanup

print(distribution_30secNoTimeToRead)
ggsave("images/distribution_30secNoTimeToRead.png")


#########################################################
# Distribution -- Belief that game was rigged (after 99 obs)
#########################################################

distribution_rigged <- 
  ggplot(data=subset(df, !is.na(df$rigged)),
         aes(x=rigged, 
             fill=falsefeedback)) +
  geom_bar(stat="count",
           position="dodge") +
  # facet_grid(. ~ treatment) + 
  facet_grid(treatment ~ .) +
  scale_fill_manual(name="Who said it",
                    labels=c("Fair Play", "Unfair"),
                    # values=c("#b1cbbb", "#c94c4c")) +
                    values=c("#eeccaa", "#8fb8bd")) +
  ggtitle("Thinking back about this geography quiz: How likely do you \n think it is that any of the places in that quiz do not exist?", 
          subtitle = "NB: Question was added after 99 observations") +
  labs(# title="How likely do you think it is that any of the places in that quiz do not exist?", 
    x="",
    y="") +
  cleanup

print(distribution_rigged)
ggsave("images/distribution_rigged.png")

#########################################################
# Distribution -- luck of the draw
#########################################################
# Page name = luckOfTheDrawTeamA
# Variable name = byChanceTeamAEasierQs

# Histogram
ggplot(df, aes(x=byChanceTeamAEasierQs, 
               fill=treatment)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")

ggplot(df, aes(x=byChanceTeamAEasierQs, 
               fill=treatment)) +
  geom_density(alpha=.3) 

# Density plot
density_luckOfTheDraw <- 
ggplot(df, aes(x=byChanceTeamAEasierQs, 
               fill=falsefeedback)) +
  # geom_density(alpha=.3) +
  geom_density() +
  facet_grid(treatment ~ .) +
  scale_fill_manual(name="False claims in",
                  labels=c("Fair Play", "Unfair"),
                  values=c("#eeccaa", "#8fb8bd")) + 
  ggtitle("How likely do you think it is that Team A had the luck of the draw?", 
          subtitle = "0=A had the luck of the draw. | 100=A was favoured.") +
  labs(
    x="",
    y="") +
  cleanup

print(density_luckOfTheDraw)
ggsave("images/density_luckOfTheDraw.png")


#########################################################
# Identifying true believers 
#########################################################

# Looking just at those who said that some of these places don't 
# exist (to see if the 2 questions measure the same thing)

x <- subset(df, df$false_fact_too_hard2 == c("Probably True",
                                               "Definitely True"))
table(x$treatment, x$rigged)

x <- subset(df, treatment=="B")
table(x$rigged, x$falsefeedback)

# Measures pretty much the same thing.

# Create a new variable for people who really did believe the
# game was rigged (who said the false claim was prob / def true
# and who said the game was rigged)

df$believe <- ifelse(df$rigged == c("Likely", "Very Likely") | 
                         df$false_fact_too_hard2 == c("Probably True",
                                                      "Definitely True"),
                       1, 0) 

table(df$falsefeedback, df$believe, df$treatment)

df$trueBelieve <- ifelse(df$rigged == c("Likely", "Very Likely") & 
                       df$false_fact_too_hard2 == c("Probably True",
                                                    "Definitely True"),
                     1, 0) 

table(df$falsefeedback, df$trueBelieve, df$treatment)

# --> Use the former.

source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
crosstab(df, row.vars = "Age", col.vars = "Sex", type = "f")

# Frequency count
crosstab(df, row.vars = "treatment", 
         col.vars = "believe", type = "f")

# Row percentages
crosstab(df, row.vars = "treatment", 
         col.vars = "believe", type = "r")

# Joint percentages (sums to 100 within final two table dimensions)
crosstab(df, row.vars = c("treatment", "falsefeedback"), 
         col.vars = "believe", type = "r")

# --> who said it makes absolutely no difference. 
# Among team B, 26/7% believe that some of these places dont 
# exist, regardless of whether it was the unfair person or the 
# fair play person saying they didn't. 
# Among team A, more people believe it if it comes from the 
# fair play person (31 v. 17)


# Colors from https://www.w3schools.com/colors/colors_palettes.asp

crosstab(df, row.vars = "treatment", 
         col.vars = "false_fact_too_hard2", type = "r")

# A: 73% 
23.21 + 50.00 # 62.61 Definitely or Probably False
23.21 + 3.57 # 26.78 Definitely or Probably True

#########################################################
# Group Differences -- Belief in False Claims
#########################################################

somePlacesDontExist <-
  ggplot(df,
         aes(x=treatment,
             y=false_fact_too_hard2_num,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Some of the places in that quiz don’t even exist.'", # \n
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  # ggtitle("How True Or False?", # \n
  #         subtitle = "'Some of the places in that quiz don’t even exist. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  cleanup

print(somePlacesDontExist)
ggsave("images/somePlacesDontExist.png")

### Equivalent bullet point in the true facts ####

# Some of the questions about state capitals or provincial capitals were too difficult.

stateCapsTooHard <-
  ggplot(df,
         aes(x=treatment,
             y=true_fact_too_hard_num,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Some of the questions about state capitals or \n provincial capitals were too difficult.'", # \n
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  # ggtitle("How True Or False?", # \n
  #         subtitle = "'Some of the places in that quiz don’t even exist. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  cleanup

print(stateCapsTooHard)
ggsave("images/stateCapsTooHard")

# Side by side
require(gridExtra)
veryFF <- grid.arrange(stateCapsTooHard, somePlacesDontExist, ncol=2)


### Bullet Point 4 (GROSS EXXAGGERATION) #### 

# 4th/4 bullet points containing false facts 

thirtySec_false_fairplay <-
  ggplot(df,
         aes(x=treatment,
             y=false_fact_30sec_fairplay_num,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="") + # Accuracy Unfair
  # ggtitle("'10 sec would have been plenty.'", # \n
  ggtitle("'30 seconds was more than enough time to answer these \n questions: 10 seconds would have been plenty.'",
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  # ggtitle("How True Or False?", # \n
  #         # subtitle = "'And 30 seconds was more than enough time to answer these questions: \n 10 seconds would have been plenty. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  #         subtitle = "'30 seconds was more than enough. 10 sec would have been plenty. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  cleanup

print(thirtySec_false_fairplay)
ggsave("images/thirtySec_false_fairplay.png")


thirtySec_false_unfair <-
  ggplot(df,
         aes(x=treatment,
             y=false_fact_30sec_unfair_num,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,4)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="") + # Accuracy Unfair
  # ggtitle("'How True Or False?'", # \n
  ggtitle("'The 30 seconds we had was barely enough to read \n the questions'", # \n
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  # subtitle = "'And the 30 seconds we had was barely enough time to read the questions. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  # subtitle = "'30 sec was barely enough time to read the questions. \n (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)'") +
  cleanup

print(thirtySec_false_unfair)
ggsave("images/thirtySec_false_unfair.png")



#########################################################
# Distribution -- number of hard Qs by Team
#########################################################

unique(df$share_hardQs)
str(df$share_hardQs)
unique(df$sum_hardQs)

# simple frequency plot
ggplot(df, aes(sum_hardQs)) +
  geom_histogram()

# total number of hardQs -- by team
ggplot(df, aes(sum_hardQs, fill = treatment)) +
  geom_histogram()


distribution_share_hardQs <- 
  ggplot(df, aes(share_hardQs, 
                 fill = treatment)) +
  geom_bar() + 
  # geom_histogram() + 
  labs(x="",
       y="") + 
  ggtitle("Share of difficult questions in per cent",
          subtitle = "Odds Team A = 1:4 difficult questions | Odds Team B = 4:1") +
  scale_fill_manual(name="Team",
                    labels=c("A", "B"),
                    values=c("#7fbf7b", "#1f78b4")) +
  # values=c("#39ac73", "#e6004c")) + 
  cleanup

print(distribution_share_hardQs)
ggsave("images/distribution_share_hardQs.png")


distribution_no_hardQs <- 
  ggplot(df, aes(sum_hardQs, 
                 fill = treatment)) +
  geom_bar() + 
  labs(x="",
       y="") + 
  ggtitle("Number of difficult questions",
          subtitle = "Odds Team A = 1:4 difficult questions | Odds Team B = 4:1") +
  scale_fill_manual(name="Team",
                    labels=c("A", "B"),
                    values=c("#7fbf7b", "#1f78b4")) +
  # values=c("#39ac73", "#e6004c")) + 
  cleanup

print(distribution_no_hardQs)
ggsave("images/distribution_no_hardQs.png")



#########################################################
# Scatterplots
#########################################################

# Does not matter HOW disadvantaged you are to tolerate lies  
# in the unfair feedback? 

#########################################################
# Scatterplot -- Team B -- educated
#########################################################

scatter_teamB_educated <- 
  ggplot(subset(df, treatment=="B"), 
         aes(x=share_hardQs, # sum_hardQs, 
             y=educationRating_unfair,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.6) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Feedback contains",
    labels=c("No False Facts", "False Facts"),
    values=c("#999999", "darkred")) + 
  ggtitle("Team B Rating the 'Unfair' Feedback",
          subtitle = "'This person is well educated.'") +
  labs(x="Share of Difficult Questions",
       y="") +
  cleanup

print(scatter_teamB_educated)
ggsave("images/scatter_teamB_educated.png")


#########################################################
# Scatterplot -- Team B -- accurate
#########################################################

scatter_teamB_accurate <- 
  ggplot(subset(df, treatment=="B"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=accuracyRating_unfair,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.6) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Feedback contains",
    labels=c("No False Facts", "False Facts"),
    values=c("#999999", "darkred")) + 
  ggtitle("Team B Rating the 'Unfair' Feedback",
          subtitle = "'The points this person makes are factually accurate.'") +
  labs(x="Number of Difficult Questions",
       y="") +
  cleanup

print(scatter_teamB_accurate)
ggsave("images/scatter_teamB_accurate.png")


#########################################################
# Scatterplot -- Team B -- good Rep
#########################################################

scatter_teamB_goodRep <- 
  ggplot(subset(df, treatment=="B"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=representationRating_unfair,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.6) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Feedback contains",
    labels=c("No False Facts", "False Facts"),
    values=c("#999999", "darkred")) + 
  ggtitle("Team B Rating the 'Unfair' Feedback",
          subtitle = "'This person is a good representative of my team.'") +
  labs(x="Number of Difficult Questions",
       y="")  +
  cleanup

print(scatter_teamB_goodRep)
ggsave("images/scatter_teamB_goodRep.png")


#########################################################
# Scatterplot -- Team B -- belief in false claim
#########################################################

scatter_teamB_false_facts <- 
  ggplot(subset(df, treatment=="B"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=false_fact_too_hard2_num, # false_fact_30sec_unfair_num,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.3) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_jitter(
    position = position_jitter(width = 0.05, height = 0.05)) +
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Who said it:",
    labels=c("Fairplay", "Unfair"),
    values=c("#999999", "darkred")) + 
  ggtitle("Team B -- 'Some of these places don't even exist.'",
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  labs(x="Number of Difficult Questions",
       y="") +
  cleanup

print(scatter_teamB_false_facts)
ggsave("images/scatter_teamB_false_facts.png")


#########################################################
# Scatterplot -- Team A -- good Rep
#########################################################

scatter_teamA_goodRep <- 
  ggplot(subset(df, treatment=="A"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=representationRating_fairplay,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.6) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Feedback contains",
    labels=c("False Facts", "No False Facts"),
    values=c("darkred", "#999999")) + 
  ggtitle("Team A Rating the 'Fair Play' Feedback",
          subtitle = "'This person is a good representative of my team.'") +
  labs(x="Number of Difficult Questions",
       y="")  +
  cleanup

print(scatter_teamA_goodRep)
ggsave("images/scatter_teamA_goodRep.png")

#########################################################
# Scatterplot -- Team A -- accurate
#########################################################

scatter_teamA_accurate <- 
  ggplot(subset(df, treatment=="A"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=accuracyRating_fairplay,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.6) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Feedback contains",
    labels=c("False Facts", "No False Facts"),
    values=c("darkred", "#999999")) + 
  ggtitle("Team A Rating the 'Fair Play' Feedback",
          subtitle = "'The points this person makes are factually accurate.'") +
  labs(x="Number of Difficult Questions",
       y="")  +
  cleanup

print(scatter_teamA_accurate)
ggsave("images/scatter_teamA_accurate.png")

#########################################################
# Scatterplot -- Team A -- belief in false claim
#########################################################

scatter_teamA_false_facts <- 
  ggplot(subset(df, treatment=="A"), 
         aes(x=sum_hardQs, # share_hardQs, # sum_hardQs, 
             y=false_fact_too_hard2_num, #false_fact_30sec_unfair_num,
             color=falsefeedback
         )) + 
  geom_point(alpha = 0.3) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_jitter(
    position = position_jitter(width = 0.05, height = 0.05)) +
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Who said it:",
    labels=c("Fairplay", "Unfair"),
    values=c("#999999", "darkred")) + 
  ggtitle("Team A -- 'Some of these places don't even exist.'",
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  labs(x="Number of Difficult Questions",
       y="")  +
  cleanup

print(scatter_teamA_false_facts)
ggsave("images/scatter_teamA_false_facts.png")


#########################################################
# Scatterplot -- both teams -- belief in false claim
#########################################################

scatter_false_facts <- 
  ggplot(df, 
         aes(x=sum_hardQs, 
             y=false_fact_30sec_unfair_num,
             color=falsefeedback
         )) + 
  facet_grid(treatment ~ .) + 
  geom_point(alpha = 0.3) + #  overplotting -- manipulating the alpha transparency
  # geom_smooth( #aes(fill=falsefeedback),
  #             se=FALSE) + # lowess, remove confidence interval
  geom_jitter(
    position = position_jitter(width = 0.1, height = 0.1)) +
  geom_smooth(method=lm, se=FALSE) + # regression line
  # scale_color_hue(l=40, c=35)
  scale_color_manual(
    name="Who said it:",
    labels=c("Fairplay", "Unfair"),
    values=c("#999999", "darkred")) + 
  ggtitle("Agreement that 'Some of these places don't even exist.'",
          subtitle = "(1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  labs(x="Number of Difficult Questions",
       # labs(x="Share of Difficult Questions",
       y="")  

print(scatter_false_facts)
ggsave("images/scatter_false_facts.png")






# # Same thing on a subset
# 
# #########################################################
# # Create a subset
# library(dplyr)
# subset_wide <- df %>% select(# id,
#                       participant.code,
#                       session.code,
#                       treatment,
#                       falsefeedback,
#                       false_fact_too_hard2_num,
#                       
#                       agreeFeedback_fairplay,
#                       agreeFeedback_unfair,
#                       agreeFeedback_fairplay_num,
#                       agreeFeedback_unfair_num,
#                       
#                       educationRating_fairplay,
#                       educationRating_unfair,
#                       accuracyRating_fairplay,
#                       accuracyRating_unfair,
#                       representationRating_fairplay,
#                       representationRating_unfair,
#                       
#                       false_fact_30sec_fairplay,
#                       false_fact_30sec_unfair, 
#                       false_fact_30sec_fairplay_num,
#                       false_fact_30sec_unfair_num)
# str(subset_wide)
# 
# names(subset_wide)
# 
# # Rename into 6-character names . FP/UF
# 
# subset_wide$session.code <- as.character(subset_wide$falsefeedback)
# subset_wide$treatment <- as.character(subset_wide$treatment)
# subset_wide$falsefeedback <- as.character(subset_wide$falsefeedback)
# 
# 
# names(subset_wide)[names(subset_wide)=="agreeFeedback_fairplay"] <- "agreed.FP"
# names(subset_wide)[names(subset_wide)=="agreeFeedback_unfair"] <- "agreed.UF"
# 
# names(subset_wide)[names(subset_wide)=="agreeFeedback_fairplay_num"] <- "agreeN.FP"
# names(subset_wide)[names(subset_wide)=="agreeFeedback_unfair_num"] <- "agreeN.UF"
# 
# names(subset_wide)[names(subset_wide)=="educationRating_fairplay"] <- "educat.FP"
# names(subset_wide)[names(subset_wide)=="educationRating_unfair"] <- "educat.UF"
# 
# names(subset_wide)[names(subset_wide)=="accuracyRating_fairplay"] <- "accura.FP"
# names(subset_wide)[names(subset_wide)=="accuracyRating_unfair"] <- "accura.UF"
# 
# names(subset_wide)[names(subset_wide)=="representationRating_fairplay"] <- "repres.FP"
# names(subset_wide)[names(subset_wide)=="representationRating_unfair"] <- "repres.UF"
# 
# names(subset_wide)[names(subset_wide)=="false_fact_30sec_fairplay"] <- "ff30se.FP"
# names(subset_wide)[names(subset_wide)=="false_fact_30sec_unfair"] <- "ff30se.UF"
# 
# names(subset_wide)[names(subset_wide)=="false_fact_30sec_fairplay_num"] <- "ff30sN.FP"
# names(subset_wide)[names(subset_wide)=="false_fact_30sec_unfair_num"] <- "ff30sN.UF"
# 
# 
# # Turn into wide
# subset_long <-
#   subset_wide %>%
#   gather(key, value,
#          -participant.code,
#          -session.code,
#          -treatment,
#          -falsefeedback,
#          -false_fact_too_hard2_num) %>%
#   extract(key, c("question", "feedback"), "(......)\\.(..)") %>%
#   spread(question, value)
# #########################################################

#########################################################
# DETOUR
# https://stackoverflow.com/questions/25925556/gather-multiple-sets-of-columns
# 
# d <- data.frame(
#   id = 1:10,
#   time = as.Date('2009-01-01') + 0:9,
#   Q3.2.1. = rnorm(10, 0, 1),
#   Q3.2.2. = rnorm(10, 0, 1),
#   Q3.2.3. = rnorm(10, 0, 1),
#   Q3.3.1. = rnorm(10, 0, 1),
#   Q3.3.2. = rnorm(10, 0, 1),
#   Q3.3.3. = rnorm(10, 0, 1)
# )
# 
# # 1. Gather all question columns
# t1 <-
# d %>%
#   gather(key, value, -id, -time)
# 
# # 2. Use extract() to separate into question and loop_number, 
# t2 <-
#   d %>%
#   gather(key, value, -id, -time) %>%
#   extract(key, c("question", "loop_number"), "(Q.\\..)\\.(.)") 
# 
# # 3. then spread() question back into the columns.
# t3 <-
# d %>%
#   gather(key, value, -id, -time) %>%
#   extract(key, c("question", "loop_number"), "(Q.\\..)\\.(.)") %>%
#   spread(question, value)
# 
# extract
# 
# Given a regular expression with capturing groups, extract() 
# turns each group into a new column. If the groups don't match, 
# or the input is NA, the output will be NA.
#########################################################


#########################################################
# First, just looking at means
#########################################################

df %>%
  group_by(treatment, falsefeedback) %>%
  summarise(
    mean(agreeFeedback_fairplay_num),
    mean(agreeFeedback_unfair_num),
    mean(representationRating_fairplay),
    mean(representationRating_unfair),
    mean(accuracyRating_fairplay),
    mean(accuracyRating_unfair))

df %>%
  group_by(treatment, falsefeedback) %>%
  summarise(
    agreed_fairplay = mean(agreeFeedback_fairplay_num),
    agreed_unfair = mean(agreeFeedback_unfair_num),
    goodRep_fairplay = mean(representationRating_fairplay),
    goodRep_unfair = mean(representationRating_unfair),
    accurate_fairplay = mean(accuracyRating_fairplay),
    accurate_unfair = mean(accuracyRating_unfair))

View(means)




#########################################################
# 1) General Agreement 
#########################################################

#########################################################
agreed_unfair <- summarySE(df, 
                           measurevar="agreeFeedback_unfair_num",
                           groupvars=c("treatment",
                                       "falsefeedback"))
#########################################################

# NB:  Error: All arguments to rename must be named.
# --> comes if plyr and dplyr are loaded. 
# --> plyr::rename

# PLOT with 95% confidence intervals

agreed_unfair <-
  ggplot(agreed_unfair, 
         aes(x=treatment, 
             y=agreeFeedback_unfair_num,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=agreeFeedback_unfair_num-ci, 
      ymax=agreeFeedback_unfair_num+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,6)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "'Generally speaking, do you agree with the author of this feedback? \n (1 = 'Strongly Disagree', 6 = 'Strongly Agree')") +
  cleanup

print(agreed_unfair)
ggsave("images/agreed/agreed_unfair.png")

# FAIRPLAY -- TEAL -- FAF0E6 
# UNFAIR -- STEEL BLUE -- E7F0F1

# Codes for darker shades: 
# https://www.w3schools.com/colors/colors_picker.asp 

# # EXACT SAME THING without the helper function
# 
# ggplot(df, 
#        aes(x=treatment, 
#            y=agreeFeedback_unfair_num,
#            fill=falsefeedback)) + 
#   stat_summary(fun.y = mean,
#                geom = "bar",
#                position="dodge") + # stack them next to each other
#   stat_summary(fun.data = mean_cl_normal,
#                geom = "errorbar",
#                position = position_dodge(width = .9),
#                width = .2) + 
#   scale_fill_manual(name="False Facts in",
#                     labels=c("Fair Play", "Unfair"),
#                     values=c("#eeccaa", "#8fb8bd")) +
#   labs(
#     x="Team",
#     y="General Agreement: 'Unfair' Feedback"
#   ) + 
#   cleanup
#   

#########################################################
agreed_fairplay <- summarySE(df,
                             measurevar="agreeFeedback_fairplay_num",
                             groupvars=c("treatment","falsefeedback"))
#########################################################

agreed_fairplay <- 
  ggplot(agreed_fairplay, 
         aes(x=treatment, 
             y=agreeFeedback_fairplay_num,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=agreeFeedback_fairplay_num-ci, 
      ymax=agreeFeedback_fairplay_num+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,6)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "'Generally speaking, do you agree with the author of this feedback?' \n (1 = 'Strongly Disagree', 6 = 'Strongly Agree')") +
  cleanup

print(agreed_fairplay)
ggsave("images/agreed/agreed_fairplay.png")

require(gridExtra)
agreed <- grid.arrange(agreed_fairplay, agreed_unfair, ncol=2)
# ggsave("images/agreed/agreed.png")

# you agree regardless of factual accuracy




#########################################################
# NEW NEW NEW 9 March 2019 CONTINUE HERE
#########################################################

agreed_unfair <-
  ggplot(agreed_unfair, 
         aes(x=treatment, 
             y=agreeFeedback_unfair_num,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=agreeFeedback_unfair_num-ci, 
      ymax=agreeFeedback_unfair_num+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  coord_cartesian(ylim=c(0,6)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "'Generally speaking, do you agree with the author of this feedback? \n (1 = 'Strongly Disagree', 6 = 'Strongly Agree')") +
  cleanup


print(agreed_unfair)
ggsave("images/agreed/agreed_unfair.png")

df$representationRating_unfair




lying_not_lying_unfair_feedback <-
  ggplot(subset(df),# df_long
         aes(x=falsefeedback,
             y=representationRating_unfair, # representationRating
             fill=falsefeedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black"#,
               # fill="#eeccaa"
               ) +
  facet_grid(. ~ treatment) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_x_discrete(labels=c("fairplay" = "no false claims", 
                             "unfair" = "false claims")) +
  # scale_fill_manual(name="False Facts in",
  #                   labels=c("Fair Play", "Unfair"),
  #                   values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") + # none
  labs(x="",
       y="") +
  ggtitle("Unfair feedback: 'Good Representative.'") +
  theme_bw()
#  cleanup

print(lying_not_lying_unfair_feedback)
ggsave("images/agreed/agreed_unfair.png")


df_long$feedback
# trials




# Trials

df_long$feedback

# Calculates mean, sd, se and IC
my_sum <- df_long %>%
  group_by(feedback, falsefeedback) %>%
  summarise( 
    n=n(),
    mean=mean(representationRating),
    sd=sd(representationRating)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ic=se * qt((1-0.05)/2 + .5, n-1))

# https://www.r-graph-gallery.com/4-barplot-with-error-bar/ 
# https://datascienceplus.com/building-barplots-with-error-bars/
# Standard Error
ggplot(my_sum) +
  geom_bar( aes(x=falsefeedback, 
                y=mean), 
            stat="identity", 
            fill="forestgreen", 
            alpha=0.5) +
  geom_errorbar( aes(x=falsefeedback, 
                     ymin=mean-se, 
                     ymax=mean+se), 
                 width=0.4, 
                 colour="orange", 
                 alpha=0.9, 
                 size=1.5) +
  ggtitle("using standard error")



# Old all inclusive graphs

# THIS WORKS:

# define labels
labels <- c(A = "advantaged team", B = "disadvantaged team")

lying_not_lying_feedback <-
  ggplot(subset(df_long), 
         aes(x=falsefeedback,
             y=representationRating,
             fill=feedback
         )) + 
  facet_grid(treatment ~ feedback, 
             # labeller = label_both, 
             labeller=labeller(treatment = labels)
             ) +
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black"#,
               # fill="#eeccaa"
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  # scale_x_discrete(labels=c("fairplay" = "fair play feedback", 
  #                           "unfair" = "unfair feedback")) +
  # scale_x_discrete(labels=c("fairplay" = "no false claims", 
  #                           "unfair" = "false claims")) +
  scale_fill_manual(name="false feedback",
                    labels=c("'fair play' feedback", "'unfair' feedback"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") + # none
  labs(x="Feedback rated",
       y="") +
  ggtitle("Representation ratings depending on factual accuracy", 
  subtitle = "'This person is a good representative of my team.' (0=no, 100=yes)") +
  theme(strip.text.x = element_text(size=12, angle=90),
        strip.text.y = element_text(size=12, face="bold") ,
        strip.background = element_rect(colour="red", fill="#CCCCFF")
  ) +
  theme_bw()
#  cleanup

lying_not_lying_feedback <-
  ggplot(subset(df_long),# df_long
         aes(x=falsefeedback,
             y=representationRating, #representationRating_unfair, # 
             fill=feedback
         )) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black"#,
               # fill="#eeccaa"
  ) +
  facet_grid(treatment ~ feedback, 
             labeller=labeller(treatment = labels)) +
  # facet_grid(. ~ treatment) +
  # facet_wrap( ~ treatment) + 
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  # scale_x_discrete(labels=c("fairplay" = "fair play feedback", 
  #                           "unfair" = "unfair feedback")) +
  # scale_x_discrete(labels=c("fairplay" = "no false claims", 
  #                           "unfair" = "false claims")) +
  scale_fill_manual(name="false feedback",
                    labels=c("'fair play' feedback", "'unfair' feedback"),
                    values=c("#eeccaa", "#8fb8bd")) +
  theme(legend.position="top") + # none
  labs(x="Feedback rated",
       y="") +
  ggtitle("Both teams rating both feedbacks") +
  theme(strip.text.x = element_text(size=12, angle=90),
        strip.text.y = element_text(size=12, face="bold") ,
        strip.background = element_rect(colour="red", fill="#CCCCFF")
        ) +
  theme_bw()
#  cleanup


sp + facet_grid(. ~ sex, labeller=labeller(treatment = labels))


#########################################################
# 2) Slider  Ratings
#########################################################

#########################################################
# a) Education Rating
#########################################################

#########################################################
education_unfair <- summarySE(df,
                              measurevar="educationRating_unfair", 
                              groupvars=c("treatment","falsefeedback"))
#########################################################
education_unfair <-
  ggplot(education_unfair, 
         aes(x=treatment, 
             y=educationRating_unfair,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=educationRating_unfair-ci, 
      ymax=educationRating_unfair+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "'This person is well educated.' (0=no, 100=yes)") +
  cleanup

print(education_unfair)
ggsave("images/education_unfair.png")

#########################################################
education_fairplay <- summarySE(df, 
                                measurevar="educationRating_fairplay",
                                groupvars=c("treatment","falsefeedback"))
#########################################################

education_fairplay <-
  ggplot(education_fairplay, 
         aes(x=treatment, 
             y=educationRating_fairplay,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=educationRating_fairplay-ci, 
      ymax=educationRating_fairplay+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "'This person is well educated.' (0=no, 100=yes)") +
  cleanup

print(education_fairplay)
ggsave("images/education_fairplay.png")

#########################################################
# without summarySE
#########################################################

educated_fairplay2 <-
  ggplot(df,
         aes(x=treatment,
             y=educationRating_fairplay,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,85)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="0 = no 100 = yes") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "'This person is well educated.'") +
  cleanup

educated_unfair2 <-
  ggplot(df,
         aes(x=treatment,
             y=educationRating_unfair,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,85)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") + 
  labs(x="Team",
       y="0 = no 100 = yes") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "'This person is well educated.'") +
  cleanup

require(gridExtra)
educated <- grid.arrange(educated_fairplay2, educated_unfair2, ncol=2)
ggsave("images/educated.png")


#########################################################
# b) Accuracy Ratings
#########################################################

#########################################################
accuracy_unfair <- summarySE(df,
                             measurevar="accuracyRating_unfair", 
                             groupvars=c("treatment","falsefeedback"))
#########################################################

accuracy_unfair <-
  ggplot(accuracy_unfair, 
         aes(x=treatment, 
             y=accuracyRating_unfair,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=accuracyRating_unfair-ci, 
      ymax=accuracyRating_unfair+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "The points this person makes are factually accurate (0=no, 100=yes)") +
  cleanup

print(accuracy_unfair)
ggsave("images/sliderQs/accuracy_unfair.png")

#########################################################
accuracy_fairplay <- summarySE(df, 
                               measurevar="accuracyRating_fairplay",
                               groupvars=c("treatment","falsefeedback"))
#########################################################

accuracy_fairplay <- 
  ggplot(accuracy_fairplay, 
         aes(x=treatment, 
             y=accuracyRating_fairplay,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=accuracyRating_fairplay-ci, 
      ymax=accuracyRating_fairplay+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "The points this person makes are factually accurate. (0=no, 100=yes)") +
  cleanup

print(accuracy_fairplay)
ggsave("images/sliderQs/accuracy_fairplay.png")

#########################################################
# Without summarySE
#########################################################

# 1) Side by Side -- USE THIS FOR PAPER

accuracy_fairplay2 <-
  ggplot(df,
         aes(x=treatment,
             y=representationRating_fairplay,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="0 = no 100 = yes") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "The points this person makes are factually accurate.") +
  cleanup

accuracy_unfair2 <-
  ggplot(df,
         aes(x=treatment,
             y=accuracyRating_unfair,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") + 
  labs(x="Team",
       y="0 = no 100 = yes") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "The points this person makes are factually accurate
          ") +
  cleanup

require(gridExtra)
goodRep <- grid.arrange(accuracy_fairplay2, accuracy_unfair2, ncol=2)

print(goodRep)
# ggsave("images/sliderQs/goodRep.png") 


#########################################################
# c) Representation Ratings
#########################################################

#########################################################
representation_unfair <- summarySE(df, 
                                   measurevar="representationRating_unfair", 
                                   groupvars=c("treatment","falsefeedback"))
#########################################################

representation_unfair <- 
  ggplot(representation_unfair, 
         aes(x=treatment, 
             y=representationRating_unfair,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=representationRating_unfair-ci, 
      ymax=representationRating_unfair+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "This person is a good representative of my team (0=no, 100=yes)") +
  cleanup

print(representation_unfair)
ggsave("images/representation_unfair.png")

#########################################################
representation_fairplay <- summarySE(df, 
                                     measurevar="representationRating_fairplay", 
                                     groupvars=c("treatment","falsefeedback"))
#########################################################

representation_fairplay <- 
  ggplot(representation_fairplay, 
         aes(x=treatment, 
             y=representationRating_fairplay,
             fill=falsefeedback)) + 
  geom_bar(position=position_dodge(), 
           stat="identity") +
  geom_errorbar(
    aes(
      ymin=representationRating_fairplay-ci, 
      ymax=representationRating_fairplay+ci),
    width=.2, # Width of the error bars
    position=position_dodge(.9)) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "This person is a good representative of my team (0=no, 100=yes)") +
  cleanup

print(representation_fairplay)
ggsave("images/representation_fairplay.png")

require(gridExtra)
goodRep <- grid.arrange(representation_fairplay, representation_unfair, ncol=2)

print(goodRep)
# ggsave("images/sliderQs/goodRep.png") 


#########################################################
# without summarySE
#########################################################

# Good Rep -- Side by Side -- USE THIS FOR PAPER

representation_fairplay2 <-
  ggplot(df,
         aes(x=treatment,
             y=representationRating_fairplay,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") +
  labs(x="Team",
       y="0 = no 100 = yes") + # Accuracy Unfair
  ggtitle("'Fair Play' Feedback", # \n
          subtitle = "This person is a good representative of my team.") +
  cleanup

representation_unfair2 <-
  ggplot(df,
         aes(x=treatment,
             y=representationRating_unfair,
             fill=falsefeedback)) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  coord_cartesian(ylim=c(0,100)) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) +
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  # theme(legend.position="none") + # GET RID OF LEGEND
  theme(legend.position="top") + 
  labs(x="Team",
       y="0 = no 100 = yes") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "This person is a good representative of my team.
          ") +
  cleanup

require(gridExtra)
goodRep <- grid.arrange(representation_fairplay2, representation_unfair2, ncol=2)

print(goodRep) 
ggsave("images/sliderQs/goodRep.png") 


# END GRAPS #############################################


