#########################################################
# Brexit Twitter Study
# -- Analyses --
# -- 07 Aug 2017 -- 
#########################################################

# plot(pexp(7:0, rate=.18))

rm(list = ls())

library(foreign) # for read.csv
library(plyr)  # for plyr::rename AND revalue
library(dplyr) # for full_join 
library(reshape) 
library(MASS) 
library(car) # for recode
library(ggplot2)
library(scales)
library(pastecs) # for descriptive stats
library(scales) # for nice histograms
library(stargazer) # for latex exports
# install.packages("psych")
library(psych)
library(ggthemes) # seconds to minutes
library(lubridate)
# install.packages("colorspace")
library("colorspace") # for colour palettes

# check effect of stress on just leave voters

#########################################################
# Load Brexit Twitter data and import BES data set to compare
#########################################################

setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/1.BrexitTwitterPaper/BES") # MAC
# setwd("C:/Users/cmsted/Dropbox/1.PhD/1.Papers/1.BrexitTwitterPaper/BES") # WINDOWS

bes <- read.spss("BES2015_W10_Panel_v0.3.sav", 
                 to.data.frame=TRUE)

setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/1.BrexitTwitterPaper/Eurobarometer")
eurobarometer <- read.spss("ZA6788_v1-2-0.sav", 
                 to.data.frame=TRUE)

qa16_1 # EU KNOWLEDGE: 28 MEMBER STATES (correct)
qa16_2 # EP MEMBERS ELECTION (correct)
qa16_3 # SWITZERLAND IS MEMBER (false)

str(eurobarometer$tnscntry) # 22 GREAT BRITAIN
levels(eurobarometer$qa16_1) # True (CORRECT)
levels(eurobarometer$qa16_2) # True (CORRECT)
levels(eurobarometer$qa16_3) # False (CORRECT)

# How many Brits? 1043
nrow(filter(eurobarometer, 
            tnscntry == "GREAT BRITAIN"))

# How many Brits got them all right? 292
nrow(filter(eurobarometer, 
            tnscntry == "GREAT BRITAIN", 
            qa16_1 == "True (CORRECT)",
            qa16_2 == "True (CORRECT)",
            qa16_3 == "False (CORRECT)"))

292/1043 # 28 per cent

# weighting???

# downloaded from https://dbk.gesis.org/dbksearch/SDesc2.asp?ll=10&notabs=&af=&nf=&search=&search2=&db=E&no=6788

setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/1.BrexitTwitterPaper/data") # MAC
# setwd("C:/Users/cmsted/Dropbox/1.PhD/1.Papers/1.BrexitTwitterPaper/data") # WINDOWS

load("BrexitTwitterData.R")
# load("post_ref")
# load("post_ref_UK")
# load("direct_links")
# load("direct_links_UK")

# Check how long people spent on the misperceptions question
# & exclude those who spent too long

# df[ , c("mp_1ST_CLICK", "mp_LASTCLICK", "mp_SUBMIT", "mp_CLICKCOUNT" ) ]

# Page Submit: How many seconds pass before the respondent clicks the Next button (i.e., how long in total the respondent spends on the page)
# One person spent a very long time on these questions... 

# seconds_to_period(1952) # 32 minutes -- maybe exclude? 
# seconds_to_period(424) # 7 minutes



#########################################################
# Natural experiment?
#########################################################

# See if I can get a natural experiment: 
# Vllt Camerons Ruecktritt? 24 June 2016. See if things were different davor/danach. 
# What happened in those days for which I have my data? 


# Before referendum #####################################

# Create variable -- before referendum (in df_all dataset)

df_all$befRef <- df_all$startTime <= "2016-06-23" ## double check
length(which(df_all$befRef == TRUE)) # 24 before the referendum

sum(df_all$startTime >= "2016-06-23")
length(which(df_all$befRef == FALSE)) # 381 after the referendum

# Graph - participants before and after 23 June 
# NB -- I am commenting this out because 


# Create date variable
df_all$Date <- as.Date(df_all$startTime)

df_all$startTime

ggplot(df_all, 
       aes(x=startTime, 
           fill=befRef)) + 
  geom_histogram(binwidth = 0.8) + 
  # geom_text(stat='bin', 
  #          aes(label=..count..), 
  #           vjust=-2) + 
  labs(x="",
       y="",
       title="Qualtrics Survey Participants") + 
  scale_y_continuous(breaks = seq(0, 80, by=5)) + 
  scale_x_date(
    # date_breaks = "46 days", 
    date_minor_breaks = "1 day", # ?
    date_labels = "%b %d") +
  theme(axis.text.x = element_text(
    size  = 8, # maxe x-axis labels smaller
    angle = 40, # rotate x-axis labels 45 degrees
    hjust = 1,
    vjust = 1)) +
  scale_fill_discrete(name="Before \n referendum",
                      breaks=c("FALSE", "TRUE"),
                      labels=c("after", "before")) +
  theme_bw() 
# scale_fill_brewer(palette="Spectral")

# http://dsgeek.com/2014/09/19/Customizingggplot2charts.html


# Create variable -- before Cameron resigned (in df_all dataset)

df_all$befCameronRes <- df_all$Date <= "2016-06-24"
length(which(df_all$befCameronRes == TRUE)) # 24 before the referendum
length(which(df_all$befCameronRes == TRUE & df_all$votedORwouldve == "leave")) # 5 leaves befRef
length(which(df_all$befCameronRes == FALSE & df_all$votedORwouldve == "leave")) # 102

ggplot(df_all, aes(x=Date, fill=befCameronRes)) + geom_histogram(binwidth = 1)

# only 24 responses before the referendum OR before Cameron resigned, including 5 leave votes
# 102 leave votes after the referendum
# --> Can't do much with this


min(df_all$Date)
max(df_all$Date)

table(df_all$Date)

# Leave voters had higher misperceptions BEFORE the referendum (but... n = 5) 
# than after the referendum (n = 102)

mean(
  subset(df_all, befCameronRes == TRUE & df_all$votedORwouldve == "leave")$misperceptions_sur5
  , na.rm = TRUE) # 11.25 (the 5 leave voters)
mean(
  subset(df_all, befCameronRes == TRUE)$misperceptions_sur5
  , na.rm = TRUE) # 7.41 (all voters)


mean(
  subset(df_all, befCameronRes == FALSE & df_all$votedORwouldve == "leave")$misperceptions_sur5
  , na.rm = TRUE) # 10.77
mean(
  subset(df_all, befCameronRes == FALSE)$misperceptions_sur5
  , na.rm = TRUE) # 7.16




#########################################################
# Descriptive Stats & Counts & Such 
#########################################################

# How many direct links did I sent out (4 spreadsheets)
# 1638 + 1743 + 1604 + 1666 # 6651 


# Time ##################################################

min(df$startTime) # 2016-06-25
max(df$startTime) # 2016-08-02
difftime( max(df$startTime), min(df$startTime) ) # Time difference of 45.99631 days --> 46 days

# Timing graph

library("ggplot2")
library("scales")

p <- 
  ggplot(df, 
         # aes(Date, ..count..)) # can leave this , ..count.. out, too
         aes(Date)) + 
  geom_histogram(binwidth = 1) + # , fill="darkseagreen") + # count FOR EACH DAY! 
  theme_bw() +
  xlab(NULL) +
  ylab("number of participants") +
  # labs(title="Qualtrics Survey Participants") + 
  scale_x_date(
    # date_breaks = "46 days", 
    date_minor_breaks = "1 day", 
    date_labels = "%b %d") 
p

# Have a look at the last person who took the survey last
df$Date
df[ df$Date == "2016-08-02", ]


# Treatment group assignment ############################

# See if random treatment assignment worked -- it did =)

# df$treatment
ggplot(data=df, aes(x=treatment, fill=vote)) + 
  geom_bar() 

table(df$treatment)

# df$trGroup_friend
# ggplot(data=df, aes(x=trGroup_friend, fill=vote)) + 
#   geom_bar() 



# where they lived #######################################

table(df$liveCountry)
round(prop.table(table(df$liveCountry)),2)
# 83% living in England, 7% Scotland, 2% Wales, 1% Northern Ireland, 5% elsewhere in the EU

table(bes$country)
round(prop.table(table(bes$country)), 2)


# gender ################################################

table(df$gender)
table(df$gender, df$votedORwouldve)
round(prop.table(table(df$gender)),2) # row percentages -- 63% male



# how they voted ########################################

table(df$vote) # 99 voted leave, 225 voted remain
prop.table(table(df$votedORwouldve))
round(prop.table(table(df$gender, df$votedORwouldve)),2)


# how each age group voted ##############################

round(prop.table(table(df$ageGroup, df$vote), 1), 2)




# how sure ##############################################

# histograms -- strength of prior opinion

plot(df$howSure_vote_f) 

# distribution across leave and remain voters
distribution_howSure <- 
  ggplot(subset(df, vote%in% c("leave", "remain") & complete.cases(df$howSure_vote_f)), 
         aes(x=howSure_vote_f,
             fill=votedORwouldve)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  facet_grid(vote ~ .) + # leave this out to have leave and remain bars next to each other
  labs(x = "", y = "",
       title = "How sure were you about your vote choice?") +
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("leave", "remain"),
                      labels=c("Leave", "Remain"))

# QUESTION: should I exclude the not sure people from the analyses?
table(df$vote, df$howSure_vote_f) 
round(prop.table(table(df$vote, df$howSure_vote_f), 1), 2) # 1 = row percentages

table(df$vote, df$goodReasons_leave_f)
round(prop.table(table(df$vote, df$goodReasons_leave_f), 1), 2)

table(df$vote, df$goodReasons_remain_f)
round(prop.table(table(df$vote, df$goodReasons_remain_f), 1), 2)



# how interested ########################################

# Q6 How interested were you in the EU referendum that was held in the UK on June 23rd?

# Here: 93 % very interested, 7 % somewhat interested
round(prop.table(table(df$interest_EUref_f)), 2) 

# BES: 63 % very interested, 28 % somewhat interested
round(prop.table(table(bes$euRefInterestW8)), 2)

# by vote

# Here: 93 % of leave and remain voters very interested
round(prop.table(table(df$vote, df$interest_EUref_f), 1), 2)

# BES: 0.61 % of remain, 71% of leave voters very interested
round(prop.table(table(bes$euRefVoteW9, bes$euRefInterestW8), 1),2)

distribution_howInterested <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$interest_EUref_f)), 
         aes(x=interest_EUref_f,
             fill=votedORwouldve)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  facet_grid(vote ~ .) + # leave this out to have leave and remain bars next to each other
  labs(x = "", y = "",
       title = "How interested were you in the EU referendum \n that was held in the UK on June 23rd?") +
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("leave", "remain"),
                      labels=c("Leave", "Remain"))

# BES
distribution_howInterested_BES <- 
  ggplot(subset(bes, euRefVoteW9 %in% c("Stay/remain in the EU", "Leave the EU") & complete.cases(bes$euRefInterestW8)), 
         aes(x=euRefInterestW8,
             fill=euRefVoteW9)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  facet_grid(euRefVoteW9 ~ .) + # leave this out to have leave and remain bars next to each other
  labs(x = "", y = "",
       title = "How interested are you in the EU referendum \n that will be held on June 23rd? (BES)") +
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("Stay/remain in the EU", "Leave the EU"),
                      labels=c("Leave", "Remain"))


# Q7 On a scale of 0 to 10, how much did or do you care about the following?
# Which side won the EU referendum on 23 June (1)
hist(df$interest_whoWon)
table(df$interest_whoWon)



# how uncertain #########################################

# Uncertainty
plot(df$whatHappens_leave_f)
plot(df$whatHappens_remain_f)



# how many good reasons #################################

# Some say that there were good reasons for both options in this EU referendum.  Others think 
# it was more clear-cut than that.  How many would you say there were of each of the following?
# Good reasons for leaving 
# Good reasons for remaining

table(df$vote, df$goodReasons_leave_f)
prop.table(table(df$vote, df$goodReasons_leave_f), 1) # row percentages

table(df$vote, df$goodReasons_remain_f)
prop.table(table(df$vote, df$goodReasons_remain_f), 1) # row percentages 

# plot(subset(df, votedORwouldve == "remain")$goodReasons_remain_f) 
# plot(subset(df, votedORwouldve == "remain")$goodReasons_leave_f) 
# 
# plot(subset(df, votedORwouldve == "leave")$goodReasons_remain_f) 
# plot(subset(df, votedORwouldve == "leave")$goodReasons_leave_f) 

# TO DO - see if vote and votedORwouldve is different




# to leave ###############################################

distribution_reasonstoLeave <- # übereinander
ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$goodReasons_leave_f)),  # df, 
       aes(
         x=goodReasons_leave_f,
         fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="How many good reasons were there to leave?",
       y="",
       title="") +
  theme(axis.text.x = element_text(
    angle = 30,
    hjust = 1))



# to remain ##############################################

distribution_reasonstoRemain <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$goodReasons_remain_f)),  # df, 
         aes(
           x=goodReasons_remain_f,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="How many good reasons were there to remain?",
       y="",
       title="") +
  theme(axis.text.x = element_text(
    angle = 30,
    hjust = 1))



# NBs (dont do anything about this)

# TWO leave voters who saw "very many" good reasons to remain 
df[ which(df$goodReasons_remain_f == "Very many" & df$votedORwouldve == "leave"), 
    c("id", "goodReasons_leave_f", "goodReasons_leave_f", "SSS", "lifeSatisfaction_f", "m1_Turkey", "m2_Army", "m3_NHS", "m4_Euro",
      "misperceptions_sur5")]

# ONE leave voter who saw "Not very many" reasons to leave (251) -- this looks like a mistake
df[ which(df$goodReasons_leave_f == "Not very many" & df$votedORwouldve == "leave"), 
    c("id", "goodReasons_leave_f", "goodReasons_leave_f", "SSS", "lifeSatisfaction_f", "m1_Turkey", "m2_Army", "m3_NHS", "m4_Euro",
      "misperceptions_sur5")]

# THREE remain voters who saw "Very many" reasons to leave
df[ which(df$goodReasons_leave_f == "Very many" & df$votedORwouldve == "remain"), 
    c("id", "goodReasons_leave_f", "goodReasons_leave_f", "SSS", "lifeSatisfaction_f", "m1_Turkey", "m2_Army", "m3_NHS", "m4_Euro",
      "misperceptions_sur5")]


#########################################################
# BES comparisons 
#########################################################

# in common #############################################

table(df$vote, df$inCommon_Brexiteers)
round(prop.table(table(df$vote, df$inCommon_Brexiteers), 1), 2) # row percentages

# Here: scale from 1 to 10
min(df$inCommon_Remainers, na.rm = T)
min(df$inCommon_Brexiteers, na.rm = T)

# Bring to 0 to 10 scale (*1.1) so I can compare it with the BES
df$inCommon_Remainers_11 <- df$inCommon_Remainers * 1.1
df$inCommon_Brexiteers_11 <- df$inCommon_Brexiteers * 1.1


# BES: scale from 0 to 10
levels(bes$socialIdentityGlobalRemainW9)
levels(bes$socialIdentityGlobalLeaveW9)

# turn BES into numeric
bes$inCommon_Remainers_num <- bes$socialIdentityGlobalRemainW9
levels(bes$inCommon_Remainers_num)[levels(bes$inCommon_Remainers_num)=="Nothing in common"] <- "0"
levels(bes$inCommon_Remainers_num)[levels(bes$inCommon_Remainers_num)=="A great deal in common"] <- "10"
levels(bes$inCommon_Remainers_num)[levels(bes$inCommon_Remainers_num)=="Don't know"] <- NA
levels(bes$inCommon_Remainers_num)
bes$inCommon_Remainers_num <- as.numeric(as.character(bes$inCommon_Remainers_num))

bes$inCommon_Brexiteers_num <- bes$socialIdentityGlobalLeaveW9
levels(bes$inCommon_Brexiteers_num)[levels(bes$inCommon_Brexiteers_num)=="Nothing in common"] <- "0"
levels(bes$inCommon_Brexiteers_num)[levels(bes$inCommon_Brexiteers_num)=="A great deal in common"] <- "10"
levels(bes$inCommon_Brexiteers_num)[levels(bes$inCommon_Brexiteers_num)=="Don't know"] <- NA
levels(bes$inCommon_Brexiteers_num)
bes$inCommon_Brexiteers_num <- as.numeric(as.character(bes$inCommon_Brexiteers_num))



# Here: How much did Brexiteers have in common with Remainers?
# (Scale from 1 to 10)
mean(subset(df, vote == "leave")$inCommon_Remainers, na.rm = T) # 6.21
sd(subset(df, vote == "leave")$inCommon_Remainers, na.rm = T) # 2.39

# Using the BES Scale (0 to 10)
mean(subset(df, vote == "leave")$inCommon_Remainers_11, na.rm = T) # 6.84
sd(subset(df, vote == "leave")$inCommon_Remainers_11, na.rm = T) # 2.63

# BES: How much did Brexiteers have in common with Remainers?
mean(subset(bes, euRefVoteW9 == "Leave the EU")$inCommon_Remainers_num, na.rm = T) # 4.42
sd(subset(bes, euRefVoteW9 == "Leave the EU")$inCommon_Remainers_num, na.rm = T) # 2.77

# In the BES sample, Brexit voters report having A LOT LESS 
# IN COMMON with Remainers! 
# BES: m=4.42, sd=2.77 , here: m=6.84, sd=2.63



# Here: How much did Remainers have in common with Brexiteers?
mean(subset(df, vote == "remain")$inCommon_Brexiteers, na.rm = T) # 5.23
sd(subset(df, vote == "remain")$inCommon_Brexiteers, na.rm = T) # 1.88

# Using the BES Scale (0 to 10)
mean(subset(df, vote == "remain")$inCommon_Brexiteers_11, na.rm = T) # 5.76
sd(subset(df, vote == "remain")$inCommon_Brexiteers_11, na.rm = T) # 2.07

# BES: How much did Remainers have in common with Brexiteers?
mean(subset(bes, euRefVoteW9 == "Stay/remain in the EU")$inCommon_Brexiteers_num, na.rm = T) # 2.97 
sd(subset(bes, euRefVoteW9 == "Stay/remain in the EU")$inCommon_Brexiteers_num, na.rm = T) # 2.53

# In the BES sample, Remain voters report having A LOT LESS 
# IN COMMON with Brexiteers! 
# BES: m=2.97, sd=2.53, here: m=5.76, sd=2.07


distribution_incommonLeave <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$inCommon_Brexiteers)),  # df, 
         aes(
           x=inCommon_Brexiteers,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="How much do you have in common \n with people who voted to leave?",
       y="",
      title="") # +
  # theme(axis.text.x = element_text(
  #   angle = 40,
  #   hjust = 1))

distribution_incommonRemain <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$inCommon_Remainers)),  # df, 
         aes(
           x=inCommon_Remainers,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="How much do you have in common \n with people who voted to remain?",
       y="",
       title="") # +
# theme(axis.text.x = element_text(
#   angle = 40,
#   hjust = 1))


# BES

# socialIdentityGlobalRemainW9
distribution_incommonRemain_BES <-
  ggplot(subset(bes, euRefVoteW9 %in% c("Stay/remain in the EU", 
                                        "Leave the EU") & 
                  complete.cases(bes$socialIdentityGlobalRemainW9)),  
         aes(
           x=socialIdentityGlobalRemainW9,
           fill=euRefVoteW9)) + 
  geom_bar(stat="count") + 
  facet_grid(euRefVoteW9 ~ .) + 
  labs(x="",
       y="",
       title="How much do you have in common with \n people who want to remain in the EU? (BES)") +
  theme(axis.text.x = element_text(
     angle = 30,
     hjust = 1)) + 
  scale_fill_discrete(name="Vote",
                      breaks=c("Stay/remain in the EU", "Leave the EU"),
                      labels=c("Remain", "Leave"))

# socialIdentityGlobalLeaveW9
distribution_incommonLeave_BES <-
  ggplot(subset(bes, euRefVoteW9 %in% c("Stay/remain in the EU", 
                                        "Leave the EU") & 
                  complete.cases(bes$socialIdentityGlobalLeaveW9)),  
         aes(
           x=socialIdentityGlobalLeaveW9,
           fill=euRefVoteW9)) + 
  geom_bar(stat="count") + 
  facet_grid(euRefVoteW9 ~ .) + 
  labs(x="",
       y="",
       title="How much do you have in common with \n people who want to leave the EU? (BES)") +
  theme(axis.text.x = element_text(
    angle = 30,
    hjust = 1)) + 
  scale_fill_discrete(name="Vote",
                      breaks=c("Stay/remain in the EU", "Leave the EU"),
                      labels=c("Remain", "Leave"))

# TO DO CHANGE COLOURS


## Big5 #################################################

# openness
# Here: 1 (not at all) - 7 (very much)
# BES: Min 0 Max 19

hist(bes$personality_openness) # more open
hist(df$openness_index) 

hist(bes$personality_agreeableness) # more agreeable
hist(df$agreeableness_index)

hist(bes$personality_conscientiousness) # bit more conscientiousness
hist(df$conscientiousness_index)

hist(bes$personality_neuroticism)
hist(df$neuroticism_index)

hist(bes$personality_extraversion)
hist(df$extraversion_index)


## Trust ################################################

# Here: 61 % most people can be trusted, 39 cannot be too careful
round(prop.table(table(df$trust)), 2)
# 68 % among remainers, only 52 % among leavers
round(prop.table(table(df$vote, df$trust), 1), 2)

# BES: 51% most people can be trusted, 41% cannot be too careful
round(prop.table(table(bes$genTrustW7)), 2) 
# 57 % among remainers, only 40% among leavers
round(prop.table(table(bes$euRefVotePost, bes$genTrustW7), 1), 2)

# FAZIT: My sample has more trust in people 




## Education ############################################

str(df$education)
df$education
plot(df$education_f)

# Here 
ggplot(df, aes(x = education_f)) + 
  geom_bar() + 
theme(axis.text.x = element_text(
  angle = 20,
  hjust = 1)) 

# BES
ggplot(bes, aes(x = profile_education_age)) + 
  geom_bar() + 
  theme(axis.text.x = element_text(
    angle = 20,
    hjust = 1)) 

round(prop.table(table(df$education_f)), 2)
# Here: 62 % 19 or older, 10 % Still at university in full-time education
# --> 72% went to uni / are going to uni

round(prop.table(table(bes$profile_education_age)), 2) 
# BES: 5% aged 19, 21 % aged 20+, 8% Still at school/Full time student 
# --> 34% went to uni / are students

# FAZIT: My sample is MUCH more educated. 


plot(bes$profile_education_age)
round(prop.table(table(bes$profile_education_age)), 2)

# sources of information


# also in BES
# euRefFinal
# marital

# Looked at big5, trust, education, inCommon (social identity), interest



## Age ##################################################

str(df$born)
# turn into date? 
# df$born_date <- as.Date(df$born)
# ?as.Date

min(df$born, na.rm = T) # 1941 -- 75 years old
max(df$born, na.rm = T) # 2000 -- 16 years old

length(which(df$votedORwouldve == "leave" & df$born >= 1986)) # 23 
length(which(df$votedORwouldve != "leave" & df$born >= 1986)) # 79 
23/79 # 29% of teens and twens voted to leave

length(which(df$votedORwouldve == "leave" & df$born >= 1976)) # 36 
length(which(df$votedORwouldve != "leave" & df$born >= 1976)) # 142
36/142 # 25% of 30 somethings voted to leave

length(which(df$votedORwouldve == "leave" & df$born >= 1966)) # 63
length(which(df$votedORwouldve != "leave" & df$born >= 1966)) # 204
63/204 # 30% of 40 somethings voted to leave

length(which(df$votedORwouldve == "leave" & df$born >= 1956)) # 88
length(which(df$votedORwouldve != "leave" & df$born >= 1956)) # 241
88/241 # 36% of 50 somethings voted to leave

length(which(df$votedORwouldve == "leave" & df$born >= 1946)) # 96
length(which(df$votedORwouldve != "leave" & df$born >= 1946)) # 251
96/251 # 38 of 60 somethings voted to leave

length(subset(df, votedORwouldve == "leave")) # 405 cases

# Using geom_bar TO DO: change colour palette
ggplot(data=df,
       aes(
         x=born, 
         fill=vote)) +
  geom_bar() +
  labs(
    x = NULL, 
    y = "number of participants") 

# Using geom_histogram -- same thing
ggplot(data=df,
       aes(
         x=born, 
         fill=vote)) +
  geom_histogram(breaks=seq(1940, 2000, by = 1)) +
  labs(
    x = NULL, 
    y = "number of participants")



# voted #################################################

# Count number of NAs and complete cases in 
# voted / votedORwouldve columns

# length(post_ref2$vote) # 381 cases... now 531?
length(df$vote) # 405 cases

# 1) vote

sum(df$vote=="leave", na.rm = TRUE) # 105 leave voters (100 in post_ref2)
sum(df$vote=="remain", na.rm = TRUE) # 248 remain voters (233 in post_ref2)
sum(df$vote=="not vote", na.rm = TRUE) # 5 not voters 
sum(is.na(df$vote)) # 47 missings
sum(!complete.cases(df$vote)) # same

sum(complete.cases(df$vote)) # 358 complete cases
# which(!complete.cases(df$vote)) # these are missing

# Look at missing values
# missing_vote <- df[is.na(df$vote), ]

# Relevel vote var so that "remain" = baseline category
df$vote <- relevel(df$vote, "remain")
levels(df$vote)
# NB: Not enough not voters in the dataset to form a baseline category


# 2) votedORwouldve

sum(df$votedORwouldve=="leave", na.rm = TRUE) # 107 leave voters (+2)
sum(df$votedORwouldve=="remain", na.rm = TRUE) # 257 remain voters (+9)
sum(df$votedORwouldve=="not vote", na.rm = TRUE) # 6 not voters (+1)
sum(is.na(df$votedORwouldve)) # 35 missings
sum(!complete.cases(df$votedORwouldve)) # same

sum(complete.cases(df$votedORwouldve)) # 370 complete cases
# which(!complete.cases(df$votedORwouldve)) # these are still missing

# Look at missing values
# missing_vote <- df[is.na(df$votedORwouldve), ]




# expectations ##########################################


# immigration #############################################

table(subset(df, vote=="leave")$change_immigration_f) 
round(prop.table(table(subset(df, vote=="leave")$change_immigration_f), 2)) 
prop.table(table(subset(df, vote=="leave")$immigrationWillDrop)) # 59% will drop


table(subset(df, vote=="remain")$change_immigration_f) 
prop.table(table(subset(df, vote=="remain")$change_immigration_f)) 
prop.table(table(subset(df, vote=="remain")$immigrationWillDrop)) # 13% will drop



# NHS ####################################################

plot(df$change_NHS_f)
plot(subset(df, vote=="leave")$change_NHS_f)
plot(subset(df, vote=="remain")$change_NHS_f)

table(subset(df, vote=="leave")$change_NHS_f) 
prop.table(table(subset(df, vote=="leave")$change_NHS_f)) 
prop.table(table(subset(df, vote=="leave")$NHSBetter)) # 57 % will get better

table(subset(df, vote=="remain")$change_NHS_f) 
prop.table(table(subset(df, vote=="remain")$change_NHS_f)) 
prop.table(table(subset(df, vote=="remain")$NHSBetter)) # 1 % will get better


# general economic situation ############################

table(subset(df, vote=="leave")$change_econ_f) 
prop.table(table(subset(df, vote=="leave")$change_econ_f)) 
prop.table(table(subset(df, vote=="leave")$econ_Better)) # 64% will get better

table(subset(df, vote=="remain")$change_econ_f) 
prop.table(table(subset(df, vote=="remain")$change_econ_f)) 
prop.table(table(subset(df, vote=="remain")$econ_Better)) # 2% will get better


# personal financial situation ###########################

table(subset(df, vote=="leave")$change_persFinance_f) 
prop.table(table(subset(df, vote=="leave")$change_persFinance_f)) 
prop.table(table(subset(df, vote=="leave")$persFinances_Better)) # 26% will get better
prop.table(table(subset(df, vote=="leave")$persFinances_Worse)) # 3% will get worse

table(subset(df, vote=="remain")$change_persFinance_f) 
prop.table(table(subset(df, vote=="remain")$change_persFinance_f)) 
prop.table(table(subset(df, vote=="remain")$persFinances_Better)) # 1% will get better
prop.table(table(subset(df, vote=="remain")$persFinances_Worse)) # 75% worse


# Risk of terrorism #####################################

table(subset(df, vote=="leave")$change_terrorism_f) # 33% lower, 63 same
prop.table(table(subset(df, vote=="leave")$change_terrorism_f))

table(subset(df, vote=="remain")$change_terrorism_f) # 2% lower, 58 same
prop.table(table(subset(df, vote=="remain")$change_terrorism_f))









#########################################################
# Playing with DVs
#########################################################


# Histograms for each misp item #########################

  # If Britain had remained in the EU it would have had to accept Turkish membership. 
  # The EU could have forced British soldiers to join a European army.
  # Leaving the EU frees up £350m a week for the NHS.
  # The EU could have made Britain join the Euro.
  # The Queen supports Brexit.
  
  
## m1_Turkey_D ##########################################

m1 <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m1_Turkey)),
         aes(x=m1_Turkey,
             fill=vote)) + 
    geom_bar(stat="count",
             position=position_dodge()) +
    labs(x = "", y = "",
   # title = "If Britain had remained \n in the EU it would  have had to \n accept Turkish membership.") + # Turkey in EU
    title = "If Britain had remained in the EU it would \n have had to accept Turkish membership.") + # Turkey in EU
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
    scale_fill_discrete(name="Vote",
                        breaks=c("leave", "remain"),
                        labels=c("Leave", "Remain")) 

# table
# table(df$vote, df$m1_Turkey)
# table(df$vote, df$m1_Turkey_D)



## m2_Army_D ############################################

  m2 <- 
    ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m2_Army)),
           aes(x=m2_Army,
               fill=vote)) + 
    geom_bar(stat="count",
             position=position_dodge()) +
    labs(x = "", y = "",
         title = "The EU could have forced British \n soldiers to join a European army.") + # Turkey in EU
    theme(axis.text.x = element_text(
      angle = 40, 
      hjust = 1)) +
    scale_fill_discrete(name="Vote",
                        breaks=c("leave", "remain"),
                        labels=c("Leave", "Remain"))

# table
# table(df$vote, df$m2_Army)
# table(df$vote, df$m2_Army_D)


## m3_NHS_D #############################################

m3 <- 
    ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m3_NHS)),
           aes(x=m3_NHS,
               fill=vote)) + 
    geom_bar(stat="count",
             position=position_dodge()) +
    labs(x = "", y = "",
        # title = "Leaving the EU frees up \n 350m a week for the NHS.") + # Turkey in EU
         title = "Leaving the EU frees up GBP350m a week for the NHS.") + # Turkey in EU
    theme(axis.text.x = element_text(
      angle = 40, 
      hjust = 1)) +
    scale_fill_discrete(name="Vote",
                        breaks=c("leave", "remain"),
                        labels=c("Leave", "Remain"))

  
## m4_Euro_D ############################################

m4 <- 
    ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m4_Euro)), 
           aes(x=m4_Euro,
               fill=vote)) + 
    geom_bar(stat="count",
             position=position_dodge()) +
    labs(x = "", y = "",
        # title = "The EU could have made \n Britain join the Euro.") + # Turkey in EU
          title = "The EU could have made Britain join the Euro.") + # Turkey in EU
    theme(axis.text.x = element_text(
      angle = 40, 
      hjust = 1)) +
    scale_fill_discrete(name="Vote",
                        breaks=c("leave", "remain"),
                        labels=c("Leave", "Remain"))

## m5_Queen #############################################

m5 <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m5_Queen)), 
         aes(x=m5_Queen,
             fill=vote)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  labs(x = "", y = "",
       title = "The Queen backs Brexit") + 
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("leave", "remain"),
                      labels=c("Leave", "Remain"))

m5

# table
# table(df$vote, df$m4_Euro)
# table(df$vote, df$m4_Euro_D)


m7 <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m5_Queen)), 
         aes(x=poundPlunged,
             fill=vote)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  labs(x = "", y = "",
       title = "The pound plunged to its lowest level since 1985 after Britain voted to leave the EU.") + 
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("leave", "remain"),
                      labels=c("Leave", "Remain"))
m7


m8 <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$m5_Queen)), 
         aes(x=ScotlandRemain,
             fill=vote)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  labs(x = "", y = "",
       title = "All council areas in Scotland voted to remain in the EU.") + 
  theme(axis.text.x = element_text(
    angle = 40, 
    hjust = 1)) +
  scale_fill_discrete(name="Vote",
                      breaks=c("leave", "remain"),
                      labels=c("Leave", "Remain"))

m8


# Is there a difference in how stressed and unstressed 
# Remainers  rated the distractors? Did stress make them more 
# likely to agree with the 2 distractors? --> if so, its 
# acquiescence bias. 

# Leave voters were more likely to agree with the Queens
# statement under stress! 
# They were also more likely to agree with the Scotland
# statement

df %>%
  # filter(
  #   vote == "remain"
  # ) %>%
  group_by(
    vote, 
    treatment 
  ) %>%
  summarise(
    queen = mean(m5_Queen_num, na.rm = T), 
    pound = mean(poundPlunged_num, na.rm = T), 
    Scotland = mean(ScotlandRemain_num, na.rm=T)
  )


#########################################################
# Multiple graphs on one page
#########################################################

# install.packages("multiplot") 
# library(multiplot)
# multiplot(m1, m2, m3, m4, m5, cols=2)

# package ‘multiplot’ is not available (for R version 3.3.1)
# workaround: http://www.peterhaschke.com/r/2013/04/24/MultiPlot.html
source("http://peterhaschke.com/Code/multiplot.R")

individMispAnswers <- multiplot(m1, m2, m3, m4, cols=2) # Using the Dummy vars

# HOW TO put multiple histograms into one beautiful plot 
# http://stackoverflow.com/questions/14818529/plot-histograms-over-factor-variables

# NO LONGER WORKING
# TRY http://stats.stackexchange.com/questions/14118/drawing-multiple-barplots-on-a-graph-in-r 
# OR http://www.sthda.com/english/wiki/ggplot2-barplot-easy-bar-graphs-in-r-software-using-ggplot2
ggplot(df, aes(x=))




#########################################################
# Creating a misperceptions index
#########################################################

# Recoding misperceptions questions


# 1) Turkey #############################################

df$m1_Turkey_num <- recode(df$m1_Turkey, '
                           "Definitely true" = "5";
                           "Probably true" = "4";
                           "Do not know" = "3";
                           "Probably false" = "2";
                           "Definitely false" = "1" ')

df$m1_Turkey_num <- as.numeric(df$m1_Turkey_num)
# table(df$m1_Turkey_num, df$vote)

df$m1_Turkey_wei <- recode(df$m1_Turkey, '
                           "Definitely true" = "4";
                           "Probably true" = "3";
                           "Do not know" = "0";
                           "Probably false" = "1";
                           "Definitely false" = "0" ')

df$m1_Turkey_wei <- as.numeric(as.character(df$m1_Turkey_wei))

df$m1_Turkey_D <- ifelse(df$m1_Turkey=="Definitely true" | df$m1_Turkey=="Probably true", 1, 0)

df$m1 <- ifelse(df$m1_Turkey == "Definitely true", 2, 
                ifelse(df$m1_Turkey == "Probably true", 1, 0))



# 2) Army ###############################################

df$m2_Army_num <- recode(df$m2_Army, '
                         "Definitely true" = "5";
                         "Probably true" = "4";
                         "Do not know" = "3";
                         "Probably false" = "2";
                         "Definitely false" = "1" ')

df$m2_Army_num <- as.numeric(df$m2_Army_num)
# table(df$m2_Army_num, df$vote)

df$m2_Army_wei <- recode(df$m2_Army, '
                         "Definitely true" = "4";
                         "Probably true" = "3";
                         "Do not know" = "0";
                         "Probably false" = "1";
                         "Definitely false" = "0" ')

df$m2_Army_wei <- as.numeric(as.character(df$m2_Army_wei))
#  table(df$m2_Army_wei, df$vote)

df$m2_Army_D <- ifelse(df$m2_Army=="Definitely true"  | df$m2_Army=="Probably true", 1, 0)

df$m2 <- ifelse(df$m2_Army == "Definitely true", 2, 
                ifelse(df$m2_Army == "Probably true", 1, 0))


# 3) NHS ################################################

df$m3_NHS_num <- recode(df$m3_NHS, '
                        "Definitely true" = "5";
                        "Probably true" = "4";
                        "Do not know" = "3";
                        "Probably false" = "2";
                        "Definitely false" = "1" ')

df$m3_NHS_num <- as.numeric(as.character(df$m3_NHS_num))
# table(df$m3_NHS_num, df$vote)

df$m3_NHS_wei <- recode(df$m3_NHS, '
                        "Definitely true" = "4";
                        "Probably true" = "3";
                        "Do not know" = "0";
                        "Probably false" = "1";
                        "Definitely false" = "0" ')

df$m3_NHS_wei <- as.numeric(as.character(df$m3_NHS_wei))
# table(df$m3_NHS_wei, df$vote)

df$m3_NHS_D <- ifelse(df$m3_NHS=="Definitely true" | df$m3_NHS=="Probably true", 1, 0)

df$m3 <- ifelse(df$m3_NHS == "Definitely true", 2, 
                ifelse(df$m3_NHS == "Probably true", 1, 0))

# 4) Euro ###############################################

df$m4_Euro_num <- recode(df$m4_Euro, '
                         "Definitely true" = "5";
                         "Probably true" = "4";
                         "Do not know" = "3";
                         "Probably false" = "2";
                         "Definitely false" = "1" ')

df$m4_Euro_num <- as.numeric(df$m4_Euro_num)
#  table(df$m4_Euro_num, df$vote)

df$m4_Euro_wei <- recode(df$m4_Euro, '
                         "Definitely true" = "4";
                         "Probably true" = "3";
                         "Do not know" = "0";
                         "Probably false" = "1";
                         "Definitely false" = "0" ')

df$m4_Euro_wei <- as.numeric(as.character(df$m4_Euro_wei))
# table(df$m4_Euro_wei, df$vote)

df$m4_Euro_D <- ifelse(df$m4_Euro=="Definitely true" | df$m4_Euro=="Probably true", 1, 0)

df$m4 <- ifelse(df$m4_Euro == "Definitely true", 2, 
                ifelse(df$m4_Euro == "Probably true", 1, 0))


# 5) Queen ###############################################

df$m5_Queen_num <- recode(df$m4_Euro, '
                         "Definitely true" = "5";
                         "Probably true" = "4";
                         "Do not know" = "3";
                         "Probably false" = "2";
                         "Definitely false" = "1" ')

df$m5_Queen_num <- as.numeric(df$m5_Queen_num)

# 6) Pound ###############################################

df$poundPlunged <- factor(df$poundPlunged,
                          levels = c("Definitely false", "Probably false", "Do not know",
                                     "Probably true", "Definitely true"))

df$poundPlunged_num <- recode(df$poundPlunged, '
                          "Definitely true" = "5";
                          "Probably true" = "4";
                          "Do not know" = "3";
                          "Probably false" = "2";
                          "Definitely false" = "1" ')

df$poundPlunged_num <- as.numeric(df$poundPlunged_num)

# 7) Pound ###############################################

df$ScotlandRemain_num <- recode(df$ScotlandRemain, '
                          "Definitely true" = "5";
                          "Probably true" = "4";
                          "Do not know" = "3";
                          "Probably false" = "2";
                          "Definitely false" = "1" ')

df$ScotlandRemain_num <- as.numeric(df$ScotlandRemain_num)


# Building misperceptions indices #######################

# mps_weighted ########################################## 

# 4 for definitely true 
# 3 for probably true
# 1 for probably false
# 0 for definitely false -- or DK
# (Additive index, definitely true = 4, probably true = 3, probably false = 1, 
# definitely false or don't know = 0. Scores on the NHS question counted double.)

df$mps_weighted <- df$m1_Turkey_wei + df$m2_Army_wei + 2 * df$m3_NHS_wei + df$m4_Euro_wei
# min 0 max 20


# Not weighted ##########################################
# cf Arthur Spirling's comments on why would you count the 
# NHS claim double? 

df$mps <- df$m1_Turkey_wei + df$m2_Army_wei + df$m3_NHS_wei + df$m4_Euro_wei
# min 0 max 16


# mps_weighted_c #########################################

# -- more conservative (nothing for probably false)

# 2 for definitely true
# 1 for probably true

df$mps_weighted_c <- df$m1 + df$m2 + df$m3 + df$m4 # not counting NHS twice
# min 0 max 10

# dummy -- 1 point for "definitely true" OR "probably true"
df$mps_weighted_mc <- df$m1_Turkey_D + df$m2_Army_D + df$m3_NHS_D + df$m4_Euro_D # was misperceptions_D 
# min 0 max 5


# misperceptions_sur5 ####################################

# sur5 -- least loss of information 
df$misperceptions_sur5 <- df$m1_Turkey_num + df$m2_Army_num + df$m3_NHS_num + df$m4_Euro_num

# definitely true = 5
# probably true = 4
# DK = 3
# probably false = 2
# definitely false = 1
# DONT USE THIS -- GIVES POINTS FOR DK


# table(df$misperceptions_sur5, df$vote)
# table(df$misperceptions_sur5, df$votedORwouldve_Leave)



#########################################################
# Test scale reliability / internal consistency
#########################################################

# NOTE RJ: Do these form a coherent index? Need to check

# Internal consistency describes the extent to which all the items in a test
# measure the same concept or construct.

# Cronbach's alpha is a measure of internal consistency, that is, how closely related a set of 
# items are as a group. It is considered to be a measure of scale reliability. A "high" value 
# for alpha does not imply that the measure is unidimensional. If, in addition to measuring 
# internal consistency, you wish to provide evidence that the scale in question is 
# unidimensional, additional analyses can be performed. Exploratory factor analysis is one 
# method of checking dimensionality.
# http://www.ats.ucla.edu/stat/spss/faq/alpha.html

# Cronbach's alpha is computed by correlating the score for each scale item with the total score 
# for each observation (usually individual survey respondents or test takers), and then comparing 
# that to the variance for all individual item scores:
# http://data.library.virginia.edu/using-and-interpreting-cronbachs-alpha/

# NB: Critique: http://onlinelibrary.wiley.com/doi/10.1111/bjop.12046/abstract 
# (suggest McDonald's omega instead of Cronbach's alpha)

# In R: 
# From scratch: http://www.uwo.ca/fhs/tc/labs/09.ItemAnalysis.pdf
# Using psych package: https://rpubs.com/hauselin/reliabilityanalysis -- GOOD! (following this)

# library(psych)
# library(dplyr)

# brexitLies_num <- dplyr::select(df, m1_Turkey_num, m2_Army_num, m3_NHS_num, m4_Euro_num) 
# alpha(brexitLies_num, keys = c(1,1,1,1))

brexitLies_wei <- dplyr::select(df, m1_Turkey_wei, m2_Army_wei, m3_NHS_wei, m4_Euro_wei) 
psych::alpha(brexitLies_wei)

brexitLies_wei_c <- dplyr::select(df, m1, m2, m3, m4) 
psych::alpha(brexitLies_wei_c)


# Reliability analyses: 

# Summary statistics
# raw_alpha: Cronbach’s α (values ≥ .7 or .8 indicate good reliability; Kline (1999))
# brexitLies_wei -->  raw_alpha = .8 --> The scale is reliable. BETTER
# brexitLies_wei_c --> raw_alpha = .77 --> fine, too

#  Reliability if an item is dropped:
#  raw_alpha: overall α when that particular item has been dropped/deleted. 
# brexitLies_wei --> None of these are greater than the overall α of .8, 
# so don't need to drop any to increase my α. 
# brexitLies_wei_c --> m3 0.78 is greater than the overall α of .77

# Item statistics: 
# raw.r: correlation between the item and the total score from the scale (i.e., item-total 
# correlations)... PROBLEM: the item itself is included in the total. So use r.cor and r.drop

# r.drop: item-total correlation without that item itself (i.e., item-rest correlation or 
# corrected item-total correlation); low item-total correlations indicate that that item 
# doesn't correlate well with the scale overall.
# All items should correlate the the total score, so we’re looking for items that don’t 
# correlate with the overall score from the scale. If r.drop values are less than about .3, 
# it means that particular item doesn’t correlate very well with the scale overall.

# brexitLies_wei: All four items correlate well with the overall scale (item-rest correlations range from
# .55 (NHS) to .70 (Army).) 

# brexitLies_wei_c: All four items correlate well with the overall scale (item-rest correlations range from
# .46 (NHS) to .70 (Army).) 

# r.cor: item-total correlation corrected for item overlap and scale reliability

# mean and sd: mean and sd of the scale if that item is dropped

# Non missing response frequency for each item: 
# This table tells us what percentage of people gave each response to each of the items 
# (i.e., if you have a 5-point scale, then it tells you how many percent of responses were 
# 1, 2, 3, 4, or 5). This helps you check the distribution of responses and whether everyone 
# is giving the same responses (which will lead to low reliability).
# brexitLies_wei --> 0.53 - 0.76 for the 0 answer
# brexitLies_wei_c --> 0.77 - 0.92 for the 0 answer

# Just for fun: same thing including the Queen item
brexitLiesQueen <- dplyr::select(df, m1_Turkey_num, m2_Army_num, m3_NHS_num, m4_Euro_num, m5_Queen_num)
alpha(brexitLiesQueen, keys = c(1,1,1,1,1))
alpha(brexitLiesQueen)
# r.drop for m5_Queen_num: 0.33 --> does not correlate well with the overall scale!

# --> USE brexitLies_wei because it has a nicer distribution and a higher alpha! 
# Should be a guter kompromiss. 


#########################################################
# Factor Analysis
#########################################################

# to check if the scale is unidimensional

# Create a subset of just the misperceptions items
df_mps <- subset(df, select = c("m1_Turkey_num", 
                                "m2_Army_num", 
                                "m3_NHS_num",
                                "m4_Euro_num"))

# drop missings (first row)
df_mps <- df_mps[complete.cases(df_mps), ]


# Exploratory factor analysis
# Run a PCA to determine the number of factors
pca <- princomp(df_mps) 
# first dimension explains 66% of the variance, 2nd only 15%
summary(pca)
plot(pca)

# Factor analysis

fa1 <- factanal(df_mps, factors = 1, rotation = "varimax") # varimax is the default
fa1

# all load on factor 1 (factor loadings between .6 and .82)
# --> as expected, one underlying factor

# https://www.youtube.com/watch?v=Ilf1XR-K3ps 



## Distribution of scores on the misperception index

distribution_mps <- ## USE THIS
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$mps_weighted)),  # df, 
         aes(
           x=mps, # mps_weighted
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="Scores on the misperceptions index",
       y="",
       title="") # +
# theme(axis.text.x = element_text(
#   angle = 40,
#   hjust = 1))

# (Additive index, definitely true = 4, probably true = 3, probably false = 1, 
# definitely false or don't know = 0. Scores on the NHS question counted double.)


distribution_mps <- ## USE THIS
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$mps)),  # df, 
         aes(
           x=mps,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="Values on misperception index",
       y="",
       title="") # +
# theme(axis.text.x = element_text(
#   angle = 40,
#   hjust = 1))

distribution_mps_weighted_c <- 
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$mps_weighted_c)),  # df, 
         aes(
           x=mps_weighted_c,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="Values on weighted misperception index",
       y="",
       title="") # +
# theme(axis.text.x = element_text(
#   angle = 40,
#   hjust = 1))

# (Additive index, definitely true = 2, probably true = 1, probably false, 
# definitely false or don't know = 0. Scores on the NHS question counted double.)

distribution_mps_weighted_mc <- ## too little variation among remainers
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(df$mps_weighted_mc)),  
         aes(
           x=mps_weighted_mc,
           fill=vote)) + 
  geom_bar(stat="count") + 
  facet_grid(vote ~ .) + 
  labs(x="Values on weighted misperception index",
       y="",
       title="") # +
# theme(axis.text.x = element_text(
#   angle = 40,
#   hjust = 1))

# For leave people: pretty even distribution
# For remain people: right-skewed




#########################################################
# EXPERIMENTAL TREATMENT GROUPS -- Stress
#########################################################


# counts  ###############################################

# See how many leave / remainers were assigned to stress & control

noPeople_StressControl <- 
ggplot(df, #  subset(df, vote %in% c("leave", "remain")), # # complete.cases(vote) --> NAs in graph
       aes(x=vote,
           fill=treatment)) + 
  geom_bar(stat="count",
           position=position_dodge()) +
  labs(x = "", y = "",
       title = "") +
  # theme(axis.text.x = element_text(
  #   angle = 40, 
  #   hjust = 1)) +
  scale_fill_discrete(name="Treatment \n condition",
                      breaks=c("control", "stress"),
                      labels=c("Control", "Stress")) 

# Around 50 stressed & unstressed leave people, around 125 stressed & unstressed remainers


# Is there a difference in stressed / unstressed people's misperceptions scores?

# Look at mean differences
# tapply(df$misperceptions_D, df$treatment, mean, na.rm = TRUE) # GREATEST DIFFERENCE
# tapply(df$misperceptions_sur2, df$treatment, mean, na.rm = TRUE) 
tapply(df$misperceptions_sur5, df$treatment, mean, na.rm = TRUE) # NOT MUCH DIFFERENCE
tapply(df$mps_weighted, df$treatment, mean, na.rm = TRUE) # NOT MUCH DIFFERENCE


# number of respondents in social status categories

# length(df[ , c(df$vote == "leave" & df$SSS == "low")])

nrow(df[df$vote == "leave" & df$SSS == "low", ]) # 39
nrow(df[df$vote == "leave" & df$SSS == "medium", ]) # 68
nrow(df[df$vote == "leave" & df$SSS == "high", ]) # 22 

nrow(df[df$vote == "leave" & df$SSS_d == "low", ]) # 64
nrow(df[df$vote == "leave" & df$SSS_d == "high", ]) # 64



#########################################################
# Treatment Effects START HERE
#########################################################

# Get rid of ugly background in ggplot
cleanup = theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_blank(),
                axis.line = element_line(color="black"))

# Get colours from colorspace color palette
# install.packages("colorspace")
# library("colorspace")
pal <- choose_palette()
# Red to blue
# pal(2) # "#023FA5", "#8E063B"
# pal(4) # "#023FA5", "#BEC1D4", "#D6BCC0", "#8E063B"
# pal(7) "#023FA5", "#7D87B9", "#BEC1D4", "#E2E2E2", "#D6BCC0", "#BB7784", "#8E063B"

# Grey
# pal(7) # "#474747", "#696969", "#8A8A8A", "#A8A8A8", "#C2C2C2",
# "#D7D7D7", "#E2E2E2"
# pal(3) # "#474747", "#A8A8A8", "#E2E2E2"
# pal(2) # "#474747", "#E2E2E2"

effect_plain <- # USE THIS
  ggplot(data=df, 
         aes(x=treatment, 
             y=mps_weighted)) + # mps mps_weighted mps_weighted_c
  stat_summary(fun.y=mean,  
               geom="bar",
               fill=c("#474747", "#E2E2E2"), # "#7D87B9", "#BB7784"), # pal(2), # "White", # colour of the bars col=rainbow_hcl(2)
               color="Black") + # colour of the outline of it
  stat_summary(fun.data = mean_cl_normal, # function for CIs 
                geom="errorbar", # what type of geometrical object I want to add
                position="dodge", # forces things to not overlap (2 bars arent on top of each other)
                width=.2) + # how large you want them +
cleanup +
  xlab("") +
  ylab("weighted misperceptions index")


# https://www.youtube.com/watch?v=N4d-qUTTY44

# vote  #################################################

effect_by_vote <- # USE THIS
  ggplot(data=subset(df, vote%in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps, # mps_weighted
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  facet_wrap(~vote) + 
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("misperceptions index") +
  cleanup

effect_by_vote_cat <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$vote_cat)), 
         aes(x=treatment, 
             y=mps, # mps_weighted
             fill=vote_cat)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_wrap(~vote_cat) + 
  scale_fill_manual(name="Vote Category",
                   labels=c("die-hard remain", "remain", "leave", "die-hard leave"),
                    values=c("#023FA5", "#BEC1D4", "#D6BCC0", "#8E063B")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("misperceptions index") +
  cleanup

# vote_cat -- simple ggplot, without error bars
effect_by_vote_cat <- 
  ggplot(data=subset(df, complete.cases(df$vote_cat)),
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D_noQ misperceptions_sur5
             fill=vote_cat)) + 
  stat_summary(fun.y=mean,
               geom="bar") +
  facet_wrap(~vote_cat) +
  labs(x=NULL, y=NULL) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +  # rotate axis labels
  scale_fill_discrete(name = "Brexit Attitudes") +
  ggtitle("Impact of a stress manipulation on belief in false claims on the EU ")


effect_time <- # USE THIS
  ggplot(data=subset(df, vote%in% c("leave", "remain") & mp_SUBMIT < 320), 
         aes(x=treatment, 
             y=mp_SUBMIT, 
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  facet_wrap(~vote) + 
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("time spent evaluating false facts") +
  cleanup

# In common

effect_incommon_LR <- 
  ggplot(data=subset(df, vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=inCommon_Remainers, 
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  facet_wrap(~vote) + 
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("time spent evaluating false facts") +
  cleanup

effect_incommon_L <- # black and white
  ggplot(data=subset(df, vote %in% c("leave")), 
         aes(x=treatment, 
             y=inCommon_Remainers)) + # mps mps_weighted mps_weighted_c
  stat_summary(fun.y=mean,  
               geom="bar",
               fill=c("#474747", "#E2E2E2"), # "#7D87B9", "#BB7784"), # pal(2), # "White", # colour of the bars col=rainbow_hcl(2)
               color="Black") + # colour of the outline of it
  stat_summary(fun.data = mean_cl_normal, # function for CIs 
               geom="errorbar", # what type of geometrical object I want to add
               position="dodge", # forces things to not overlap (2 bars arent on top of each other)
               width=.2) + # how large you want them +
  cleanup +
  xlab("") +
  ylab("in common with remainers")
# How much do you have in common with people who voted to remain?


effect_incommon_L <- # colour # USE THIS
  ggplot(data=subset(df, vote %in% c("leave")), 
         aes(x=treatment, 
             y=inCommon_Remainers, 
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  facet_wrap(~vote) + 
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("in common with remainers") +
  cleanup


# SSS  ##################################################

effect_by_SSS <- 
ggplot(data=subset(df, complete.cases(df$SSS)), 
       aes(x=treatment, 
           y=mps, # mps_weighted misperceptions_D
           fill=SSS)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_wrap(~SSS) + 
  scale_fill_manual(name="Subjective Social Status",
                    labels=c("low", "medium", "high"),
                    values=c("#023FA5", "#E2E2E2", "#8E063B")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("misperceptions index") +
  cleanup


effect_by_SSS_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$SSS) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps, # mps_weighted misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ SSS) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("misperceptions index") +
  cleanup


effect_by_SSS_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$SSS_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps, # mps_weighted misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ SSS_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("misperceptions index") +
  cleanup

mean(df$mps, na.rm = T)
mean(df$mps, na.rm = T)

# dplyr
df %>% 
  group_by(treatment) %>%
  summarise(mean_mps = mean(mps, na.rm=T),
            total = n())

df %>% 
  filter(vote == "leave") %>%
  group_by(treatment, SSS) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())


df %>% 
  filter(vote == "leave") %>%
  group_by(treatment, SSS_d) %>%
  summarise(mean_mps = mean(mps, na.rm=T),
            total = n())



# unemployment ##########################################

df %>% 
  filter(sourceIncome == "unemployment benefits") %>%
  group_by(vote) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())

table(df$sourceIncome)

# unemployment benefits 29
# income support 14
# invalidity/ sickness/ disabled pension 11

df$lowinc <- factor(
    ifelse(df$sourceIncome == "unemployment benefits" |
             df$sourceIncome == "income support" | 
             df$sourceIncome == "invalidity/ sickness/ disabled pension",
          "low income", "no low income"), 
    levels = c("low income", "no low income"))
  
table(df$lowinc, df$vote)

df %>% 
  filter(lowinc == "low income") %>%
  group_by(vote, treatment) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())

effect_by_lowinc <- 
  ggplot(data=subset(df, lowinc == "low income"),
         aes(x=treatment, 
             y=mps_weighted)) + # misperceptions_sur5 
  stat_summary(fun.y=mean,  
               geom="bar",
               fill=c("#474747", "#E2E2E2"), # "#7D87B9", "#BB7784"), # pal(2), # "White", # colour of the bars col=rainbow_hcl(2)
               color="Black") + # colour of the outline of it
  stat_summary(fun.data = mean_cl_normal, # function for CIs 
               geom="errorbar", # what type of geometrical object I want to add
               position="dodge", # forces things to not overlap (2 bars arent on top of each other)
               width=.2) + # how large you want them +
  cleanup +
  xlab("") +
  ylab("weighted misperceptions index")

# TOO FEW CASES TO DISTINGUISH BETWEEN LEAVE AND REMAIN


# persFinances_Worse  ###################################

levels(df$persFinances_Worse)

effect_by_persFinances_Worse <- # NOPE
  ggplot(data=subset(df, vote == "remain"),
         aes(x=treatment, 
             y=mps_weighted)) + # misperceptions_sur5 
  stat_summary(fun.y=mean,  
               geom="bar",
               fill=c("#474747", "#E2E2E2"), # "#7D87B9", "#BB7784"), # pal(2), # "White", # colour of the bars col=rainbow_hcl(2)
               color="Black") + # colour of the outline of it
  stat_summary(fun.data = mean_cl_normal, # function for CIs 
               geom="errorbar", # what type of geometrical object I want to add
               position="dodge", # forces things to not overlap (2 bars arent on top of each other)
               width=.2) + # how large you want them +
  cleanup +
  xlab("") +
  ylab("weighted misperceptions index")

table(df$persFinances_Worse, df$vote)

# lifeSatisfaction_d  ###################################

effect_by_lifeSatisfaction_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$lifeSatisfaction_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ lifeSatisfaction_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup


effect_by_lifeSatisfaction_sur3_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$lifeSatisfaction_sur3) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ lifeSatisfaction_sur3) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

# TEST FOR REMAIN

# econ_Worse  ###########################################

table(df$econ_Worse, df$vote) ## too few obs

effect_by_econ_Worse_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$econ_Worse) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ econ_Worse) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

# NEED JITTER PLOT!!



# persFinances_Worse  ###################################

table(df$persFinances_Worse, df$vote) ## too few obs for leave voters

effect_by_econ_Worse_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$persFinances_Worse) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ persFinances_Worse) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

# NEED JITTER PLOT!!

# TEST FOR REMAIN


# trust_D  ##############################################

table(df$trust_D, df$vote) # should work

effect_by_trust_D_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$trust_D) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ trust_D) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup


# lifeSatisfaction_d  ###################################

table(df$lifeSatisfaction_d, df$vote)  # should work

effect_by_lifeSatisfaction_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$lifeSatisfaction_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ lifeSatisfaction_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup


# selfEsteem_d  ###################################

df$selfEsteem_d

table(df$selfEsteem_d, df$vote)  # should work

effect_by_selfEsteem_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$selfEsteem_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ selfEsteem_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

table(df$selfEsteem_sur3, df$vote)

effect_by_selfEsteem_sur3_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$selfEsteem_sur3) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ selfEsteem_sur3) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup


# anxious_d  ##########################################

table(df$anxious_d, df$vote) # should work

effect_by_anxious_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$anxious_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ anxious_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

# neuroticism_d  #######################################

table(df$neuroticism_d, df$vote) # should work

effect_by_neuroticism_d_vote <- # USE THIS
  ggplot(data=subset(df, complete.cases(df$neuroticism_d) & vote %in% c("leave", "remain")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=vote)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  facet_grid(vote ~ neuroticism_d) + # facet_wrap(~SSS) + 
  scale_fill_manual(name="Vote",
                    labels=c("leave", "remain"),
                    values=c("#BB7784", "#7D87B9")) +
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

# NO IMPACT




# others  ###############################################

table(df$sourceIncome) # too few unemployed leave voters
table(df$sourceIncome, df$vote) 

round(prop.table(table(df$sourceIncome, df$vote), 2), 2)





# unemployment  #########################################

  effect_by_unemployment <- # DO THIS
  effect_by_anxiety <- # DO THIS
  effect_by_neuroticism <- # DO THIS
  effect_by_life_satisfaction
  
  # Fear of the future persFinances_Worse econ_Worse NHSBetter
  # Personal finances , education (or other levels of low status) selfEsteem
  # trust 
  # Unemployment # Neuroticism... maybe needToEvaluate_f needforCognition curiosity gutDecision basedOnFacts anxious happy

  
effect_by_Gender <- # USE THIS
  ggplot(data=subset(df, gender%in% c("male", "female")), 
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D
             fill=gender)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Vote",
                    labels=c("male", "female"),
                    values=c("#474747", "#E2E2E2")) + #"#BB7784", "#7D87B9")) + # "#E2E2E2",  # c("Grey40", "Grey")) +
  facet_wrap(~gender) + 
  theme(legend.position="none") + # Drop legend
  xlab("") +
  ylab("weighted misperceptions index") +
  cleanup

effect_by_Gender <- # WORKS BUT MEN WOMEN NEXT TO EACH OTHER
  ggplot(data=subset(df, complete.cases(df$gender)),
         aes(x=treatment, 
             y=mps_weighted, # misperceptions_D_noQ misperceptions_sur5
             fill=gender)) + 
  stat_summary(fun.y=mean,
               geom="bar",
               color="Black", # colour of the outline of it
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position=position_dodge(width = .9), # need this when you have 2 IVs
               width=.2) +
  scale_fill_manual(name="Gender",
                    labels=c("Men", "Women"),
                    values=c("#7D87B9", "#BB7784")) + # c("Grey40", "Grey")) +
  cleanup



#########################################################

# MAIN ANALYSES FOR RESEARCH NOTE

#########################################################

# time spent

table(df$treatment, df$mp_SUBMIT)
hist(subset(df, mp_SUBMIT<320 & vote == "leave")$mp_SUBMIT)
hist(subset(df, mp_SUBMIT<180 & vote == "leave")$mp_SUBMIT)
hist(subset(df, mp_SUBMIT<320)$mp_SUBMIT)

# How long did they spend?

df %>% 
  filter(mp_SUBMIT < 320) %>%
  group_by(treatment) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            mean_time = mean(mp_SUBMIT, na.rm=T),
            total = n())

# HIER WEITER

jitterPlot_time 

# means (using dplyr)

df %>% 
  filter(vote == "leave" | vote == "remain") %>%
  group_by(treatment, vote) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())

# control leave 7.695652
# stress leave 7.730769



# t-tests ###############################################

# NEW

t.test(m5_Queen_num ~ treatment, data=subset(df, vote=="leave")) 
t.test(m5_Queen_num ~ treatment, data=subset(df, vote=="remain")) 


# TIME

# All subjects 
model_time <- t.test(mp_SUBMIT ~ treatment, data=subset(df, mp_SUBMIT < 320))
model_time # excluding 2 people who spent more than 5 min
# t = 2.4044, df = 288.34, p-value = 0.01683 # SIGNIFICANT! 
# mean in group control: 57.09264
# mean in group stress: 49.71181

# leave voters
model_time_L <- t.test(mp_SUBMIT ~ treatment, data=subset(df, mp_SUBMIT < 320 & vote == "leave"))
model_time_L
# t = 2.3592, df = 83.395, p-value = 0.02065  # SIGNIFICANT! 
# mean in group control 64.55849
# mean in group stress 53.40691

# remain voters
model_time_R <- t.test(mp_SUBMIT ~ treatment, data=subset(df, mp_SUBMIT < 320 & vote == "remain"))
model_time_R # n.s.


# IN COMMON

model_common_L <- t.test(inCommon_Remainers ~ treatment, 
                       data=subset(df, vote == "leave"))
model_common_L # not excluding people who spent forever -- n.s.
# t = 1.7424, df = 92.916, p-value = 0.08474
# mean in group control 6.666667
# mean in group stress 5.823529

model_common_R <- t.test(inCommon_Brexiteers ~ treatment, 
                         data=subset(df, vote == "remain"))
model_common_R # n.s.

# leave voters and abstentions p-value = 0.06479
model_common_L <- t.test(inCommon_Remainers ~ treatment, 
                         data=subset(df, vote != "remain"))
model_common_L 



# SSS ###################################################

# SSS (low=1-4, medium=5-7, high= 8-10) -- 29
## SSS_d (low=1-5, high=6-10) -- 51

table(df$SSS, df$vote)
table(df$SSS_d, df$vote)

# SSS  ##################################################

# How many low SSS leave voters? 29

df %>% 
  filter(vote == "leave") %>%
  group_by(SSS) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())

# a) All subjects 

# (low SSS) 
model_SSS_low <- t.test(mps_weighted ~ treatment, data=subset(df, SSS=="low")) 
model_SSS_low
# t = -1.8413, df = 83.594, p-value = 0.06912 # ALMOST

model_SSS_low <- t.test(mps ~ treatment, data=subset(df, SSS=="low")) 
model_SSS_low
# t = -1.9263, df = 84.13, p-value = 0.05744 # closer!!

# (high SSS) 
model_SSS_high <- t.test(mps_weighted ~ treatment, data=subset(df, SSS=="high")) 
model_SSS_high # n.s.

model_SSS_high <- t.test(mps ~ treatment, data=subset(df, SSS=="high")) 
model_SSS_high # n.s.

# Time 
time_SSS_low <- t.test(mp_SUBMIT ~ treatment, data=subset(df, SSS=="low" & mp_SUBMIT < 320))
time_SSS_low # SIGNIFICANT 
# t = 2.3071, df = 55.6, p-value = 0.0248
# mean in group control 64.41258
# mean in group stress 46.03993


# b) Remainers  #########################################

# (low SSS) 
model_SSS_low <- t.test(mps_weighted ~ treatment, data=subset(df, SSS=="low" & vote == "remain")) 
model_SSS_low
# t = -2.0538, df = 25.779, p-value = 0.05027 # SIGNIFICANT 
# mean in group control: 0.75 
# mean in group stress: 2.00 

model_SSS_low <- t.test(mps ~ treatment, data=subset(df, SSS=="low" & vote == "remain")) 
model_SSS_low # similarly significant, p-value = 0.05199

# (high SSS) 
model_SSS_low <- t.test(mps_weighted ~ treatment, data=subset(df, SSS=="high" & vote == "remain")) 
model_SSS_low
# t = -2.0828, df = 22.289, p-value = 0.04895 # SIGNIFICANT



# c) Brexiteers  #########################################

# (low SSS) 
model_SSS_low <- t.test(mps ~ treatment, data=subset(df, SSS=="low" & vote == "leave")) 
model_SSS_low # n.s. AT ALL

# (high SSS) 
model_SSS_low <- t.test(mps ~ treatment, data=subset(df, SSS=="high" & vote == "leave")) 
model_SSS_low # n.s. AT ALL


# FAZIT

# low and high SSS do worse when stressed (medium SSS not affected)
# This is true for the dataset that includes both leave and remain
# and for the sample including only remain voters 

# --> EVEN AMONG THOSE WHO HAD NO MOTIVATION TO BELIEVE IN THIS, 
# LOW SSS IS BAD!!

# Interestingly, HIGH SSS is also bad!

# QUESTION: mechanism -- is this because they spend less time? 
# Looks like it... 



# SSS_d  ################################################

# How many low SSS_d leave voters? 25+26

df %>% 
  group_by(vote, SSS_d) %>%
  summarise(mean_mps = mean(mps_weighted, na.rm=T),
            total = n())


# a) All subjects #######################################

# (low SSS_d) 
model_SSS_d_low <- t.test(mps ~ treatment, data=subset(df, SSS_d=="low")) 
model_SSS_d_low # almost significant

# t = -1.9111, df = 126.49, p-value = 0.05826
# mean in group control 3.162162
# mean in group stress 4.538462


# Time
time_SSS_d_low <- t.test(mp_SUBMIT ~ treatment, data=subset(df, SSS_d=="low" & mp_SUBMIT < 320))
time_SSS_d_low # SIGNIFICANT 
# t = 3.013, df = 98.854, p-value = 0.003285
# mean in group control 62.63149 
# mean in group stress 47.00857 


# b) Remainers  #########################################

# (low SSS_d) 
model_SSS_d_low_R <- t.test(mps ~ treatment, data=subset(df, SSS_d=="low" & vote == "remain")) 
model_SSS_d_low_R # n.s. 
# t = -1.7707, df = 42.774, p-value = 0.08374
# mean in group control 0.9767442
# mean in group stress 1.8709677  

# Time 
time_SSS_d_low_R <- t.test(mp_SUBMIT ~ treatment, data=subset(df, vote == "remain" & SSS_d=="low" & mp_SUBMIT < 320))
time_SSS_d_low_R # SIGNIFICANT 
# t = 2.4164, df = 50.011, p-value = 0.01937
# mean in group control 64.51819 
# mean in group stress 45.72060 

# In Common
common_SSS_d_low_R <- t.test(inCommon_Brexiteers ~ treatment, 
                             data=subset(df, SSS_d=="low" & vote == "remain"))
common_SSS_d_low_R  # n.s.


# c) Brexiteers  ########################################

# SSS_d #################################################

table(df$vote, df$SSS_d)

# (low SSS_d) 
model_SSS_d_low_L <- t.test(mps ~ treatment, data=subset(df, SSS_d=="low" & vote == "leave")) 
model_SSS_d_low_L # n.s.
# t = -0.99742, df = 47.458, p-value = 0.3236
# mean in group control 6.92
# mean in group stress 8.12 

# Time
time_SSS_d_low <- t.test(mp_SUBMIT ~ treatment, data=subset(df, SSS_d=="low" & vote == "leave"))
time_SSS_d_low # n.s.

# In Common
common_SSS_d_low_L <- t.test(inCommon_Brexiteers ~ treatment, 
                             data=subset(df, SSS_d=="low" & vote == "leave"))
common_SSS_d_low_L  # n.s.


# (high SSS_d) 
model_SSS_high_L <- t.test(mps_weighted ~ treatment, data=subset(df, SSS_d=="high" & vote == "leave")) 
model_SSS_high_L # n.s.




# lowinc #############################################

# How many low income voters? 54

df %>% 
  filter(lowinc == "low income") %>% 
  summarise(total = n())

# How many low lowinc leave voters? 2 leave voters 7 remain voters

df %>% 
  filter(lowinc == "low income") %>% 
  group_by(vote) %>%
  summarise(total = n())
# control    low  8.080000
# stress    low  9.840000


# ALL 
model_lowinc <- 
  t.test(mps ~ treatment, data=subset(df, lowinc=="low income")) 
model_lowinc # n.s.
# t = -1.9215, df = 49.879, p-value = 0.06039
# mean in group control 1.958333
# mean in group stress 3.600000

# Time
time_lowinc <- t.test(mp_SUBMIT ~ treatment, data=subset(df, lowinc=="low income"))
time_lowinc # n.s.

time_lowinc_L <- t.test(mp_SUBMIT ~ treatment, data=subset(df, lowinc=="low income" & vote == "leave"))
time_lowinc_L # SIGNIFICANT!But only 9 people
# t = 3.1726, df = 5.3658, p-value = 0.02244

table(df$vote, df$lowinc)

time_lowinc_R <- t.test(mp_SUBMIT ~ treatment, data=subset(df, lowinc=="low income" & vote == "remain"))
time_lowinc_R # n.s.

# In Common
common_SSS_d_low_L <- t.test(inCommon_Brexiteers ~ treatment, 
                             data=subset(df, SSS_d=="low" & vote == "leave"))
common_SSS_d_low_L  # n.s.


# neuroticism ########################################

df$neuroticism_index
hist(df$neuroticism_index)

# high in neuroticism
# 5, 6, or 7 (very much) on anxious, easily upset (neuroticism)
# AND 1,2,3 (not at all) on calm, emotionally (neuroticism_R)

str(df$neuroticism)
df$neuroticism_R # not reverse coded (neuroticism_r is reverse coded)

df$highneur <- ifelse(df$neuroticism == 5:7 &
                        df$neuroticism_R == 1:3,
                      1, 0)

table(df$highneur, df$vote) # only 2!!



# selfEsteem_d #######################################

table(df$selfEsteem_d, df$vote)

# ALL
model_selfEsteem_d_low <- t.test(mps_weighted ~ treatment, data=subset(df, selfEsteem_d=="low/medium")) 
model_selfEsteem_d_low # ALMOST SIGNIFICANT
# t = -1.9238, df = 116.7, p-value = 0.05682
# mean in group control: 3.192982  
# mean in group stress: 4.725806 

model_selfEsteem_d_low <- t.test(mps ~ treatment, data=subset(df, selfEsteem_d=="low/medium")) 
model_selfEsteem_d_low # p-value = 0.0473

# LEAVE
model_selfEsteem_d_low_L <- t.test(mps_weighted ~ treatment, data=subset(df, selfEsteem_d=="low/medium" & df$vote == "leave")) 
model_selfEsteem_d_low_L # n.s. AT ALL

# REMAIN
model_selfEsteem_d_low_R <- t.test(mps_weighted ~ treatment, data=subset(df, selfEsteem_d=="low/medium" & df$vote == "remain")) 
model_selfEsteem_d_low_R # n.s. but close
# t = -1.6949, df = 58.931, p-value = 0.09537
# mean in group control: 1.435897 
# mean in group stress: 2.514286 





# persFinances_Worse #######################################

levels(df$persFinances_Worse)

# ALL
model_persFinances_Worse <- t.test(mps_weighted ~ treatment, data=subset(df, persFinances_Worse=="will get worse")) 
model_persFinances_Worse # SIGNIFICANT
# t = -2.3104, df = 173.91, p-value = 0.02204
# mean in group control: 1.150538 
# mean in group stress: 1.949495

model_persFinances_Worse <- t.test(mps ~ treatment, data=subset(df, persFinances_Worse=="will get worse")) 
model_persFinances_Worse # SIGNIFICANT 
# t = -2.5563, df = 173.33, p-value = 0.01144
# mean in group control: 1.043011 
# mean in group stress: 1.828283
# LEAVE
model_econ_Worse_L <- t.test(mps_weighted ~ treatment, data=subset(df, persFinances_Worse=="will get worse" & df$vote == "leave")) 
model_econ_Worse_L # not enough observations!

# REMAIN
model_econ_Worse_R <- t.test(mps_weighted ~ treatment, data=subset(df, persFinances_Worse=="will get worse" & df$vote == "remain")) 
model_econ_Worse_R # CLOSE
# t = -1.8787, df = 149.04, p-value = 0.06223
# mean in group control: 0.9880952  
# mean in group stress: 1.5853659 

model_econ_Worse_R <- t.test(mps ~ treatment, data=subset(df, persFinances_Worse=="will get worse" & df$vote == "remain")) 
model_econ_Worse_R # p-value = 0.04046

# HIGHER levels of belief among stressed and fearful remain voters


# Time
time_persFinances_Worse <- t.test(mp_SUBMIT ~ treatment, data=subset(df, persFinances_Worse=="will get worse"))
time_persFinances_Worse # n.s.

time_persFinances_Worse_R <- t.test(mp_SUBMIT ~ treatment, data=subset(df, persFinances_Worse=="will get worse" & vote=="remain"))
time_persFinances_Worse_R # n.s.

table(df$persFinances_Worse, df$vote) 
# only 3 leave voters say it will get worse

# In Common
common_persFinances_Worse_R <- t.test(inCommon_Brexiteers ~ treatment, 
                              data=subset(df, persFinances_Worse=="will get worse" & vote == "remain"))
common_persFinances_Worse_R  # n.s.


# How many leave voters
table(df$vote)
table(df$vote, df$lowinc)
table(df$vote, df$persFinances_Worse)
table(df$vote, df$SSS)
table(df$vote, df$SSS_d)

# trust_D #######################################

# ALL
model_trust_D <- t.test(mps_weighted ~ treatment, data=subset(df, trust_D=="no trust")) 
model_trust_D # n.s.

model_trust_D <- t.test(mps ~ treatment, data=subset(df, trust_D=="no trust")) 
model_trust_D # n.s. p-value = 0.07794

# LEAVE
model_trust_D_L <- t.test(mps_weighted ~ treatment, data=subset(df, trust_D=="no trust" & df$vote == "leave")) 
model_trust_D_L # n.s.

# REMAIN
model_trust_D_R <- t.test(mps_weighted ~ treatment, data=subset(df, trust_D=="no trust" & df$vote == "remain")) 
model_trust_D_R # SIGNIFICANT!
# t = -1.9787, df = 104.04, p-value = 0.05049
# mean in group control: 0.9506173  
# mean in group stress: 1.6666667

model_trust_D_R <- t.test(mps ~ treatment, data=subset(df, trust_D=="no trust" & df$vote == "remain")) 
model_trust_D_R
# t = -2.0785, df = 105.68, p-value = 0.04008
# mean in group control: 0.8888889  
# mean in group stress:  1.5942029

# Stressed remain voters did worse!


# selfEsteem_d #############################################

table(df$selfEsteem_d, df$vote)

# ALL
model_selfEsteem_d_low <- t.test(mps_weighted ~ treatment, data=subset(df, selfEsteem_d=="low/medium")) 
model_selfEsteem_d_low # ALMOST SIGNIFICANT
# t = -1.9238, df = 116.7, p-value = 0.05682
# mean in group control: 3.192982  
# mean in group stress: 4.725806 

model_selfEsteem_d_low <- t.test(mps ~ treatment, data=subset(df, selfEsteem_d=="low/medium")) 
model_selfEsteem_d_low # SIGNIFICANT!!
# t = -2.0047, df = 117, p-value = 0.0473
# mean in group control 2.754386
# mean in group stress 4.064516

# LEAVE
model_selfEsteem_d_low_L <- t.test(mps_weighted ~ treatment, data=subset(df, selfEsteem_d=="low/medium" & df$vote == "leave")) 
model_selfEsteem_d_low_L # n.s.

# REMAIN
model_selfEsteem_d_low_R <- t.test(mps ~ treatment, data=subset(df, selfEsteem_d=="low/medium" & df$vote == "remain")) 
model_selfEsteem_d_low_R # SIGNIFICANT!
# t = -2.0805, df = 53.358, p-value = 0.0423
# mean in group control: 1.230769 
# mean in group stress: 2.400000 

# Time
# All subjects (low/medium self-esteem)
model_selfesteem_d_TIME <- t.test(mp_SUBMIT ~ treatment, data=subset(df, selfEsteem_d=="low/medium" & df$mp_SUBMIT < 400)) 
model_selfesteem_d_TIME # n.s.

# All subjects (medium self-esteem)
model_selfesteem_sur3_med_TIME <- t.test(mp_SUBMIT ~ treatment, data=subset(df, selfEsteem_sur3=="medium" & df$mp_SUBMIT < 400)) 
model_selfesteem_sur3_med_TIME 
# t = 2.411, df = 76.368, p-value = 0.01831 SIGNIFICANT!
# medium self-esteem people spend stat. sign. less time when stressed! 



# econ_Worse #######################################

levels(df$econ_Worse)

# ALL
model_econ_Worse <- t.test(mps_weighted ~ treatment, data=subset(df, econ_Worse=="Economy will get worse")) 
model_econ_Worse # n.s. 

# Time
time_econ_Worse <- t.test(mp_SUBMIT ~ treatment, data=subset(df, econ_Worse=="Economy will get worse"))
time_econ_Worse # n.s.


# LEAVE
model_econ_Worse_L <- t.test(mps_weighted ~ treatment, data=subset(df, econ_Worse=="Economy will get worse" & df$vote == "leave")) 
model_econ_Worse_L # n.s. almost nobody in there

# REMAIN
model_econ_Worse_R <- t.test(mps_weighted ~ treatment, data=subset(df, econ_Worse=="Economy will get worse" & df$vote == "remain")) 
model_econ_Worse_R # n.s. 

# In Common
common_econ_Worse_R <- t.test(inCommon_Brexiteers ~ treatment, 
                              data=subset(df, econ_Worse=="Economy will get worse" & vote == "remain"))
common_econ_Worse_R  # n.s.


# lifeSatisfaction_d #######################################

# ALL
model_lifeSatisfaction_d <- t.test(mps_weighted ~ treatment, data=subset(df, lifeSatisfaction_d=="not satisfied")) 
model_lifeSatisfaction_d # n.s.

model_lifeSatisfaction_d <- t.test(mps ~ treatment, data=subset(df, lifeSatisfaction_d=="not satisfied")) 
model_lifeSatisfaction_d # n.s.

# LEAVE
model_lifeSatisfaction_d_L <- t.test(mps_weighted ~ treatment, data=subset(df, lifeSatisfaction_d=="not satisfied" & df$vote == "leave")) 
model_lifeSatisfaction_d_L # n.s.

# REMAIN
model_lifeSatisfaction_d_R <- t.test(mps_weighted ~ treatment, data=subset(df, lifeSatisfaction_d=="not satisfied" & df$vote == "remain")) 
model_lifeSatisfaction_d_R # n.s.

# Remain voters low in life Satisfaction did worse!

# Time

# All subjects (not satisfied)
model_lifeSatisfaction_d_TIME <- t.test(mp_SUBMIT ~ treatment, data=subset(df, lifeSatisfaction_d=="not satisfied" & df$mp_SUBMIT < 400)) 
model_lifeSatisfaction_d_TIME 
# t = 2.103, df = 41.826, p-value = 0.04153 SIGNIFICANT!
# All subjects: not satisfied people spend less time on mp items when stressed



# age ###################################################

# All subjects 
model_ageGroup3 <- t.test(mps ~ treatment, data=subset(df, ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3 # almost SIGNIFICANT p-value = 0.06084
# mean in group control 3.041667
# mean in group stress 4.861111 

# Remain
model_ageGroup3_R <- t.test(mps ~ treatment, data=subset(df, vote == "remain" & ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3_R # SIGNIFICANT but n = 16
# t = -2.135, df = 16.112, p-value = 0.04846

# Remain
model_ageGroup3_L <- t.test(mps ~ treatment, data=subset(df, vote == "leave" & ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3_L # n.s.

# Older people do worse under stress


# education ##############################################

df$education_d

# All subjects 
model_education_d <- t.test(mps ~ treatment, data=subset(df, education_d=="uni")) 
model_education_d # n.s.

# REMAIN
model_education_d_R <- t.test(mps ~ treatment, data=subset(df, vote == "remain" & education_d=="uni")) 
model_education_d_R # n.s.








#########################################################
# Correlation Matrix
#########################################################

# Are low SSS, low self-esteem, and low life satisfaction the same people? 

# Look at data
df[ which(df$SSS=="low") , c("SSS", "selfEsteem_sur3", "lifeSatisfaction_d")]
df[ which(df$lifeSatisfaction_d == "not satisfied") , c("SSS", "selfEsteem_sur3", "lifeSatisfaction_d")]
df[ which(df$selfEsteem_sur3 == "low") , c("SSS", "selfEsteem_sur3", "lifeSatisfaction_d")]

# Same thing using subset
subset(df, lifeSatisfaction_d == "not satisfied", select=c("SSS", "selfEsteem_sur3", "lifeSatisfaction_d"))

# Simple correlations
cor.test(df$subjSocialStatus, df$selfEsteem) # 0.3043816
cor.test(df$subjSocialStatus, df$lifeSatisfaction_num_rev) # 0.3832344
cor.test(df$lifeSatisfaction_num_rev, df$selfEsteem) # 0.349554

cor.test(df$subjSocialStatus, as.numeric(df$trust)) # 0.1821831 -- very weak
# t = 3.6824, df = 395, p-value = 0.0002631

# --> weak/medium correlations Pearson's r = .3 to .37 
# --> QUESTION FOR ROB: IS THIS ENOUGH TO SAY THEY ARE CORRELATED?



# Correlation Matrix 
df$trust_num <- as.numeric(df$trust_D)
vars <- subset(df, select=c("subjSocialStatus", "selfEsteem", "lifeSatisfaction_num_rev", "trust_num"))
vars <- vars[complete.cases(vars), ]
cor(vars)


# Plot correlations
pairs(vars)
# SSS and life satisfaction and SSS and self esteem seem to correlate (somewhat)


# Export correlation matrix to latex
# http://myowelt.blogspot.co.uk/2008/04/beautiful-correlation-tables-in-r.html


corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  # mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " "))) # too harsh
  mystars <- ifelse(p < .01, "***", ifelse(p < .05, "** ", ifelse(p < .1, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}


library(xtable)
xtable(corstarsl( subset(df, select=c("subjSocialStatus", 
                                      "selfEsteem", 
                                      "lifeSatisfaction_num_rev",
                                      "trust_num"))))












#########################################################
# Age Group
#########################################################

plot(df$ageGroup3)

jitterPlot_ageGroup3_LR <- 
  ggplot(subset(df, vote != "not vote" & complete.cases(ageGroup3)), #  vote %in% c("remain") &
         aes(x = ageGroup3, # treatment # --> levels of x control point colors
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) +  
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .6), # how far the - are from each other
               size = 30, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Age Group",
    subtitle = "Leave and Remain Voters") + # Leave & Remain Voters #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 



jitterPlot_ageGroup3 <- 
  ggplot(subset(df, vote != "not vote" & complete.cases(ageGroup3)), #  vote %in% c("remain") &
         aes(x = ageGroup3, # treatment # --> levels of x control point colors
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) +  
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  # facet_wrap(~ vote,
  #            nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .6), # how far the - are from each other
               size = 50, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Age Group",
    subtitle = "Leave and Remain Voters") + # Leave & Remain Voters #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 




# age ###################################################

# All subjects 
model_ageGroup3 <- t.test(mps ~ treatment, data=subset(df, ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3 # almost SIGNIFICANT p-value = 0.06084
# mean in group control 3.041667
# mean in group stress 4.861111 

# Remain
model_ageGroup3_R <- t.test(mps ~ treatment, data=subset(df, vote == "remain" & ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3_R # SIGNIFICANT but n = 16
# t = -2.135, df = 16.112, p-value = 0.04846

# Remain
model_ageGroup3_L <- t.test(mps ~ treatment, data=subset(df, vote == "leave" & ageGroup_sur3=="50s/60s/70s")) 
model_ageGroup3_L # n.s.

# Older people do worse under stress


# education ##############################################

df$education_d

# All subjects 
model_education_d <- t.test(mps ~ treatment, data=subset(df, education_d=="uni")) 
model_education_d # n.s.

# REMAIN
model_education_d_R <- t.test(mps ~ treatment, data=subset(df, vote == "remain" & education_d=="uni")) 
model_education_d_R # n.s.



#########################################################

# Knowledge

#########################################################

# Using the stressed / unstressed answers to the EU questions
# as a proxy for knowledge.

df$ctl_EUknowledge
df$stress_EUknowledge

# EUknowledge
df$EUknowledge <- ifelse(complete.cases(df$ctl_EUknowledge), df$ctl_EUknowledge, 
                         df$stress_EUknowledge)
hist(df$EUknowledge)

# EUknowledge_f
df$EUknowledge_f <- factor(ifelse(df$EUknowledge == 1, "low",
                          ifelse(df$EUknowledge == 2, "medium", 
                          ifelse(df$EUknowledge == 3, "high", NA))))
                                  
plot(df$EUknowledge_f)

table(df$EUknowledge_f, df$vote)
round(prop.table(table(df$EUknowledge_f, df$vote), 2), 2)
# --> HIGHLY EDUCATED SAMPLE!!

# EUknowledge_d
df$EUknowledge_d <- factor(ifelse(df$EUknowledge == 3, "high", "low"))
str(df$EUknowledge_d)
plot(df$EUknowledge_d)

jitterPlot_EUknowledge_d <- 
  ggplot(subset(df, complete.cases(EUknowledge_d) & vote %in% c("remain", "leave")),
         aes(x = EUknowledge_d,
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) +  
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .6), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Knowledge \n -- (high=answered all 3 questions correctly, low=1 or 2 mistakes)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by EU Knowledge",
    subtitle = "Leave and Remain Voters") + # Leave & Remain Voters #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 






#########################################################

# Need to Evaluate

#########################################################

plot(df$needToEvaluate_f)
plot(df$needToEvaluate_sur3)
plot(df$needToEvaluate_d)

# All subjects (low need to evaluate) 
model_needToEvaluate_low <- 
  t.test(mps ~ treatment, data=subset(df, needToEvaluate_sur3=="(4,9]")) 
model_needToEvaluate_low # n.s.

# All subjects (high need to evaluate) 
model_needToEvaluate_high <- 
  t.test(mps ~ treatment, data=subset(df, needToEvaluate_sur3=="(11,17]")) 
model_needToEvaluate_high # n.s.


# Leave voters (low need to evaluate) 
model_needToEvaluate_low_L <- 
  t.test(misperceptions_sur5 ~ treatment, data=subset(df, vote == "leave" & needToEvaluate_sur3=="(4,9]")) 
model_needToEvaluate_low_L # n.s.

# Leave voters (high need to evaluate) 
model_needToEvaluate_high_L <- 
  t.test(misperceptions_sur5 ~ treatment, data=subset(df, vote == "leave" & needToEvaluate_sur3=="(11,17]")) 
model_needToEvaluate_high_L # n.s.



#########################################################

# Need for Cognition

#########################################################

plot(df$needforCognition)
plot(df$needforCognition_sur3)
plot(df$needforCognition_d)

# All subjects (high need for cognition) 
model_needforCognition_sur3_high <- 
  t.test(misperceptions_sur5 ~ treatment, data = subset(df, needforCognition_sur3=="(11,17]")) 
model_needforCognition_sur3_high # n.s.

model_needforCognition_sur3_high <- 
  t.test(misperceptions_weighted ~ treatment, data = subset(df, needforCognition_sur3=="(11,17]")) 
model_needforCognition_sur3_high # n.s.



#########################################################
# Trust
#########################################################

df$trust_D

# trust_D
jitterPlot_trust_D_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(trust_D)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = trust_D, 
             y = mps, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
              nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Trust \n (self-reported, one item)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Trust",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 


# All subjects (low/medium self-esteem)
model_trust_D <- t.test(mps ~ treatment, data=subset(df, trust_D=="no trust")) 
model_trust_D # n.s. but close p-value = 0.07794



#########################################################
# Big 5 - neuroticism
#########################################################

plot(df$neuroticism_sur3)
plot(df$neuroticism_d)

# neuroticism_d
jitterPlot_neuroticism_d_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(neuroticism_d)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = neuroticism_d, 
             y = mps, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Neuroticism \n (additive index, two items)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Neuroticism",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 

# All voters (high neuroticism) 
model_neuroticism_d_low <- 
  t.test(mps ~ treatment, data = subset(df, neuroticism_d == "(1,6]" )) 
model_neuroticism_d_low # n.s.

# Leave voters (high neuroticism) 
model_neuroticism_d_low_L <- 
  t.test(mps ~ treatment, data = subset(df, vote == "leave" & neuroticism_d == "(1,6]" )) 
model_neuroticism_d_low_L # n.s.




#########################################################
# curiosity
#########################################################

plot(df$curiosity_f)
plot(df$curiosity_sur3)


# curiosity_sur3
jitterPlot_curiosity_sur3_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(curiosity_sur3)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = curiosity_sur3, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Curiosity \n (additive index, two items)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Curiosity",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 


# curiosity_d
jitterPlot_curiosity_d_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(curiosity_d)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = curiosity_d, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Curiosity \n (additive index, two items)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Curiosity",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 



# curiosity_d
jitterPlot_curiosity_d <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(curiosity_d)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = curiosity_d, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  # facet_wrap(~ vote,
  #            nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 50, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "\n Curiosity \n (additive index, two items)",  # add a hard return
    y = "Misperception Scores \n" #,
  ) + 
  ggtitle(
    "Misperception Scores by Curiosity",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 

# FAZIT

# Seems that low curiosity leave voters are MORE affected by stress; 
# high curiosity leave voters are LESS affected by stress, but n.s.:


# 3) t-tests ############################################

levels(df$curiosity_d)

# All subjects (low curiosity) 
model_curiosity_d_low <- 
  t.test(mps ~ treatment, data = subset(df, curiosity_d == "(5,12]")) 
model_curiosity_d_low # SIGNIFICANT


# All subjects (high curiosity) 
model_curiosity_d_high <- 
  t.test(mps ~ treatment, data = subset(df, curiosity_d == "(12,20]")) 
model_curiosity_d_high # n.s.



#########################################################
# gut decision
#########################################################

plot(df$gutDecision_D)

# gutDecision_D
jitterPlot_gutDecision_D_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(gutDecision_D)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = gutDecision_D, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "",  # \n It was a gut decision
    y = "Misperception Scores \n" #, 
  ) + 
  ggtitle(
    "Misperception Scores by classifying vote as a gut decision",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 


plot(df$basedOnFacts_d)

# basedOnFacts_d
jitterPlot_basedOnFacts_d_LR <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(basedOnFacts_d)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = basedOnFacts_d, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  facet_wrap(~ vote,
             nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "",  # \n It was based on facts and thorough research
    y = "Misperception Scores \n" #, 
  ) + 
  ggtitle(
    "Misperception Scores by classification of vote as based on facts & research",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 


jitterPlot_basedOnFacts_d <-
  ggplot(subset(df, vote %in% c("leave", "remain") & complete.cases(basedOnFacts_d)), #  subset(df, complete.cases(selfEsteem_sur3))
         aes(x = basedOnFacts_d, 
             y = misperceptions_sur5, 
             fill = treatment,
             colour = treatment)) + 
  geom_jitter(
    aes(colour = treatment),
    position = position_jitterdodge(
      jitter.width = .5, # how far the dots are apart from each other
      jitter.height = .6,
      dodge.width = .8), # wie weit rot und grün ineinander über gehen
    size = 2, # how big the points are (1=tiny)
    alpha = .85) +
  # facet_wrap(~ vote,
  #            nrow = 1) +
  theme_bw() + # theme_classic # axis lines and no grid lines
  scale_color_manual(values=c("darkslategrey", "indianred4")) + 
  stat_summary(fun.y = mean,
               geom = "point",
               position = position_dodge(width = .9), # how far the - are from each other
               size = 40, # size mean bar
               shape = "-", # plot mean as "-"
               show.legend = F,
               alpha = .8)  +
  labs(
    x = "",  # \n It was based on facts and thorough research
    y = "Misperception Scores \n" #, 
  ) + 
  ggtitle(
    "Misperception Scores by classification of vote as based on facts & research",
    subtitle = "Leave & Remain Voters") + # All Subjects #\n (Experimental Stress v Control Condition)") +
  theme(legend.position = "top") + 
  theme(plot.title=element_text(
    size = 13,
    hjust = 0.5 # center main title
  )) + 
  theme(plot.subtitle=element_text(
    size = 11, 
    hjust = 0.5
  )) 

# FAZIT

# Seems that leave voters who say it was NOT a gut decision are less affected, but n.s.:



# 3) t-tests ############################################

# All subjects (not a gut decision) 
model_gutDecision_D_not <- 
  t.test(mps ~ treatment, data = subset(df, vote == "leave" & gutDecision_D == "not a gut decision")) 
model_gutDecision_D_not # n.s.

model_gutDecision_D_not <- 
  t.test(misperceptions_sur5 ~ treatment, data = subset(df, gutDecision_D == "not a gut decision")) 
model_gutDecision_D_not # n.s.







#########################################################

# Stress --> IN COMMON (plain)

#########################################################

# All subjects (low SSS) 
model_plain_InCOMMON_L <- t.test(inCommon_Remainers ~ treatment, data=subset(df, vote == "leave")) 
model_plain_InCOMMON_L # close to significance!
# t = 1.7424, df = 92.916, p-value = 0.08474
# mean in group control 6.666667
# mean in group stress 5.823529

model_plain_InCOMMON_R <- t.test(inCommon_Brexiteers ~ treatment, data=subset(df, vote == "remain")) 
model_plain_InCOMMON_R # nope





# ... ########################################## 

# HIER WEITER
  
# TO DO

# reliability of the index (Cronbachs alpha) DONE
# see if low SSS, low self-esteem, and low life satisfaction are the same people DONE
# Control for strength of attitudes CREATED STRENGTH OF CONVICTION VAR

# 3) Think about whats behind this -- why are low SSS etc affected? 
# 4) Think about how to bring this to a rep sample




# POTENTIAL MODERATORS ##################################

# Check how all these things affect reactions to stress
# (or if any of these moderate the effect of conviction --> misperceptions)


# Personality ###########################################

# Big 5
# Q36 Here are a number of personality traits that may or may not apply to you. Please say how well you feel each pair describes you.  
df$extraversion_index   
hist(df$extraversion_index)
df$conscientiousness_index 
df$openness_index
df$agreeableness_index
df$neuroticism_index

# Curiosity
df$curiosity

# Need for Cognition
df$needforCognition

# Need to evaluate
df$needToEvaluate

# self-esteem (Life control / powerlessness)
df$selfEsteem

# Q35 Generally speaking, would you say that most people can be trusted, or that you can't be too careful in dealing with people?
df$trust





#########################################################
# T-test interactions
#########################################################


#########################################################
# Subjective Social Status
#########################################################

table(df$SSS, df$vote)
table(df$SSS_d, df$vote)
truehist(df$subjSocialStatus)
plot(df$SSS)


# DV -- misperceptions_D ################################

# SSS ###################################################

model_SSS_low <- t.test(mps ~ treatment, data=subset(df, SSS=="low")) 
model_SSS_low # SIGNIFICANT! (almost)

model_SSS_medium <- t.test(mps ~ treatment, data=subset(df, SSS=="medium")) 
model_SSS_medium  # n.s.

model_SSS_high <- t.test(mps ~ treatment, data=subset(df, SSS=="high")) 
model_SSS_high # n.s.


# SSS_d #################################################

model_SSS_d_low <- t.test(mps ~ treatment, data=subset(df, SSS_d=="low")) 
model_SSS_d_low # significant (almost) p-value = 0.05826

model_SSS_d_high <- t.test(mps ~ treatment, data=subset(df, SSS_d=="high")) 
model_SSS_d_high # n.s.




#########################################################
# Explorative -- Correlational analyses (Vote Choice --> Misperceptions)
#########################################################

m1 <- lm(misperceptions_sur5 ~ voteLeave, data=df, na.action = na.omit)
summary(m1) # LARGE positive effect

m1 <- lm(misperceptions_sur5 ~ strengthConviction, data=df, na.action = na.omit)
summary(m1) # LARGE positive effect



#########################################################

# All in 

#########################################################

# Find if any of these personality variables moderate the relationship between
# vote choice & misperceptions 

m_all <- lm(misperceptions_sur5 ~ strengthConviction +  # voteLeave
              
              extraversion_index +                   
              conscientiousness_index +
              openness_index +
              agreeableness_index +
              neuroticism_index +
              
              voteLeave:extraversion_index +        
              voteLeave:conscientiousness_index +    
              voteLeave:openness_index +             # significant!
              voteLeave:agreeableness_index +        # significant!
              voteLeave:neuroticism_index +          # almost significant!
              
              subjSocialStatus +                     # significant!
              trust +
              selfEsteem +                           # almost significant!
              curiosity +
              needToEvaluate +
              needforCognition +
              
              voteLeave:subjSocialStatus +           # significant!
              voteLeave:trust +
              voteLeave:selfEsteem +                 # almost significant!
              voteLeave:curiosity +                  # significant!
              voteLeave:needToEvaluate +             # significant!
              voteLeave:needforCognition             # significant!
            ,
            data=df) # no.na.data
summary(m_all)

#########################################################


# Reduced model (playing / looking for control variables) ## USE THIS!!

m_some <- lm(misperceptions_sur5 ~ strengthConviction + # vote
               
               # extraversion_index +                   
               # conscientiousness_index +
               openness_index +                 # significant! (pos.)
               # agreeableness_index +
               # neuroticism_index +
               
               # vote:extraversion_index +        
               # vote:conscientiousness_index +    
               vote:openness_index +             # significant! (neg.)
               # vote:agreeableness_index +        
               # vote:neuroticism_index +          
               
               subjSocialStatus +                # significant! (neg.)
               # trust +
               selfEsteem +
               # curiosity +
               # needToEvaluate +
               # needforCognition +
               
               vote:subjSocialStatus +           # significant! (neg.)
               # vote:trust +
               vote:selfEsteem                  # almost significant! (neg.)
               # vote:curiosity +                  # significant!
               # vote:needToEvaluate          # significant!
               # vote:needforCognition             
             , 
             data=df)
summary(m_some)

# Fazit: significant or close to significance -- 

# subjSocialStatus -0.90
# voteremain:subjSocialStatus 1.04

# voteremain:selfEsteem (close) -0.60

# openness_index 0.33
# openness_index:voteremain (close) -0.37




#########################################################
# big5 (and controls)
#########################################################

m_big5 <- lm(misperceptions_sur5 ~ voteLeave + # strengthConviction + 
               
               extraversion_index +                   
               conscientiousness_index +
               openness_index +
               agreeableness_index +
               neuroticism_index +
               
               voteLeave:extraversion_index +        
               voteLeave:conscientiousness_index +
               voteLeave:openness_index +             # almost significant! (pos.)
               voteLeave:agreeableness_index +
               voteLeave:neuroticism_index +            # almost significant! (pos.)
               
               # gender +
               education_d + # education_d
               selfEsteem + 
               subjSocialStatus +
               openness_index + 
               neuroticism_index + 
               # voteLeave:education_d +
               voteLeave:selfEsteem + 
               voteLeave:subjSocialStatus +
               openness_index:voteremain +
               voteLeaveleave:neuroticism_index
             ,
             
             data=df, na.action = na.omit)
summary(m_big5) 

# FAZIT: education highly significant, nothing else




## export to latex #####################################

# library(stargazer)
# stargazer.table <- stargazer(m_all, m_some_voteLeave
#                              , single.row = T
#                              # , no.space=TRUE # übereinander, ohne leerzeile
#                              , font.size = "scriptsize"
#                              , dep.var.labels.include = FALSE
#                              , dep.var.caption  = "Misperceptions Score"
#                              , title="Regression results (Personality and misperception scores)"
#                              , out="models.txt") 





#########################################################
# Vote Choice --> Misperceptions (Situation as moderators)
#########################################################

# See if financial situation or fears of the future have an impact on susceptibility to misp


m_sit <- lm(misperceptions_sur5 ~ strengthConviction + # vote
               
              interest_whoWon + # insert sit variables
              
               openness_index +                 # significant! (pos.)
               vote:openness_index +             # significant! (neg.)
               subjSocialStatus +                # significant! (neg.)
               selfEsteem +
               vote:subjSocialStatus +           # significant! (neg.)
               vote:selfEsteem                # almost significant! (neg.)
          
             ,
             data=df)
summary(m_sit)

# check if any of these are significant
df$gutDecision
df$basedOnFacts
df$interest_EUref_f
df$interest_nextElection # turn into factor
df$interest_Euro2016 # turn into factor

# expectations (mildly interesting) -- turn all these into dummies / 3-level vars
df$change_immigration_f
df$change_terrorism_f
df$change_influence_f
df$change_econ_f
df$change_persFinance_f
df$change_persFinance_sur3
df$change_NHS_f

df$SecondRef_f

# demographics
df$gender
df$born
df$noChildren # turn into factor
df$maritalStatus # turn into factor
df$townVillage # turn into factor

df$LocationLatitude # TO DO: map this in QGIS =))
df$LocationLongitude




# R - How 2s

# How2 change variable names
# colnames(df)[colnames(df) == 'old.var.name'] <- 'new.var.name'
# names(df)[names(df) == 'old.var.name'] <- 'new.var.name'

# setnames(DF, "oldName", "newName")

# df[df$progress == "Progress", ] # rows, columns --> The "progress" level is the header line=)

# How2 rename factor levels
levels(x)[levels(x)=="beta"] <- "two"

# How2 replace values for character variables
df$twitterNameCorrect[df$twitterNameCorrect %in% ""] -> is.na

# How2 replace values for character variables
levels(junk$nm)[levels(junk$nm)=="B"] <- "b"

# How2 create factor vars
df$vote_index <- factor(df$vote_index, 
                        levels = c("die_hard_remain", "remain", "doubtful_remain", "did not vote", 
                                   "doubtful_leave", "leave", "die_hard_leave"))

df$vote_index[df$vote == "remain" & df$howSure_vote == 6] <- "die_hard_remain"



# How2drop levels
mydf <- droplevels(mydf)

# How2 Sort by date
billboard3 %>% arrange(date, rank)

# How2 find number of missings in one column
sum(is.na(df$vote))

# Look at ALL variables again ############################

# str(df, list.len=ncol(df))


# RESOURCES

# Colours
# http://www.sthda.com/english/wiki/ggplot2-colors-how-to-change-colors-automatically-and-manually
# http://docs.ggplot2.org/current/scale_brewer.html
# http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/

# nice R colours: 
# http://sape.inf.usi.ch/quick-reference/ggplot2/colour

# Jitter Plots
# http://docs.ggplot2.org/current/geom_jitter.html 

# legends
# http://is-r.tumblr.com/post/35557848297/troubleshooting-legends-in-ggplot
# http://www.sthda.com/english/wiki/ggplot2-legend-easy-steps-to-change-the-position-and-the-appearance-of-a-graph-legend-in-r-software

# Cheat sheet
# http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/#change-the-title-of-the-legend-scale...


# Merge twitter names variables

# post_ref$twee
# post_ref$twitter_name

# if twitterNameCorrect==1, then twitter_name
# if twitterNameCorrect==2 OR NA, then twitterName

post_ref$twitter_name <- as.character(post_ref$twitter_name)

post_ref$twitter <- ifelse(post_ref$twitterNameCorrect=="1", post_ref$twitter_name, post_ref$twitterName)
# Look into this again


# Easier way using library(plyr): 
# revalue(x, c("beta"="two", "gamma"="three"))
