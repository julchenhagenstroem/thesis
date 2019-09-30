#########################################################
# Brexit Twitter Study
# -- Data Management --
# -- 07 Aug 2017 -- 
#########################################################

rm(list = ls())
setwd("/Users/cstedtnitz/Dropbox/1.PhD/1. Papers/1. BrexitTwitterPaper/data") # MAC
# setwd("C:/Users/cmsted/Dropbox/1.PhD/1. Papers/1. BrexitTwitterPaper/data") # WINDOWS
# install.packages("foreign")
library(foreign) # for read.csv
# install.packages("foreign")
library(plyr)  # for plyr::rename AND revalue
# install.packages("dplyr")
library(dplyr) # for full_join 
# install.packages("reshape")
library(reshape) # for rename
# install.packages("MASS")
library(MASS) 
# install.packages("car")
library(car) # for recode
# install.packages("ggplot2")
library(ggplot2)
# install.packages("pastecs")
library(pastecs) # for descriptive stats
# install.packages("scales")
library(scales) # for nice histograms
# install.packages("stargazer")
library(stargazer) # for latex exports
# install.packages("ggthemes")
library(ggthemes)
# install.packages("xtable")
library(xtable) # to produce LaTex tables

# devtools::install_github('rich-iannone/DiagrammeR')
# library(DiagrammeR)
# http://rich-iannone.github.io/DiagrammeR/


#########################################################
# Import data sets
#########################################################

# pre_ref (42 responses)
pre_ref <- read.csv( "EU_ref_Twitter_PSYCH.csv", header=TRUE, na.strings=(c("", "-99")))  
nrow(pre_ref) 

# post_ref (382 responses)
post_ref <- read.csv( "EUREF_twitter_psych__POST__UvA_Copy.csv", header=TRUE, na.strings=(c("", "-99"))) 
nrow(post_ref) 

# direct_links (150 responses)
direct_links <- read.csv("EURefStudy2.csv", header=TRUE, na.strings=(c("", "-99")))
length(direct_links) 
nrow(direct_links) 

# non_psych (652 responses)
# non_psych <- read.csv("/Users/cstedtnitz/Desktop/BrexitTwitterPaper/data/EU ref Twitter NOT PSYCH - POST - UvA Copy_August 13, 2016_13.47.csv", 
#                      header=TRUE, na.strings="", stringsAsFactors = FALSE)

# lone_macs (2 responses)
# lone_macs <- read.csv("/Users/cstedtnitz/Desktop/BrexitTwitterPaper/data/EURefStudy2 - Copy_August 13, 2016_20.49.csv", 
#                      header=TRUE, stringsAsFactors = FALSE)
# one of these 2 is Vicky, don't bother



#########################################################
# Delete randomizer variables & labels row 
#########################################################

# How2 find the codebook in qualtrics: Tools --> Export survey to word

# How2 include randomizer variables in Qualtrics data set: 
# Data & Analysis --> Export Data --> Export data with legacy format --> "more options" 
# --> Tick "Export viewing order for randomized surveys"

# Show ALL variables -- look for "Display Order: Block Randomizer..."
# str(pre_ref, list.len=ncol(pre_ref) ) ## DO.BR.FL_30 and DO.BR.FL_33
# str(post_ref, list.len=ncol(post_ref)) ## DO.BR.FL_6 and DO.BR.FL_7
# str(direct_links, list.len=ncol(direct_links)) ## DO.BR.FL_6 and DO.BR.FL_7 

# pre_ref
# Find randomizer variables
which( colnames(pre_ref)=="DO.Q.Q72") # 152 (1st uninteresting var... display order etc.)
which( colnames(pre_ref)=="DO.Q.Q11") # 165 (last uninteresting var)
# Delete randomizer variables 
pre_ref <- pre_ref[, -c(152:165)]
# Delete first row (labels)
pre_ref <- pre_ref[-1 ,]

# post_ref
# Find randomizer variables
which( colnames(post_ref)=="DO.Q.Q46") # 134 (first uninteresting var)
which( colnames(post_ref)=="DO.Q.Q22") # 147 (last uninteresting var) 
# Delete randomizer variables 
post_ref <- post_ref[, -c(134:147) ]
# Delete first row (labels)
post_ref <- post_ref[-1 ,]

# direct_links
# Find randomizer variables
which( colnames(direct_links)=="DO.Q.Q46") # 136 (first uninteresting var)
which( colnames(direct_links)=="DO.Q.Q22") # 150 (last uninteresting var) 
# Delete randomizer variables 
direct_links <- direct_links[, -c(136:150) ]
# Delete first row (labels)
direct_links <- direct_links[-1 ,]

# Find row and column numbers for a given value
# levels(direct_links$Q9_1)
# which(direct_links == "Some say that there were good reasons for both options in this EU referendum. Others think it was...-Good reasons for <u>leaving</u>", arr.ind=TRUE)


#########################################################

# Prep to merge (post_ref and direct_links)

#########################################################

# Those participants invited through twitter are in the post_ref dataset. 
# I created a direct link that I sent to a few twitter users (I also shared it on facebook,
# and Joe Greenwood e-mailed it to some of his friends... unfortunately I cannot trace back
# the personal acquaintances.)

# I changed 3 things in the survey when I set up the direct_links copy: 
# a) The first question on Twitter names
# b) How would you have voted if you could have & how sure are you about that
# b) Added "ashamed" to the "how do you feel now" question. 

# Change the values so that the 2 datasets match again. 


# a) Twitter names

pre_ref <- plyr::rename(pre_ref, c(Q126="twitterNameCorrect"))
pre_ref <- plyr::rename(pre_ref, c(Q128="twitterName_writeIn"))
pre_ref <- plyr::rename(pre_ref, c(twee="twitterName"))

post_ref <- plyr::rename(post_ref, c(Q3="twitterNameCorrect"))
post_ref <- plyr::rename(post_ref, c(Q4="twitterName_writeIn"))
post_ref <- plyr::rename(post_ref, c(twee="twitterName"))

direct_links <- plyr::rename(direct_links, c(Q3="onTwitter"))
direct_links <- plyr::rename(direct_links, c(Q61="twitterName_writeIn"))
direct_links$twee <- NULL



# b) How would you have voted if you had had the chance? 

direct_links <- plyr::rename(direct_links, c(Q62="wouldHave_voted"))

# CONTINUE HERE -- SEE WHY THIS IS NOT WORKING
# object of type 'closure' is not subsettable

levels(direct_links$wouldHave_voted)[levels(direct_links$wouldHave_voted)=="1"] <- "leave"
levels(direct_links$wouldHave_voted)[levels(direct_links$wouldHave_voted)=="2"] <- "remain"
levels(direct_links$wouldHave_voted)[levels(direct_links$wouldHave_voted)=="3"] <- "don't know"
levels(direct_links$wouldHave_voted)[levels(direct_links$wouldHave_voted)=="4"] <- "not vote"

str(direct_links$wouldHave_voted)


# How sure?     

direct_links <- plyr::rename(direct_links, c(Q63_1="howSure_voteIfCould")) 

direct_links$howSure_voteIfCould <- as.numeric(direct_links$howSure_voteIfCould)


# c) How do you feel now?

levels(pre_ref$Q52_1) # just one question

post_ref <- plyr::rename(post_ref, c(Q60_1="happy"))
post_ref <- plyr::rename(post_ref, c(Q60_2="proud"))
post_ref <- plyr::rename(post_ref, c(Q60_3="surprised"))
post_ref <- plyr::rename(post_ref, c(Q60_4="anxious"))
post_ref <- plyr::rename(post_ref, c(Q60_5="angry"))
post_ref <- plyr::rename(post_ref, c(Q60_6="guilty")) 

direct_links <- plyr::rename(direct_links, c(Q60_1="happy"))
direct_links <- plyr::rename(direct_links, c(Q60_2="proud"))
direct_links <- plyr::rename(direct_links, c(Q60_3="surprised"))
direct_links <- plyr::rename(direct_links, c(Q60_4="anxious"))
direct_links <- plyr::rename(direct_links, c(Q60_5="angry"))
direct_links <- plyr::rename(direct_links, c(Q60_6="ashamed")) # ONLY for direct links
direct_links <- plyr::rename(direct_links, c(Q60_7="guilty")) 



# Rename all the variables in the 3 data sets so as to make it easier to
# compare the distributions of a key variable to see if the people who participated
# differ in any important aspects. 

# The code for the post_ref and the direct_links data set is copy and pasted, I 
# only changed the name of the data set. 



#########################################################

# # RENAME b) post_ref 

#########################################################


# Treatment group assignment ############################

# Imagination / friend / no friend

levels(post_ref$DO.BR.FL_6)[levels(post_ref$DO.BR.FL_6)=="Psychological stuff - control"] <- "no_friend"
levels(post_ref$DO.BR.FL_6)[levels(post_ref$DO.BR.FL_6)=="Psychological stuff - treatment 1  (friend)"] <- "friend"

post_ref <- plyr::rename(post_ref, c(DO.BR.FL_6="trGroup_friend")) # rename using plyr


#########################################################

# EU knowledge questions / stress / no stress

levels(post_ref$DO.BR.FL_7)[levels(post_ref$DO.BR.FL_7)=="Psychological stuff - EU knowledge questions (no stress)"] <- "control"
levels(post_ref$DO.BR.FL_7)[levels(post_ref$DO.BR.FL_7)=="Psychological stuff - EU knowledge questions (stress)"] <- "stress"

post_ref <- plyr::rename(post_ref, c(DO.BR.FL_7="treatment")) # treatment

# Leave geolocation as it is
# post_ref$LocationLatitude
# post_ref$LocationLongitude
# post_ref$LocationAccuracy


# First variables #######################################

post_ref <- plyr::rename(post_ref, c(V1="id")) # weirdly, not working
post_ref <- plyr::rename(post_ref, c(?..V1="id"))

post_ref$V2 <- NULL # Thanks for participating
post_ref$V3 <- NULL # Name (anonymous)
post_ref$V4 <- NULL # External data reference

post_ref <- plyr::rename(post_ref, c(V5="email"))

post_ref$V6 <- NULL # ipAddress 
post_ref$V7 <- NULL # status (SPAM!)

post_ref <- plyr::rename(post_ref, c(V8="startTime")) # startDate
post_ref <- plyr::rename(post_ref, c(V9="endTime")) # endDate
post_ref <- plyr::rename(post_ref, c(V10="finished"))  

post_ref$Q_URL <- NULL

# Questions #############################################

post_ref$Q2 <- NULL # Thanks for participating

post_ref <- plyr::rename(post_ref, c(Q5="liveCountry"))

# Referendum 

post_ref <- plyr::rename(post_ref, c(Q6="interest_EUref"))
post_ref <- plyr::rename(post_ref, c(Q7_1="interest_whoWon"))
post_ref <- plyr::rename(post_ref, c(Q7_2="interest_nextElection"))
post_ref <- plyr::rename(post_ref, c(Q7_3="interest_Euro2016"))

post_ref <- plyr::rename(post_ref, c(Q8_1="whatHappens_leave"))
post_ref <- plyr::rename(post_ref, c(Q8_2="whatHappens_remain"))

post_ref <- plyr::rename(post_ref, c(Q9_1="goodReasons_leave"))
post_ref <- plyr::rename(post_ref, c(Q9_2="goodReasons_remain"))
post_ref <- plyr::rename(post_ref, c(Q10_2="change_immigration"))

post_ref <- plyr::rename(post_ref, c(Q10_3="change_terrorism"))
post_ref <- plyr::rename(post_ref, c(Q10_4="change_influence"))
post_ref <- plyr::rename(post_ref, c(Q11_2="change_econ"))
post_ref <- plyr::rename(post_ref, c(Q11_3="change_persFinance"))
post_ref <- plyr::rename(post_ref, c(Q11_4="change_NHS"))

# Voted

post_ref <- plyr::rename(post_ref, c(Q12="registered"))
post_ref <- plyr::rename(post_ref, c(Q14="vote"))
post_ref <- plyr::rename(post_ref, c(Q16_1="howSure_vote")) # how sure were you about your ref vote choice?
# post_ref <- plyr::rename(post_ref, c(Q62="wouldHave_voted")) # If you had been eligible ONLY in direct links
post_ref <- plyr::rename(post_ref, c(Q17="whatMatteredMost"))

# SecondRef

post_ref <- plyr::rename(post_ref, c(Q20="SecondRef"))

# Time spent following news about politics

post_ref <- plyr::rename(post_ref, c(Q21_1="tv")) # during last 7 days
post_ref <- plyr::rename(post_ref, c(Q21_2="newspapers"))
post_ref <- plyr::rename(post_ref, c(Q21_3="radio"))
post_ref <- plyr::rename(post_ref, c(Q21_4="internet"))
post_ref <- plyr::rename(post_ref, c(Q21_5="talktoPeople"))

# Most important source of information

levels(post_ref$Q22_1) 

levels(post_ref$Q22_1)[1] <- "tv_4Ref"
levels(post_ref$Q22_1)[2] <- "radio_4Ref"
levels(post_ref$Q22_1)[3] <- "newspapers_4Ref"
levels(post_ref$Q22_1)[4] <- "onlinenews_4Ref"
levels(post_ref$Q22_1)[5] <- "twitter_4Ref"
levels(post_ref$Q22_1)[6] <- "facebook_4Ref"
levels(post_ref$Q22_1)[7] <- "blogs_4Ref"
post_ref <- plyr::rename(post_ref, c(Q22_1="mostImpSoure"))

# Second most important source of information

levels(post_ref$Q22_2)

levels(post_ref$Q22_2)[1] <- "tv_4Ref"
levels(post_ref$Q22_2)[2] <- "radio_4Ref"
levels(post_ref$Q22_2)[3] <- "newspapers_4Ref"
levels(post_ref$Q22_2)[4] <- "onlinenews_4Ref"
levels(post_ref$Q22_2)[5] <- "twitter_4Ref"
levels(post_ref$Q22_2)[6] <- "facebook_4Ref"
levels(post_ref$Q22_2)[7] <- "blogs_4Ref"
post_ref <- plyr::rename(post_ref, c(Q22_2="SecondmostImpSoure"))

# Twitter behaviour

post_ref <- plyr::rename(post_ref, c(Q23="tweetOnRef"))
post_ref <- plyr::rename(post_ref, c(Q24_1_1_TEXT="no_tweets"))
post_ref <- plyr::rename(post_ref, c(Q24_1_2_TEXT="no_retweets"))
post_ref <- plyr::rename(post_ref, c(Q25="tweetsPolitics"))

# Demographics

post_ref <- plyr::rename(post_ref, c(Q26="gender"))
post_ref <- plyr::rename(post_ref, c(Q27="born")) 
post_ref <- plyr::rename(post_ref, c(Q28="education"))
post_ref <- plyr::rename(post_ref, c(Q29_1="childrenUnder5"))
post_ref <- plyr::rename(post_ref, c(Q29_2="children5to15"))
post_ref <- plyr::rename(post_ref, c(Q29_3="children16+"))
post_ref <- plyr::rename(post_ref, c(Q29_4="noChildren"))
post_ref <- plyr::rename(post_ref, c(Q30="maritalStatus"))
post_ref <- plyr::rename(post_ref, c(Q31="townVillage")) 
post_ref <- plyr::rename(post_ref, c(Q32="sourceIncome"))
post_ref <- plyr::rename(post_ref, c(Q32_TEXT="otherIncome")) # merge!

# Psych / Big 5

post_ref <- plyr::rename(post_ref, c(Q35 = "trust")) # interpersonal trust

post_ref <- plyr::rename(post_ref, c(Q36_1="extraversion"))
post_ref <- plyr::rename(post_ref, c(Q36_2="agreeableness_R"))
post_ref <- plyr::rename(post_ref, c(Q36_3="conscientiousness"))
post_ref <- plyr::rename(post_ref, c(Q36_4="neuroticism"))
post_ref <- plyr::rename(post_ref, c(Q36_5="openness"))
post_ref <- plyr::rename(post_ref, c(Q37_6="extraversion_R"))
post_ref <- plyr::rename(post_ref, c(Q37_7="agreeableness"))
post_ref <- plyr::rename(post_ref, c(Q37_8="conscientiousness_R"))
post_ref <- plyr::rename(post_ref, c(Q37_9="neuroticism_R"))
post_ref <- plyr::rename(post_ref, c(Q37_10="openness_R")) 

# Accuracy goals

post_ref <- plyr::rename(post_ref, c(Q38_4="gutDecision"))
post_ref <- plyr::rename(post_ref, c(Q38_5="basedOnFacts"))
post_ref <- plyr::rename(post_ref, c(Q39_1="selfEsteem"))
post_ref <- plyr::rename(post_ref, c(Q40="subjSocialStatus"))
post_ref <- plyr::rename(post_ref, c(Q41="lifeSatisfaction"))

# Treatment 1 - Think Remain

post_ref <- plyr::rename(post_ref, c(Q42="t1_thinkRemain"))

post_ref <- plyr::rename(post_ref, c(Q43_1="thinkRemain_1ST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q43_2="thinkRemain_LASTCLICK"))
post_ref <- plyr::rename(post_ref, c(Q43_3 = "thinkRemain_SUBMIT"))
post_ref <- plyr::rename(post_ref, c(Q43_4 = "thinkRemain_CLICKCOUNT"))

# Treatment 1 - Think Leave

post_ref <- plyr::rename(post_ref, c(Q44="t1_thinkLeave"))

post_ref <- plyr::rename(post_ref, c(Q45_1="thinkLeave_1ST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q45_2="thinkLeave_LASTCLICK"))
post_ref <- plyr::rename(post_ref, c(Q45_3="thinkLeave_SUBMIT"))
post_ref <- plyr::rename(post_ref, c(Q45_4="thinkLeave_CLICKCOUNT"))

# Treatment 2 - EU Knowledge WITH timer 

post_ref <- plyr::rename(post_ref, c(Q33_9="stress_EU28"))
post_ref <- plyr::rename(post_ref, c(Q33_10="stress_MEPsElected"))
post_ref <- plyr::rename(post_ref, c(Q33_11="stress_CHinEU"))

post_ref <- plyr::rename(post_ref, c(Q34_1="stress_1ST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q34_2="stress_LAST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q34_3="stress_SUBMIT"))
post_ref <- plyr::rename(post_ref, c(Q34_4="stress_CLICKCOUNT"))

# Control - EU Knowledge WITHOUT timer

post_ref <- plyr::rename(post_ref, c(Q54_9="ctl_EU28")) 
post_ref <- plyr::rename(post_ref, c(Q54_10="ctl_MEPsElected"))
post_ref <- plyr::rename(post_ref, c(Q54_11="ctl_CHinEU"))

post_ref <- plyr::rename(post_ref, c(Q55_1="ctl_1ST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q55_2="ctl_LASTCLICK"))
post_ref <- plyr::rename(post_ref, c(Q55_3="ctl_SUBMIT"))
post_ref <- plyr::rename(post_ref, c(Q55_4="ctl_CLICKCOUNT"))

# Misperceptions

post_ref <- plyr::rename(post_ref, c(Q46_12="m1_Turkey"))
post_ref <- plyr::rename(post_ref, c(Q46_14="m2_Army"))
post_ref <- plyr::rename(post_ref, c(Q46_15="m3_NHS"))
post_ref <- plyr::rename(post_ref, c(Q46_25="m4_Euro"))
post_ref <- plyr::rename(post_ref, c(Q46_26="m5_Queen"))
post_ref <- plyr::rename(post_ref, c(Q46_33="poundPlunged"))
post_ref <- plyr::rename(post_ref, c(Q46_36="ScotlandRemain"))

post_ref <- plyr::rename(post_ref, c(Q47_1="mp_1ST_CLICK"))
post_ref <- plyr::rename(post_ref, c(Q47_2="mp_LASTCLICK"))
post_ref <- plyr::rename(post_ref, c(Q47_3="mp_SUBMIT"))
post_ref <- plyr::rename(post_ref, c(Q47_4="mp_CLICKCOUNT"))

# Is this the right time variable for the misperceptions questions?
# post_ref[1,] # YES


# In Common

post_ref <- plyr::rename(post_ref, c(Q50_2="inCommon_Brexiteers"))
post_ref <- plyr::rename(post_ref, c(Q50_3="inCommon_Remainers"))

post_ref <- plyr::rename(post_ref, c(Q51_1="curiosity1")) # merge / reverse!
post_ref <- plyr::rename(post_ref, c(Q51_2="curiosity2"))
post_ref <- plyr::rename(post_ref, c(Q51_3="curiosity3"))
post_ref <- plyr::rename(post_ref, c(Q51_4="curiosity4"))

post_ref <- plyr::rename(post_ref, c(Q51_5="needToEvaluate1")) # merge / reverse!
post_ref <- plyr::rename(post_ref, c(Q51_6="needToEvaluate2_R"))

post_ref <- plyr::rename(post_ref, c(Q52_4="needforCognition1")) # merge / reverse!
post_ref <- plyr::rename(post_ref, c(Q52_8="needforCognition2_R"))
post_ref <- plyr::rename(post_ref, c(Q52_5="needforCognition3"))
post_ref <- plyr::rename(post_ref, c(Q52_7="needforCognition4_R"))

post_ref <- plyr::rename(post_ref, c(Q52_11="needToEvaluate3_R")) 
post_ref <- plyr::rename(post_ref, c(Q52_12="needToEvaluate4"))

post_ref$Q56 <- NULL # "Now the next question is a little quiz."

post_ref$Q53 <- NULL # "That's the end of the survey."












#########################################################

# RENAME b) direct_links

#########################################################


# Treatment group assignment ############################

# Imagination / friend / no friend

levels(direct_links$DO.BR.FL_6)[levels(direct_links$DO.BR.FL_6)=="Psychological stuff - control"] <- "no_friend"
levels(direct_links$DO.BR.FL_6)[levels(direct_links$DO.BR.FL_6)=="Psychological stuff - treatment 1  (friend)"] <- "friend"

direct_links <- plyr::rename(direct_links, c(DO.BR.FL_6="trGroup_friend")) # rename using plyr


#########################################################


# EU knowledge questions / stress / no stress

levels(direct_links$DO.BR.FL_7)

levels(direct_links$DO.BR.FL_7)[levels(direct_links$DO.BR.FL_7)=="Psychological stuff - EU knowledge questions (no stress)"] <- "control"
levels(direct_links$DO.BR.FL_7)[levels(direct_links$DO.BR.FL_7)=="Psychological stuff - EU knowledge questions (stress)"] <- "stress"

direct_links <- plyr::rename(direct_links, c(DO.BR.FL_7="treatment")) # treatment

# Leave geolocation as it is

# direct_links$LocationLatitude
# direct_links$LocationLongitude
# direct_links$LocationAccuracy

# First variables #######################################

direct_links <- plyr::rename(direct_links, c(V1="id")) # weirdly, this did not work
direct_links <- plyr::rename(direct_links, c(?..V1="id"))

direct_links$V2 <- NULL # Thanks for participating
direct_links$V3 <- NULL # Name (anonymous)
direct_links$V4 <- NULL # External data reference

direct_links <- plyr::rename(direct_links, c(V5="email"))

direct_links$V6 <- NULL # ipAddress 
direct_links$V7 <- NULL # status (SPAM!)

direct_links <- plyr::rename(direct_links, c(V8="startTime")) # startDate
direct_links <- plyr::rename(direct_links, c(V9="endTime")) # endDate
direct_links <- plyr::rename(direct_links, c(V10="finished"))  

direct_links$Q_URL <- NULL

# Questions #############################################

direct_links$Q2 <- NULL # Thanks for participating

direct_links <- plyr::rename(direct_links, c(Q5="liveCountry"))

# Referendum 

direct_links <- plyr::rename(direct_links, c(Q6="interest_EUref"))
direct_links <- plyr::rename(direct_links, c(Q7_1="interest_whoWon"))
direct_links <- plyr::rename(direct_links, c(Q7_2="interest_nextElection"))
direct_links <- plyr::rename(direct_links, c(Q7_3="interest_Euro2016"))

direct_links <- plyr::rename(direct_links, c(Q8_1="whatHappens_leave"))
direct_links <- plyr::rename(direct_links, c(Q8_2="whatHappens_remain"))

direct_links <- plyr::rename(direct_links, c(Q9_1="goodReasons_leave"))
direct_links <- plyr::rename(direct_links, c(Q9_2="goodReasons_remain"))
direct_links <- plyr::rename(direct_links, c(Q10_2="change_immigration"))

direct_links <- plyr::rename(direct_links, c(Q10_3="change_terrorism"))
direct_links <- plyr::rename(direct_links, c(Q10_4="change_influence"))
direct_links <- plyr::rename(direct_links, c(Q11_2="change_econ"))
direct_links <- plyr::rename(direct_links, c(Q11_3="change_persFinance"))
direct_links <- plyr::rename(direct_links, c(Q11_4="change_NHS"))

# Voted

direct_links <- plyr::rename(direct_links, c(Q12="registered"))
direct_links <- plyr::rename(direct_links, c(Q14="vote"))
direct_links <- plyr::rename(direct_links, c(Q16_1="howSure_vote")) # how sure were you about your ref vote choice?
direct_links <- plyr::rename(direct_links, c(Q62="wouldHave_voted")) # If you had been eligible ONLY in direct links
direct_links <- plyr::rename(direct_links, c(Q17="whatMatteredMost"))

# SecondRef

direct_links <- plyr::rename(direct_links, c(Q20="SecondRef"))

# Time spent following news about politics

direct_links <- plyr::rename(direct_links, c(Q21_1="tv")) # during last 7 days
direct_links <- plyr::rename(direct_links, c(Q21_2="newspapers"))
direct_links <- plyr::rename(direct_links, c(Q21_3="radio"))
direct_links <- plyr::rename(direct_links, c(Q21_4="internet"))
direct_links <- plyr::rename(direct_links, c(Q21_5="talktoPeople"))

# Most important source of information

levels(direct_links$Q22_1)[1] <- "tv_4Ref"
levels(direct_links$Q22_1)[2] <- "radio_4Ref"
levels(direct_links$Q22_1)[3] <- "newspapers_4Ref"
levels(direct_links$Q22_1)[4] <- "onlinenews_4Ref"
levels(direct_links$Q22_1)[5] <- "twitter_4Ref"
levels(direct_links$Q22_1)[6] <- "facebook_4Ref"
levels(direct_links$Q22_1)[7] <- "blogs_4Ref"
direct_links <- plyr::rename(direct_links, c(Q22_1="mostImpSoure"))

# Second most important source of information

levels(direct_links$Q22_2)[1] <- "tv_4Ref"
levels(direct_links$Q22_2)[2] <- "radio_4Ref"
levels(direct_links$Q22_2)[3] <- "newspapers_4Ref"
levels(direct_links$Q22_2)[4] <- "onlinenews_4Ref"
levels(direct_links$Q22_2)[5] <- "twitter_4Ref"
levels(direct_links$Q22_2)[6] <- "facebook_4Ref"
levels(direct_links$Q22_2)[7] <- "blogs_4Ref"
direct_links <- plyr::rename(direct_links, c(Q22_2="SecondmostImpSoure"))

# Twitter behaviour

direct_links <- plyr::rename(direct_links, c(Q23="tweetOnRef"))
direct_links <- plyr::rename(direct_links, c(Q24_1_1_TEXT="no_tweets"))
direct_links <- plyr::rename(direct_links, c(Q24_1_2_TEXT="no_retweets"))
direct_links <- plyr::rename(direct_links, c(Q25="tweetsPolitics"))

# Demographics

direct_links <- plyr::rename(direct_links, c(Q26="gender"))
direct_links <- plyr::rename(direct_links, c(Q27="born")) 
direct_links <- plyr::rename(direct_links, c(Q28="education"))
direct_links <- plyr::rename(direct_links, c(Q29_1="childrenUnder5"))
direct_links <- plyr::rename(direct_links, c(Q29_2="children5to15"))
direct_links <- plyr::rename(direct_links, c(Q29_3="children16+"))
direct_links <- plyr::rename(direct_links, c(Q29_4="noChildren"))
direct_links <- plyr::rename(direct_links, c(Q30="maritalStatus"))
direct_links <- plyr::rename(direct_links, c(Q31="townVillage")) 
direct_links <- plyr::rename(direct_links, c(Q32="sourceIncome"))
direct_links <- plyr::rename(direct_links, c(Q32_TEXT="otherIncome")) # merge!

# Psych / Big 5

direct_links <- plyr::rename(direct_links, c(Q35 = "trust"))

direct_links <- plyr::rename(direct_links, c(Q36_1="extraversion"))
direct_links <- plyr::rename(direct_links, c(Q36_2="agreeableness_R"))
direct_links <- plyr::rename(direct_links, c(Q36_3="conscientiousness"))
direct_links <- plyr::rename(direct_links, c(Q36_4="neuroticism"))
direct_links <- plyr::rename(direct_links, c(Q36_5="openness"))
direct_links <- plyr::rename(direct_links, c(Q37_6="extraversion_R"))
direct_links <- plyr::rename(direct_links, c(Q37_7="agreeableness"))
direct_links <- plyr::rename(direct_links, c(Q37_8="conscientiousness_R"))
direct_links <- plyr::rename(direct_links, c(Q37_9="neuroticism_R"))
direct_links <- plyr::rename(direct_links, c(Q37_10="openness_R")) 

# Accuracy goals

direct_links <- plyr::rename(direct_links, c(Q38_4="gutDecision"))
levels(direct_links$gutDecision)
direct_links <- plyr::rename(direct_links, c(Q38_5="basedOnFacts"))
direct_links <- plyr::rename(direct_links, c(Q39_1="selfEsteem"))
direct_links <- plyr::rename(direct_links, c(Q40="subjSocialStatus"))
direct_links <- plyr::rename(direct_links, c(Q41="lifeSatisfaction"))

# Treatment 1 - Think Remain

direct_links <- plyr::rename(direct_links, c(Q42="t1_thinkRemain"))

direct_links <- plyr::rename(direct_links, c(Q43_1="thinkRemain_1ST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q43_2="thinkRemain_LASTCLICK"))
direct_links <- plyr::rename(direct_links, c(Q43_3 = "thinkRemain_SUBMIT"))
direct_links <- plyr::rename(direct_links, c(Q43_4 = "thinkRemain_CLICKCOUNT"))

# Treatment 1 - Think Leave

direct_links <- plyr::rename(direct_links, c(Q44="t1_thinkLeave"))

direct_links <- plyr::rename(direct_links, c(Q45_1="thinkLeave_1ST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q45_2="thinkLeave_LASTCLICK"))
direct_links <- plyr::rename(direct_links, c(Q45_3="thinkLeave_SUBMIT"))
direct_links <- plyr::rename(direct_links, c(Q45_4="thinkLeave_CLICKCOUNT"))

# Treatment 2 - EU Knowledge WITH timer 

direct_links <- plyr::rename(direct_links, c(Q33_9="stress_EU28"))
direct_links <- plyr::rename(direct_links, c(Q33_10="stress_MEPsElected"))
direct_links <- plyr::rename(direct_links, c(Q33_11="stress_CHinEU"))

direct_links <- plyr::rename(direct_links, c(Q34_1="stress_1ST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q34_2="stress_LAST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q34_3="stress_SUBMIT"))
direct_links <- plyr::rename(direct_links, c(Q34_4="stress_CLICKCOUNT"))

# Control - EU Knowledge WITHOUT timer

direct_links <- plyr::rename(direct_links, c(Q54_9="ctl_EU28")) 
direct_links <- plyr::rename(direct_links, c(Q54_10="ctl_MEPsElected"))
direct_links <- plyr::rename(direct_links, c(Q54_11="ctl_CHinEU"))

direct_links <- plyr::rename(direct_links, c(Q55_1="ctl_1ST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q55_2="ctl_LASTCLICK"))
direct_links <- plyr::rename(direct_links, c(Q55_3="ctl_SUBMIT"))
direct_links <- plyr::rename(direct_links, c(Q55_4="ctl_CLICKCOUNT"))

# Misperceptions

direct_links <- plyr::rename(direct_links, c(Q46_12="m1_Turkey"))
direct_links <- plyr::rename(direct_links, c(Q46_14="m2_Army"))
direct_links <- plyr::rename(direct_links, c(Q46_15="m3_NHS"))
direct_links <- plyr::rename(direct_links, c(Q46_25="m4_Euro"))
direct_links <- plyr::rename(direct_links, c(Q46_26="m5_Queen"))
direct_links <- plyr::rename(direct_links, c(Q46_33="poundPlunged"))
direct_links <- plyr::rename(direct_links, c(Q46_36="ScotlandRemain"))

direct_links <- plyr::rename(direct_links, c(Q47_1="mp_1ST_CLICK"))
direct_links <- plyr::rename(direct_links, c(Q47_2="mp_LASTCLICK"))
direct_links <- plyr::rename(direct_links, c(Q47_3="mp_SUBMIT"))
direct_links <- plyr::rename(direct_links, c(Q47_4="mp_CLICKCOUNT"))

# See if correct
# direct_links[1, ]

# In Common

direct_links <- plyr::rename(direct_links, c(Q50_2="inCommon_Brexiteers"))
direct_links <- plyr::rename(direct_links, c(Q50_3="inCommon_Remainers"))

direct_links <- plyr::rename(direct_links, c(Q51_1="curiosity1")) # merge / reverse!
direct_links <- plyr::rename(direct_links, c(Q51_2="curiosity2"))
direct_links <- plyr::rename(direct_links, c(Q51_3="curiosity3"))
direct_links <- plyr::rename(direct_links, c(Q51_4="curiosity4"))

direct_links <- plyr::rename(direct_links, c(Q51_5="needToEvaluate1")) # merge / reverse!
direct_links <- plyr::rename(direct_links, c(Q51_6="needToEvaluate2_R"))

direct_links <- plyr::rename(direct_links, c(Q52_4="needforCognition1")) # merge / reverse!
direct_links <- plyr::rename(direct_links, c(Q52_8="needforCognition2_R"))
direct_links <- plyr::rename(direct_links, c(Q52_5="needforCognition3"))
direct_links <- plyr::rename(direct_links, c(Q52_7="needforCognition4_R"))

direct_links <- plyr::rename(direct_links, c(Q52_11="needToEvaluate3_R")) 
direct_links <- plyr::rename(direct_links, c(Q52_12="needToEvaluate4"))


direct_links$Q56 <- NULL # "Now the next question is a little quiz."

direct_links$Q53 <- NULL # "That's the end of the survey"

# End renaming direct_links. 





#########################################################

# RENAME c) pre_ref

#########################################################


# str(pre_ref, list.len=ncol(pre_ref) ) ## DO.BR.FL_30 and DO.BR.FL_33



# Treatment group assignment ############################

# Imagination / friend / no friend

levels(pre_ref$DO.BR.FL_30)[levels(pre_ref$DO.BR.FL_30)=="Psychological stuff - control"] <- "no_friend"
levels(pre_ref$DO.BR.FL_30)[levels(pre_ref$DO.BR.FL_30)=="Psychological stuff - treatment 1  (friend)"] <- "friend"

pre_ref <- plyr::rename(pre_ref, c(DO.BR.FL_30="trGroup_friend")) # rename using plyr


#########################################################


# EU knowledge questions / stress / no stress

levels(pre_ref$DO.BR.FL_33)[levels(pre_ref$DO.BR.FL_33)=="Psychological stuff - EU knowledge questions (no stress)"] <- "control"
levels(pre_ref$DO.BR.FL_33)[levels(pre_ref$DO.BR.FL_33)=="Psychological stuff - EU knowledge questions (stress)"] <- "stress"

pre_ref <- plyr::rename(pre_ref, c(DO.BR.FL_33="treatment"))

# Leave geolocation as it is

# pre_ref$LocationLatitude
# pre_ref$LocationLongitude
# pre_ref$LocationAccuracy


# First variables #######################################

pre_ref <- plyr::rename(pre_ref, c(?..V1="id")) # check if this is actually the id variable
# pre_ref <- plyr::rename(pre_ref, c(V1="id"))

pre_ref$V2 <- NULL # Thanks for participating
pre_ref$V3 <- NULL # Name (anonymous)
pre_ref$V4 <- NULL # External data reference

pre_ref <- plyr::rename(pre_ref, c(V5="email"))

pre_ref$V6 <- NULL # ipAddress 
pre_ref$V7 <- NULL # status (SPAM!)

pre_ref <- plyr::rename(pre_ref, c(V8="startTime")) # startDate
pre_ref <- plyr::rename(pre_ref, c(V9="endTime")) # endDate
pre_ref <- plyr::rename(pre_ref, c(V10="finished"))  

pre_ref$Q_URL <- NULL

# twitter names: see above 



# Questions #############################################

pre_ref$Q1 <- NULL # Thanks for participating

pre_ref <- plyr::rename(pre_ref, c(Q48="liveCountry"))

# Referendum 

pre_ref <- plyr::rename(pre_ref, c(Q2="interest_EUref"))
pre_ref <- plyr::rename(pre_ref, c(Q13_1="interest_whoWon"))
pre_ref <- plyr::rename(pre_ref, c(Q13_2="interest_nextElection"))
pre_ref <- plyr::rename(pre_ref, c(Q13_3="interest_Euro2016"))

pre_ref <- plyr::rename(pre_ref, c(Q57_1="whatHappens_leave"))
pre_ref <- plyr::rename(pre_ref, c(Q57_2="whatHappens_remain"))

pre_ref <- plyr::rename(pre_ref, c(Q130_1="goodReasons_leave"))
pre_ref <- plyr::rename(pre_ref, c(Q130_2="goodReasons_remain"))

pre_ref <- plyr::rename(pre_ref, c(Q58_2="change_immigration"))
pre_ref <- plyr::rename(pre_ref, c(Q58_3="change_terrorism"))
pre_ref <- plyr::rename(pre_ref, c(Q58_4="change_influence"))
pre_ref <- plyr::rename(pre_ref, c(Q59_2="change_econ"))
pre_ref <- plyr::rename(pre_ref, c(Q59_3="change_persFinance"))
pre_ref <- plyr::rename(pre_ref, c(Q59_4="change_NHS"))

# Vote
pre_ref <- plyr::rename(pre_ref, c(Q6="registered"))
pre_ref <- plyr::rename(pre_ref, c(Q8="willVote")) # ONLY in pre_ref dataset 
# Error names() applied to a non-vector
pre_ref <- plyr::rename(pre_ref, c(Q9="vote")) # technically would have to be voteIntention
pre_ref <- plyr::rename(pre_ref, c(Q49="leaningToward"))
pre_ref <- plyr::rename(pre_ref, c(Q19_1="howSure_vote")) # how sure are you about your ref vote choice?
# pre_ref <- plyr::rename(pre_ref, c(Q62="wouldHave_voted")) # If you had been eligible ONLY in direct_links
pre_ref <- plyr::rename(pre_ref, c(Q10="whatMatteredMost"))

pre_ref <- plyr::rename(pre_ref, c(Q50_1="howLikelyIsBrexit")) # ONLY in pre-ref


# SecondRef 

pre_ref <- plyr::rename(pre_ref, c(Q51="SecondRef"))

# Time spent following news about politics

pre_ref <- plyr::rename(pre_ref, c(Q25_1="tv")) # during last 7 days
pre_ref <- plyr::rename(pre_ref, c(Q25_2="newspapers"))
pre_ref <- plyr::rename(pre_ref, c(Q25_3="radio"))
pre_ref <- plyr::rename(pre_ref, c(Q25_4="internet"))
pre_ref <- plyr::rename(pre_ref, c(Q25_5="talktoPeople"))

# Most important source of information

levels(pre_ref$Q11_1)[1] <- "tv_4Ref"
levels(pre_ref$Q11_1)[2] <- "radio_4Ref"
levels(pre_ref$Q11_1)[3] <- "newspapers_4Ref"
levels(pre_ref$Q11_1)[4] <- "onlinenews_4Ref"
levels(pre_ref$Q11_1)[5] <- "twitter_4Ref"
levels(pre_ref$Q11_1)[6] <- "facebook_4Ref"
levels(pre_ref$Q11_1)[7] <- "blogs_4Ref"
pre_ref <- plyr::rename(pre_ref, c(Q11_1="mostImpSoure"))

# Second most important source of information

levels(pre_ref$Q11_2)[1] <- "tv_4Ref"
levels(pre_ref$Q11_2)[2] <- "radio_4Ref"
levels(pre_ref$Q11_2)[3] <- "newspapers_4Ref"
levels(pre_ref$Q11_2)[4] <- "onlinenews_4Ref"
levels(pre_ref$Q11_2)[5] <- "twitter_4Ref"
levels(pre_ref$Q11_2)[6] <- "facebook_4Ref"
levels(pre_ref$Q11_2)[7] <- "blogs_4Ref"
pre_ref <- plyr::rename(pre_ref, c(Q11_2="SecondmostImpSoure"))

# Twitter behaviour
pre_ref <- plyr::rename(pre_ref, c(Q12="tweetOnRef"))
pre_ref <- plyr::rename(pre_ref, c(Q27_1_1_TEXT="no_tweets"))
pre_ref <- plyr::rename(pre_ref, c(Q27_1_2_TEXT="no_retweets"))
pre_ref <- plyr::rename(pre_ref, c(Q26="tweetsPolitics"))

# Demographics
pre_ref <- plyr::rename(pre_ref, c(Q28="gender"))
pre_ref <- plyr::rename(pre_ref, c(Q29="born")) 
pre_ref <- plyr::rename(pre_ref, c(Q30="education")) # aint got not education! 
pre_ref <- plyr::rename(pre_ref, c(Q31_1="childrenUnder5"))
pre_ref <- plyr::rename(pre_ref, c(Q31_2="children5to15"))
pre_ref <- plyr::rename(pre_ref, c(Q31_3="children16+"))
pre_ref <- plyr::rename(pre_ref, c(Q31_4="noChildren"))
pre_ref <- plyr::rename(pre_ref, c(Q32="maritalStatus"))
pre_ref <- plyr::rename(pre_ref, c(Q33="townVillage")) 
pre_ref <- plyr::rename(pre_ref, c(Q34="sourceIncome"))
pre_ref <- plyr::rename(pre_ref, c(Q34_TEXT="otherIncome")) # merge!


# Psych / Big 5

pre_ref <- plyr::rename(pre_ref, c(Q216 = "trust")) # interpersonal trust

pre_ref <- plyr::rename(pre_ref, c(Q72_1="extraversion"))
pre_ref <- plyr::rename(pre_ref, c(Q72_2="agreeableness_R"))
pre_ref <- plyr::rename(pre_ref, c(Q72_3="conscientiousness"))
pre_ref <- plyr::rename(pre_ref, c(Q72_4="neuroticism"))
pre_ref <- plyr::rename(pre_ref, c(Q72_5="openness"))
pre_ref <- plyr::rename(pre_ref, c(Q71_6="extraversion_R"))
pre_ref <- plyr::rename(pre_ref, c(Q71_7="agreeableness"))
pre_ref <- plyr::rename(pre_ref, c(Q71_8="conscientiousness_R"))
pre_ref <- plyr::rename(pre_ref, c(Q71_9="neuroticism_R"))
pre_ref <- plyr::rename(pre_ref, c(Q71_10="openness_R")) 

# Accuracy goals

pre_ref <- plyr::rename(pre_ref, c(Q218_4="gutDecision"))
pre_ref <- plyr::rename(pre_ref, c(Q218_5="basedOnFacts")) 
pre_ref <- plyr::rename(pre_ref, c(Q219_1="selfEsteem"))
pre_ref <- plyr::rename(pre_ref, c(Q220="subjSocialStatus"))
pre_ref <- plyr::rename(pre_ref, c(Q221="lifeSatisfaction"))

# Treatment 1 - Think Remain NB LEAVE THIS OUT THIS DID NOT WORK

# pre_ref <- plyr::rename(pre_ref, c(Q257_22="t1_thinkRemain"))
pre_ref$Q257_22_TEXT <- NULL
pre_ref$Q257_24_TEXT <- NULL
pre_ref$Q257_25_TEXT <- NULL
pre_ref$Q257_27_TEXT <- NULL
pre_ref$Q257_28_TEXT <- NULL
pre_ref$Q257_29_TEXT <- NULL
pre_ref$Q257_30_TEXT <- NULL
pre_ref$Q257_31_TEXT <- NULL

pre_ref <- plyr::rename(pre_ref, c(Q258_1="thinkRemain_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q258_2="thinkRemain_LASTCLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q258_3 = "thinkRemain_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q258_4 = "thinkRemain_CLICKCOUNT"))


# Treatment 1 - Think Leave

# pre_ref <- plyr::rename(pre_ref, c(Q67="t1_thinkLeave"))

pre_ref$Q67_22_TEXT <- NULL
pre_ref$Q67_24_TEXT <- NULL
pre_ref$Q67_25_TEXT <- NULL
pre_ref$Q67_27_TEXT <- NULL
pre_ref$Q67_28_TEXT <- NULL
pre_ref$Q67_29_TEXT <- NULL
pre_ref$Q67_30_TEXT <- NULL
pre_ref$Q67_31_TEXT <- NULL

pre_ref <- plyr::rename(pre_ref, c(Q260_1="thinkLeave_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q260_2="thinkLeave_LASTCLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q260_3="thinkLeave_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q260_4="thinkLeave_CLICKCOUNT"))

pre_ref$Q123 <- NULL # Now the next question is a little quiz...

# Treatment 2 - EU Knowledge WITH timer 

pre_ref <- plyr::rename(pre_ref, c(Q206_9="stress_EU28"))
pre_ref <- plyr::rename(pre_ref, c(Q206_10="stress_MEPsElected"))
pre_ref <- plyr::rename(pre_ref, c(Q206_11="stress_CHinEU"))

pre_ref <- plyr::rename(pre_ref, c(Q118_1="stress_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q118_2="stress_LAST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q118_3="stress_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q118_4="stress_CLICKCOUNT"))

# Control - EU Knowledge WITHOUT timer

pre_ref <- plyr::rename(pre_ref, c(Q117_9="ctl_EU28")) 
pre_ref <- plyr::rename(pre_ref, c(Q117_10="ctl_MEPsElected"))
pre_ref <- plyr::rename(pre_ref, c(Q117_11="ctl_CHinEU"))

pre_ref <- plyr::rename(pre_ref, c(Q207_1="ctl_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q207_2="ctl_LASTCLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q207_3="ctl_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q207_4="ctl_CLICKCOUNT"))

# Misperceptions

pre_ref <- plyr::rename(pre_ref, c(Q72_12="m1_Turkey"))
pre_ref <- plyr::rename(pre_ref, c(Q72_20="m1_France")) # ONLY in pre_ref
pre_ref <- plyr::rename(pre_ref, c(Q72_13="m1_UKPays")) # ONLY in pre_ref
pre_ref <- plyr::rename(pre_ref, c(Q72_14="m2_Army"))
pre_ref <- plyr::rename(pre_ref, c(Q72_15="m3_NHS"))

pre_ref <- plyr::rename(pre_ref, c(Q74_1="mp_pre_ref_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q74_2="mp_pre_ref_LASTCLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q74_3="mp_pre_ref_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q74_4="mp_pre_ref_CLICKCOUNT"))

pre_ref <- plyr::rename(pre_ref, c(Q73_16="NHSfreetoEU")) # ONLY in pre_ref
pre_ref <- plyr::rename(pre_ref, c(Q73_17="GBPforfutureBailouts")) # ONLY in pre_ref
pre_ref <- plyr::rename(pre_ref, c(Q73_28="m4_Euro"))
pre_ref <- plyr::rename(pre_ref, c(Q73_18="ManufactReliesonEU")) # ONLY in pre_ref
pre_ref <- plyr::rename(pre_ref, c(Q73_19="m5_Queen")) 

# pre_ref <- plyr::rename(pre_ref, c(Q72_33="poundPlunged")) # ONLY in post_ref and direct_links
# pre_ref <- plyr::rename(pre_ref, c(Q72_36="ScotlandRemain")) # ONLY in post_ref and direct_links

pre_ref <- plyr::rename(pre_ref, c(Q278_1="mp_pre_ref_1ST_CLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q278_2="mp_pre_ref_LASTCLICK"))
pre_ref <- plyr::rename(pre_ref, c(Q278_3="mp_pre_ref_SUBMIT"))
pre_ref <- plyr::rename(pre_ref, c(Q278_4="mp_pre_ref_CLICKCOUNT"))

# NB Warning message:
# The plyr::rename operation has created duplicates for the following name(s): 
# (`mp_pre_ref_1ST_CLICK`) ...

# In Common

pre_ref <- plyr::rename(pre_ref, c(Q274_2="inCommon_Brexiteers"))
pre_ref <- plyr::rename(pre_ref, c(Q274_3="inCommon_Remainers"))

pre_ref <- plyr::rename(pre_ref, c(Q279_1="curiosity1")) # merge / reverse!
pre_ref <- plyr::rename(pre_ref, c(Q279_2="curiosity2"))
pre_ref <- plyr::rename(pre_ref, c(Q279_3="curiosity3"))
pre_ref <- plyr::rename(pre_ref, c(Q279_4="curiosity4"))

pre_ref <- plyr::rename(pre_ref, c(Q279_5="needToEvaluate1")) # merge / reverse!
pre_ref <- plyr::rename(pre_ref, c(Q279_6="needToEvaluate2_R"))

pre_ref <- plyr::rename(pre_ref, c(Q69_4="needForCognition1")) # merge / reverse!
pre_ref <- plyr::rename(pre_ref, c(Q69_8="needForCognition2_R"))
pre_ref <- plyr::rename(pre_ref, c(Q69_5="needForCognition3"))
pre_ref <- plyr::rename(pre_ref, c(Q69_7="needForCognition4_R"))

pre_ref <- plyr::rename(pre_ref, c(Q69_11="needToEvaluate3_R")) 
pre_ref <- plyr::rename(pre_ref, c(Q69_12="needToEvaluate4"))


pre_ref$Q35 <- NULL # That's the end of the survey

# End renaming. 








#########################################################

# RECODE a) post_ref

#########################################################


# treatment Group assignment variables ##################

post_ref$treatment <- as.factor(post_ref$treatment)
post_ref$trGroup_friend <- as.factor(post_ref$trGroup_friend)



# liveCountry ###########################################

# str(post_ref$liveCountry)
post_ref$liveCountry <- as.factor(post_ref$liveCountry)

levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="1"] <- "England"
levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="2"] <- "Scotland"
levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="3"] <- "Wales"
levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="4"] <- "Northern Ireland"
levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="5"] <- "Elsewhere in the EU"
levels(post_ref$liveCountry)[levels(post_ref$liveCountry)=="6"] <- "Outside the EU"

# plot(post_ref$liveCountry)


# interest variables ####################################

# Q6 How interested were you in the EU referendum that was held in the UK on June 23rd?
post_ref$interest_EUref <- as.numeric(as.character(post_ref$interest_EUref))

post_ref$interest_EUref_f <- as.factor(post_ref$interest_EUref)
levels(post_ref$interest_EUref_f)[levels(post_ref$interest_EUref_f)=="1"] <- "Very interested"
levels(post_ref$interest_EUref_f)[levels(post_ref$interest_EUref_f)=="2"] <- "Somewhat interested"
levels(post_ref$interest_EUref_f)[levels(post_ref$interest_EUref_f)=="3"] <- "Not very interested"
levels(post_ref$interest_EUref_f)[levels(post_ref$interest_EUref_f)=="4"] <- "Not at all interested"
levels(post_ref$interest_EUref_f)[levels(post_ref$interest_EUref_f)=="5"] <- "Don't know"

# plot(post_ref$interest_EUref_f)


# Q7 On a scale of 0 to 10, how much did or do you care about the following?

# Which side won the EU referendum on 23 June (1)
# post_ref$interest_whoWon
post_ref$interest_whoWon <- as.numeric(as.character(post_ref$interest_whoWon))
# hist(post_ref$interest_whoWon)

# The outcome of the next UK General Election (2)
# post_ref$interest_nextElection
post_ref$interest_nextElection <- as.numeric(as.character(post_ref$interest_nextElection))
# truehist(post_ref$interest_nextElection)

# Who wins the 2016 UEFA European Football Championship (3)
# post_ref$interest_Euro2016
post_ref$interest_Euro2016 <- as.numeric(as.character(post_ref$interest_Euro2016))
# truehist(post_ref$interest_Euro2016)



# insecurity -- after Brexit vote  #####################

# Q8 How sure are you about what will happen/what would have happened to the UK:

# post_ref$whatHappens_leave

post_ref$whatHappens_leave <- as.numeric(as.character(post_ref$whatHappens_leave))

post_ref$whatHappens_leave_f <- as.factor(post_ref$whatHappens_leave)
levels(post_ref$whatHappens_leave_f)[levels(post_ref$whatHappens_leave_f)=="1"] <- "Very unsure"
levels(post_ref$whatHappens_leave_f)[levels(post_ref$whatHappens_leave_f)=="2"] <- "Quite unsure"
levels(post_ref$whatHappens_leave_f)[levels(post_ref$whatHappens_leave_f)=="3"] <- "Quite sure"
levels(post_ref$whatHappens_leave_f)[levels(post_ref$whatHappens_leave_f)=="4"] <- "Very sure"

# plot(post_ref$whatHappens_leave_f) # BY VOTE! 


# insecurity -- after Remain vote  #####################

# post_ref$whatHappens_remain

post_ref$whatHappens_remain <- as.numeric(as.character(post_ref$whatHappens_remain))

post_ref$whatHappens_remain_f <- as.factor(post_ref$whatHappens_remain)
levels(post_ref$whatHappens_remain_f)[levels(post_ref$whatHappens_remain_f)=="1"] <- "Very unsure"
levels(post_ref$whatHappens_remain_f)[levels(post_ref$whatHappens_remain_f)=="2"] <- "Quite unsure"
levels(post_ref$whatHappens_remain_f)[levels(post_ref$whatHappens_remain_f)=="3"] <- "Quite sure"
levels(post_ref$whatHappens_remain_f)[levels(post_ref$whatHappens_remain_f)=="4"] <- "Very sure"

# plot(post_ref$whatHappens_remain_f) # BY VOTE! 



# reasons to leave ######################################

# Q9 Some say that there were good reasons for both options in this EU referendum.  
# Others think it was more clear-cut than that.  How many would you say there were of each 
# of the following?


# post_ref$goodReasons_leave

post_ref$goodReasons_leave <- as.numeric(as.character(post_ref$goodReasons_leave))

post_ref$goodReasons_leave_f <- as.factor(post_ref$goodReasons_leave)
levels(post_ref$goodReasons_leave_f)[levels(post_ref$goodReasons_leave_f)=="1"] <- "None at all"
levels(post_ref$goodReasons_leave_f)[levels(post_ref$goodReasons_leave_f)=="2"] <- "Not very many"
levels(post_ref$goodReasons_leave_f)[levels(post_ref$goodReasons_leave_f)=="3"] <- "Quite a few"
levels(post_ref$goodReasons_leave_f)[levels(post_ref$goodReasons_leave_f)=="4"] <- "Very many"

# plot(post_ref$goodReasons_leave_f) 


# post_ref$goodReasons_remain

post_ref$goodReasons_remain <- as.numeric(as.character(post_ref$goodReasons_remain))

post_ref$goodReasons_remain_f <- as.factor(post_ref$goodReasons_remain)
levels(post_ref$goodReasons_remain_f)[levels(post_ref$goodReasons_remain_f)=="1"] <- "None at all"
levels(post_ref$goodReasons_remain_f)[levels(post_ref$goodReasons_remain_f)=="2"] <- "Not very many"
levels(post_ref$goodReasons_remain_f)[levels(post_ref$goodReasons_remain_f)=="3"] <- "Quite a few"
levels(post_ref$goodReasons_remain_f)[levels(post_ref$goodReasons_remain_f)=="4"] <- "Very many"

# plot(post_ref$goodReasons_remain_f) 



# Changes after Brexit ####################################

# Q10, Q11 Do you think the following will be higher, lower or about the same now that the UK 
# has voted to leave the European Union?



# immigration ###########################################

post_ref$change_immigration <- as.numeric(as.character(post_ref$change_immigration))

post_ref$change_immigration_f <- as.factor(post_ref$change_immigration)
levels(post_ref$change_immigration_f)[levels(post_ref$change_immigration_f)=="1"] <- "Much lower"
levels(post_ref$change_immigration_f)[levels(post_ref$change_immigration_f)=="2"] <- "Lower"
levels(post_ref$change_immigration_f)[levels(post_ref$change_immigration_f)=="3"] <- "About the same"
levels(post_ref$change_immigration_f)[levels(post_ref$change_immigration_f)=="4"] <- "Higher"
levels(post_ref$change_immigration_f)[levels(post_ref$change_immigration_f)=="5"] <- "Much higher"

# plot(post_ref$change_immigration_f)
# barplot(prop.table(table(post_ref$change_immigration_f))) 


# terrorism ###########################################

# The risk of terrorism 

post_ref$change_terrorism <- as.numeric(as.character(post_ref$change_terrorism))

post_ref$change_terrorism_f <- as.factor(post_ref$change_terrorism)
levels(post_ref$change_terrorism_f)[levels(post_ref$change_terrorism_f)=="1"] <- "Much lower"
levels(post_ref$change_terrorism_f)[levels(post_ref$change_terrorism_f)=="2"] <- "Lower"
levels(post_ref$change_terrorism_f)[levels(post_ref$change_terrorism_f)=="3"] <- "About the same"
levels(post_ref$change_terrorism_f)[levels(post_ref$change_terrorism_f)=="4"] <- "Higher"
levels(post_ref$change_terrorism_f)[levels(post_ref$change_terrorism_f)=="5"] <- "Much higher"

# plot(post_ref$change_terrorism_f)
# barplot(prop.table(table(post_ref$change_terrorism_f))) 



# influence ###########################################

# Britain's influence in the world ... 

post_ref$change_influence <- as.numeric(as.character(post_ref$change_influence))

post_ref$change_influence_f <- as.factor(post_ref$change_influence)
levels(post_ref$change_influence_f)[levels(post_ref$change_influence_f)=="1"] <- "Much lower"
levels(post_ref$change_influence_f)[levels(post_ref$change_influence_f)=="2"] <- "Lower"
levels(post_ref$change_influence_f)[levels(post_ref$change_influence_f)=="3"] <- "About the same"
levels(post_ref$change_influence_f)[levels(post_ref$change_influence_f)=="4"] <- "Higher"
levels(post_ref$change_influence_f)[levels(post_ref$change_influence_f)=="5"] <- "Much higher"

# plot(post_ref$change_influence_f)
# barplot(prop.table(table(post_ref$change_influence_f))) 



# economy #############################################

# The general economic situation in the UK 

post_ref$change_econ <- as.numeric(as.character(post_ref$change_econ))

post_ref$change_econ_f <- as.factor(post_ref$change_econ)
levels(post_ref$change_econ_f)[levels(post_ref$change_econ_f)=="1"] <- "Much worse"
levels(post_ref$change_econ_f)[levels(post_ref$change_econ_f)=="2"] <- "Worse"
levels(post_ref$change_econ_f)[levels(post_ref$change_econ_f)=="3"] <- "About the same"
levels(post_ref$change_econ_f)[levels(post_ref$change_econ_f)=="4"] <- "Better"
levels(post_ref$change_econ_f)[levels(post_ref$change_econ_f)=="5"] <- "Much better"

# plot(post_ref$change_econ_f)
# barplot(prop.table(table(post_ref$change_econ_f))) 


# personal finances #######################################

# Your personal financial situation 

post_ref$change_persFinance <- as.numeric(as.character(post_ref$change_persFinance))

post_ref$change_persFinance_f <- as.factor(post_ref$change_persFinance)
levels(post_ref$change_persFinance_f)[levels(post_ref$change_persFinance_f)=="1"] <- "Much worse"
levels(post_ref$change_persFinance_f)[levels(post_ref$change_persFinance_f)=="2"] <- "Worse"
levels(post_ref$change_persFinance_f)[levels(post_ref$change_persFinance_f)=="3"] <- "About the same"
levels(post_ref$change_persFinance_f)[levels(post_ref$change_persFinance_f)=="4"] <- "Better"
levels(post_ref$change_persFinance_f)[levels(post_ref$change_persFinance_f)=="5"] <- "Much better"

# plot(post_ref$change_persFinance_f)
# barplot(prop.table(table(post_ref$change_persFinance_f))) 


# NHS #####################################################

# The NHS

post_ref$change_NHS <- as.numeric(as.character(post_ref$change_NHS))

post_ref$change_NHS_f <- as.factor(post_ref$change_NHS)
levels(post_ref$change_NHS_f)[levels(post_ref$change_NHS_f)=="1"] <- "Much worse"
levels(post_ref$change_NHS_f)[levels(post_ref$change_NHS_f)=="2"] <- "Worse"
levels(post_ref$change_NHS_f)[levels(post_ref$change_NHS_f)=="3"] <- "About the same"
levels(post_ref$change_NHS_f)[levels(post_ref$change_NHS_f)=="4"] <- "Better"
levels(post_ref$change_NHS_f)[levels(post_ref$change_NHS_f)=="5"] <- "Much better"

# plot(post_ref$change_NHS_f)
# barplot(prop.table(table(post_ref$change_NHS_f))) 




# vote ##################################################

# Q12 To the best of your knowledge, were you eligible and registered to vote in the EU referendum?

# registered
# str(post_ref$registered)
post_ref$registered <- as.factor(post_ref$registered)
levels(post_ref$registered)[levels(post_ref$registered)=="1"] <- "Yes"
levels(post_ref$registered)[levels(post_ref$registered)=="2"] <- "No, eligible but not registered"
levels(post_ref$registered)[levels(post_ref$registered)=="3"] <- "No, too young to vote"
levels(post_ref$registered)[levels(post_ref$registered)=="5"] <- "No, not eligible (living elsewhere / other reason)"
levels(post_ref$registered)[levels(post_ref$registered)=="6"] <- "Don't know"


# Q14 Many people don't vote these days.  Which of these best describes what you did in the 
# referendum on Britain's membership of the European Union?

# vote 
str(post_ref$vote)
post_ref$vote <- as.factor(post_ref$vote)
levels(post_ref$vote)[levels(post_ref$vote)=="1"] <- "leave"
levels(post_ref$vote)[levels(post_ref$vote)=="2"] <- "remain"
levels(post_ref$vote)[levels(post_ref$vote)=="3"] <- "don't know"
levels(post_ref$vote)[levels(post_ref$vote)=="4"] <- "not vote"


# how sure ##############################################

# Q16 And how sure were you about your referendum vote choice?

str(post_ref$howSure_vote)
post_ref$howSure_vote <- as.numeric(as.character(post_ref$howSure_vote))

# as factor (I invented the labels except for the 1st and last)
post_ref$howSure_vote_f <- as.factor(post_ref$howSure_vote)
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="0"] <- "not sure at all"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="1"] <- "not sure"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="2"] <- "a little unsure"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="3"] <- "don't know"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="4"] <- "pretty sure"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="5"] <- "sure"
levels(post_ref$howSure_vote_f)[levels(post_ref$howSure_vote_f)=="6"] <- "absolutely sure"


# Psych vars ############################################

# How do you feel now ###################################

# Q60 How do you feel now that the UK has voted to leave the EU?

post_ref$happy <- as.numeric(as.character(post_ref$happy))
post_ref$proud <- as.numeric(as.character(post_ref$proud))
post_ref$surprised <- as.numeric(as.character(post_ref$surprised))
post_ref$anxious <- as.numeric(as.character(post_ref$anxious))
post_ref$angry <- as.numeric(as.character(post_ref$angry))
post_ref$guilty <- as.numeric(as.character(post_ref$guilty))
# post_ref$ashamed <- as.numeric(as.character(post_ref$ashamed)) # only for direct links!!


# SecondRef ##############################################

# Q20 Now that the UK has voted to leave the EU, do you think the decision will be final or 
# might there be a second EU referendum?

post_ref$SecondRef <- as.factor(post_ref$SecondRef)
post_ref$SecondRef_f[post_ref$SecondRef == "1"] <- "decision is final"
post_ref$SecondRef_f[post_ref$SecondRef == "2"] <- "second referendum"



# demographics ###########################################

# gender #################################################

post_ref$gender <- as.factor(post_ref$gender)
levels(post_ref$gender)[levels(post_ref$gender)=="1"] <- "male"  
levels(post_ref$gender)[levels(post_ref$gender)=="2"] <- "female"  


# age ####################################################

# str(post_ref$born)

# Convert to character variable so I can change the values
post_ref$born <- as.character(post_ref$born)

post_ref$born[post_ref$born == "78"] <- "1978" # post_ref
post_ref$born[post_ref$born == "75"] <- "1975" # post_ref
post_ref$born[post_ref$born == "72"] <- "1972" # post_ref
post_ref$born[post_ref$born == "73"] <- "1973" # post_ref
post_ref$born[post_ref$born == "195"] <- NA # post_ref
post_ref$born[post_ref$born == "0"] <- NA # post_ref
post_ref$born[post_ref$born == "1064"] <- "1964" # post_ref
post_ref$born[post_ref$born == "1068"] <- "1968" # post_ref
post_ref$born[post_ref$born == "1901"] <- NA # post_ref

# post_ref$born[post_ref$born == "88"] <- "1988"

post_ref$born <- as.numeric(as.character(post_ref$born))



# education ##############################################

# Q28 At what age did you finish full-time education?

post_ref$education <- as.numeric(as.character(post_ref$education))

post_ref$education_f <- as.factor(post_ref$education)
levels(post_ref$education_f)[levels(post_ref$education_f)=="1"] <- "15 or younger"  
levels(post_ref$education_f)[levels(post_ref$education_f)=="2"] <- "16" 
levels(post_ref$education_f)[levels(post_ref$education_f)=="3"] <- "17" 
levels(post_ref$education_f)[levels(post_ref$education_f)=="4"] <- "18" 
levels(post_ref$education_f)[levels(post_ref$education_f)=="5"] <- "19 or older" 
levels(post_ref$education_f)[levels(post_ref$education_f)=="6"] <- "Still at school in full-time education" 
levels(post_ref$education_f)[levels(post_ref$education_f)=="7"] <- "Still at university in full-time education"

# plot(post_ref$education_f)


# marital status #########################################

# Q30 Which of these best describes your marital status?

post_ref$maritalStatus <- as.factor(post_ref$maritalStatus)
levels(post_ref$maritalStatus)[levels(post_ref$maritalStatus)=="1"] <- "Married" 
levels(post_ref$maritalStatus)[levels(post_ref$maritalStatus)=="2"] <- "Living as married "
levels(post_ref$maritalStatus)[levels(post_ref$maritalStatus)=="3"] <- "Divorced"
levels(post_ref$maritalStatus)[levels(post_ref$maritalStatus)=="4"] <- "Widowed"
levels(post_ref$maritalStatus)[levels(post_ref$maritalStatus)=="5"] <- "Single"


# town / village #########################################

# Q31 Is your home (or your main home if you have more than one) in..

str(post_ref$townVillage)
levels(post_ref$townVillage)[levels(post_ref$townVillage)=="1"] <- "centre of a city or large town" 
levels(post_ref$townVillage)[levels(post_ref$townVillage)=="2"] <- "suburb of a city or large town" 
levels(post_ref$townVillage)[levels(post_ref$townVillage)=="3"] <- "small or medium-sized town" 
levels(post_ref$townVillage)[levels(post_ref$townVillage)=="4"] <- "village" 
levels(post_ref$townVillage)[levels(post_ref$townVillage)=="5"] <- "countryside" 



# sourceIncome #########################################

# Q32 Which of the following is the main source of income for you at present?

# post_ref$sourceIncome
post_ref$sourceIncome <- as.factor(post_ref$sourceIncome)
levels(post_ref$sourceIncome)

levels(post_ref$sourceIncome)[1] <- "employment"
levels(post_ref$sourceIncome)[2] <- "occupational pensions"
levels(post_ref$sourceIncome)[3] <- "retired"
levels(post_ref$sourceIncome)[4] <- "unemployment benefits"
levels(post_ref$sourceIncome)[5] <- "income support"
levels(post_ref$sourceIncome)[6] <- "invalidity/ sickness/ disabled pension"
levels(post_ref$sourceIncome)[7] <- "savings/ investments"
levels(post_ref$sourceIncome)[8] <- "student loan"
levels(post_ref$sourceIncome)[9] <- "parents"
levels(post_ref$sourceIncome)[10] <- "other"


# trust ################################################

# Q35 Generally speaking, would you say that most people can be trusted, or that you 
# can't be too careful in dealing with people?

post_ref$trust <- recode(post_ref$trust, " '1' = 'Most people can be trusted' ; 
                         '2' = 'Cannot be too careful in dealing with people'  ")



# Big 5 #################################################

# Q36 Here are a number of personality traits that may or may not apply to you.  Please say 
# how well you feel each pair describes you.  (You should rate the extent to which the pair of 
# traits applies to you, even if one characteristic applies more strongly than the other.)  
# How much do you see yourself as...


# extraversion ##########################################

# Q36_1 ______ Extraverted, enthusiastic (1)
post_ref$extraversion <- as.numeric(as.character(post_ref$extraversion))

# Q37_6 ______ Reserved, quiet (6)
post_ref$extraversion_r <- recode(post_ref$extraversion_R, '
                                  "1"="7";
                                  "2"="6";
                                  "3"="5";
                                  "4"="4";
                                  "5"="3";
                                  "6"="2";
                                  "7"="1" ')
post_ref$extraversion_r <- as.numeric(post_ref$extraversion_r)


# agreeableness #########################################

# Q37_7 ______ Sympathetic, warm (7)
post_ref$agreeableness <- as.numeric(as.character(post_ref$agreeableness))

# Q36_2 ______ Critical, quarrelsome (2)
post_ref$agreeableness_r <- recode(post_ref$agreeableness_R, '  
                                   "1"="7";
                                   "2"="6";
                                   "3"="5";
                                   "4"="4";
                                   "5"="3";
                                   "6"="2";
                                   "7"="1" ')
post_ref$agreeableness_r <- as.numeric(as.character(post_ref$agreeableness_r))


# conscientiousness #####################################

# Q36_3 ______ Dependable, self-disciplined (3)
post_ref$conscientiousness <- as.numeric(as.character(post_ref$conscientiousness))

# Q37_8 ______ Disorganized, careless (8)
post_ref$conscientiousness_r <- recode(post_ref$conscientiousness_R, '  
                                       "1"="7"; 
                                       "2"="6";
                                       "3"="5";
                                       "4"="4";
                                       "5"="3";
                                       "6"="2";
                                       "7"="1" ')
post_ref$conscientiousness_r <- as.numeric(as.character(post_ref$conscientiousness_r))


# openness ##############################################

# Q36_5 ______ Open to new experiences, complex (5)
post_ref$openness <- as.numeric(as.character(post_ref$openness))

# Q37_10 ______ Conventional, uncreative (10)
post_ref$openness_r <- recode(post_ref$openness_R, '  
                              "1"="7"; 
                              "2"="6";
                              "3"="5";
                              "4"="4";
                              "5"="3";
                              "6"="2";
                              "7"="1" ')
post_ref$openness_r <- as.numeric(as.character(post_ref$openness_r))


# neuroticism ###########################################

# Q36_4 ______ Anxious, easily upset (4)
post_ref$neuroticism <- as.numeric(as.character(post_ref$neuroticism))

# Q37_9 ______ Calm, emotionally stable (9)
post_ref$neuroticism_r <- recode(post_ref$neuroticism_R, '  
                                 "1"="7"; 
                                 "2"="6";
                                 "3"="5";
                                 "4"="4";
                                 "5"="3";
                                 "6"="2";
                                 "7"="1" ')
post_ref$neuroticism_r <- as.numeric(as.character(post_ref$neuroticism_r))




# accuracy goals ########################################

# Q38 Now everybody has a different approach to taking decisions. 
# To what extent do these statements describe your voting decision in the EU referendum?

# gut decision #########################################

post_ref$gutDecision

post_ref <- plyr::rename(post_ref, c(Q38_4="gutDecision"))
# WARNING -- The following `from` values were not present in `x`: Q38_4

post_ref$gutDecision <- as.factor(post_ref$gutDecision)
post_ref$gutDecision <- recode(post_ref$gutDecision, '
                               "11"="Definitely true";
                               "12"="Probably true";
                               "14"="Probably false";
                               "15"="Definitely false";
                               "16"="Do not know" ')

# based on facts #######################################

post_ref <- plyr::rename(post_ref, c(Q38_5="basedOnFacts"))

post_ref$basedOnFacts <- factor(recode(post_ref$basedOnFacts, '
                                       "11"="Definitely true";
                                       "12"="Probably true";
                                       "14"="Probably false";
                                       "15"="Definitely false";
                                       "16"="Do not know" ')) 


# self esteem ##########################################

# Q39 And how far would you say that this is true of you:  "I have high self-esteem."
post_ref <- plyr::rename(post_ref, c(Q39_1="selfEsteem"))

# str(post_ref$selfEsteem) # 1 = Not very true of me, 7 = Very true of me
post_ref$selfEsteem <- as.numeric(post_ref$selfEsteem)
# hist(post_ref$selfEsteem)


# Subjective social status ##############################

# Q40 Think of this ladder as representing where people stand in the United Kingdom...

str(post_ref$subjSocialStatus)
levels(post_ref$subjSocialStatus)

post_ref$subjSocialStatus <- as.character(post_ref$subjSocialStatus)

post_ref$subjSocialStatus[post_ref$subjSocialStatus=="Seven"] <- "7" 
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="1-Mentally disabled, can't get a job."] <- "1" 
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="4"] # (Is the scale linear or logarithmic? Assuming logarithmic)"] <- 4 
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="6 or 7"] <- "6.5"
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="4-5"] <- "4.5" 
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="2 and 3"] <- "2.5" 
post_ref$subjSocialStatus[post_ref$subjSocialStatus=="5-6"] <- "5.5" 

# post_ref$subjSocialStatus[post_ref$subjSocialStatus=="7 or 8"] <- "7.5" # in direct_links dataset
# post_ref$subjSocialStatus[post_ref$subjSocialStatus=="four"] <- "4"
# post_ref$subjSocialStatus[post_ref$subjSocialStatus=="7-8"] <- "7.5"
# post_ref$subjSocialStatus[post_ref$subjSocialStatus=="5/6"] <- "5.5"

post_ref$subjSocialStatus <- as.numeric(as.character((post_ref$subjSocialStatus))) 

# which(post_ref$subjSocialStatus == "6 or 7") # this doesn't work
# which(post_ref$subjSocialStatus == 4) # this works



# life satisfaction #####################################

# Q41 On  the whole, how satisfied are you with the life you lead ?

post_ref <- plyr::rename(post_ref, c(Q41="lifeSatisfaction"))



# life satisfaction #####################################

str(post_ref$lifeSatisfaction)
# truehist(post_ref$lifeSatisfaction)
# table(post_ref$lifeSatisfaction)

# as factor
post_ref$lifeSatisfaction

post_ref$lifeSatisfaction_f <- as.factor(post_ref$lifeSatisfaction)
levels(post_ref$lifeSatisfaction_f)[levels(post_ref$lifeSatisfaction_f)=="1"] <- "satisfied"
levels(post_ref$lifeSatisfaction_f)[levels(post_ref$lifeSatisfaction_f)=="2"] <- "fairly satisfied"
levels(post_ref$lifeSatisfaction_f)[levels(post_ref$lifeSatisfaction_f)=="3"] <- "not very satisfied"
levels(post_ref$lifeSatisfaction_f)[levels(post_ref$lifeSatisfaction_f)=="4"] <- "not satisfied"

# plot(post_ref$lifeSatisfaction_f)
# barplot(prop.table(table(post_ref$lifeSatisfaction_f))) 

# as numeric
post_ref$lifeSatisfaction_num <- as.numeric(post_ref$lifeSatisfaction_f)




# EU knowledge ##########################################

# Q33 True or false? We've introduced a time limit to make this a bit more exciting!  
# You have 35 seconds to answer these questions.

# Q54 True or false?

post_ref$stress_EU28 <- recode(post_ref$stress_EU28, '
                               "4"="True";
                               "7"="False";
                               "8"="Do not know" ')

post_ref$ctl_EU28 <- recode(post_ref$ctl_EU28,'
                            "4"="True";
                            "7"="False";
                            "8"="Do not know" ')

post_ref$stress_CHinEU <- recode(post_ref$stress_CHinEU, '
                                 "4"="True";
                                 "7"="False";
                                 "8"="Do not know" ')

post_ref$ctl_CHinEU <- recode(post_ref$ctl_CHinEU, '
                              "4"="True";
                              "7"="False";
                              "8"="Do not know" ')

post_ref$stress_MEPsElected <- recode(post_ref$stress_MEPsElected, '
                                      "4"="True";
                                      "7"="False";
                                      "8"="Do not know" ')

post_ref$ctl_MEPsElected <- recode(post_ref$ctl_MEPsElected,'
                                   "4"="True";
                                   "7"="False";
                                   "8"="Do not know" ')



#########################################################

# MISPERCEPTIONS items

#########################################################

post_ref$m1_Turkey <- recode(post_ref$m1_Turkey, '
                             "20"="Definitely true";
                             "21"="Probably true";
                             "23"="Probably false";
                             "24"="Definitely false";
                             "25"="Do not know" ',
                             levels = c("Definitely false", "Probably false", "Do not know",
                                        "Probably true", "Definitely true")
)
# str(post_ref$m1_Turkey)
# levels(post_ref$m1_Turkey)

post_ref$m2_Army <- recode(post_ref$m2_Army,'
                           "20"="Definitely true";
                           "21"="Probably true";
                           "23"="Probably false";
                           "24"="Definitely false";
                           "25"="Do not know" ',
                           levels = c("Definitely false", "Probably false", "Do not know",
                                      "Probably true", "Definitely true"))
# str(post_ref$m2_Army)
# levels(post_ref$m2_Army)


post_ref$m3_NHS <- recode(post_ref$m3_NHS, '
                          "20"="Definitely true";
                          "21"="Probably true";
                          "23"="Probably false";
                          "24"="Definitely false";
                          "25"="Do not know" ',
                          levels = c("Definitely false", "Probably false", "Do not know",
                                     "Probably true", "Definitely true"))
# str(post_ref$m3_NHS)
# levels(post_ref$m3_NHS)

post_ref$m4_Euro <- recode(post_ref$m4_Euro, '
                           "20"="Definitely true";
                           "21"="Probably true";
                           "23"="Probably false";
                           "24"="Definitely false";
                           "25"="Do not know" ',
                           levels = c("Definitely false", "Probably false", "Do not know",
                                      "Probably true", "Definitely true"))
# str(post_ref$m4_Euro)
# levels(post_ref$m4_Euro)

post_ref$m5_Queen <- recode(post_ref$m5_Queen, '
                            "20"="Definitely true";
                            "21"="Probably true";
                            "23"="Probably false";
                            "24"="Definitely false";
                            "25"="Do not know" ',
                            levels = c("Definitely false", "Probably false", "Do not know",
                                       "Probably true", "Definitely true"))
# str(post_ref$m5_Queen)
# levels(post_ref$m5_Queen)

post_ref$poundPlunged <- recode(post_ref$poundPlunged, '
                            "20"="Definitely true";
                            "21"="Probably true";
                            "23"="Probably false";
                            "24"="Definitely false";
                            "25"="Do not know" ',
                            levels = c("Definitely false", "Probably false", "Do not know",
                                       "Probably true", "Definitely true"))

post_ref$ScotlandRemain <- recode(post_ref$ScotlandRemain, '
                            "20"="Definitely true";
                                "21"="Probably true";
                                "23"="Probably false";
                                "24"="Definitely false";
                                "25"="Do not know" ',
                                levels = c("Definitely false", "Probably false", "Do not know",
                                           "Probably true", "Definitely true"))


# In Common #############################################

# Q50 When you think about the following groups, how much would you say you have in 
# common with them (apart from what they think about Europe) ? 

# with Remainers?
post_ref$inCommon_Remainers <- as.numeric(post_ref$inCommon_Remainers) # the higher the more in common

# with Brexiteers?
post_ref$inCommon_Brexiteers <- as.numeric(post_ref$inCommon_Brexiteers) # the higher the more in common



# Date / time variables #################################

post_ref$startTime <- as.POSIXct(post_ref$startTime, format="%Y-%m-%d %H:%M:%S")
post_ref$endTime <- as.POSIXct(post_ref$endTime, format="%Y-%m-%d %H:%M:%S")


# First/ Last click ######################################

post_ref$thinkRemain_1ST_CLICK <- as.numeric(as.character((post_ref$thinkRemain_1ST_CLICK)))
post_ref$thinkRemain_LASTCLICK <- as.numeric(as.character((post_ref$thinkRemain_LASTCLICK)))
post_ref$thinkRemain_SUBMIT <- as.numeric(as.character((post_ref$thinkRemain_SUBMIT)))
post_ref$thinkRemain_CLICKCOUNT <- as.numeric(as.character((post_ref$thinkRemain_CLICKCOUNT)))

post_ref$thinkLeave_1ST_CLICK <- as.numeric(as.character((post_ref$thinkLeave_1ST_CLICK)))
post_ref$thinkLeave_LASTCLICK <- as.numeric(as.character((post_ref$thinkLeave_LASTCLICK)))
post_ref$thinkLeave_SUBMIT <- as.numeric(as.character((post_ref$thinkLeave_SUBMIT)))
post_ref$thinkLeave_CLICKCOUNT <- as.numeric(as.character((post_ref$thinkLeave_CLICKCOUNT)))

post_ref$stress_1ST_CLICK <- as.numeric(as.character((post_ref$stress_1ST_CLICK)))
post_ref$stress_LAST_CLICK <- as.numeric(as.character((post_ref$stress_LAST_CLICK)))
post_ref$stress_SUBMIT <- as.numeric(as.character((post_ref$stress_SUBMIT)))
post_ref$stress_CLICKCOUNT <- as.numeric(as.character((post_ref$stress_CLICKCOUNT)))

post_ref$ctl_1ST_CLICK <- as.numeric(as.character((post_ref$ctl_1ST_CLICK)))
post_ref$ctl_LASTCLICK <- as.numeric(as.character((post_ref$ctl_LASTCLICK)))
post_ref$ctl_SUBMIT <- as.numeric(as.character((post_ref$ctl_SUBMIT)))
post_ref$ctl_CLICKCOUNT <- as.numeric(as.character((post_ref$ctl_CLICKCOUNT)))

post_ref$mp_1ST_CLICK <- as.numeric(as.character((post_ref$mp_1ST_CLICK)))
post_ref$mp_LASTCLICK <- as.numeric(as.character((post_ref$mp_LASTCLICK)))
post_ref$mp_SUBMIT <- as.numeric(as.character((post_ref$mp_SUBMIT)))
post_ref$mp_CLICKCOUNT <- as.numeric(as.character((post_ref$mp_CLICKCOUNT)))

# View how much time people spent on the misperceptions items
# post_ref[ , c("mp_1ST_CLICK", "mp_LASTCLICK", "mp_SUBMIT", "mp_CLICKCOUNT" ) ]

# End recoding post_ref.


#########################################################

# RECODE b) direct_links

#########################################################


# treatment Group assignment variables ##################

direct_links$treatment <- as.factor(direct_links$treatment)
direct_links$trGroup_friend <- as.factor(direct_links$trGroup_friend)


# liveCountry ###########################################

# str(direct_links$liveCountry)
direct_links$liveCountry <- as.factor(direct_links$liveCountry)

levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="1"] <- "England"
levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="2"] <- "Scotland"
levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="3"] <- "Wales"
levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="4"] <- "Northern Ireland"
levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="5"] <- "Elsewhere in the EU"
levels(direct_links$liveCountry)[levels(direct_links$liveCountry)=="6"] <- "Outside the EU"

# plot(direct_links$liveCountry)


# interest variables ####################################

# Q6 How interested were you in the EU referendum that was held in the UK on June 23rd?
direct_links$interest_EUref <- as.numeric(as.character(direct_links$interest_EUref))

direct_links$interest_EUref_f <- as.factor(direct_links$interest_EUref)
levels(direct_links$interest_EUref_f)[levels(direct_links$interest_EUref_f)=="1"] <- "Very interested"
levels(direct_links$interest_EUref_f)[levels(direct_links$interest_EUref_f)=="2"] <- "Somewhat interested"
levels(direct_links$interest_EUref_f)[levels(direct_links$interest_EUref_f)=="3"] <- "Not very interested"
levels(direct_links$interest_EUref_f)[levels(direct_links$interest_EUref_f)=="4"] <- "Not at all interested"
levels(direct_links$interest_EUref_f)[levels(direct_links$interest_EUref_f)=="5"] <- "Don't know"

# plot(direct_links$interest_EUref_f)


# Q7 On a scale of 0 to 10, how much did or do you care about the following?

# Which side won the EU referendum on 23 June (1)
# direct_links$interest_whoWon
direct_links$interest_whoWon <- as.numeric(as.character(direct_links$interest_whoWon))
# hist(direct_links$interest_whoWon)

# The outcome of the next UK General Election (2)
# direct_links$interest_nextElection
direct_links$interest_nextElection <- as.numeric(as.character(direct_links$interest_nextElection))
# truehist(direct_links$interest_nextElection)

# Who wins the 2016 UEFA European Football Championship (3)
# direct_links$interest_Euro2016
direct_links$interest_Euro2016 <- as.numeric(as.character(direct_links$interest_Euro2016))
# truehist(direct_links$interest_Euro2016)



# insecurity -- after Brexit vote  #####################

# Q8 How sure are you about what will happen/what would have happened to the UK:

# direct_links$whatHappens_leave

direct_links$whatHappens_leave <- as.numeric(as.character(direct_links$whatHappens_leave))

direct_links$whatHappens_leave_f <- as.factor(direct_links$whatHappens_leave)
levels(direct_links$whatHappens_leave_f)[levels(direct_links$whatHappens_leave_f)=="1"] <- "Very unsure"
levels(direct_links$whatHappens_leave_f)[levels(direct_links$whatHappens_leave_f)=="2"] <- "Quite unsure"
levels(direct_links$whatHappens_leave_f)[levels(direct_links$whatHappens_leave_f)=="3"] <- "Quite sure"
levels(direct_links$whatHappens_leave_f)[levels(direct_links$whatHappens_leave_f)=="4"] <- "Very sure"

# plot(direct_links$whatHappens_leave_f) # BY VOTE! 


# insecurity -- after Remain vote  #####################

# direct_links$whatHappens_remain

direct_links$whatHappens_remain <- as.numeric(as.character(direct_links$whatHappens_remain))

direct_links$whatHappens_remain_f <- as.factor(direct_links$whatHappens_remain)
levels(direct_links$whatHappens_remain_f)[levels(direct_links$whatHappens_remain_f)=="1"] <- "Very unsure"
levels(direct_links$whatHappens_remain_f)[levels(direct_links$whatHappens_remain_f)=="2"] <- "Quite unsure"
levels(direct_links$whatHappens_remain_f)[levels(direct_links$whatHappens_remain_f)=="3"] <- "Quite sure"
levels(direct_links$whatHappens_remain_f)[levels(direct_links$whatHappens_remain_f)=="4"] <- "Very sure"

# plot(direct_links$whatHappens_remain_f) # BY VOTE! 



# reasons to leave ######################################

# Q9 Some say that there were good reasons for both options in this EU referendum.  
# Others think it was more clear-cut than that.  How many would you say there were of each 
# of the following?


# direct_links$goodReasons_leave

direct_links$goodReasons_leave <- as.numeric(as.character(direct_links$goodReasons_leave))

direct_links$goodReasons_leave_f <- as.factor(direct_links$goodReasons_leave)
levels(direct_links$goodReasons_leave_f)[levels(direct_links$goodReasons_leave_f)=="1"] <- "None at all"
levels(direct_links$goodReasons_leave_f)[levels(direct_links$goodReasons_leave_f)=="2"] <- "Not very many"
levels(direct_links$goodReasons_leave_f)[levels(direct_links$goodReasons_leave_f)=="3"] <- "Quite a few"
levels(direct_links$goodReasons_leave_f)[levels(direct_links$goodReasons_leave_f)=="4"] <- "Very many"

# plot(direct_links$goodReasons_leave_f) 


# direct_links$goodReasons_remain

direct_links$goodReasons_remain <- as.numeric(as.character(direct_links$goodReasons_remain))

direct_links$goodReasons_remain_f <- as.factor(direct_links$goodReasons_remain)
levels(direct_links$goodReasons_remain_f)[levels(direct_links$goodReasons_remain_f)=="1"] <- "None at all"
levels(direct_links$goodReasons_remain_f)[levels(direct_links$goodReasons_remain_f)=="2"] <- "Not very many"
levels(direct_links$goodReasons_remain_f)[levels(direct_links$goodReasons_remain_f)=="3"] <- "Quite a few"
levels(direct_links$goodReasons_remain_f)[levels(direct_links$goodReasons_remain_f)=="4"] <- "Very many"

# plot(direct_links$goodReasons_remain_f) 



# Changes after Brexit ####################################

# Q10, Q11 Do you think the following will be higher, lower or about the same now that the UK 
# has voted to leave the European Union?



# immigration ###########################################

direct_links$change_immigration <- as.numeric(as.character(direct_links$change_immigration))

direct_links$change_immigration_f <- as.factor(direct_links$change_immigration)
levels(direct_links$change_immigration_f)[levels(direct_links$change_immigration_f)=="1"] <- "Much lower"
levels(direct_links$change_immigration_f)[levels(direct_links$change_immigration_f)=="2"] <- "Lower"
levels(direct_links$change_immigration_f)[levels(direct_links$change_immigration_f)=="3"] <- "About the same"
levels(direct_links$change_immigration_f)[levels(direct_links$change_immigration_f)=="4"] <- "Higher"
levels(direct_links$change_immigration_f)[levels(direct_links$change_immigration_f)=="5"] <- "Much higher"

# plot(direct_links$change_immigration_f)
# barplot(prop.table(table(direct_links$change_immigration_f))) 


# terrorism ###########################################

# The risk of terrorism 

direct_links$change_terrorism <- as.numeric(as.character(direct_links$change_terrorism))

direct_links$change_terrorism_f <- as.factor(direct_links$change_terrorism)
levels(direct_links$change_terrorism_f)[levels(direct_links$change_terrorism_f)=="1"] <- "Much lower"
levels(direct_links$change_terrorism_f)[levels(direct_links$change_terrorism_f)=="2"] <- "Lower"
levels(direct_links$change_terrorism_f)[levels(direct_links$change_terrorism_f)=="3"] <- "About the same"
levels(direct_links$change_terrorism_f)[levels(direct_links$change_terrorism_f)=="4"] <- "Higher"
levels(direct_links$change_terrorism_f)[levels(direct_links$change_terrorism_f)=="5"] <- "Much higher"

# plot(direct_links$change_terrorism_f)
# barplot(prop.table(table(direct_links$change_terrorism_f))) 



# influence ###########################################

# Britain's influence in the world ... 

direct_links$change_influence <- as.numeric(as.character(direct_links$change_influence))

direct_links$change_influence_f <- as.factor(direct_links$change_influence)
levels(direct_links$change_influence_f)[levels(direct_links$change_influence_f)=="1"] <- "Much lower"
levels(direct_links$change_influence_f)[levels(direct_links$change_influence_f)=="2"] <- "Lower"
levels(direct_links$change_influence_f)[levels(direct_links$change_influence_f)=="3"] <- "About the same"
levels(direct_links$change_influence_f)[levels(direct_links$change_influence_f)=="4"] <- "Higher"
levels(direct_links$change_influence_f)[levels(direct_links$change_influence_f)=="5"] <- "Much higher"

# plot(direct_links$change_influence_f)
# barplot(prop.table(table(direct_links$change_influence_f))) 



# economy #############################################

# The general economic situation in the UK 

direct_links$change_econ <- as.numeric(as.character(direct_links$change_econ))

direct_links$change_econ_f <- as.factor(direct_links$change_econ)
levels(direct_links$change_econ_f)[levels(direct_links$change_econ_f)=="1"] <- "Much worse"
levels(direct_links$change_econ_f)[levels(direct_links$change_econ_f)=="2"] <- "Worse"
levels(direct_links$change_econ_f)[levels(direct_links$change_econ_f)=="3"] <- "About the same"
levels(direct_links$change_econ_f)[levels(direct_links$change_econ_f)=="4"] <- "Better"
levels(direct_links$change_econ_f)[levels(direct_links$change_econ_f)=="5"] <- "Much better"

# plot(direct_links$change_econ_f)
# barplot(prop.table(table(direct_links$change_econ_f))) 


# personal finances #######################################

# Your personal financial situation 

direct_links$change_persFinance <- as.numeric(as.character(direct_links$change_persFinance))

direct_links$change_persFinance_f <- as.factor(direct_links$change_persFinance)
levels(direct_links$change_persFinance_f)[levels(direct_links$change_persFinance_f)=="1"] <- "Much worse"
levels(direct_links$change_persFinance_f)[levels(direct_links$change_persFinance_f)=="2"] <- "Worse"
levels(direct_links$change_persFinance_f)[levels(direct_links$change_persFinance_f)=="3"] <- "About the same"
levels(direct_links$change_persFinance_f)[levels(direct_links$change_persFinance_f)=="4"] <- "Better"
levels(direct_links$change_persFinance_f)[levels(direct_links$change_persFinance_f)=="5"] <- "Much better"

# plot(direct_links$change_persFinance_f)
# barplot(prop.table(table(direct_links$change_persFinance_f))) 

# NHS #####################################################

# The NHS

direct_links$change_NHS <- as.numeric(as.character(direct_links$change_NHS))

direct_links$change_NHS_f <- as.factor(direct_links$change_NHS)
levels(direct_links$change_NHS_f)[levels(direct_links$change_NHS_f)=="1"] <- "Much worse"
levels(direct_links$change_NHS_f)[levels(direct_links$change_NHS_f)=="2"] <- "Worse"
levels(direct_links$change_NHS_f)[levels(direct_links$change_NHS_f)=="3"] <- "About the same"
levels(direct_links$change_NHS_f)[levels(direct_links$change_NHS_f)=="4"] <- "Better"
levels(direct_links$change_NHS_f)[levels(direct_links$change_NHS_f)=="5"] <- "Much better"

# plot(direct_links$change_NHS_f)
# barplot(prop.table(table(direct_links$change_NHS_f))) 




# vote ##################################################

# Q12 To the best of your knowledge, were you eligible and registered to vote in the EU referendum?

# registered
str(direct_links$registered)
direct_links$registered <- as.factor(direct_links$registered)
levels(direct_links$registered)[levels(direct_links$registered)=="1"] <- "Yes"
levels(direct_links$registered)[levels(direct_links$registered)=="2"] <- "No, eligible but not registered"
levels(direct_links$registered)[levels(direct_links$registered)=="3"] <- "No, too young to vote"
levels(direct_links$registered)[levels(direct_links$registered)=="5"] <- "No, not eligible (living elsewhere / other reason)"
levels(direct_links$registered)[levels(direct_links$registered)=="6"] <- "Don't know"


# Q14 Many people don't vote these days.  Which of these best describes what you did in the 
# referendum on Britain's membership of the European Union?

# vote 
str(direct_links$vote)
direct_links$vote <- as.factor(direct_links$vote)
levels(direct_links$vote)[levels(direct_links$vote)=="1"] <- "leave"
levels(direct_links$vote)[levels(direct_links$vote)=="2"] <- "remain"
levels(direct_links$vote)[levels(direct_links$vote)=="3"] <- "don't know"
levels(direct_links$vote)[levels(direct_links$vote)=="4"] <- "not vote"


# how sure ##############################################

# Q16 And how sure were you about your referendum vote choice?

str(direct_links$howSure_vote)
direct_links$howSure_vote <- as.numeric(as.character(direct_links$howSure_vote))

# as factor (I invented the labels except for the 1st and last)
direct_links$howSure_vote_f <- as.factor(direct_links$howSure_vote)
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="0"] <- "not sure at all"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="1"] <- "not sure"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="2"] <- "a little unsure"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="3"] <- "don't know"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="4"] <- "pretty sure"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="5"] <- "sure"
levels(direct_links$howSure_vote_f)[levels(direct_links$howSure_vote_f)=="6"] <- "absolutely sure"


# Psych vars ############################################

# How do you feel now ###################################

# Q60 How do you feel now that the UK has voted to leave the EU?

direct_links$happy <- as.numeric(as.character(direct_links$happy))
direct_links$proud <- as.numeric(as.character(direct_links$proud))
direct_links$surprised <- as.numeric(as.character(direct_links$surprised))
direct_links$anxious <- as.numeric(as.character(direct_links$anxious))
direct_links$angry <- as.numeric(as.character(direct_links$angry))
direct_links$guilty <- as.numeric(as.character(direct_links$guilty))
direct_links$ashamed <- as.numeric(as.character(direct_links$ashamed)) # only for direct links!!


# SecondRef ##############################################

# Q20 Now that the UK has voted to leave the EU, do you think the decision will be final or 
# might there be a second EU referendum?

direct_links$SecondRef <- as.factor(direct_links$SecondRef)
direct_links$SecondRef_f[direct_links$SecondRef == "1"] <- "decision is final"
direct_links$SecondRef_f[direct_links$SecondRef == "2"] <- "second referendum"



# demographics ###########################################

# gender #################################################

direct_links$gender <- as.factor(direct_links$gender)
levels(direct_links$gender)[levels(direct_links$gender)=="1"] <- "male"  
levels(direct_links$gender)[levels(direct_links$gender)=="2"] <- "female"  


# age ####################################################

# Corrected these in the post_ref data set


# str(post_ref$born)

# Convert to character variable so I can change the values

direct_links$born <- as.character(direct_links$born)

# direct_links$born[direct_links$born == "78"] <- "1978" 
# direct_links$born[direct_links$born == "75"] <- "1975" 
# direct_links$born[direct_links$born == "72"] <- "1972" 
# direct_links$born[direct_links$born == "73"] <- "1973" 
# direct_links$born[direct_links$born == "195"] <- NA 
# direct_links$born[direct_links$born == "0"] <- NA 
# direct_links$born[direct_links$born == "1064"] <- "1964" 
# direct_links$born[direct_links$born == "1068"] <- "1968" 
# direct_links$born[direct_links$born == "1901"] <- NA 

direct_links$born[direct_links$born == "88"] <- "1988"

direct_links$born <- as.numeric(as.character(direct_links$born)) # some NAs ("Haha")



# education ##############################################

# Q28 At what age did you finish full-time education?

direct_links$education <- as.numeric(as.character(direct_links$education))

direct_links$education_f <- as.factor(direct_links$education)
levels(direct_links$education_f)[levels(direct_links$education_f)=="1"] <- "15 or younger"  
levels(direct_links$education_f)[levels(direct_links$education_f)=="2"] <- "16" 
levels(direct_links$education_f)[levels(direct_links$education_f)=="3"] <- "17" 
levels(direct_links$education_f)[levels(direct_links$education_f)=="4"] <- "18" 
levels(direct_links$education_f)[levels(direct_links$education_f)=="5"] <- "19 or older" 
levels(direct_links$education_f)[levels(direct_links$education_f)=="6"] <- "Still at school in full-time education" 
levels(direct_links$education_f)[levels(direct_links$education_f)=="7"] <- "Still at university in full-time education"

# plot(direct_links$education_f)


# marital status #########################################

# Q30 Which of these best describes your marital status?

direct_links$maritalStatus <- as.factor(direct_links$maritalStatus)
levels(direct_links$maritalStatus)[levels(direct_links$maritalStatus)=="1"] <- "Married" 
levels(direct_links$maritalStatus)[levels(direct_links$maritalStatus)=="2"] <- "Living as married "
levels(direct_links$maritalStatus)[levels(direct_links$maritalStatus)=="3"] <- "Divorced"
levels(direct_links$maritalStatus)[levels(direct_links$maritalStatus)=="4"] <- "Widowed"
levels(direct_links$maritalStatus)[levels(direct_links$maritalStatus)=="5"] <- "Single"


# town / village #########################################

# Q31 Is your home (or your main home if you have more than one) in..

str(direct_links$townVillage)
levels(direct_links$townVillage)[levels(direct_links$townVillage)=="1"] <- "centre of a city or large town" 
levels(direct_links$townVillage)[levels(direct_links$townVillage)=="2"] <- "suburb of a city or large town" 
levels(direct_links$townVillage)[levels(direct_links$townVillage)=="3"] <- "small or medium-sized town" 
levels(direct_links$townVillage)[levels(direct_links$townVillage)=="4"] <- "village" 
levels(direct_links$townVillage)[levels(direct_links$townVillage)=="5"] <- "countryside" 



# sourceIncome #########################################

# Q32 Which of the following is the main source of income for you at present?

# direct_links$sourceIncome
direct_links$sourceIncome <- as.factor(direct_links$sourceIncome)
levels(direct_links$sourceIncome)

levels(direct_links$sourceIncome)[1] <- "employment"
levels(direct_links$sourceIncome)[2] <- "occupational pensions"
levels(direct_links$sourceIncome)[3] <- "retired"
levels(direct_links$sourceIncome)[4] <- "unemployment benefits"
levels(direct_links$sourceIncome)[5] <- "income support"
levels(direct_links$sourceIncome)[6] <- "invalidity/ sickness/ disabled pension"
levels(direct_links$sourceIncome)[7] <- "savings/ investments"
levels(direct_links$sourceIncome)[8] <- "student loan"
levels(direct_links$sourceIncome)[9] <- "parents"
levels(direct_links$sourceIncome)[10] <- "other"


# trust ################################################

# Q35 Generally speaking, would you say that most people can be trusted, or that you 
# can't be too careful in dealing with people?

direct_links$trust <- recode(direct_links$trust, " '1' = 'Most people can be trusted' ; 
                             '2' = 'Cannot be too careful in dealing with people'  ")



# Big 5 #################################################

# Q36 Here are a number of personality traits that may or may not apply to you.  Please say 
# how well you feel each pair describes you.  (You should rate the extent to which the pair of 
# traits applies to you, even if one characteristic applies more strongly than the other.)  
# How much do you see yourself as...


# extraversion ##########################################

# Q36_1 ______ Extraverted, enthusiastic (1)
direct_links$extraversion <- as.numeric(as.character(direct_links$extraversion))

# Q37_6 ______ Reserved, quiet (6)
direct_links$extraversion_r <- recode(direct_links$extraversion_R, '
                                      "1"="7";
                                      "2"="6";
                                      "3"="5";
                                      "4"="4";
                                      "5"="3";
                                      "6"="2";
                                      "7"="1" ')
direct_links$extraversion_r <- as.numeric(direct_links$extraversion_r)


# agreeableness #########################################

# Q37_7 ______ Sympathetic, warm (7)
direct_links$agreeableness <- as.numeric(as.character(direct_links$agreeableness))

# Q36_2 ______ Critical, quarrelsome (2)
direct_links$agreeableness_r <- recode(direct_links$agreeableness_R, '  
                                       "1"="7";
                                       "2"="6";
                                       "3"="5";
                                       "4"="4";
                                       "5"="3";
                                       "6"="2";
                                       "7"="1" ')
direct_links$agreeableness_r <- as.numeric(as.character(direct_links$agreeableness_r))


# conscientiousness #####################################

# Q36_3 ______ Dependable, self-disciplined (3)
direct_links$conscientiousness <- as.numeric(as.character(direct_links$conscientiousness))

# Q37_8 ______ Disorganized, careless (8)
direct_links$conscientiousness_r <- recode(direct_links$conscientiousness_R, '  
                                           "1"="7"; 
                                           "2"="6";
                                           "3"="5";
                                           "4"="4";
                                           "5"="3";
                                           "6"="2";
                                           "7"="1" ')
direct_links$conscientiousness_r <- as.numeric(as.character(direct_links$conscientiousness_r))


# openness ##############################################

# Q36_5 ______ Open to new experiences, complex (5)
direct_links$openness <- as.numeric(as.character(direct_links$openness))

# Q37_10 ______ Conventional, uncreative (10)
direct_links$openness_r <- recode(direct_links$openness_R, '  
                                  "1"="7"; 
                                  "2"="6";
                                  "3"="5";
                                  "4"="4";
                                  "5"="3";
                                  "6"="2";
                                  "7"="1" ')
direct_links$openness_r <- as.numeric(as.character(direct_links$openness_r))


# neuroticism ###########################################

# Q36_4 ______ Anxious, easily upset (4)
direct_links$neuroticism <- as.numeric(as.character(direct_links$neuroticism))

# Q37_9 ______ Calm, emotionally stable (9)
direct_links$neuroticism_r <- recode(direct_links$neuroticism_R, '  
                                     "1"="7"; 
                                     "2"="6";
                                     "3"="5";
                                     "4"="4";
                                     "5"="3";
                                     "6"="2";
                                     "7"="1" ')
direct_links$neuroticism_r <- as.numeric(as.character(direct_links$neuroticism_r))




# accuracy goals ########################################

# Q38 Now everybody has a different approach to taking decisions. 
# To what extent do these statements describe your voting decision in the EU referendum?

# gut decision #########################################

direct_links$gutDecision <- as.factor(direct_links$gutDecision)
direct_links$gutDecision <- recode(direct_links$gutDecision, '
                                   "11"="Definitely true";
                                   "12"="Probably true";
                                   "14"="Probably false";
                                   "15"="Definitely false";
                                   "16"="Do not know" ')

# based on facts #######################################

direct_links$basedOnFacts <- factor(recode(direct_links$basedOnFacts, '
                                           "11"="Definitely true";
                                           "12"="Probably true";
                                           "14"="Probably false";
                                           "15"="Definitely false";
                                           "16"="Do not know" ')) 


# self esteem ##########################################

# Q39 And how far would you say that this is true of you:  "I have high self-esteem."

# str(direct_links$selfEsteem) # 1 = Not very true of me, 7 = Very true of me
direct_links$selfEsteem <- as.numeric(direct_links$selfEsteem)
# hist(direct_links$selfEsteem)


# Subjective social status ##############################

# Q40 Think of this ladder as representing where people stand in the United Kingdom...

direct_links$subjSocialStatus <- as.character(direct_links$subjSocialStatus)

direct_links$subjSocialStatus[direct_links$subjSocialStatus=="four"] <- "4" # direct_links
direct_links$subjSocialStatus[direct_links$subjSocialStatus=="7 or 8"] <- "7.5" # direct_links
direct_links$subjSocialStatus[direct_links$subjSocialStatus=="7-8"] <- "7.5" # direct_links
direct_links$subjSocialStatus[direct_links$subjSocialStatus=="5/6"] <- "5.5" # direct_links

direct_links$subjSocialStatus <- as.numeric(as.character((direct_links$subjSocialStatus))) 



# life satisfaction #####################################

# Q41 On  the whole, how satisfied are you with the life you lead ?

# as factor
direct_links$lifeSatisfaction_f <- as.factor(direct_links$lifeSatisfaction)
levels(direct_links$lifeSatisfaction_f)[levels(direct_links$lifeSatisfaction_f)=="1"] <- "satisfied"
levels(direct_links$lifeSatisfaction_f)[levels(direct_links$lifeSatisfaction_f)=="2"] <- "fairly satisfied"
levels(direct_links$lifeSatisfaction_f)[levels(direct_links$lifeSatisfaction_f)=="3"] <- "not very satisfied"
levels(direct_links$lifeSatisfaction_f)[levels(direct_links$lifeSatisfaction_f)=="4"] <- "not satisfied"

# str(direct_links$lifeSatisfaction)
# truehist(direct_links$lifeSatisfaction)
# table(direct_links$lifeSatisfaction)
# plot(direct_links$lifeSatisfaction_f)
# barplot(prop.table(table(direct_links$lifeSatisfaction_f))) 

# as numeric
direct_links$lifeSatisfaction_num <- as.numeric(direct_links$lifeSatisfaction_f)




# EU knowledge ##########################################

# Q33 True or false? We've introduced a time limit to make this a bit more exciting!  
# You have 35 seconds to answer these questions.

# Q54 True or false?

direct_links$stress_EU28 <- recode(direct_links$stress_EU28, '
                                   "4"="True";
                                   "7"="False";
                                   "8"="Do not know" ')

direct_links$ctl_EU28 <- recode(direct_links$ctl_EU28,'
                                "4"="True";
                                "7"="False";
                                "8"="Do not know" ')

direct_links$stress_CHinEU <- recode(direct_links$stress_CHinEU, '
                                     "4"="True";
                                     "7"="False";
                                     "8"="Do not know" ')

direct_links$ctl_CHinEU <- recode(direct_links$ctl_CHinEU, '
                                  "4"="True";
                                  "7"="False";
                                  "8"="Do not know" ')

direct_links$stress_MEPsElected <- recode(direct_links$stress_MEPsElected, '
                                          "4"="True";
                                          "7"="False";
                                          "8"="Do not know" ')

direct_links$ctl_MEPsElected <- recode(direct_links$ctl_MEPsElected,'
                                       "4"="True";
                                       "7"="False";
                                       "8"="Do not know" ')



#########################################################

# MISPERCEPTIONS items

#########################################################

direct_links$m1_Turkey <- recode(direct_links$m1_Turkey, '
                                 "20"="Definitely true";
                                 "21"="Probably true";
                                 "23"="Probably false";
                                 "24"="Definitely false";
                                 "25"="Do not know" ',
                                 levels = c("Definitely false", "Probably false", "Do not know",
                                            "Probably true", "Definitely true")
)
# str(direct_links$m1_Turkey)
# levels(direct_links$m1_Turkey)

direct_links$m2_Army <- recode(direct_links$m2_Army,'
                               "20"="Definitely true";
                               "21"="Probably true";
                               "23"="Probably false";
                               "24"="Definitely false";
                               "25"="Do not know" ',
                               levels = c("Definitely false", "Probably false", "Do not know",
                                          "Probably true", "Definitely true"))
# str(direct_links$m2_Army)
# levels(direct_links$m2_Army)

direct_links$m3_NHS <- recode(direct_links$m3_NHS, '
                              "20"="Definitely true";
                              "21"="Probably true";
                              "23"="Probably false";
                              "24"="Definitely false";
                              "25"="Do not know" ',
                              levels = c("Definitely false", "Probably false", "Do not know",
                                         "Probably true", "Definitely true"))
# str(direct_links$m3_NHS)
# levels(direct_links$m3_NHS)

direct_links$m4_Euro <- recode(direct_links$m4_Euro, '
                               "20"="Definitely true";
                               "21"="Probably true";
                               "23"="Probably false";
                               "24"="Definitely false";
                               "25"="Do not know" ',
                               levels = c("Definitely false", "Probably false", "Do not know",
                                          "Probably true", "Definitely true"))
# str(direct_links$m4_Euro)
# levels(direct_links$m4_Euro)

direct_links$m5_Queen <- recode(direct_links$m5_Queen, '
                                "20"="Definitely true";
                                "21"="Probably true";
                                "23"="Probably false";
                                "24"="Definitely false";
                                "25"="Do not know" ',
                                levels = c("Definitely false", "Probably false", "Do not know",
                                           "Probably true", "Definitely true"))
# str(direct_links$m5_Queen)
# levels(direct_links$m5_Queen)

direct_links$poundPlunged <- recode(direct_links$poundPlunged, '
                            "20"="Definitely true";
                                "21"="Probably true";
                                "23"="Probably false";
                                "24"="Definitely false";
                                "25"="Do not know" ',
                                levels = c("Definitely false", "Probably false", "Do not know",
                                           "Probably true", "Definitely true"))

direct_links$ScotlandRemain <- recode(direct_links$ScotlandRemain, '
                                  "20"="Definitely true";
                                  "21"="Probably true";
                                  "23"="Probably false";
                                  "24"="Definitely false";
                                  "25"="Do not know" ',
                                  levels = c("Definitely false", "Probably false", "Do not know",
                                             "Probably true", "Definitely true"))



# In Common #############################################

# Q50 When you think about the following groups, how much would you say you have in 
# common with them (apart from what they think about Europe) ? 

# with Remainers?
direct_links$inCommon_Remainers <- as.numeric(direct_links$inCommon_Remainers) # the higher the more in common

# with Brexiteers?
direct_links$inCommon_Brexiteers <- as.numeric(direct_links$inCommon_Brexiteers) # the higher the more in common



# Date / time variables #################################

direct_links$startTime <- as.POSIXct(direct_links$startTime, format="%Y-%m-%d %H:%M:%S")
direct_links$endTime <- as.POSIXct(direct_links$endTime, format="%Y-%m-%d %H:%M:%S")


# First/ Last click ######################################

direct_links$thinkRemain_1ST_CLICK <- as.numeric(as.character((direct_links$thinkRemain_1ST_CLICK)))
direct_links$thinkRemain_LASTCLICK <- as.numeric(as.character((direct_links$thinkRemain_LASTCLICK)))
direct_links$thinkRemain_SUBMIT <- as.numeric(as.character((direct_links$thinkRemain_SUBMIT)))
direct_links$thinkRemain_CLICKCOUNT <- as.numeric(as.character((direct_links$thinkRemain_CLICKCOUNT)))

direct_links$thinkLeave_1ST_CLICK <- as.numeric(as.character((direct_links$thinkLeave_1ST_CLICK)))
direct_links$thinkLeave_LASTCLICK <- as.numeric(as.character((direct_links$thinkLeave_LASTCLICK)))
direct_links$thinkLeave_SUBMIT <- as.numeric(as.character((direct_links$thinkLeave_SUBMIT)))
direct_links$thinkLeave_CLICKCOUNT <- as.numeric(as.character((direct_links$thinkLeave_CLICKCOUNT)))

direct_links$stress_1ST_CLICK <- as.numeric(as.character((direct_links$stress_1ST_CLICK)))
direct_links$stress_LAST_CLICK <- as.numeric(as.character((direct_links$stress_LAST_CLICK)))
direct_links$stress_SUBMIT <- as.numeric(as.character((direct_links$stress_SUBMIT)))
direct_links$stress_CLICKCOUNT <- as.numeric(as.character((direct_links$stress_CLICKCOUNT)))

direct_links$ctl_1ST_CLICK <- as.numeric(as.character((direct_links$ctl_1ST_CLICK)))
direct_links$ctl_LASTCLICK <- as.numeric(as.character((direct_links$ctl_LASTCLICK)))
direct_links$ctl_SUBMIT <- as.numeric(as.character((direct_links$ctl_SUBMIT)))
direct_links$ctl_CLICKCOUNT <- as.numeric(as.character((direct_links$ctl_CLICKCOUNT)))

direct_links$mp_1ST_CLICK <- as.numeric(as.character((direct_links$mp_1ST_CLICK)))
direct_links$mp_LASTCLICK <- as.numeric(as.character((direct_links$mp_LASTCLICK)))
direct_links$mp_SUBMIT <- as.numeric(as.character((direct_links$mp_SUBMIT)))
direct_links$mp_CLICKCOUNT <- as.numeric(as.character((direct_links$mp_CLICKCOUNT)))

# View how much time people spent on the misperceptions items
# direct_links[ , c("mp_1ST_CLICK", "mp_LASTCLICK", "mp_SUBMIT", "mp_CLICKCOUNT" ) ]

# End recoding direct_links. 



#########################################################

# RECODE c) pre_ref

#########################################################

# Start with the variables that differ 
# In the pre_ref dataset, I had one "neither true nor false" level; in the post_ref dataset
# I changed it to "don't know". I will rename it "don't know in the pre_ref dataset, too. 

pre_ref$m1_Turkey <- recode(pre_ref$m1_Turkey, '
                            "20"="Definitely true";
                            "21"="Probably true";
                            "23"="Probably false";
                            "24"="Definitely false";
                            "25"="Do not know" ',
                            levels = c("Definitely false", "Probably false", "Do not know",
                                       "Probably true", "Definitely true")
)
# str(pre_ref$m1_Turkey)
# levels(pre_ref$m1_Turkey)
# pre_ref$m1_Turkey <- as.factor(pre_ref$m1_Turkey)

pre_ref$m2_Army <- recode(pre_ref$m2_Army,'
                          "20"="Definitely true";
                          "21"="Probably true";
                          "23"="Probably false";
                          "24"="Definitely false";
                          "22"="Do not know" ',
                          levels = c("Definitely false", "Probably false", "Do not know",
                                     "Probably true", "Definitely true"))
# str(pre_ref$m2_Army)
# levels(pre_ref$m2_Army)

pre_ref$m3_NHS <- recode(pre_ref$m3_NHS, '
                         "20"="Definitely true";
                         "21"="Probably true";
                         "23"="Probably false";
                         "24"="Definitely false";
                         "22"="Do not know" ',
                         levels = c("Definitely false", "Probably false", "Do not know",
                                    "Probably true", "Definitely true"))
# str(pre_ref$m3_NHS)
# levels(pre_ref$m3_NHS)

pre_ref$m4_Euro <- recode(pre_ref$m4_Euro, '
                          "20"="Definitely true";
                          "21"="Probably true";
                          "23"="Probably false";
                          "24"="Definitely false";
                          "22"="Do not know" ',
                          levels = c("Definitely false", "Probably false", "Do not know",
                                     "Probably true", "Definitely true"))
# str(pre_ref$m4_Euro)
# levels(pre_ref$m4_Euro)

pre_ref$m5_Queen <- recode(pre_ref$m5_Queen, '
                           "20"="Definitely true";
                           "21"="Probably true";
                           "23"="Probably false";
                           "24"="Definitely false";
                           "22"="Do not know" ',
                           levels = c("Definitely false", "Probably false", "Do not know",
                                      "Probably true", "Definitely true"))
# str(pre_ref$m5_Queen)
# levels(pre_ref$m5_Queen)

pre_ref$poundPlunged <- recode(pre_ref$poundPlunged, '
                            "20"="Definitely true";
                                    "21"="Probably true";
                                    "23"="Probably false";
                                    "24"="Definitely false";
                                    "25"="Do not know" ',
                                    levels = c("Definitely false", "Probably false", "Do not know",
                                               "Probably true", "Definitely true"))

pre_ref$ScotlandRemain <- recode(pre_ref$ScotlandRemain, '
                                      "20"="Definitely true";
                                      "21"="Probably true";
                                      "23"="Probably false";
                                      "24"="Definitely false";
                                      "25"="Do not know" ',
                                      levels = c("Definitely false", "Probably false", "Do not know",
                                                 "Probably true", "Definitely true"))



# Starting here, same code as for post_ref and direct_links


# treatment Group assignment variables ##################

pre_ref$treatment <- as.factor(pre_ref$treatment)
pre_ref$trGroup_friend <- as.factor(pre_ref$trGroup_friend)



# liveCountry ###########################################

# str(pre_ref$liveCountry)
pre_ref$liveCountry <- as.factor(pre_ref$liveCountry)

levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="1"] <- "England"
levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="2"] <- "Scotland"
levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="3"] <- "Wales"
levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="4"] <- "Northern Ireland"
levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="5"] <- "Elsewhere in the EU"
levels(pre_ref$liveCountry)[levels(pre_ref$liveCountry)=="6"] <- "Outside the EU"

# plot(pre_ref$liveCountry)


# interest variables ####################################

# Q6 How interested were you in the EU referendum that was held in the UK on June 23rd?
pre_ref$interest_EUref <- as.numeric(as.character(pre_ref$interest_EUref))

pre_ref$interest_EUref_f <- as.factor(pre_ref$interest_EUref)
levels(pre_ref$interest_EUref_f)[levels(pre_ref$interest_EUref_f)=="1"] <- "Very interested"
levels(pre_ref$interest_EUref_f)[levels(pre_ref$interest_EUref_f)=="2"] <- "Somewhat interested"
levels(pre_ref$interest_EUref_f)[levels(pre_ref$interest_EUref_f)=="3"] <- "Not very interested"
levels(pre_ref$interest_EUref_f)[levels(pre_ref$interest_EUref_f)=="4"] <- "Not at all interested"
levels(pre_ref$interest_EUref_f)[levels(pre_ref$interest_EUref_f)=="5"] <- "Don't know"

# plot(pre_ref$interest_EUref_f)


# Q7 On a scale of 0 to 10, how much did or do you care about the following?

# Which side won the EU referendum on 23 June (1)
# pre_ref$interest_whoWon
pre_ref$interest_whoWon <- as.numeric(as.character(pre_ref$interest_whoWon))
# hist(pre_ref$interest_whoWon)

# The outcome of the next UK General Election (2)
# pre_ref$interest_nextElection
pre_ref$interest_nextElection <- as.numeric(as.character(pre_ref$interest_nextElection))
# truehist(pre_ref$interest_nextElection)

# Who wins the 2016 UEFA European Football Championship (3)
# pre_ref$interest_Euro2016
pre_ref$interest_Euro2016 <- as.numeric(as.character(pre_ref$interest_Euro2016))
# truehist(pre_ref$interest_Euro2016)



# insecurity -- after Brexit vote  #####################

# Q8 How sure are you about what will happen/what would have happened to the UK:

# pre_ref$whatHappens_leave

pre_ref$whatHappens_leave <- as.numeric(as.character(pre_ref$whatHappens_leave))

pre_ref$whatHappens_leave_f <- as.factor(pre_ref$whatHappens_leave)
levels(pre_ref$whatHappens_leave_f)[levels(pre_ref$whatHappens_leave_f)=="1"] <- "Very unsure"
levels(pre_ref$whatHappens_leave_f)[levels(pre_ref$whatHappens_leave_f)=="2"] <- "Quite unsure"
levels(pre_ref$whatHappens_leave_f)[levels(pre_ref$whatHappens_leave_f)=="3"] <- "Quite sure"
levels(pre_ref$whatHappens_leave_f)[levels(pre_ref$whatHappens_leave_f)=="4"] <- "Very sure"

# plot(pre_ref$whatHappens_leave_f) # BY VOTE! 


# insecurity -- after Remain vote  #####################

# pre_ref$whatHappens_remain

pre_ref$whatHappens_remain <- as.numeric(as.character(pre_ref$whatHappens_remain))

pre_ref$whatHappens_remain_f <- as.factor(pre_ref$whatHappens_remain)
levels(pre_ref$whatHappens_remain_f)[levels(pre_ref$whatHappens_remain_f)=="1"] <- "Very unsure"
levels(pre_ref$whatHappens_remain_f)[levels(pre_ref$whatHappens_remain_f)=="2"] <- "Quite unsure"
levels(pre_ref$whatHappens_remain_f)[levels(pre_ref$whatHappens_remain_f)=="3"] <- "Quite sure"
levels(pre_ref$whatHappens_remain_f)[levels(pre_ref$whatHappens_remain_f)=="4"] <- "Very sure"

# plot(pre_ref$whatHappens_remain_f) # BY VOTE! 



# reasons to leave ######################################

# Q9 Some say that there were good reasons for both options in this EU referendum.  
# Others think it was more clear-cut than that.  How many would you say there were of each 
# of the following?


# pre_ref$goodReasons_leave

pre_ref$goodReasons_leave <- as.numeric(as.character(pre_ref$goodReasons_leave))

pre_ref$goodReasons_leave_f <- as.factor(pre_ref$goodReasons_leave)
levels(pre_ref$goodReasons_leave_f)[levels(pre_ref$goodReasons_leave_f)=="1"] <- "None at all"
levels(pre_ref$goodReasons_leave_f)[levels(pre_ref$goodReasons_leave_f)=="2"] <- "Not very many"
levels(pre_ref$goodReasons_leave_f)[levels(pre_ref$goodReasons_leave_f)=="3"] <- "Quite a few"
levels(pre_ref$goodReasons_leave_f)[levels(pre_ref$goodReasons_leave_f)=="4"] <- "Very many"

# plot(pre_ref$goodReasons_leave_f) 


# pre_ref$goodReasons_remain

pre_ref$goodReasons_remain <- as.numeric(as.character(pre_ref$goodReasons_remain))

pre_ref$goodReasons_remain_f <- as.factor(pre_ref$goodReasons_remain)
levels(pre_ref$goodReasons_remain_f)[levels(pre_ref$goodReasons_remain_f)=="1"] <- "None at all"
levels(pre_ref$goodReasons_remain_f)[levels(pre_ref$goodReasons_remain_f)=="2"] <- "Not very many"
levels(pre_ref$goodReasons_remain_f)[levels(pre_ref$goodReasons_remain_f)=="3"] <- "Quite a few"
levels(pre_ref$goodReasons_remain_f)[levels(pre_ref$goodReasons_remain_f)=="4"] <- "Very many"

# plot(pre_ref$goodReasons_remain_f) 



# Changes after Brexit ####################################

# Q10, Q11 Do you think the following will be higher, lower or about the same now that the UK 
# has voted to leave the European Union?



# immigration ###########################################

pre_ref$change_immigration <- as.numeric(as.character(pre_ref$change_immigration))

pre_ref$change_immigration_f <- as.factor(pre_ref$change_immigration)
levels(pre_ref$change_immigration_f)[levels(pre_ref$change_immigration_f)=="1"] <- "Much lower"
levels(pre_ref$change_immigration_f)[levels(pre_ref$change_immigration_f)=="2"] <- "Lower"
levels(pre_ref$change_immigration_f)[levels(pre_ref$change_immigration_f)=="3"] <- "About the same"
levels(pre_ref$change_immigration_f)[levels(pre_ref$change_immigration_f)=="4"] <- "Higher"
levels(pre_ref$change_immigration_f)[levels(pre_ref$change_immigration_f)=="5"] <- "Much higher"

# plot(pre_ref$change_immigration_f)
# barplot(prop.table(table(pre_ref$change_immigration_f))) 


# terrorism ###########################################

# The risk of terrorism 

pre_ref$change_terrorism <- as.numeric(as.character(pre_ref$change_terrorism))

pre_ref$change_terrorism_f <- as.factor(pre_ref$change_terrorism)
levels(pre_ref$change_terrorism_f)[levels(pre_ref$change_terrorism_f)=="1"] <- "Much lower"
levels(pre_ref$change_terrorism_f)[levels(pre_ref$change_terrorism_f)=="2"] <- "Lower"
levels(pre_ref$change_terrorism_f)[levels(pre_ref$change_terrorism_f)=="3"] <- "About the same"
levels(pre_ref$change_terrorism_f)[levels(pre_ref$change_terrorism_f)=="4"] <- "Higher"
levels(pre_ref$change_terrorism_f)[levels(pre_ref$change_terrorism_f)=="5"] <- "Much higher"

# plot(pre_ref$change_terrorism_f)
# barplot(prop.table(table(pre_ref$change_terrorism_f))) 



# influence ###########################################

# Britain's influence in the world ... 

pre_ref$change_influence <- as.numeric(as.character(pre_ref$change_influence))

pre_ref$change_influence_f <- as.factor(pre_ref$change_influence)
levels(pre_ref$change_influence_f)[levels(pre_ref$change_influence_f)=="1"] <- "Much lower"
levels(pre_ref$change_influence_f)[levels(pre_ref$change_influence_f)=="2"] <- "Lower"
levels(pre_ref$change_influence_f)[levels(pre_ref$change_influence_f)=="3"] <- "About the same"
levels(pre_ref$change_influence_f)[levels(pre_ref$change_influence_f)=="4"] <- "Higher"
levels(pre_ref$change_influence_f)[levels(pre_ref$change_influence_f)=="5"] <- "Much higher"

# plot(pre_ref$change_influence_f)
# barplot(prop.table(table(pre_ref$change_influence_f))) 



# economy #############################################

# The general economic situation in the UK 

pre_ref$change_econ <- as.numeric(as.character(pre_ref$change_econ))

pre_ref$change_econ_f <- as.factor(pre_ref$change_econ)
levels(pre_ref$change_econ_f)[levels(pre_ref$change_econ_f)=="1"] <- "Much worse"
levels(pre_ref$change_econ_f)[levels(pre_ref$change_econ_f)=="2"] <- "Worse"
levels(pre_ref$change_econ_f)[levels(pre_ref$change_econ_f)=="3"] <- "About the same"
levels(pre_ref$change_econ_f)[levels(pre_ref$change_econ_f)=="4"] <- "Better"
levels(pre_ref$change_econ_f)[levels(pre_ref$change_econ_f)=="5"] <- "Much better"

# plot(pre_ref$change_econ_f)
# barplot(prop.table(table(pre_ref$change_econ_f))) 


# personal finances #######################################

# Your personal financial situation 

pre_ref$change_persFinance <- as.numeric(as.character(pre_ref$change_persFinance))

pre_ref$change_persFinance_f <- as.factor(pre_ref$change_persFinance)
levels(pre_ref$change_persFinance_f)[levels(pre_ref$change_persFinance_f)=="1"] <- "Much worse"
levels(pre_ref$change_persFinance_f)[levels(pre_ref$change_persFinance_f)=="2"] <- "Worse"
levels(pre_ref$change_persFinance_f)[levels(pre_ref$change_persFinance_f)=="3"] <- "About the same"
levels(pre_ref$change_persFinance_f)[levels(pre_ref$change_persFinance_f)=="4"] <- "Better"
levels(pre_ref$change_persFinance_f)[levels(pre_ref$change_persFinance_f)=="5"] <- "Much better"

# plot(pre_ref$change_persFinance_f)
# barplot(prop.table(table(pre_ref$change_persFinance_f))) 

# NHS #####################################################

# The NHS

pre_ref$change_NHS <- as.numeric(as.character(pre_ref$change_NHS))

pre_ref$change_NHS_f <- as.factor(pre_ref$change_NHS)
levels(pre_ref$change_NHS_f)[levels(pre_ref$change_NHS_f)=="1"] <- "Much worse"
levels(pre_ref$change_NHS_f)[levels(pre_ref$change_NHS_f)=="2"] <- "Worse"
levels(pre_ref$change_NHS_f)[levels(pre_ref$change_NHS_f)=="3"] <- "About the same"
levels(pre_ref$change_NHS_f)[levels(pre_ref$change_NHS_f)=="4"] <- "Better"
levels(pre_ref$change_NHS_f)[levels(pre_ref$change_NHS_f)=="5"] <- "Much better"

# plot(pre_ref$change_NHS_f)
# barplot(prop.table(table(pre_ref$change_NHS_f))) 



# vote ##################################################

# Q6 To the best of your knowledge, are you eligible and registered to vote in the EU referendum?

# registered
# str(pre_ref$registered)
pre_ref$registered <- as.factor(pre_ref$registered)
levels(pre_ref$registered)[levels(pre_ref$registered)=="1"] <- "Yes"
levels(pre_ref$registered)[levels(pre_ref$registered)=="2"] <- "No, eligible but not registered"
levels(pre_ref$registered)[levels(pre_ref$registered)=="3"] <- "No, too young to vote"
levels(pre_ref$registered)[levels(pre_ref$registered)=="5"] <- "No, not eligible (living elsewhere / other reason)"
levels(pre_ref$registered)[levels(pre_ref$registered)=="6"] <- "Don't know"

# Q8 Many people don't vote in elections these days. How likely is it that you will vote 
# in the referendum on Britain’s membership of the European Union on June 23rd?

# Q9 If you do vote in the referendum on Britain's membership of the European Union, 
# how do you think you will vote?

# vote 
# str(pre_ref$vote)
# levels(pre_ref$vote)
pre_ref$vote <- as.factor(pre_ref$vote)
levels(pre_ref$vote)[levels(pre_ref$vote)=="1"] <- "leave"
levels(pre_ref$vote)[levels(pre_ref$vote)=="2"] <- "remain"
levels(pre_ref$vote)[levels(pre_ref$vote)=="3"] <- "don't know"
levels(pre_ref$vote)[levels(pre_ref$vote)=="4"] <- "not vote"
pre_ref$vote <- factor(pre_ref$vote, c("leave", "remain", "don't know", "not vote"))



# how sure ##############################################

# Q16 And how sure were you about your referendum vote choice?

# str(pre_ref$howSure_vote)
pre_ref$howSure_vote <- as.numeric(as.character(pre_ref$howSure_vote))

# as factor (I invented the labels except for the 1st and last)
pre_ref$howSure_vote_f <- as.factor(pre_ref$howSure_vote)
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="0"] <- "not sure at all"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="1"] <- "not sure"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="2"] <- "a little unsure"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="3"] <- "don't know"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="4"] <- "pretty sure"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="5"] <- "sure"
levels(pre_ref$howSure_vote_f)[levels(pre_ref$howSure_vote_f)=="6"] <- "absolutely sure"


# Psych vars ############################################

# SecondRef ##############################################

# Q20 Now that the UK has voted to leave the EU, do you think the decision will be final or 
# might there be a second EU referendum?

pre_ref$SecondRef <- as.factor(pre_ref$SecondRef)
pre_ref$SecondRef_f[pre_ref$SecondRef == "1"] <- "decision is final"
pre_ref$SecondRef_f[pre_ref$SecondRef == "2"] <- "second referendum"



# demographics ###########################################

# gender #################################################

pre_ref$gender <- as.factor(pre_ref$gender)
levels(pre_ref$gender)[levels(pre_ref$gender)=="1"] <- "male"  
levels(pre_ref$gender)[levels(pre_ref$gender)=="2"] <- "female"  


# age ####################################################

pre_ref$born <- as.character(pre_ref$born)

pre_ref$born[pre_ref$born == "88"] <- "1988"
pre_ref$born[pre_ref$born == "37"] <- "1988"
pre_ref$born[pre_ref$born == "57"] <- "1988"
pre_ref$born[pre_ref$born == "0"] <- NA

pre_ref$born <- as.numeric(as.character(pre_ref$born)) 


# education ##############################################

# Q28 At what age did you finish full-time education?

pre_ref$education <- as.numeric(as.character(pre_ref$education))

pre_ref$education_f <- as.factor(pre_ref$education)
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="1"] <- "15 or younger"  
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="2"] <- "16" 
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="3"] <- "17" 
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="4"] <- "18" 
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="5"] <- "19 or older" 
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="6"] <- "Still at school in full-time education" 
levels(pre_ref$education_f)[levels(pre_ref$education_f)=="7"] <- "Still at university in full-time education"

# plot(pre_ref$education_f)


# marital status #########################################

# Q30 Which of these best describes your marital status?

pre_ref$maritalStatus <- as.factor(pre_ref$maritalStatus)
levels(pre_ref$maritalStatus)[levels(pre_ref$maritalStatus)=="1"] <- "Married" 
levels(pre_ref$maritalStatus)[levels(pre_ref$maritalStatus)=="2"] <- "Living as married "
levels(pre_ref$maritalStatus)[levels(pre_ref$maritalStatus)=="3"] <- "Divorced"
levels(pre_ref$maritalStatus)[levels(pre_ref$maritalStatus)=="4"] <- "Widowed"
levels(pre_ref$maritalStatus)[levels(pre_ref$maritalStatus)=="5"] <- "Single"


# town / village #########################################

# Q31 Is your home (or your main home if you have more than one) in..

str(pre_ref$townVillage)
levels(pre_ref$townVillage)[levels(pre_ref$townVillage)=="1"] <- "centre of a city or large town" 
levels(pre_ref$townVillage)[levels(pre_ref$townVillage)=="2"] <- "suburb of a city or large town" 
levels(pre_ref$townVillage)[levels(pre_ref$townVillage)=="3"] <- "small or medium-sized town" 
levels(pre_ref$townVillage)[levels(pre_ref$townVillage)=="4"] <- "village" 
levels(pre_ref$townVillage)[levels(pre_ref$townVillage)=="5"] <- "countryside" 

 
# agreeableness #########################################

# Q37_7 ______ Sympathetic, warm (7)
pre_ref$agreeableness <- as.numeric(as.character(pre_ref$agreeableness))

# Q36_2 ______ Critical, quarrelsome (2)
pre_ref$agreeableness_r <- recode(pre_ref$agreeableness_R, '  
                                  "1"="7";
                                  "2"="6";
                                  "3"="5";
                                  "4"="4";
                                  "5"="3";
                                  "6"="2";
                                  "7"="1" ')
pre_ref$agreeableness_r <- as.numeric(as.character(pre_ref$agreeableness_r))


# conscientiousness #####################################

# Q36_3 ______ Dependable, self-disciplined (3)
pre_ref$conscientiousness <- as.numeric(as.character(pre_ref$conscientiousness))

# Q37_8 ______ Disorganized, careless (8)
pre_ref$conscientiousness_r <- recode(pre_ref$conscientiousness_R, '  
                                      "1"="7"; 
                                      "2"="6";
                                      "3"="5";
                                      "4"="4";
                                      "5"="3";
                                      "6"="2";
                                      "7"="1" ')
pre_ref$conscientiousness_r <- as.numeric(as.character(pre_ref$conscientiousness_r))


# openness ##############################################

# Q36_5 ______ Open to new experiences, complex (5)
pre_ref$openness <- as.numeric(as.character(pre_ref$openness))

# Q37_10 ______ Conventional, uncreative (10)
pre_ref$openness_r <- recode(pre_ref$openness_R, '  
                             "1"="7"; 
                             "2"="6";
                             "3"="5";
                             "4"="4";
                             "5"="3";
                             "6"="2";
                             "7"="1" ')
pre_ref$openness_r <- as.numeric(as.character(pre_ref$openness_r))


# neuroticism ###########################################

# Q36_4 ______ Anxious, easily upset (4)
pre_ref$neuroticism <- as.numeric(as.character(pre_ref$neuroticism))

# Q37_9 ______ Calm, emotionally stable (9)
pre_ref$neuroticism_r <- recode(pre_ref$neuroticism_R, '  
                                "1"="7"; 
                                "2"="6";
                                "3"="5";
                                "4"="4";
                                "5"="3";
                                "6"="2";
                                "7"="1" ')
pre_ref$neuroticism_r <- as.numeric(as.character(pre_ref$neuroticism_r))




# accuracy goals ########################################

# Q38 Now everybody has a different approach to taking decisions. 
# To what extent do these statements describe your voting decision in the EU referendum?

pre_ref$gutDecision <- as.factor(pre_ref$gutDecision)
pre_ref$gutDecision <- recode(pre_ref$gutDecision, '
                              "11"="Definitely true";
                              "12"="Probably true";
                              "14"="Probably false";
                              "15"="Definitely false";
                              "16"="Do not know" ')

# based on facts #######################################

pre_ref$basedOnFacts <- factor(recode(pre_ref$basedOnFacts, '
                                      "11"="Definitely true";
                                      "12"="Probably true";
                                      "14"="Probably false";
                                      "15"="Definitely false";
                                      "16"="Do not know" ')) 


# self esteem ##########################################

# Q39 And how far would you say that this is true of you:  "I have high self-esteem."

# str(pre_ref$selfEsteem) # 1 = Not very true of me, 7 = Very true of me
pre_ref$selfEsteem <- as.numeric(pre_ref$selfEsteem)
# hist(pre_ref$selfEsteem)


# Subjective social status ##############################

# Q40 Think of this ladder as representing where people stand in the United Kingdom...

pre_ref$subjSocialStatus <- as.character(pre_ref$subjSocialStatus)

pre_ref$subjSocialStatus[pre_ref$subjSocialStatus=="four"] <- "4" # pre_ref
pre_ref$subjSocialStatus[pre_ref$subjSocialStatus=="7 or 8"] <- "7.5" # pre_ref
pre_ref$subjSocialStatus[pre_ref$subjSocialStatus=="7-8"] <- "7.5" # pre_ref
pre_ref$subjSocialStatus[pre_ref$subjSocialStatus=="5/6"] <- "5.5" # pre_ref

pre_ref$subjSocialStatus <- as.numeric(as.character((pre_ref$subjSocialStatus))) 



# life satisfaction #####################################

# Q41 On  the whole, how satisfied are you with the life you lead ?

# as factor
pre_ref$lifeSatisfaction_f <- as.factor(pre_ref$lifeSatisfaction)
levels(pre_ref$lifeSatisfaction_f)[levels(pre_ref$lifeSatisfaction_f)=="1"] <- "satisfied"
levels(pre_ref$lifeSatisfaction_f)[levels(pre_ref$lifeSatisfaction_f)=="2"] <- "fairly satisfied"
levels(pre_ref$lifeSatisfaction_f)[levels(pre_ref$lifeSatisfaction_f)=="3"] <- "not very satisfied"
levels(pre_ref$lifeSatisfaction_f)[levels(pre_ref$lifeSatisfaction_f)=="4"] <- "not satisfied"

# str(pre_ref$lifeSatisfaction)
# truehist(pre_ref$lifeSatisfaction)
# table(pre_ref$lifeSatisfaction)
# plot(pre_ref$lifeSatisfaction_f)
# barplot(prop.table(table(pre_ref$lifeSatisfaction_f))) 

# as numeric
pre_ref$lifeSatisfaction_num <- as.numeric(pre_ref$lifeSatisfaction_f)




# EU knowledge ##########################################

# Q33 True or false? We've introduced a time limit to make this a bit more exciting!  
# You have 35 seconds to answer these questions.

# Q54 True or false?

pre_ref$stress_EU28 <- recode(pre_ref$stress_EU28, '
                              "4"="True";
                              "7"="False";
                              "8"="Do not know" ')

pre_ref$ctl_EU28 <- recode(pre_ref$ctl_EU28,'
                           "4"="True";
                           "7"="False";
                           "8"="Do not know" ')

pre_ref$stress_CHinEU <- recode(pre_ref$stress_CHinEU, '
                                "4"="True";
                                "7"="False";
                                "8"="Do not know" ')

pre_ref$ctl_CHinEU <- recode(pre_ref$ctl_CHinEU, '
                             "4"="True";
                             "7"="False";
                             "8"="Do not know" ')

pre_ref$stress_MEPsElected <- recode(pre_ref$stress_MEPsElected, '
                                     "4"="True";
                                     "7"="False";
                                     "8"="Do not know" ')

pre_ref$ctl_MEPsElected <- recode(pre_ref$ctl_MEPsElected,'
                                  "4"="True";
                                  "7"="False";
                                  "8"="Do not know" ')




# In Common #############################################

# Q50 When you think about the following groups, how much would you say you have in 
# common with them (apart from what they think about Europe) ? 

# with Remainers?
pre_ref$inCommon_Remainers <- as.numeric(pre_ref$inCommon_Remainers) # the higher the more in common

# with Brexiteers?
pre_ref$inCommon_Brexiteers <- as.numeric(pre_ref$inCommon_Brexiteers) # the higher the more in common



# Date / time variables #################################

pre_ref$startTime <- as.POSIXct(pre_ref$startTime, format="%Y-%m-%d %H:%M:%S")
pre_ref$endTime <- as.POSIXct(pre_ref$endTime, format="%Y-%m-%d %H:%M:%S")


# First/ Last click ######################################

pre_ref$thinkRemain_1ST_CLICK <- as.numeric(as.character((pre_ref$thinkRemain_1ST_CLICK)))
pre_ref$thinkRemain_LASTCLICK <- as.numeric(as.character((pre_ref$thinkRemain_LASTCLICK)))
pre_ref$thinkRemain_SUBMIT <- as.numeric(as.character((pre_ref$thinkRemain_SUBMIT)))
pre_ref$thinkRemain_CLICKCOUNT <- as.numeric(as.character((pre_ref$thinkRemain_CLICKCOUNT)))

pre_ref$thinkLeave_1ST_CLICK <- as.numeric(as.character((pre_ref$thinkLeave_1ST_CLICK)))
pre_ref$thinkLeave_LASTCLICK <- as.numeric(as.character((pre_ref$thinkLeave_LASTCLICK)))
pre_ref$thinkLeave_SUBMIT <- as.numeric(as.character((pre_ref$thinkLeave_SUBMIT)))
pre_ref$thinkLeave_CLICKCOUNT <- as.numeric(as.character((pre_ref$thinkLeave_CLICKCOUNT)))

pre_ref$stress_1ST_CLICK <- as.numeric(as.character((pre_ref$stress_1ST_CLICK)))
pre_ref$stress_LAST_CLICK <- as.numeric(as.character((pre_ref$stress_LAST_CLICK)))
pre_ref$stress_SUBMIT <- as.numeric(as.character((pre_ref$stress_SUBMIT)))
pre_ref$stress_CLICKCOUNT <- as.numeric(as.character((pre_ref$stress_CLICKCOUNT)))

pre_ref$ctl_1ST_CLICK <- as.numeric(as.character((pre_ref$ctl_1ST_CLICK)))
pre_ref$ctl_LASTCLICK <- as.numeric(as.character((pre_ref$ctl_LASTCLICK)))
pre_ref$ctl_SUBMIT <- as.numeric(as.character((pre_ref$ctl_SUBMIT)))
pre_ref$ctl_CLICKCOUNT <- as.numeric(as.character((pre_ref$ctl_CLICKCOUNT)))

pre_ref$mp_1ST_CLICK <- as.numeric(as.character((pre_ref$mp_pre_ref_1ST_CLICK)))
pre_ref$mp_LASTCLICK <- as.numeric(as.character((pre_ref$mp_pre_ref_LASTCLICK)))
pre_ref$mp_SUBMIT <- as.numeric(as.character((pre_ref$mp_pre_ref_SUBMIT)))
pre_ref$mp_CLICKCOUNT <- as.numeric(as.character((pre_ref$mp_pre_ref_CLICKCOUNT)))


# View how much time people spent on the misperceptions items
# pre_ref[ , c("mp_1ST_CLICK", "mp_LASTCLICK", "mp_SUBMIT", "mp_CLICKCOUNT" ) ]

# End recoding pre_ref. 



# NB -- I DID NOT ADAPT THE QUESTION WORDING for all of the variables






#########################################################

# Sort out incomplete cases & sort by start date

#########################################################


# pre_ref ###############################################

all_pre_ref <- pre_ref
nrow(all_pre_ref) # 41
pre_ref <- pre_ref[ which(pre_ref$finished=="1"), ] 
nrow(pre_ref) # 24 people finished the entire survey


# post_ref ##############################################

all_post_ref <- post_ref
nrow(all_post_ref) # 382
post_ref <- post_ref[ which(post_ref$finished=="1"), ] 
nrow(post_ref) # 278 people finished the entire survey

# 382-278 # 104 excluded because they did not finish

# Sort by start time 
post_ref <- post_ref[order(post_ref$startTime) , ]


# One lady said she had taken this survey before -- she has 4 email accounts
# which(post_ref$twitterName == "strayfish") # not in there
# which(post_ref$twitterName == "disabledanimals") # 251
# which(post_ref$twitterName == "DemTigerpaw") # not in there
# which(post_ref$twitterName == "GQinterview") # not in there


# direct links ##########################################

all_direct_links <- direct_links
nrow(all_direct_links) # 149
direct_links <- direct_links[ which(direct_links$finished=="1"), ] 
nrow(direct_links) # 103 people finished the entire survey

# Sort by start time 
all_direct_links <- all_direct_links[order(all_direct_links$startTime) , ]
direct_links <- direct_links[order(direct_links$startTime) , ]




#########################################################

# Scan for personal acquaintances

#########################################################

# post_ref
post_ref$twitter_name # I don't recognize any of these twitter names

# direct_links
levels(direct_links$twitterName_writeIn)

# Victoria Curran
direct_links <- 
  direct_links[ - which(direct_links$twitterName_writeIn=="VicWorks"), ]

# Joe Greenwood @niceonecombo 
# E-mailed me (gmail) on 5/7/ 19:09 "I'll crack on with it now." 
# but cannot find him -- first entries are on 6/7/2017
# CHECK time settings in qualtrics (and if that does not work, scan by year of birth)

# Phillip Nelson (not on facebook) -- possibly ask what he wrote for whatMatteredMost
direct_links[ which(direct_links$born=="1985" & 
                      direct_links$liveCountry == "England"
), ] ## ?? 

# Scott Huntly @ScottDafydd


# Hannah Jaeschke (not on twitter, but will be dropped as living elsewhere in the EU)

# QUESTION: Do I need to exclude acquaintances of acquaintances?

# Joe knows...
# jantalipinski 
# newildash
# etinsuburbiaego
# 1WayOrAnother

# levels(direct_links$twitterName_writeIn)

# Email this to Phillip to see if he recognises any of them
# yersin
# sandy
# robryan_uk
# lewisacroberts
# ganesha_be
# waller67
# bazippit
# harrymason96
# Mark Moore
# sheltiesretreat
# junius101
# HelAGoode
# goingtotheheart
# tcjuk
# ascorbic
# balchinlawyer
# hugoschonbeck
# chairmanmoet
# adamdfox
# leepettman
# Docemz
# johnfass
# janeymac_jane
# Fifield_Jem
# stefanpaetow


#########################################################
# Exclude those living outside the UK 
#########################################################

# Because I am not sure if they have been exposed to any of the campaign statements

# Exclude non-UK residents from the pre_ref sample

# length(which(pre_ref$liveCountry == "Outside the EU" |
#                pre_ref$liveCountry=="Elsewhere in the EU")) # 5
# 
# pre_ref_UK <- pre_ref[- which(pre_ref$liveCountry == "Outside the EU" |
#                                  pre_ref$liveCountry=="Elsewhere in the EU"), ] # 264



# post_ref 

# length(which(post_ref$liveCountry == "Outside the EU" |
#                 post_ref$liveCountry=="Elsewhere in the EU")) # 14

post_ref_UK <- post_ref[- which(post_ref$liveCountry == "Outside the EU" |
                                  post_ref$liveCountry=="Elsewhere in the EU"), ] # 264
# table(post_ref_UK$liveCountry)

# 278-0-14 # total number of 264 participants 


# direct_links 

# length(which(direct_links$liveCountry == "Outside the EU" |
#                 direct_links$liveCountry=="Elsewhere in the EU")) # 9
# length(direct_links)

# 103-1-9 # total number of 93 participants (direct links who finished - pers acquaintances - nonUK)

direct_links_UK <- direct_links[- which(direct_links$liveCountry ==
                                          "Outside the EU" |
                                          direct_links$liveCountry=="Elsewhere in the EU"), ] # 93
# table(direct_links_UK$liveCountry)







#########################################################

# Compare samples

#########################################################


# to see if they differ substantially in terms of... 

# vote
# Gender
# Age
# Living
# Education
# SHOULD CHECK BUT DONT HAVE THESE VARIABLES
# party ID
# Income


# vote choice / vote intention ###########################

# plot(pre_ref$vote)
# table(pre_ref$vote) # 5 leave, 15 (75%) remain
# prop.table(table(pre_ref$vote))

# plot(post_ref$vote) 
# table(post_ref$vote) # 71 leave, 174 (70%) remain, 3 not vote  
# prop.table(table(post_ref$vote))

plot(direct_links$vote) 
table(direct_links$vote) # 29 leave, 59 (65%) remain, 2 not vote
prop.table(table(direct_links$vote)) 

# # Compare the UK-only samples
prop.table(table(post_ref_UK$vote)) # 70% remain
prop.table(table(direct_links_UK$vote)) # 64% remain


# gender ################################################

# plot(pre_ref$gender)
# table(pre_ref$gender) 

# plot(post_ref$gender)
table(post_ref$gender) 
prop.table(table(post_ref$gender) )

# plot(direct_links$gender)  
table(direct_links$gender) 
prop.table(table(direct_links$gender)) # 52% male -- a lot more women 

# Compare the UK-only samples
# plot(post_ref_UK$gender)
prop.table(table(post_ref_UK$gender) ) # 66% male
# plot(direct_links_UK$gender)  # similar
prop.table(table(direct_links_UK$gender) ) # 53% male


# born ##################################################

# hist(pre_ref$born)
hist(post_ref$born) 
hist(direct_links$born) # not too different, both mainly 5, i.e. 19 or older Q28

# Compare the UK-only samples
hist(post_ref_UK$born)
boxplot(post_ref_UK$born)
mean(post_ref_UK$born, na.rm = T) # 1977
sd(post_ref_UK$born, na.rm = T) # 13.89
# mean age
2016-1977 # 39

hist(direct_links_UK$born) # similar
boxplot(direct_links_UK$born)
mean(direct_links_UK$born, na.rm = T) # 1974
sd(direct_links_UK$born, na.rm = T) # 14.17
# mean age
2016-1974 # 42



# live ##################################################

# plot(pre_ref$liveCountry) 
# table(pre_ref$liveCountry) # 17 England, 2 Scotland, 3 Elsewhere, 2 Outside the EU

plot(post_ref$liveCountry) 
table(post_ref$liveCountry) # 237 England, 22 Scotland, 3 Wales, 2 Northern Ireland, 11 Elsewhere in the EU

plot(direct_links$liveCountry) 
table(direct_links$liveCountry) # 82 England, 4 Scotland, 6 Wales, 1 Northern Ireland, 8 Elsewhere in the EU

# Compare the UK-only samples
plot(post_ref_UK$liveCountry)
prop.table(table(post_ref_UK$liveCountry)) # 90% England, 8% Scotland, 1% Wales, 1% Northern Ireland
plot(direct_links_UK$liveCountry) # similar
prop.table(table(direct_links_UK$liveCountry)) # 88% England, 4% Scotland, 1% Wales, 1% Northern Ireland


# education #############################################

# plot(pre_ref$education_f)
plot(post_ref$education_f)
plot(direct_links$education_f)

# Compare the UK-only samples
plot(post_ref_UK$education_f)
prop.table(table(post_ref_UK$education_f)) # 57% 19 or older
plot(direct_links_UK$education_f) # similar
prop.table(table(direct_links_UK$education_f)) # 66% 19 or older


# source of income ######################################

levels(post_ref$sourceIncome)

# plot(pre_ref$sourceIncome)
plot(post_ref$sourceIncome)
plot(direct_links$sourceIncome)

# # Compare the UK-only samples
# plot(post_ref_UK$sourceIncome)
prop.table(table(post_ref_UK$sourceIncome)) # 71 % employment, 1.5% unemployment benefits
# plot(direct_links_UK$sourceIncome)
prop.table(table(direct_links_UK$sourceIncome)) # 80% employment, 1% unemployment benefits




#########################################################

# Prep to Merge -- see if I have the same factor levels etc.

#########################################################

# str(direct_links)
# str(post_ref)

# Drop unused levels
pre_ref <- droplevels(pre_ref)
direct_links <- droplevels(direct_links)
post_ref <- droplevels(post_ref)

pre_ref_UK <- droplevels(pre_ref_UK)
direct_links_UK <- droplevels(direct_links_UK)
post_ref_UK <- droplevels(post_ref_UK)





#########################################################

# Merge the 2 post-ref datasets

#########################################################

# NB: I am merging these after I renamed all the variables 
# in the 3 data sets and adapted the levels but BEFORE I 
# created any new variables

# library(dplyr)
df_all <- full_join(post_ref, direct_links) # 381
df_UK <- full_join(post_ref_UK, direct_links_UK) # 357

# 264+93 # 357

# warnings()

# I will use the UK-only dataset
df <- df_UK
df <- droplevels(df)

# Look at ALL variables
# str(df, list.len=ncol(df))

# Double check that I excluded those who did not finish
# df <- df[ which(df$finished=="1"), ] # still 357




# OLD
# Drop the labels in the dataframe (they are in rows 1 & 284 of the merged dataset)
# df <- subset(df[-(c(1, 384)), ])
# post_ref2$V4
# post_ref2 <- post_ref2[-(c(1, 384)), ] # Be careful not to run this more than once




# Merge the 3 datasets ###################################

# library(dplyr)
# all <- full_join(post_ref2, pre_ref) # need to get levels straight first 

# Take out the label row from pre_ref 
# df <- df[-(c(532)), ] # Be careful not to run this more than once

# Drop unused levels
# df <- droplevels(df)

# Look at ALL variables
# str(df, list.len=ncol(df))

# 149-103



#########################################################
# Create new variables in df
#########################################################


# Date ##################################################

df$Date <- as.Date(df$startTime)



# interest ##############################################

df$interest_whoWon_d <- factor(
  ifelse(df$interest_whoWon < 10, "less than 10", "10"), 
  levels = c("less than 10", "10"))

# plot(df$interest_whoWon_d)



# insecurity variables ##################################

# df$whatHappens_leave

df$whatHappens_leave <- as.numeric(as.character(df$whatHappens_leave))

df$whatHappens_leave_f <- as.factor(df$whatHappens_leave)
levels(df$whatHappens_leave_f)[levels(df$whatHappens_leave_f)=="1"] <- "Very unsure"
levels(df$whatHappens_leave_f)[levels(df$whatHappens_leave_f)=="2"] <- "Quite unsure"
levels(df$whatHappens_leave_f)[levels(df$whatHappens_leave_f)=="3"] <- "Quite sure"
levels(df$whatHappens_leave_f)[levels(df$whatHappens_leave_f)=="4"] <- "Very sure"

# plot(df$whatHappens_leave_f) # BY VOTE! 


# df$whatHappens_remain

df$whatHappens_remain <- as.numeric(as.character(df$whatHappens_remain))

df$whatHappens_remain_f <- as.factor(df$whatHappens_remain)
levels(df$whatHappens_remain_f)[levels(df$whatHappens_remain_f)=="1"] <- "Very unsure"
levels(df$whatHappens_remain_f)[levels(df$whatHappens_remain_f)=="2"] <- "Quite unsure"
levels(df$whatHappens_remain_f)[levels(df$whatHappens_remain_f)=="3"] <- "Quite sure"
levels(df$whatHappens_remain_f)[levels(df$whatHappens_remain_f)=="4"] <- "Very sure"

# plot(df$whatHappens_remain_f) # BY VOTE! 



# vote ##################################################

# Create a factor variable with 3 levels

df$howSure_sur3 <- ifelse(df$howSure_vote == 6, "die_hard",
                          ifelse(df$howSure_vote == 4 | df$howSure_vote == 5, "doubtful", 
                                 "no clue"))
df$howSure_sur3 <- as.factor(df$howSure_sur3)


# Create a factor variable with 2 levels die_hard

df$die_hard <- ifelse(df$howSure_vote == 6, "die-hard", "not so sure")
df$die_hard <- as.factor(df$die_hard)


# Look at NAs in the "voted" variable.
# df[is.na(df$vote), c("vote", "registered", "wouldHave_voted")]

# ALL of the missings ticked: 
# 5 = No, not registered -- living elsewhere (or for some other reason)
# 3 = No, too young to vote
# 2 = No, eligible but not registered

# Create new variable voted OR would have voted if could have voted
# --> This includes people who DID NOT ACTUALLY VOTE

df$votedORwouldve <- df$vote
df$votedORwouldve[is.na(df$vote) & df$wouldHave_voted=="leave"] <- "leave"
df$votedORwouldve[is.na(df$vote) & df$wouldHave_voted=="remain"] <- "remain"
df$votedORwouldve[is.na(df$vote) & df$wouldHave_voted=="not vote"] <- "not vote"

# numeric 
df$vote_num[df$votedORwouldve=="leave"] <- "6"
df$vote_num[df$votedORwouldve=="remain"] <- "0"
df$vote_num[df$votedORwouldve=="not vote"] <- "3"
df$vote_num <- as.numeric(as.character(df$vote_num))

# Look at vote and votedORwouldve
# df[, c("vote", "wouldHave_voted", "votedORwouldve")]

# Create dummy - voteLeave
df$voteLeave <- ifelse(df$vote=="leave", 1, 0)
str(df$voteLeave)
df$voteLeave <- as.factor(df$voteLeave)
levels(df$voteLeave)[levels(df$voteLeave)=="1"] <- "leave"
levels(df$voteLeave)[levels(df$voteLeave)=="0"] <- "remain"


# Create dummy - votedORwouldve_Leave
df$votedORwouldve_Leave <- ifelse(df$votedORwouldve=="leave", 1, 0)
str(df$votedORwouldve_Leave)
df$votedORwouldve_Leave <- as.factor(df$votedORwouldve_Leave)
levels(df$votedORwouldve_Leave)[levels(df$votedORwouldve_Leave)=="1"] <- "leave"
levels(df$votedORwouldve_Leave)[levels(df$votedORwouldve_Leave)=="0"] <- "remain"

# table(df$misperceptions_sur5, df$votedORwouldve_Leave) # looks good



# how sure ##############################################

str(df$howSure_vote)
df$howSure_vote <- as.numeric(as.character(df$howSure_vote))

# as factor (I invented the labels except for the 1st and last)
df$howSure_vote_f <- as.factor(df$howSure_vote)
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="0"] <- "not sure at all"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="1"] <- "not sure"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="2"] <- "a little unsure"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="3"] <- "don't know"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="4"] <- "pretty sure"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="5"] <- "sure"
levels(df$howSure_vote_f)[levels(df$howSure_vote_f)=="6"] <- "absolutely sure"


# Create a factor variable with 3 levels

df$howSure_sur3 <- ifelse(df$howSure_vote == 6, "die_hard",
                          ifelse(df$howSure_vote == 4 | df$howSure_vote == 5, "doubtful", 
                                 "no clue"))
df$howSure_sur3 <- as.factor(df$howSure_sur3)


# Create a factor variable with 2 levels

df$howSure_sur2 <- ifelse(df$howSure_vote == 6, "die_hard", "doubtful")
df$howSure_sur2 <- as.factor(df$howSure_sur2)




# vote_cat ##############################################

# Create a factor variable for strength of party affiliation

# 5-level vote category var

df$vote_cat <- factor(df$vote_cat, 
                      levels = c("die_hard_remain", "doubtful_remain", "did not vote", 
                                 "doubtful_leave", "die_hard_leave"))
df$vote_cat[df$votedORwouldve == "remain" & df$howSure_vote == 6] <- "die_hard_remain"
df$vote_cat[df$votedORwouldve == "remain" & df$howSure_vote != 6] <- "doubtful_remain"
df$vote_cat[df$votedORwouldve == "not vote"] <- "did not vote"
df$vote_cat[df$votedORwouldve == "leave" & df$howSure_vote != 6] <- "doubtful_leave"
df$vote_cat[df$votedORwouldve == "leave" & df$howSure_vote == 6] <- "die_hard_leave"


# 4-level vote category var

df$vote_cat <- factor(df$vote_cat, 
                      levels = c("die-hard remain", "remain", 
                                 "leave", "die-hard leave"))
df$vote_cat[df$votedORwouldve == "remain" & df$howSure_vote == 6] <- "die-hard remain"
df$vote_cat[df$votedORwouldve == "remain" & df$howSure_vote != 6] <- "remain"
# df$vote_cat[df$votedORwouldve == "not vote"] <- "did not vote"
df$vote_cat[df$votedORwouldve == "leave" & df$howSure_vote != 6] <- "leave"
df$vote_cat[df$votedORwouldve == "leave" & df$howSure_vote == 6] <- "die-hard leave"


# plot(df$vote_cat)
# table(df$vote_cat)

# 6-level vote category var
df$vote_index <- factor(NA, 
                        levels = c("die_hard_remain", 
                                   "remain", 
                                   "doubtful_remain", 
                                   "did not vote", 
                                   "doubtful_leave", 
                                   "leave", 
                                   "die_hard_leave"))
# changed from factor(df$vote_index) which gave me this ERROR:
# replacement has 0 rows, data has 380

df$vote_index[df$votedORwouldve == "remain" & df$howSure_vote == 6] <- "die_hard_remain"
df$vote_index[df$votedORwouldve == "remain" & df$howSure_vote == 5] <- "remain"
df$vote_index[df$votedORwouldve == "remain" & df$howSure_vote < 5] <- "doubtful_remain"
df$vote_index[df$votedORwouldve == "not vote"] <- "did not vote"
df$vote_index[df$votedORwouldve == "leave" & df$howSure_vote < 5] <- "doubtful_leave"
df$vote_index[df$votedORwouldve == "leave" & df$howSure_vote == 5] <- "leave"
df$vote_index[df$votedORwouldve == "leave" & df$howSure_vote == 6] <- "die_hard_leave"

# plot(df$vote_index)
# df$votedORwouldve


# Create a strength of prior attitude index, taking into account 
# Vote
# How sure
# Good reasons to leave / remain



# strength of conviction ################################

# df$goodReasons_leave
df$goodReasons_leave <- as.numeric(as.character(df$goodReasons_leave))

# df$goodReasons_remain
df$goodReasons_remain <- as.numeric(as.character(df$goodReasons_remain))




# changes after Brexit ####################################

# Do you think the following will be higher, lower or about the same now that the UK 
# has voted to leave the European Union?



# immigration ###########################################

df$change_immigration_f <- as.factor(df$change_immigration_f)
# str(df$change_immigration_f)
# levels(df$change_immigration_f)

df$immigrationWillRise <- factor(ifelse(df$change_immigration_f=="Higher" |
                                          df$change_immigration_f=="Much higher", "will rise", "will not rise"),
                                 levels = c("will rise", "will not rise"))

df$immigrationWillDrop <- factor(ifelse(df$change_immigration_f=="Lower" |
                                          df$change_immigration_f=="Much lower", "will drop", "will not drop"),
                                 levels = c("will drop", "will not drop"))
# str(df$change_immigration_d)



# terrorism ###########################################

# The risk of terrorism 

# df$change_terrorism <- as.numeric(as.character(df$change_terrorism))

df$change_terrorism_f <- as.factor(df$change_terrorism)
levels(df$change_terrorism_f)[levels(df$change_terrorism_f)=="1"] <- "Much lower"
levels(df$change_terrorism_f)[levels(df$change_terrorism_f)=="2"] <- "Lower"
levels(df$change_terrorism_f)[levels(df$change_terrorism_f)=="3"] <- "About the same"
levels(df$change_terrorism_f)[levels(df$change_terrorism_f)=="4"] <- "Higher"
levels(df$change_terrorism_f)[levels(df$change_terrorism_f)=="5"] <- "Much higher"

# str(df$change_terrorism_f)
# levels(df$change_terrorism_f)


df$terrorismIncrease <- factor(ifelse(df$change_terrorism_f=="Higher" |
                                         df$change_terrorism_f=="Much higher", "will increase", "will not increase"),
                                levels = c("will increase", "will not increase"))




# influence ###########################################

# Britain's influence in the world ... 

# df$change_influence <- as.numeric(as.character(df$change_influence))

# df$change_influence_f <- as.factor(df$change_influence)
# levels(df$change_influence_f)[levels(df$change_influence_f)=="1"] <- "Much lower"
# levels(df$change_influence_f)[levels(df$change_influence_f)=="2"] <- "Lower"
# levels(df$change_influence_f)[levels(df$change_influence_f)=="3"] <- "About the same"
# levels(df$change_influence_f)[levels(df$change_influence_f)=="4"] <- "Higher"
# levels(df$change_influence_f)[levels(df$change_influence_f)=="5"] <- "Much higher"

# str(df$change_influence_f)
# levels(df$change_influence_f)


df$change_influence_d <- factor(ifelse(df$change_influence_f=="Much lower" |
                                         df$change_influence_f=="Lower", "will lose influence", "won't"),
                                levels = c("will lose influence", "won't"))
# str(df$change_influence_d)



# economy #############################################

# The general economic situation in the UK 

# df$change_econ <- as.numeric(as.character(df$change_econ))
# 
# df$change_econ_f <- as.factor(df$change_econ)
# levels(df$change_econ_f)[levels(df$change_econ_f)=="1"] <- "Much worse"
# levels(df$change_econ_f)[levels(df$change_econ_f)=="2"] <- "Worse"
# levels(df$change_econ_f)[levels(df$change_econ_f)=="3"] <- "About the same"
# levels(df$change_econ_f)[levels(df$change_econ_f)=="4"] <- "Better"
# levels(df$change_econ_f)[levels(df$change_econ_f)=="5"] <- "Much better"

# str(df$change_econ_f)
# levels(df$change_econ_f)

# Create dummy variable

# df$change_econ_d <- factor(
#   ifelse(df$change_econ <= 2, "economy will get worse", "won't"),
#   levels = c("economy will get worse", "won't"))

df$econ_Better <- factor(ifelse(df$change_econ_f=="Much better" |
                                  df$change_econ_f=="Better", 
                                "Economy will get better", "won't"),
                         levels = c("Economy will get better", "won't"))

df$econ_Worse <- factor(ifelse(df$change_econ_f=="Much worse" |
                                 df$change_econ_f=="Worse", 
                               "Economy will get worse", "won't"),
                        levels = c("Economy will get worse", "won't"))


# personal finances #######################################

# Your personal financial situation 

# df$change_persFinance <- as.numeric(as.character(df$change_persFinance))
# 
# df$change_persFinance_f <- as.factor(df$change_persFinance)
# levels(df$change_persFinance_f)[levels(df$change_persFinance_f)=="1"] <- "Much worse"
# levels(df$change_persFinance_f)[levels(df$change_persFinance_f)=="2"] <- "Worse"
# levels(df$change_persFinance_f)[levels(df$change_persFinance_f)=="3"] <- "About the same"
# levels(df$change_persFinance_f)[levels(df$change_persFinance_f)=="4"] <- "Better"
# levels(df$change_persFinance_f)[levels(df$change_persFinance_f)=="5"] <- "Much better"

# str(df$change_persFinance_f)
# levels(df$change_persFinance_f)


# Look at histogram / bar plot
# truehist(df$change_persFinance)
# barplot(prop.table(table(df$change_persFinance_f))) # most say "about the same" or "worse"


# Create 3-level factor
df$change_persFinance_sur3 <- factor(
  ifelse(df$change_persFinance <= 2, "worse", 
         ifelse(df$change_persFinance >= 4, "better", "same")),
  levels = c("worse", "same", "better"))

# barplot(prop.table(table(df$change_persFinance_sur3))) # pretty pessimistic!

# Create dummy variable

# df$change_persFinance_d <- factor(
#   ifelse(df$change_persFinance <= 2, "pers finances will get worse", "won't"),
#   levels = c("pers finances will get worse", "won't"))

df$persFinances_Better <- factor(ifelse(df$change_persFinance_f=="Much better" |
                                          df$change_persFinance_f=="Better", 
                                        "will get better", "won't get better"),
                                 levels = c("will get better", "won't get better"))

df$persFinances_Worse <- factor(ifelse(df$change_persFinance_f=="Much worse" |
                                         df$change_persFinance_f=="Worse", 
                                       "will get worse", "will not get worse"),
                                levels = c("will get worse", "will not get worse"))


# NHS #####################################################

# The NHS

# df$change_NHS <- as.numeric(as.character(df$change_NHS))
# 
# df$change_NHS_f <- as.factor(df$change_NHS)
# levels(df$change_NHS_f)[levels(df$change_NHS_f)=="1"] <- "Much worse"
# levels(df$change_NHS_f)[levels(df$change_NHS_f)=="2"] <- "Worse"
# levels(df$change_NHS_f)[levels(df$change_NHS_f)=="3"] <- "About the same"
# levels(df$change_NHS_f)[levels(df$change_NHS_f)=="4"] <- "Better"
# levels(df$change_NHS_f)[levels(df$change_NHS_f)=="5"] <- "Much better"

# str(df$change_NHS_f)
# levels(df$change_NHS_f)

# truehist(df$change_NHS)
# barplot(prop.table(table(df$change_NHS_f))) 

# Create dummy variable
df$NHSBetter <- factor(ifelse(df$change_NHS_f=="Much better" |
                                df$change_NHS_f=="Better", "NHS will get better", "won't"),
                       levels = c("NHS will get better", "won't"))

df$NHSWorse <- factor(ifelse(df$change_NHS_f=="Much worse" |
                               df$change_NHS_f=="Worse", "NHS will get worse", "won't"),
                      levels = c("NHS will get worse", "won't"))

# Psych vars ############################################

# How do you feel now ###################################

# df$happy <- as.numeric(as.character(df$happy))
# hist(df$happy)
df$happy_f <- as.factor(df$happy)
# plot(df$happy_f)
df$happy_sur3 <- cut(df$happy, c(0, 2, 5, 7)) # middle category larger
# plot(df$happy_sur3)
df$happy_d <- cut(df$happy, c(0, 2, 7)) 
# plot(df$happy_d)

# df$proud <- as.numeric(as.character(df$proud))
# hist(df$proud)
df$proud_f <- as.factor(df$proud)
# plot(df$proud_f)
df$proud_sur3 <- cut(df$proud, c(0, 2, 5, 7)) # middle category larger
# plot(df$proud_sur3)
df$proud_d <- cut(df$proud, c(0, 2, 7))
# plot(df$proud_d)

# df$surprised <- as.numeric(as.character(df$surprised))
# hist(df$surprised)
df$surprised_f <- as.factor(df$surprised)
# plot(df$surprised_f)
df$surprised_sur3 <- cut(df$surprised, c(0, 2, 5, 7)) # middle category larger
# plot(df$surprised_sur3)
df$surprised_d <- cut(df$surprised, c(0, 4, 7))
# plot(df$surprised_d)

# df$anxious <- as.numeric(as.character(df$anxious))
# hist(df$anxious)
df$anxious_f <- as.factor(as.character(df$anxious))
# plot(df$anxious_f)
df$anxious_sur3 <- cut(df$anxious, c(0, 2, 5, 7)) # middle category larger
# plot(df$anxious_sur3)
df$anxious_d <- cut(df$anxious, c(0, 4, 7))
# plot(df$anxious_d)

# df$angry <- as.numeric(as.character(df$angry))
# hist(df$angry)
df$angry_f <- as.factor(df$angry)
# plot(df$angry_f)
df$angry_sur3 <- cut(df$angry, c(0, 2, 5, 7)) # middle category larger
# plot(df$angry_sur3)
df$angry_d <- cut(df$angry, c(0, 4, 7))
# plot(df$angry_d)

# df$guilty <- as.numeric(as.character(df$guilty))
# hist(df$guilty)
df$guilty_f <- as.factor(df$guilty)
# plot(df$guilty_f)
df$guilty_sur3 <- cut(df$guilty, c(0, 2, 5, 7)) # middle category larger
# plot(df$guilty_sur3)
df$guilty_d <- cut(df$guilty, c(0, 3, 7))
# plot(df$guilty_d)

# ashamed -- only for direct links!!
# df$ashamed <- as.numeric(as.character(df$ashamed)) # only for direct links!!
# hist(df$ashamed)
df$ashamed_f <- as.factor(df$ashamed)
# plot(df$ashamed_f)
df$ashamed_sur3 <- cut(df$ashamed, c(0, 2, 5, 7)) # middle category larger
# plot(df$ashamed_sur3)
df$ashamed_d <- cut(df$ashamed, c(0, 3, 7))
# plot(df$ashamed_d)



# SecondRef ##############################################

# Q20 Now that the UK has voted to leave the EU, do you think the decision will be final or 
# might there be a second EU referendum?

df$SecondRef <- as.factor(df$SecondRef)
df$SecondRef_f[df$SecondRef == "1"] <- "decision is final"
df$SecondRef_f[df$SecondRef == "2"] <- "second referendum"


# tweets ################################################

# str(df$no_tweets) # as.factor? (but not very important)
# str(df$no_retweets) # as.factor? (but not very important)




# demographics ###########################################

# gender #################################################

# df$gender <- as.factor(df$gender)
# levels(df$gender)[levels(df$gender)=="1"] <- "male"  
# levels(df$gender)[levels(df$gender)=="2"] <- "female"  



# age ####################################################

df$born <- as.numeric(as.character(df$born)) # some NAs ("Haha")

# Age as factor

df$ageGroup <- cut(df$born, c(0, 1946, 1956, 1966, 1976, 1986, 1996, 2006))
levels(df$ageGroup)
levels(df$ageGroup)[levels(df$ageGroup)=="(0,1.95e+03]"] <- "seventies"
levels(df$ageGroup)[levels(df$ageGroup)=="(1.95e+03,1.96e+03]"] <- "sixties"
levels(df$ageGroup)[levels(df$ageGroup)=="(1.96e+03,1.97e+03]"] <- "fifties"
levels(df$ageGroup)[levels(df$ageGroup)=="(1.97e+03,1.98e+03]"] <- "forties"
levels(df$ageGroup)[levels(df$ageGroup)=="(1.98e+03,1.99e+03]"] <- "thirties"
levels(df$ageGroup)[levels(df$ageGroup)=="(1.99e+03,2e+03]"] <- "twens"
levels(df$ageGroup)[levels(df$ageGroup)=="(2e+03,2.01e+03]"] <- "teens"

hist(df$born)
plot(df$ageGroup)


# create 4 categories with equal number of observations
# df$ageGroup_sur3 <- ggplot2::cut_number(df$born, n = 4, labels = 1:4)

df$ageGroup_sur3 <- df$ageGroup
levels(df$ageGroup_sur3)[levels(df$ageGroup_sur3)=="teens" |  levels(df$ageGroup_sur3)=="twens"] <- "teens/twens"
levels(df$ageGroup_sur3)[levels(df$ageGroup_sur3)=="thirties" |  levels(df$ageGroup_sur3)=="forties"] <- "30s/40s"
levels(df$ageGroup_sur3)[levels(df$ageGroup_sur3)=="fifties" |  levels(df$ageGroup_sur3)=="sixties"|  levels(df$ageGroup_sur3)=="seventies"] <- "50s/60s/70s"
plot(df$ageGroup_sur3)
levels(df$ageGroup_sur3)

# children ###############################################

# str(df$children5to15)
# df$children5to15 <- as.factor(df$children5to15)
# df$children5to15[df$children5to15 == "0"] <- "no 5-15yr old kids"
# df$children5to15[df$children5to15 == "1"] <- "5-15yr old kids"


# marital status #########################################

# df$maritalStatus <- as.factor(df$maritalStatus)
# levels(df$maritalStatus)[levels(df$maritalStatus)=="1"] <- "Married" 
# levels(df$maritalStatus)[levels(df$maritalStatus)=="2"] <- "Living as married "
# levels(df$maritalStatus)[levels(df$maritalStatus)=="3"] <- "Divorced"
# levels(df$maritalStatus)[levels(df$maritalStatus)=="4"] <- "Widowed"
# levels(df$maritalStatus)[levels(df$maritalStatus)=="5"] <- "Single"


# town / village #########################################

# str(df$townVillage)
# levels(df$townVillage)[levels(df$townVillage)=="1"] <- "centre of a city or large town" 
# levels(df$townVillage)[levels(df$townVillage)=="2"] <- "suburb of a city or large town" 
# levels(df$townVillage)[levels(df$townVillage)=="3"] <- "small or medium-sized town" 
# levels(df$townVillage)[levels(df$townVillage)=="4"] <- "village" 
# levels(df$townVillage)[levels(df$townVillage)=="5"] <- "countryside" 


# education ##############################################

# df$education <- as.numeric(as.character(df$education))
# 
# df$education_f <- as.factor(df$education)
# levels(df$education_f)[levels(df$education_f)=="1"] <- "15 or younger"  # HIER
# levels(df$education_f)[levels(df$education_f)=="2"] <- "16" 
# levels(df$education_f)[levels(df$education_f)=="3"] <- "17" 
# levels(df$education_f)[levels(df$education_f)=="4"] <- "18" 
# levels(df$education_f)[levels(df$education_f)=="5"] <- "19 or older" 
# levels(df$education_f)[levels(df$education_f)=="6"] <- "Still at school in full-time education" 
# levels(df$education_f)[levels(df$education_f)=="7"] <- "Still at university in full-time education"
# 
# plot(df$education_f)

# Those born in 1997 have had 19 years of schooling so group them in with 19 or older
df$education_f[df$education_f == "Still at university in full-time education" &
                 df$born == 1997] <- "19 or older" 

# plot(df$education_f)

# education_sur2 - finished before the age of 18 (people )
df$education_sur2 <- factor(ifelse(df$education_f == "15 or younger" |
                                     df$education_f == "16" |  
                                     df$education_f == "17" |  
                                     df$education_f == "18",
                                   ">=18", "19+"),
                            levels = c(">=18", "19+"))
# plot(df$education_sur2)

# supposing that whoever was still in school at age 19 went to university
df$education_d <- factor(ifelse(df$education_f == "Still at university in full-time education" |
                                  df$education_f == "19 or older",
                                "uni", "no uni"),
                         levels = c("uni", "no uni"))
# plot(df$education_d)





# sourceIncome #########################################

# df$sourceIncome
df$sourceIncome <- as.factor(df$sourceIncome)
levels(df$sourceIncome)

levels(df$sourceIncome)[1] <- "employment"
levels(df$sourceIncome)[2] <- "occupational pensions"
levels(df$sourceIncome)[3] <- "retired"
levels(df$sourceIncome)[4] <- "unemployment benefits"
levels(df$sourceIncome)[5] <- "income support"
levels(df$sourceIncome)[6] <- "invalidity/ sickness/ disabled pension"
levels(df$sourceIncome)[7] <- "savings/ investments"
levels(df$sourceIncome)[8] <- "student loan"
levels(df$sourceIncome)[9] <- "parents"
levels(df$sourceIncome)[10] <- "other"




# trust ################################################

# str(df$trust) # 2 = Most people can be trusted / 1 = Can't be too careful in dealing with people
# levels(df$trust)

# df$trust <- recode(df$trust, "
#                     '1' = 'Most people can be trusted' ;
#                     '2' = 'Cannot be too careful in dealing with people'  ")


df$trust_D <- factor(ifelse(df$trust=="Cannot be too careful in dealing with people", 
                            "trust", "no trust"))
# plot(df$trust_D)



# accuracy goals ########################################

# gut decision #########################################

# gut decision
# df$gutDecision <- as.factor(df$gutDecision)
# df$gutDecision <- recode(df$gutDecision, '
#                          "11"="Definitely true";
#                          "12"="Probably true";
#                          "14"="Probably false";
#                          "15"="Definitely false";
#                          "16"="Do not know" ')
# 
# df$gutDecision <- factor(df$gutDecision, 
#                          levels = c("Definitely true", "Probably true", "Do not know",
#                                     "Probably false", "Definitely false"))

# plot(df$gutDecision)


# gutDecision_sur3
df$gutDecision_sur3 <- 
  factor(df$gutDecision, levels = c("gut decision", "don't know", "not a gut decision"))

df$gutDecision_sur3[df$gutDecision == "Definitely true" | 
                      df$gutDecision == "Probably true"] <- "gut decision"
df$gutDecision_sur3[df$gutDecision == "Definitely false" | 
                      df$gutDecision=="Probably false"] <- "not a gut decision"
df$gutDecision_sur3[df$gutDecision == "Do not know" ] <- "don't know"

# plot(df$gutDecision_sur3)


# gutDecision_D
df$gutDecision_D <- factor(ifelse(df$gutDecision=="Definitely true" | 
                                    df$gutDecision=="Probably true", 
                                  "gut decision", 
                                  "not a gut decision"))
# plot(df$gutDecision_D)



# based on facts #######################################

# based on facts 
# df$basedOnFacts <- factor(recode(df$basedOnFacts, '
#                           "11"="Definitely true";
#                           "12"="Probably true";
#                           "14"="Probably false";
#                           "15"="Definitely false";
#                           "16"="Do not know" ')) 
# 
# df$basedOnFacts <- factor(df$basedOnFacts, levels = c("Definitely true", "Probably true", 
#                             "Do not know", "Probably false", "Definitely false"))

# plot(df$basedOnFacts)
# xtabs(~basedOnFacts+gutDecision, data=df)



# basedOnFacts_sur3
df$basedOnFacts_sur3 <- factor(df$basedOnFacts, 
                               levels = c("based on facts", "don't know", "not based on facts"))
df$basedOnFacts_sur3[df$basedOnFacts == "Definitely true" | 
                       df$basedOnFacts == "Probably true"] <- "based on facts"
df$basedOnFacts_sur3[df$basedOnFacts == "Definitely false" | 
                       df$basedOnFacts=="Probably false"] <- "not based on facts"
df$basedOnFacts_sur3[df$basedOnFacts == "Do not know" ] <- "don't know"

# plot(df$basedOnFacts_sur3)


# basedOnFacts_d
df$basedOnFacts_d <- factor(ifelse(df$basedOnFacts=="Definitely true" | 
                                     df$basedOnFacts=="Probably true", 
                                   "based on facts", 
                                   "not based on facts"))
# plot(df$basedOnFacts_d)




# self esteem ##########################################

str(df$selfEsteem) # 1 = Not very true of me, 7 = Very true of me
# df$selfEsteem <- as.numeric(df$selfEsteem)
# hist(df$selfEsteem)

# table(df$selfEsteem)

# Create factor variable
df$selfEsteem_sur3 <- factor(
  ifelse(df$selfEsteem <= 3, "low", 
         ifelse(df$selfEsteem >= 6, "high", "medium")),
  levels = c("low", "medium", "high"))
# low = 1,2,3 medium = 4,5, high = 6,7

# table(df$selfEsteem_sur3) # better

# df$selfEsteem_sur3b <- factor(
#   ifelse(df$selfEsteem < 3, "low", 
#          ifelse(df$selfEsteem > 5, "high", "medium")),
#   levels = c("low", "medium", "high"))
# # low = 1,2 medium = 3,4,5, high = 6,7
# 
# table(df$selfEsteem_sur3b)


df$selfEsteem_d <- factor(
  ifelse(df$selfEsteem <= 4, "low/medium", "high"),
  levels = c("low/medium", "high"))
# low/medium = 1,2,3,4, high = 5,6,7

# table(df$selfEsteem_d) # better




# Big 5 #################################################

# extraversion ##########################################
df$extraversion <- as.numeric(as.character(df$extraversion))
df$extraversion_r <- recode(df$extraversion_R, '
                            "1"="7";
                            "2"="6";
                            "3"="5";
                            "4"="4";
                            "5"="3";
                            "6"="2";
                            "7"="1" ')

df$extraversion_r <- as.numeric(df$extraversion_r)
df$extraversion_index <- (df$extraversion + df$extraversion_r)
# hist(df$extraversion_index)

# min(df$extraversion_index, na.rm = T) # 2
# max(df$extraversion_index, na.rm = T) # 14

df$extraversion_sur4 <- cut(df$extraversion_index, c(1, 5, 8, 11, 14)) # 6.5 to first cat, else add # , right=FALSE).
# plot(df$extraversion_sur4)

df$extraversion_sur3 <- cut(df$extraversion_index, c(1, 6, 10, 14)) # middle category only comprising 3 numbers
# plot(df$extraversion_sur3)

df$extraversion_d <- cut(df$extraversion_index, c(1, 8, 14)) # first category larger
# plot(df$extraversion_d)


# agreeableness #########################################
# df$agreeableness <- as.numeric(as.character(df$agreeableness))
# df$agreeableness_r <- recode(df$agreeableness_R, '  
#                              "1"="7";
#                              "2"="6";
#                              "3"="5";
#                              "4"="4";
#                              "5"="3";
#                              "6"="2";
#                              "7"="1" ')
# df$agreeableness_r <- as.numeric(as.character(df$agreeableness_r))
df$agreeableness_index <- df$agreeableness + df$agreeableness_r

# hist(df$agreeableness_index)
# min(df$agreeableness_index, na.rm = T) # 4
# max(df$agreeableness_index, na.rm = T) # 14

df$agreeableness_sur4 <- cut(df$agreeableness_index, c(3, 6.5, 9, 11.5, 14)) # 6.5 to first cat, else add # , right=FALSE).
# plot(df$agreeableness_sur4)

df$agreeableness_sur3 <- cut(df$agreeableness_index, c(3, 7, 10, 14)) # middle category only comprising 3 numbers
# plot(df$agreeableness_sur3)

df$agreeableness_d <- cut(df$agreeableness_index, c(3, 9, 14)) # first category larger
# plot(df$agreeableness_d)



# conscientiousness #####################################

# df$conscientiousness <- as.numeric(as.character(df$conscientiousness))
# df$conscientiousness_r <- recode(df$conscientiousness_R, '  
#                                  "1"="7"; 
#                                  "2"="6";
#                                  "3"="5";
#                                  "4"="4";
#                                  "5"="3";
#                                  "6"="2";
#                                  "7"="1" ')
# df$conscientiousness_r <- as.numeric(as.character(df$conscientiousness_r))
df$conscientiousness_index <- (df$conscientiousness + df$conscientiousness_r) 

# hist(df$conscientiousness_index)
# min(df$conscientiousness_index, na.rm = T) # 4
# max(df$conscientiousness_index, na.rm = T) # 14

df$conscientiousness_sur4 <- cut(df$conscientiousness_index, c(3, 6.5, 9, 11.5, 14)) # 6.5 to first cat, else add # , right=FALSE).
# plot(df$conscientiousness_sur4)

df$conscientiousness_sur3 <- cut(df$conscientiousness_index, c(3, 7, 10, 14)) # middle category only comprising 3 numbers
# plot(df$conscientiousness_sur3)

df$conscientiousness_d <- cut(df$conscientiousness_index, c(3, 10, 14)) # first category a lot larger
# plot(df$conscientiousness_d)


# openness ##############################################

# df$openness <- as.numeric(as.character(df$openness))
# df$openness_r <- recode(df$openness_R, '  
#                         "1"="7"; 
#                         "2"="6";
#                         "3"="5";
#                         "4"="4";
#                         "5"="3";
#                         "6"="2";
#                         "7"="1" ')
# df$openness_r <- as.numeric(as.character(df$openness_r))
df$openness_index <- (df$openness + df$openness_r) 

# hist(df$openness_index)
# min(df$openness_index, na.rm = T) # 5
# max(df$openness_index, na.rm = T) # 14

df$openness_sur4 <- cut(df$openness_index, c(4, 7.25, 9.5, 11.75, 14)) # 6.5 to first cat, else add # , right=FALSE).
# plot(df$openness_sur4)

df$openness_sur3 <- cut(df$openness_index, c(4, 8, 11, 14)) # middle category only comprising 3 numbers
# plot(df$openness_sur3)

df$openness_d <- cut(df$openness_index, c(4, 10, 14)) # first category a lot larger
# plot(df$openness_d)



# neuroticism ###########################################

# df$neuroticism <- as.numeric(as.character(df$neuroticism))
# hist(df$neuroticism)
# 
# str(df$neuroticism_R)
# df$neuroticism_r <- recode(df$neuroticism_R, '  
#                            "1"="7"; 
#                            "2"="6";
#                            "3"="5";
#                            "4"="4";
#                            "5"="3";
#                            "6"="2";
#                            "7"="1" ')
# df$neuroticism_r <- as.numeric(as.character(df$neuroticism_r))
# hist(df$neuroticism_r)
df$neuroticism_index <- (df$neuroticism + df$neuroticism_r) 

# hist(df$neuroticism_index)
# min(df$neuroticism_index, na.rm = T) # 2
# max(df$neuroticism_index, na.rm = T) # 13

df$neuroticism_sur4 <- cut(df$neuroticism_index, c(1, 4.75, 7.5, 10.25, 13))
# plot(df$neuroticism_sur4)

df$neuroticism_sur3 <- cut(df$neuroticism_index, c(1, 5, 9, 13)) 
# plot(df$neuroticism_sur3)

df$neuroticism_d <- cut(df$neuroticism_index, c(1, 6, 13)) # second category a lot larger
# plot(df$neuroticism_d)


# Delete vars (OR DON'T)
# df$twee <- NULL
# df$twitterName <- NULL
# df$twitter_name <- NULL
# df$twitterNameCorrect  <- NULL


# EU knowledge ##########################################

# df$stress_EU28 <- recode(df$stress_EU28, '
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')
# 
# df$ctl_EU28 <- recode(df$ctl_EU28,'
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')
# 
# df$stress_CHinEU <- recode(df$stress_CHinEU, '
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')
# 
# df$ctl_CHinEU <- recode(df$ctl_CHinEU, '
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')
# 
# df$stress_MEPsElected <- recode(df$stress_MEPsElected, '
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')
# 
# df$ctl_MEPsElected <- recode(df$ctl_MEPsElected,'
#                   "4"="True";
#                   "7"="False";
#                   "8"="Do not know" ')


# Turn into dummies

df$stress_EU28_D <- ifelse(df$stress_EU28=="True", 1, 0)
df$ctl_EU28_D <- ifelse(df$ctl_EU28=="True", 1, 0)

df$stress_CHinEU_D <- ifelse(df$stress_CHinEU=="False", 1, 0)
df$ctl_CHinEU_D <- ifelse(df$ctl_CHinEU=="False", 1, 0)

df$stress_MEPsElected_D <- ifelse(df$stress_MEPsElected=="True", 1, 0)
df$ctl_MEPsElected_D <- ifelse(df$ctl_MEPsElected=="True", 1, 0)


# Make an index (1 point for every correct answer - I am treating NAs as false answers)
df$stress_EUknowledge <- df$stress_EU28_D + df$stress_CHinEU_D + df$stress_MEPsElected_D
df$ctl_EUknowledge <- df$ctl_EU28_D + df$ctl_CHinEU_D + df$ctl_MEPsElected_D




# Subjective social status ##############################

# df$subjSocialStatus[df$subjSocialStatus=="Seven"] <- 7
# df$subjSocialStatus[df$subjSocialStatus=="4 (Is the scale linear or logarithmic? Assuming logarithmic)"] <- 4
# df$subjSocialStatus[df$subjSocialStatus=="1-Mentally disabled, can't get a job."] <- 1
# df$subjSocialStatus[df$subjSocialStatus=="6 or 7"] <- 6.5
# df$subjSocialStatus[df$subjSocialStatus=="4-5"] <- 4.5
# df$subjSocialStatus[df$subjSocialStatus=="2 and 3"] <- 2.5
# df$subjSocialStatus[df$subjSocialStatus=="5-6"] <- 5.5
# df$subjSocialStatus[df$subjSocialStatus=="7 or 8"] <- 7.5
# df$subjSocialStatus[df$subjSocialStatus=="four"] <- 4
# df$subjSocialStatus[df$subjSocialStatus=="7-8"] <- 7.5
# df$subjSocialStatus[df$subjSocialStatus=="5/6"] <- 5.5

# df$subjSocialStatus <- as.numeric(as.character((df$subjSocialStatus))) 

# hist(df$subjSocialStatus)
# min(df$subjSocialStatus, na.rm = T) # 1
# max(df$subjSocialStatus, na.rm = T) # 10

# create 5-level factor variable
df$SSS_sur5 <- cut(df$subjSocialStatus, c(0, 2, 4, 6, 8, 10))
# plot(df$SSS_sur5)

# create 4-level factor variable
df$SSS_sur4 <- cut(df$subjSocialStatus, c(0, 2.5, 5, 7.5, 10))
# plot(df$SSS_sur4)

# create 3-level factor variable
df$SSS <- factor(ifelse(df$subjSocialStatus <= 4, "low",
                        ifelse(df$subjSocialStatus >= 8, "high", "medium")),
                 levels = c("low", "medium", "high"))


# truehist(df$subjSocialStatus)
# table(df$SSS)
# barplot(prop.table(table(df$SSS))) 

# create SSS dummy
df$SSS_d <- factor(ifelse(df$subjSocialStatus <= 5, "low", "high"), levels = c("low", "high"))

# table(df$SSS_d)
# table(df$SSS_d, df$vote) # ACHTUNG: leave people report lower SSS! 53% of leave people report lower SES, only 32% or remain people!
# prop.table(table(df$SSS_d))
# prop.table(table(df$vote, df$SSS_d, 1)) # 2-Way Frequency Table, row percentages
# barplot(prop.table(table(df$SSS_d))) 





# life satisfaction #####################################

# str(df$lifeSatisfaction)
# truehist(df$lifeSatisfaction)
# table(df$lifeSatisfaction)

# as factor
# df$lifeSatisfaction_f <- as.factor(df$lifeSatisfaction)
# levels(df$lifeSatisfaction_f)[levels(df$lifeSatisfaction_f)=="1"] <- "satisfied"
# levels(df$lifeSatisfaction_f)[levels(df$lifeSatisfaction_f)=="2"] <- "fairly satisfied"
# levels(df$lifeSatisfaction_f)[levels(df$lifeSatisfaction_f)=="3"] <- "not very satisfied"
# levels(df$lifeSatisfaction_f)[levels(df$lifeSatisfaction_f)=="4"] <- "not satisfied"

# plot(df$lifeSatisfaction_f)
# barplot(prop.table(table(df$lifeSatisfaction_f))) 

# as numeric
# df$lifeSatisfaction_num <- as.numeric(df$lifeSatisfaction_f)

# create 3-level factor variable
df$lifeSatisfaction_sur3 <- df$lifeSatisfaction_f

levels(df$lifeSatisfaction_sur3)[levels(df$lifeSatisfaction_sur3)=="not very satisfied"] <- "not (very) satisfied"
levels(df$lifeSatisfaction_sur3)[levels(df$lifeSatisfaction_sur3)=="not satisfied"] <- "not (very) satisfied"
plot(df$lifeSatisfaction_sur3)

# as dummy
df$lifeSatisfaction_d <- factor(
  ifelse(df$lifeSatisfaction == 1 | df$lifeSatisfaction == 2, "satisfied",
         "not satisfied"),
  levels = c("satisfied", "not satisfied"))

table(df$lifeSatisfaction_d)


# Reverse code manually so that I can turn this into a numeric var with 4 = satisfied
df$lifeSatisfaction_rev <- ifelse(df$lifeSatisfaction_f == "satisfied", 4,
                                  ifelse(df$lifeSatisfaction_f == "fairly satisfied", 3,
                                         ifelse(df$lifeSatisfaction_f == "not very satisfied", 2,
                                                ifelse(df$lifeSatisfaction_f == "not satisfied", 1, NA))))

# as numeric
df$lifeSatisfaction_num_rev <- as.numeric(df$lifeSatisfaction_rev)


# See if it worked 
# df[sample(1:nrow(df), 20), c("lifeSatisfaction_num", "lifeSatisfaction_num_rev") ] # It did =)


 


# in common #############################################

# how much do you have in common with Remainers / Brexiteers?

str(df$inCommon_Remainers)
df$inCommon_Remainers <- as.numeric(df$inCommon_Remainers) # the higher the more in common


str(df$inCommon_Brexiteers)
df$inCommon_Brexiteers <- as.numeric(df$inCommon_Brexiteers) # the higher the more in common




# more psych vars #######################################

# curiosity #############################################

str(df$curiosity1)
df$curiosity1_num <- as.numeric(df$curiosity1)

str(df$curiosity2)
df$curiosity2_num <- as.numeric(as.character(df$curiosity2))

str(df$curiosity3)
df$curiosity3_num <- as.numeric(df$curiosity3)

str(df$curiosity4)
df$curiosity4 <- as.factor(df$curiosity4)
df$curiosity4_num <- as.numeric(as.character(df$curiosity4))


# curiosity index
df$curiosity <- (df$curiosity4_num + df$curiosity3_num + 
                   df$curiosity2_num + df$curiosity1_num) 
# hist(df$curiosity)
df$curiosity_f <- as.factor(df$curiosity)

# min(df$curiosity, na.rm=T) # 6
# max(df$curiosity, na.rm=T) # 20

# curiosity_sur4
df$curiosity_sur4 <- cut(df$curiosity, c(5, 9.5, 13, 16.5, 20))
# plot(df$curiosity_sur4)

# curiosity_sur3
df$curiosity_sur3 <- cut(df$curiosity, c(5, 10, 15, 20))
# plot(df$curiosity_sur3)

# curiosity_d
df$curiosity_d <- cut(df$curiosity, c(5, 12, 20))
# plot(df$curiosity_d)


# Need to evaluate ######################################

# needToEvaluate1 -- I form opinions about everything
df$needToEvaluate1 <- as.numeric(df$needToEvaluate1)

# needToEvaluate2_R -- I prefer to avoid taking extreme positions
str(df$needToEvaluate2_R) 
df$needToEvaluate2_r <- recode(df$needToEvaluate2_R, '
                               "5"="1"; # Extremely
                               "4"="2"; 
                               "2"="4"; 
                               "1"="5" ') # Not at all
df$needToEvaluate2_r <- as.numeric(df$needToEvaluate2_r)



# needToEvaluate3_R -- There are many things for which I do not have a preference
str(df$needToEvaluate3_R)
df$needToEvaluate3_R <- as.factor(df$needToEvaluate3_R)
df$needToEvaluate3_r <- recode(df$needToEvaluate3_R, '
                               "4"="5"; # Not at all
                               "5"="4"; # Only a little 
                               "6"="3"; # Somewhat
                               "7"="2"; # Quite a lot
                               "8"="1" ') # Extremely
df$needToEvaluate3_r <- as.numeric(df$needToEvaluate3_r)

# needToEvaluate4 -- I like to have strong opinions even when I am not personally involved
df$needToEvaluate4 <- recode(df$needToEvaluate4, '
                             "4"="1"; # Not at all
                             "5"="2"; # Only a little
                             "6"="3"; # Somewhat
                             "7"="4"; # Quite a lot
                             "8"="5" ') #Extremely
df$needToEvaluate4 <- as.numeric(df$needToEvaluate4)


# Create needToEvaluate index variable
df$needToEvaluate <- df$needToEvaluate4 + df$needToEvaluate3_r + 
  df$needToEvaluate2_r + df$needToEvaluate1
# hist(df$needToEvaluate)
df$needToEvaluate_f <- as.factor(df$needToEvaluate)
# plot(df$needToEvaluate_f)

# min(df$needToEvaluate, na.rm = T) # 5
# max(df$needToEvaluate, na.rm = T) # 17

# needToEvaluate_sur4
df$needToEvaluate_sur4 <- cut(df$needToEvaluate, c(4, 8, 11, 14, 17)) # first category comprises 4 numbers
# plot(df$needToEvaluate_sur4)

# needToEvaluate_sur3
# df$needToEvaluate_sur3 <- cut(df$needToEvaluate, c(4, 9, 12, 17)) 
df$needToEvaluate_sur3 <- cut(df$needToEvaluate, c(4, 9, 11, 17)) # last category comprises 5 numbers
# plot(df$needToEvaluate_sur3)

# needToEvaluate_d
df$needToEvaluate_d <- cut(df$needToEvaluate, c(4, 10, 17)) # last category comprises 5 numbers
# plot(df$needToEvaluate_d)



# Need for cognition ####################################

# needforCognition1 --  I like to have the responsibility of handling a situation that 
# requires a lot of thinking
df$needforCognition1 <- recode(df$needforCognition1,'
                               "4"="1"; # Not at all
                               "5"="2"; # Only a little
                               "6"="3"; # Somewhat
                               "7"="4"; # Quite a lot
                               "8"="5" ') #Extremely
df$needforCognition1 <- as.numeric(df$needforCognition1)

# needforCognition2_R --  I feel relief rather than satisfaction after completing a task 
# that requires a lot of mental effort
df$needforCognition2_r <- recode(df$needforCognition2_R, '
                                 "4"="5"; # Not at all
                                 "5"="4"; # Only a little 
                                 "6"="3"; # Somewhat
                                 "7"="2"; # Quite a lot
                                 "8"="1" ') # Extremely
df$needforCognition2_r <- as.numeric(df$needforCognition2_r)


# needforCognition3 -- I find satisfaction in deliberating hard and for long hours
df$needforCognition3 <- recode(df$needforCognition3, '
                               "4"="1"; # Not at all
                               "5"="2"; # Only a little
                               "6"="3"; # Somewhat
                               "7"="4"; # Quite a lot
                               "8"="5" ') #Extremely
df$needforCognition3 <- as.numeric(df$needforCognition3)


# needforCognition4_R -- It's enough for me that something gets the job done; 
# I don't care how or wh..
df$needforCognition4_r <- recode(df$needforCognition4_R,'
                                 "4"="5"; # Not at all
                                 "5"="4"; # Only a little 
                                 "6"="3"; # Somewhat
                                 "7"="2"; # Quite a lot
                                 "8"="1" ') # Extremely
df$needforCognition4_r <- as.numeric(df$needforCognition4_r)


# Create needforCognition index variable

df$needforCognition <- df$needforCognition4_r + df$needforCognition3 + 
  df$needforCognition2_r + df$needforCognition1
hist(df$needforCognition)
#
# min(df$needforCognition, na.rm = T) # 4
# max(df$needforCognition, na.rm = T) # 17

# needforCognition_sur4
df$needforCognition_sur4 <- cut(df$needforCognition, c(3, 6.5, 10, 13.5, 17)) 
plot(df$needforCognition_sur4)

# needforCognition_sur3
df$needforCognition_sur3 <- cut(df$needforCognition, c(3, 8, 11, 17)) 
plot(df$needforCognition_sur3)

# needforCognition_d
df$needforCognition_d <- cut(df$needforCognition, c(3, 10, 17)) 
plot(df$needforCognition_d)





# Strength of prior beliefs #############################

# additive index, 0 (extreme left / Bremain) to 16 (extreme right / Brexit)


# VOTE ##################################################

# numeric vote var (for strength of prior opinion index)
df$vote_num[df$votedORwouldve=="leave"] <- "6"
df$vote_num[df$votedORwouldve=="remain"] <- "1"
df$vote_num[df$votedORwouldve=="not vote"] <- "3.5"
df$vote_num <- as.numeric(as.character(df$vote_num))
hist(df$vote_num)


# HOW SURE ##############################################

# howSure_vote -- leave coding for leave voters, inverse for remain
# leave it as it is for leave voters, reverse scale for remain voters so that I get a continuum
# from 1 (remain voters who are very sure about their vote) to 6 (leave voters who are very sure
# about their vote)
df$howSure_num <- df$howSure_vote
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 6 ] <- 1
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 5 ] <- 2
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 4 ] <- 3
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 3 ] <- 4
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 2 ] <- 5
df$howSure_num[df$votedORwouldve=="remain" & df$howSure_vote == 1 ] <- 6
hist(df$howSure_num)

# are there any don't knows who answered the how sure question? 
df[ is.na(df$votedORwouldve), c("votedORwouldve","howSure_vote", "born")]  # nope 

plot(df$goodReasons_remain_f) 
plot(df$goodReasons_leave_f) 


# GOOD REASONS ###########################################

# goodReasons to remain for LEAVE voters -- the fewer reasons to remain, the higher the score
df$goodReasons_remain_num[df$votedORwouldve=="leave" & df$goodReasons_remain == 4 ] <- 0
df$goodReasons_remain_num[df$votedORwouldve=="leave" & df$goodReasons_remain == 3 ] <- 1
df$goodReasons_remain_num[df$votedORwouldve=="leave" & df$goodReasons_remain == 2 ] <- 2
df$goodReasons_remain_num[df$votedORwouldve=="leave" & df$goodReasons_remain == 1 ] <- 3
df$goodReasons_remain_num

# goodReasons_leave -- the more reasons to remain, the higher the score
# -- deduct 1 for leave voters so that the max conviction = 3 = extreme right
df$goodReasons_leave_num[df$votedORwouldve=="leave" & df$goodReasons_leave == 4 ] <- 3
df$goodReasons_leave_num[df$votedORwouldve=="leave" & df$goodReasons_leave == 3 ] <- 2
df$goodReasons_leave_num[df$votedORwouldve=="leave" & df$goodReasons_leave == 2 ] <- 1
df$goodReasons_leave_num[df$votedORwouldve=="leave" & df$goodReasons_leave == 1 ] <- 0
df$goodReasons_leave_num

# goodReasons to remain for REMAIN voters 
# -- inverse scale -- the more reasons to remain the lower the score
df$goodReasons_remain_num[df$votedORwouldve=="remain" & df$goodReasons_remain == 4 ] <- 0
df$goodReasons_remain_num[df$votedORwouldve=="remain" & df$goodReasons_remain == 3 ] <- 1
df$goodReasons_remain_num[df$votedORwouldve=="remain" & df$goodReasons_remain == 2 ] <- 2
df$goodReasons_remain_num[df$votedORwouldve=="remain" & df$goodReasons_remain == 1 ] <- 3
df$goodReasons_remain_num

# goodReasons_leave for REMAIN voters -- the more reasons to leave the higher the score
# deduct 1 for leave voters so that the max conviction = 0 = extreme left
df$goodReasons_leave_num[df$votedORwouldve=="remain" & df$goodReasons_leave == 4 ] <- 3
df$goodReasons_leave_num[df$votedORwouldve=="remain" & df$goodReasons_leave == 3 ] <- 2
df$goodReasons_leave_num[df$votedORwouldve=="remain" & df$goodReasons_leave == 2 ] <- 1
df$goodReasons_leave_num[df$votedORwouldve=="remain" & df$goodReasons_leave == 1 ] <- 0
df$goodReasons_leave_num

df$strengthConviction <- df$vote_num + df$howSure_num + 
  df$goodReasons_remain_num + df$goodReasons_leave_num

# hist(df$strengthConviction)
# df$strengthConviction
# table(df$strengthConviction)



# Date / time variables #################################

str(df$startTime) 
df$startTime <- as.POSIXct(df$startTime, format="%Y-%m-%d %H:%M:%S")
class(df$startTime) 

str(df$endTime)
df$endTime <- as.POSIXct(df$endTime, format="%Y-%m-%d %H:%M:%S")
class(df$endTime) 



# First/ Last click ######################################

# df$thinkRemain_1ST_CLICK <- as.numeric(as.character((df$thinkRemain_1ST_CLICK)))
# str(df$thinkRemain_1ST_CLICK) 
# df$thinkRemain_LASTCLICK <- as.numeric(as.character((df$thinkRemain_LASTCLICK)))
# str(df$thinkRemain_LASTCLICK)
# df$thinkRemain_SUBMIT <- as.numeric(as.character((df$thinkRemain_SUBMIT)))
# str(df$thinkRemain_SUBMIT)
# df$thinkRemain_CLICKCOUNT <- as.numeric(as.character((df$thinkRemain_CLICKCOUNT)))
# str(df$thinkRemain_CLICKCOUNT)
# 
# df$thinkLeave_1ST_CLICK <- as.numeric(as.character((df$thinkLeave_1ST_CLICK)))
# str(df$thinkLeave_1ST_CLICK) 
# df$thinkLeave_LASTCLICK <- as.numeric(as.character((df$thinkLeave_LASTCLICK)))
# str(df$thinkLeave_LASTCLICK)
# df$thinkLeave_SUBMIT <- as.numeric(as.character((df$thinkLeave_SUBMIT)))
# str(df$thinkLeave_SUBMIT)
# df$thinkLeave_CLICKCOUNT <- as.numeric(as.character((df$thinkLeave_CLICKCOUNT)))
# str(df$thinkLeave_CLICKCOUNT)
# 
# df$stress_1ST_CLICK <- as.numeric(as.character((df$stress_1ST_CLICK)))
# str(df$stress_1ST_CLICK) 
# df$stress_LAST_CLICK <- as.numeric(as.character((df$stress_LAST_CLICK)))
# str(df$stress_LAST_CLICK)
# df$stress_SUBMIT <- as.numeric(as.character((df$stress_SUBMIT)))
# str(df$stress_SUBMIT)
# df$stress_CLICKCOUNT <- as.numeric(as.character((df$stress_CLICKCOUNT)))
# str(df$stress_CLICKCOUNT)
# 
# df$ctl_1ST_CLICK <- as.numeric(as.character((df$ctl_1ST_CLICK)))
# str(df$ctl_1ST_CLICK) 
# df$ctl_LASTCLICK <- as.numeric(as.character((df$ctl_LASTCLICK)))
# str(df$ctl_LASTCLICK)
# df$ctl_SUBMIT <- as.numeric(as.character((df$ctl_SUBMIT)))
# str(df$ctl_SUBMIT)
# df$ctl_CLICKCOUNT <- as.numeric(as.character((df$ctl_CLICKCOUNT)))
# str(df$ctl_CLICKCOUNT)
# 
# df$mp_1ST_CLICK <- as.numeric(as.character((df$mp_1ST_CLICK)))
# str(df$mp_1ST_CLICK) 
# df$mp_LASTCLICK <- as.numeric(as.character((df$mp_LASTCLICK)))
# str(df$mp_LASTCLICK)
# df$mp_SUBMIT <- as.numeric(as.character((df$mp_SUBMIT)))
# str(df$mp_SUBMIT)
# df$mp_CLICKCOUNT <- as.numeric(as.character((df$mp_CLICKCOUNT)))
# str(df$mp_CLICKCOUNT)


# End data management.




#########################################################
# Save data set
#########################################################


save(df, file="BrexitTwitterData.R")
save(post_ref, file="post_ref")
save(post_ref_UK, file="post_ref_UK")
save(direct_links, file="direct_links")
save(direct_links_UK, file="direct_links_UK")
# load("BrexitTwitterData.R")


