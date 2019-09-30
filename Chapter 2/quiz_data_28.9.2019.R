#########################################################
# ESSEXLab Experiments 2018 (Round 2)
# 28 September 2019
# Part 1 -- Data Management
#########################################################

rm(list = ls()) 
setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/2.SeedcornProject/Data/Round2/") 
library(foreign) # for read.csv
# library(plyr) # for bind_rows -- PLYR DOES NOT LIKE DPLYR
library(dplyr) 
library(tidyr) 

# Comment out: Command Shift C

# R Version
# sessionInfo() # -- R version 3.3.1 (2016-06-21)
# R.Version()


#########################################################
# Import heroku datasets
#########################################################

### Round 1 ####

# june19_wide <- read.csv("20 June 2018/all_apps_wide_2018-06-20.csv", header=TRUE, sep=",", na.strings=c(""," "))
# june19_long <- read.csv("20 June 2018/quiz_2018-06-20.csv", header=TRUE, sep=",", na.strings=c(""," "))

# june21_wide <- read.csv("21 June 2018/all_apps_wide_2018-06-21.csv", header=TRUE, sep=",", na.strings=c(""," "))
# june21_long <- read.csv("21 June 2018/quiz_2018-06-21.csv", header=TRUE, sep=",", na.strings=c(""," "))

### Round 2 ####

june22_wide <- read.csv("22 June 2018/all_apps_wide_2018-06-22.csv", header=TRUE, sep=",", na.strings=c(""," "))
june22_long <- read.csv("22 June 2018/quiz_2018-06-22.csv", header=TRUE, sep=",", na.strings=c(""," "))
## I FORGOT TO DOWNLOAD THE TIME DOCUMENT =// 

july30_wide <- read.csv("30 July 2018/all_apps_wide_2018-07-30.csv", header=TRUE, sep=",", na.strings=c(""," "))
july30_long <- read.csv("30 July 2018/quiz_2018-07-30.csv", header=TRUE, sep=",", na.strings=c(""," "))
july30_time <- read.csv("30 July 2018/TimeSpent (accessed 2018-07-30).csv", header=TRUE, sep=",", na.strings=c(""," "))

sep27_wide <- read.csv("27 September 2018/all_apps_wide_2018-09-27.csv", header=TRUE, sep=",", na.strings=c(""," "))
sep27_long <- read.csv("27 September 2018/quiz_2018-09-27.csv", header=TRUE, sep=",", na.strings=c(""," "))
sep27_time <- read.csv("27 September 2018/TimeSpent (accessed 2018-09-27).csv", header=TRUE, sep=",", na.strings=c(""," "))

#########################################################
# Compare datasets
#########################################################

# Compare number of variables (I added variables)

# names(june22_long) # 144
# names(july30_long) # 149
# names(sep27_long) # 148

# See how many variables I added to each 

# install.packages('arsenal', dependencies=TRUE, repos='http://cran.rstudio.com/')
# library(arsenal)
# 
# compare(june22_long, july30_long) # Not shared: 7 variables
# compare(july30_long, sep27_long) # Not shared: 1 variables
# --> Alright

# https://cran.r-project.org/web/packages/arsenal/vignettes/compare.html


#########################################################
# Import list of easy and hard questions 
#########################################################

easyQs_28 <- read.csv("Qs/easyQs_28.csv", header=TRUE, sep=",", na.strings=c(""," "))
hardQs_28 <- read.csv("Qs/hardQs_28.csv", header=TRUE, sep=",", na.strings=c(""," "))
manipulatedQs <- read.csv("Qs/manipulatedQs.csv", header=TRUE, sep=",", na.strings=c(""," "))
manipulatedQs2 <- read.csv("Qs/manipulatedQs2.csv", header=TRUE, sep=",", na.strings=c(""," "))


#########################################################
# Data Management -- heroku data
#########################################################

# See how many participants I need to delete because they
# were me, pre-testing
# length(unique(june22_wide$participant.code)) # 118 
# length(unique(july30_long$participant.code)) # 150 
# length(unique(sep27_long$participant.code)) # 86

#########################################################
# June 22nd data set
#########################################################

# Time variable
june22_wide$participant.time_started <- 
  as.POSIXct(june22_wide$participant.time_started, format="%Y-%m-%d %H:%M:%S")

june22_long$participant.time_started <- 
  as.POSIXct(june22_long$participant.time_started, format="%Y-%m-%d %H:%M:%S")

# The first session was at 12.15 UK time -- remove everything 
# before 2018-06-19 11:00 & remove sessions with 2 or 3 people 
# That was me pre-testing. Also forgot to close the room a couple of 
# times. 

june22_wide <- subset(june22_wide, 
                      participant.time_started > 
                        "2018-06-19 11:00:00 BST" &
                        session.code!="i8shlxdb" & 
                        session.code!="safsdsnz" & 
                        session.code!="vgqxacgz") 

june22_long <- subset(june22_long, 
                      participant.time_started > 
                        "2018-06-19 11:00:00 BST" &
                        session.code!="i8shlxdb" & 
                        session.code!="safsdsnz" & 
                        session.code!="vgqxacgz") 

june22_wide$session.code <- droplevels(june22_wide$session.code)
june22_long$session.code <- droplevels(june22_long$session.code)

## How many sessions? -- 8 -- correct
# 
# june22_long %>%
#   group_by(session.code) %>%
#   summarise(n=length(unique(session.code)))

# unique(june22_long$session.code) 
# unique(june22_wide$session.code) 

# Sort
june22_wide <- june22_wide[order(june22_wide$participant.time_started, 
                                 june22_wide$participant.code), ]
june22_long <- june22_long[order(june22_long$participant.time_started, 
                                 june22_long$participant.code), ]

## How many players? -- 98 -- correct
# june22_long %>%
#   summarise(n=length(unique(participant.code)))


#########################################################
# July 30th data set
#########################################################

july30_wide$participant.time_started <- 
  as.POSIXct(july30_wide$participant.time_started, format="%Y-%m-%d %H:%M:%S")

july30_long$participant.time_started <- 
  as.POSIXct(july30_long$participant.time_started, format="%Y-%m-%d %H:%M:%S")

# Remove everything before 2018-07-03 15:15 & other non-sessions

july30_wide <- subset(july30_wide, 
                      participant.time_started > "2018-07-03 15:13:00 BST" &
                        session.code != "7ka8h0no" &
                        session.code != "jpdthfqn" &
                        session.code != "dx48smmv" )
                        
july30_long <- subset(july30_long, 
                      participant.time_started > "2018-07-03 15:13:00 BST" &
                        session.code != "7ka8h0no" & # 2 people at 13:23
                        session.code != "jpdthfqn" & # 1st real participants
                        session.code != "dx48smmv") # 2nd real participants

# Drop levels
july30_wide$session.code <- droplevels(july30_wide$session.code)
july30_long$session.code <- droplevels(july30_long$session.code)

# Sort
july30_wide <- july30_wide[order(july30_wide$participant.time_started, 
                                 july30_wide$participant.code), ]
july30_long <- july30_long[order(july30_long$participant.time_started, 
                                 july30_long$participant.code), ]

# # How many participants? -- 111 -- Should be 112
# july30_long %>% 
#   summarise(n=length(unique(participant.code)))
# 
# # Identify number of people in each session
# temp <- 
#   july30_long %>%
#   group_by(session.code) %>%
#   summarise(
#     n = length(unique(participant.code))
#   )
# 
# # Problem in session 4hdzc11m: 1 person short
# 
# # Identify missing participant label
# temp <- july30_wide[ , c("participant.time_started", 
#                          "session.code",
#                          "participant.label",
#                          "participant.payoff")]

# DES RÄTSELS LÖSUNG

# In der session um 16.15 (hier 15.15) haben 16 Leute 
# teilgenommen. Im Datensatz sind aber nur 15. 
# Keiner hat ein participant label 4, dafür haben zwei eine 10. 
# 
# -- CHECK // ASK DOMINIK: 
# Was macht oTree, wenn 2 Leute die gleiche Nummer eingeben? 
# -- If so: Type in participant labels / ping pong ball numbers
# before participants come into the lab so they don't make any mistakes? 

## How many sessions? -- 9 -- correct
# unique(july30_long$session.code) 

#########################################################
# September 27th data set
#########################################################

sep27_wide$participant.time_started <- 
  as.POSIXct(sep27_wide$participant.time_started, format="%Y-%m-%d %H:%M:%S")

sep27_long$participant.time_started <- 
  as.POSIXct(sep27_long$participant.time_started, format="%Y-%m-%d %H:%M:%S")

# # Number of people -- 86
# sep27_long %>% 
#   summarise(n=length(unique(participant.code)))

# Identify 2-player sessions (i.e. me  pre-testing)
# temp <- 
#   sep27_long %>%
#   group_by(session.code, player.treatment) %>%
#   summarise(
#     n = length(unique(participant.code))
#   )

# Kick out those 2-player sessions
sep27_long <- subset(sep27_long, 
                        session.code != "s6v952dj" & 
                        session.code != "uw8pa9md" & 
                        session.code != "zq1fveb1" & 
                        session.code != "65tyi3hn") 
sep27_wide <- subset(sep27_wide, 
                     session.code != "s6v952dj" & 
                       session.code != "uw8pa9md" & 
                       session.code != "zq1fveb1" & 
                       session.code != "65tyi3hn") 

sep27_long$session.code <- droplevels(sep27_long$session.code)

## How many players? -- 68 -- correct
sep27_long %>% 
  summarise(n=length(unique(participant.code))) # apparently this did not drop the levels


#########################################################
# Merge the 3 datasets
#########################################################

# NB -- I am merging datasets that have different numbers of columns. 
# rbind throws an error in such a case whereas bind_rows assigns "NA" 
# to those rows of columns missing in one of the data frames. 

# NO NO: 
# df <- rbind(june22_long, july30_long)

# BETTER: 
library(dplyr)
df <- bind_rows(june22_long, july30_long, sep27_long)

# Re-install dplyr
# options(repos='http://cran.rstudio.com/')
# install.packages("dplyr")


# Check again if I have the correct number of sessions
# unique(df$session.code) # 23 -- correct


#########################################################
# Clean data set
#########################################################

#### Add a round number ###

df <- 
  df %>%
  group_by(session.code, participant.code) %>% 
  mutate(
    round = 1:12)

# class(df)
# colnames(df)

# Kick out unneeded participant vars

df <- subset(df, 
             select = -c(
               participant._is_bot, 
               participant._index_in_pages,
               participant._max_page_index,
               participant._current_app_name,
               participant._round_number,
               participant._current_page_name,
               participant.mturk_worker_id,
               participant.mturk_assignment_id,
               participant.id_in_session,
               participant.visited,
               participant.ip_address,
               player.id_in_group,
               player.payoff, # REMOVE from oTree code -- payoffs are automatically calculated as participant.payoff
               player.question_id,
               player.other_question_id,
               player.is_correct_this_round,
               player.earned_this_round,
               player.payoff_score,
               # player.team_a_earned_total, # added this as a participant variable in re-run
               # player.team_b_earned_total, 
               player.earned_total, # same as is_correct_total
               player.half_own_earnings, # only needed to this to calculate payoffs
               player.team_a_is_correct_this_round, # only recorded after 12 rounds
               player.team_b_is_correct_this_round, # 
               player.team_a_avg_correct_this_round,
               player.team_b_avg_correct_this_round, 
               player.team_a_earned_total, # coding mistake for first 2 days (corrected before sessions June 22)
               player.team_b_earned_total,
               player.labExperience, 
               player.major, # forgot to add this to pages.py
               player.treatment_new,
               player.paid # differs from payoff in that it shows 5 if they made less than 5
               ))

# Also kicking out player.fairTeamAEasierQs
# Because, luckily, they are all NAs: nobody got this Q
# sum(complete.cases(df$player.fairTeamAEasierQs))
df$player.fairTeamAEasierQs <- NULL

# Rename factor levels 
# (NB: oTree data set seems to be an hour behind GMT)
# str(df$session.code)

df$session <- as.factor(df$session.code)

levels(df$session)[levels(df$session)=="kilcvupc"] <- "2018/06/19_1215" 
levels(df$session)[levels(df$session)=="1sx0pqif"] <- "2018/06/19_1315"
levels(df$session)[levels(df$session)=="2myiso5l"] <- "2018/06/19_1500_a"
levels(df$session)[levels(df$session)=="joyriq4o"] <- "2018/06/19_1500_b"

levels(df$session)[levels(df$session)=="c41g9055"] <- "2018/06/21_1130"
levels(df$session)[levels(df$session)=="7bnw8ztf"] <- "2018/06/21_1230"
levels(df$session)[levels(df$session)=="udiuuv86"] <- "2018/06/21_1330"

levels(df$session)[levels(df$session)=="7lbpysto"] <- "2018/06/22_1515"

levels(df$session)[levels(df$session)=="4hdzc11m"] <- "2018/07/03_1615"
levels(df$session)[levels(df$session)=="t6whae1u"] <- "2018/07/03_1715"

levels(df$session)[levels(df$session)=="lggmyvlc"] <- "2018/07/04_1000"
levels(df$session)[levels(df$session)=="jk87u48g"] <- "2018/07/04_1100"
levels(df$session)[levels(df$session)=="blct35dp"] <- "2018/07/04_1315"
levels(df$session)[levels(df$session)=="979qn27a"] <- "2018/07/04_1515"

levels(df$session)[levels(df$session)=="4ecvj1r3"] <- "2018/07/25_1630"
levels(df$session)[levels(df$session)=="01mbbov9"] <- "2018/07/30_1500"
levels(df$session)[levels(df$session)=="k8jcg5gu"] <- "2018/07/30_1600"

## Identify session codes
# temp <- sep27_wide[  , c("session.code", "participant.time_started")]

levels(df$session)[levels(df$session)=="gbui9l82"] <- "2018/09/17_1215"
levels(df$session)[levels(df$session)=="qgsekvjj"] <- "2018/09/19_1315"
levels(df$session)[levels(df$session)=="mobgeok5"] <- "2018/09/21_1215"
levels(df$session)[levels(df$session)=="zio4nwww"] <- "2018/09/21_1315"

levels(df$session)[levels(df$session)=="7ei7meqp"] <- "2018/09/27_1215"
levels(df$session)[levels(df$session)=="b42yr09e"] <- "2018/09/27_1315"

df$session <- as.factor(as.character(df$session))
df$session.code <- as.factor(as.character(df$session.code))
# x <- df[ , c("session.code", "session", "participant.time_started")]
# temp <- unique(df$session.code) # all good
# levels(df$session.code)

# df$participant.time_started

df$participant.code <- as.factor(as.character(df$participant.code))


#########################################################
# Data Management -- LISTS OF EASY AND HARD QUESTIONS 
#########################################################

# Merge questions dataset
Qs <- rbind(easyQs_28, hardQs_28, manipulatedQs, manipulatedQs2)
# names(Qs)

# dim(easyQs_28)
# dim(hardQs_28)
# dim(manipulatedQs)
# dim(manipulatedQs2)

# 2 Create a new id variable (1-60)
# (Because, so far, the ids are 1-28 and two times 1-2)
Qs$question_id_60 <- as.numeric(as.character(Qs$id))

Qs$question_id_60[ Qs$question=="What is the capital city of Germany?" ] <- 29
Qs$question_id_60[ Qs$question=="What is the capital city of Spain?" ] <- 30

Qs$question_id_60[ Qs$question=="Concord is the capital of which U.S. state?" ] <- 29
Qs$question_id_60[ Qs$question=="In Canada, to get from Edmonton to Calgary you would travel:" ] <- 30

# I want the easyQs to have ids ranging from 1-30
# and the hardQs from 31-60 so I add 30 to the hardQs
Qs$question_id_60 <- ifelse(Qs$difficulty == "hard", 
                            Qs$question_id_60 + 30, 
                            Qs$question_id_60) 

# Sort
Qs <- Qs[order(Qs$question_id_60, Qs$difficulty), ] 

# Create numeric / dummy variable for hard question
Qs$hard <- ifelse(Qs$difficulty == "hard", 1, 0) 
# str(Qs$hard)

# Drop the initial id (that went from 0 to 30)
Qs$id <- NULL

# Qs_subset <- Qs[c("question_id", "question", "solution", "difficulty", "hard")]

# colnames(Qs)
# names(Qs)


# Add Qs to df 
# (I want to add the difficulty of your own team's question and the choices)
df <- left_join(df, Qs, by = c("player.question" = "question",
                               "player.solution" = "solution"))

# Add player. so as to free up the choice1 etc vars so I can add the other team's Qs
names(df)[names(df)=="choice1"] <- "player.choice1"
names(df)[names(df)=="choice2"] <- "player.choice2"
names(df)[names(df)=="choice3"] <- "player.choice3"
names(df)[names(df)=="choice4"] <- "player.choice4"
names(df)[names(df)=="difficulty"] <- "player.difficulty"
names(df)[names(df)=="hard"] <- "player.hardQ"
names(df)[names(df)=="question_id_60"] <- "player.question_id_60"

df$participant.label <- as.numeric(df$participant.label)

# Rename the question id so I can add the same variables for the other player
names(Qs)[names(Qs)=="question_id_60"] <- "player.question_id_60_otherQ"

# # Remove all NA columns
# df <- df[, colSums(is.na(df))<nrow(df)]
# Qs <- Qs[, colSums(is.na(Qs))<nrow(Qs)]

# # Merge to add the difficulty of the other team's question
df <- left_join(df, Qs, by = c("player.other_question" = "question",
                              "player.other_solution" = "solution"))
# Rename vars
names(df)[names(df)=="choice1"] <- "player.choice1_otherQ"
names(df)[names(df)=="choice2"] <- "player.choice2_otherQ"
names(df)[names(df)=="choice3"] <- "player.choice3_otherQ"
names(df)[names(df)=="choice4"] <- "player.choice4_otherQ"
names(df)[names(df)=="difficulty"] <- "player.difficulty_otherQ"
names(df)[names(df)=="hard"] <- "player.hardQ_otherQ"
names(df)[names(df)=="question_id_60"] <- "player.question_id_60_otherQ"

# Sort
df <- df[order(df$participant.time_started, df$participant.code), ]

# Show 3 random rows
# df[sample(nrow(df), 3), ]


#########################################################
# Create new variables for total number & share of easy & hard Qs
#########################################################

# NB: player.hardQ == dummy: for each question: is this a hardQ?
# In the df dataset, every person has 12 rows

# # dplyr tutorial
# # https://datacarpentry.org/R-genomics/04-dplyr.html
# 
# # select a subset of rows
# filter(df, session.code=="qgsekvjj")
# 
# # re-order rows (i.e. start with the ones who started first)
# arrange(df, participant.time_started)
# 
# # descending order (i.e. start with the ones who started last)
# arrange(df, desc(participant.time_started))
# 
# # zoom in on a few columns (drops all others)
# select(df, participant.code, session.code, 
#        player.treatment, player.hardQ)
# 
# # take a random sample of rows
# sample_n(df, 20)

# Achtung -- the following gives an error if plyr and dplyr are loaded
# at the same time -- so unload plyr:
library(dplyr)
detach("package:plyr", unload=TRUE) 

# df[1:100, c("participant.code", "session.code", 
#             "player.treatment", "player.hardQ")]

df <-
  df %>%
  group_by(session.code, participant.code) %>% # redo using participant.code, not label
  mutate(
    # id = 1: length(unique(participant.label)),
    sum_hardQs = sum(player.hardQ),
    sum_hardQs_other_team = sum(player.hardQ_otherQ),
    share_hardQs = (sum(player.hardQ) / 12),
    #share_hardQs2 = mean(player.hardQ), #same
    share_hardQs_other_team = (sum(player.hardQ_otherQ) / 12)) # %>%
  # select(sum_hardQs, share_hardQs, 
  #        sum_hardQs_other_team, share_hardQs_other_team)

# create a variable for average payoffs 
df <-
  df %>%
  group_by(session.code, player.treatment) %>% # redo using participant.code, not label
  mutate(
    meanPayoffsOwnGroup = mean(participant.payoff)
  )


# df$player.n_team_a_consented

# # See if it worked - yes
# temp <- df[ , c("meanPayoffsOwnGroup", "session.code", "player.treatment")]


# Count number of participants
# length(unique(df$participant.code)) # 277 -- correct

# Assign each participant a number between 1 and 264. 

# Create id variable
# https://stackoverflow.com/questions/42921674/assign-unique-id-based-on-two-columns

df$id = cumsum(!duplicated(df$participant.code))

# Check if it worked -- yes
# temp <- df[ , c("participant.code",
#                 "id",
#                 "player.treatment",
#                 "player.falsefeedback",
#                 "player.accuracyRating_fairplay",
#                 "player.accuracyRating_unfair")] # different values -- fine



#########################################################
# Anomalies in the dataset 
#########################################################
# 
# Checking data for problematic participants
# 
# Case 1 
# -- participant label 2 in session 7lbpysto = 2018/06/22_1515
# Did not read the payoffs page at all, could not remember his own payoff. 
# 
# 
# temp <- 
#   df %>%
#   group_by(session.code, participant.code, participant.label) %>%
#   summarise(
#     n = length(unique(participant.code))
#   )
# 
# # Have a look at label 2
# temp <- subset(df, 
#                session.code == "2018/06/22_1515" &
#                  participant.label == "2")
# 
# temp$player.wantFeedbackOn0
# temp$player.wantFeedbackOn1
# temp$player.wantFeedbackOn2
# 
# temp$player.fairplay_manipulation_check # 96
# temp$player.unfair_manipulation_check # 73 --- should be less than 50. 
# # But at least this person thinks the unfair person thought it was less
# # fair than the fairplay person did
# 
# # VERDICT 
# # Will leave this person in the dataset -- feedback suggests he did get it
# 
# Case 2
# Participant label 6 in session 2018/07/04_1315
# --> Had participated before (in round 1)
# 
# temp <- subset(df, 
#                session.code == "2018/07/04_1315" &
#                  participant.label == "6")
# temp$participant.payoff
# 
# VERDICT I am not discarding this person because the low payoff 
# suggests that there were no learning effects
# 
# Case 3 
# -- Session t6whae1u --  weird thing, 13th person in a 12 people game, 
# finished early, made 9.5 
# --> not recorded. 
# 
# # temp <- df[ , c("session.code", "participant.label", "participant.payoff")]



#########################################################
# Data Management
#########################################################

# names(df)

# Kick out everything that is not a participant or player variable
# i.e. group and session vars

df <- 
  subset(df, select=c(
    id,
    session, 
    session.code,
    round, 
    sum_hardQs, 
    sum_hardQs_other_team, 
    share_hardQs, 
    share_hardQs_other_team,
    meanPayoffsOwnGroup, 
    grep("participant", names(df)), 
    grep("player", names(df))))

# Next, remove the player. prefix from the variables
 
names(df) <- gsub("player.", "", names(df))

# Same for the june22 and july30 datasets
names(june22_wide) <- gsub("player.", "", names(june22_wide))
names(june22_long) <- gsub("player.", "", names(june22_long))
names(july30_wide) <- gsub("player.", "", names(july30_wide))
names(july30_long) <- gsub("player.", "", names(july30_long))
names(sep27_wide) <- gsub("player.", "", names(sep27_wide))
names(sep27_long) <- gsub("player.", "", names(sep27_long))

# Look at variable names
# names(df)

# Sort
df <- df[order(df$participant.time_started, df$participant.code), ]


# Fill up NA cells

# Variables collected in round 1 -- fill up (direction: down)

# ACHTUNG: This needs tidyr!! 

library(tidyr)

df <- 
  df %>%
  group_by(participant.code) %>%
  fill(consented, falsefeedback, firstfeedback) # default direction: down

# Variables collected in round 12 -- fill up (direction: UP)
# n_team_a_consented : team_b_avg_is_correct
# whoGotHigherPayoff : anythingElse

df <-
  df %>%
  group_by(participant.code) %>%
  fill(
    n_team_a_consented:rigged,
    .direction="up")

# Variables collected in round 11 -- fill UP 6 rows and DOWN 1 row

# UP 6 rows und 1 row down:
df <-
  df %>%
  group_by(participant.code) %>%
  fill(expectationsLastQ) # default direction: down

# See if it worked -- yes
# temp <- df[  , c("id", "session.code", "n_team_a_consented")] # works

# I am saving this dataset as the LONG dataset 
# (Each player has 12 rows -- one for each round)

save(df, file="df_long")

# see if bullet points are OK -- yes
# temp <- df[ , c("id", 
#                 "treatment",
#                 "falsefeedback", 
#                 "false_fact_too_hard2", 
#                 "true_fact_too_hard2")]


#########################################################
# From long to wide
#########################################################

# Create 2 dataframes: 

# 1) One with just the variables that are repeated every round.
# 2) One with all the other ones (that stay the same).

# Variables that differ in each round (need to spread these out)

rep <- 
  df %>%
  dplyr::select(
    # id, # participant.code # need this as a grouping variable
    participant.code, # grouping variable
    round,
    question,
    # choice1, # don't really need these in the dataset
    # choice2,
    # choice3,
    # choice4,
    solution,
    submitted_answer,
    is_correct,
    other_question,
    # choice1_otherQ, # don't really need these in the dataset
    # choice2_otherQ,
    # choice3_otherQ,
    # choice4_otherQ,
    other_solution)

# length(unique(rep$participant.code))

# rep <- rep[order(df$participant.time_started,
#                  df$participant.code,
#                  df$round), ]

# # Adding missing grouping variables: `participant.code`
# 
# length(unique(rep$id))
# rep$participant.code <- NULL
# 
# rep <- rep[order(df$participant.time_started, df$id, df$round), ]

# Spread -- so that I have one column for each row

# require(dplyr)
# detach("package:plyr", unload=TRUE)
# # THE FOLLOWING ONLY WORKS IF PLYR IS NOT LOADED!! (Wrong order but tough beans)

rep <-
  rep %>%
  gather(variable, value, -(participant.code:round)) %>%
  unite(temp, round, variable) %>%
  spread(temp, value) 

rep <- rep[c(
  "participant.code",
  "1_question",
  "1_solution",
  "1_submitted_answer",
  "1_is_correct",
  "1_other_question",
  "1_other_solution",
  "2_question",
  "2_solution",
  "2_submitted_answer",
  "2_is_correct",
  "2_other_question",
  "2_other_solution",
  "3_question",
  "3_solution",
  "3_submitted_answer",
  "3_is_correct",
  "3_other_question",
  "3_other_solution",
  "4_question",
  "4_solution",
  "4_submitted_answer",
  "4_is_correct",
  "4_other_question",
  "4_other_solution",
  "5_question",
  "5_solution",
  "5_submitted_answer",
  "5_is_correct",
  "5_other_question",
  "5_other_solution",
  "6_question",
  "6_solution",
  "6_submitted_answer",
  "6_is_correct",
  "6_other_question",
  "6_other_solution",
  "7_question",
  "7_solution",
  "7_submitted_answer",
  "7_is_correct",
  "7_other_question",
  "7_other_solution",
  "8_question",
  "8_solution",
  "8_submitted_answer",
  "8_is_correct",
  "8_other_question",
  "8_other_solution",
  "9_question",
  "9_solution",
  "9_submitted_answer",
  "9_is_correct",
  "9_other_question",
  "9_other_solution",
  "10_question",
  "10_solution",
  "10_submitted_answer",
  "10_is_correct",
  "10_other_question",
  "10_other_solution",
  "11_question",
  "11_solution",
  "11_submitted_answer",
  "11_is_correct",
  "11_other_question",
  "11_other_solution",
  "12_question",
  "12_solution",
  "12_submitted_answer",
  "12_is_correct",
  "12_other_question",
  "12_other_solution"
)]


# b) Variables that remain constant across all 12 rounds
# (i.e. all others)

# exclude variables
out <- names(df) %in% c("round",
                        "question",
                        "choice1",
                        "choice2",
                        "choice3",
                        "choice4",
                        "solution",
                        "submitted_answer",
                        "is_correct",
                        "other_question",
                        "choice1_otherQ",
                        "choice2_otherQ",
                        "choice3_otherQ",
                        "choice4_otherQ",
                        "other_solution") 

constant <- df[!out]

## used to work
# constant <- df %>%
#   select(
#     -round,
#     -question,
#     -choice1,
#     -choice2,
#     -choice3,
#     -choice4,
#     -solution,
#     -submitted_answer,
#     -is_correct,
#     -other_question,
#     -choice1_otherQ,
#     -choice2_otherQ,
#     -choice3_otherQ,
#     -choice4_otherQ,
#     -other_solution)

# length(names(df)) # 111
# length(names(other)) # 96

# Remove duplicates rows using duplicated [baseR]
constant <- constant[!duplicated(constant$participant.code), ]

# length(names(rep)) # 73

# Merge
df <- left_join(constant, rep, by = c("participant.code"))
df <- df[order(df$participant.time_started, df$participant.code), ]

# NB: Naturally the new dataset no longer has a round variable.

# Save
save(df, file="df")

# unique(df$participant.code) # 277 -- correct



#########################################################
# DVs 
#########################################################

df$id <- as.numeric(df$id)

# Create Numeric Variables for DVs

# a) General Agreement

### Re-order Factor Levels

df$agreeFeedback_fairplay <- factor(df$agreeFeedback_fairplay, 
                                    levels = c(
  "Strongly disagree","Disagree","Slightly disagree", 
  "Slightly agree", "Agree", "Strongly agree"))

df$agreeFeedback_unfair <- factor(df$agreeFeedback_unfair, 
                                  levels = c(
  "Strongly disagree","Disagree","Slightly disagree", 
  "Slightly agree", "Agree", "Strongly agree"))

#########################################################
# DVs -- Create numeric variables
#########################################################

df$agreeFeedback_fairplay_num <- NA
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Strongly disagree"] <- 1
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Disagree"] <- 2
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Slightly disagree"] <- 3
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Slightly agree"] <- 4
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Agree"] <- 5
df$agreeFeedback_fairplay_num[df$agreeFeedback_fairplay == "Strongly agree"] <- 6

df$agreeFeedback_unfair_num <- NA
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Strongly disagree"] <- 1
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Disagree"] <- 2
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Slightly disagree"] <- 3
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Slightly agree"] <- 4
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Agree"] <- 5
df$agreeFeedback_unfair_num[df$agreeFeedback_unfair == "Strongly agree"] <- 6


# b) Accuracy Rating

# Turn into numeric
# intonum <- function(x){as.numeric(as.character(x))}
# Turn columns 3:17 into numeric
# df[3:20] <- lapply(unemp[3:20], intonum)

df$educationRating_fairplay <- as.numeric(df$educationRating_fairplay)
df$educationRating_unfair <- as.numeric(df$educationRating_unfair)

df$accuracyRating_fairplay <- as.numeric(df$accuracyRating_fairplay)
df$accuracyRating_unfair <- as.numeric(df$accuracyRating_unfair)

df$representationRating_fairplay <- as.numeric(df$representationRating_fairplay)
df$representationRating_unfair <- as.numeric(df$representationRating_unfair)


# c) Agreement with false facts

### Re-order Factor Levels

# false facts

df$true_fact_too_easy <- factor(df$true_fact_too_easy, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$true_fact_too_hard <- factor(df$true_fact_too_hard, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$true_fact_too_hard2 <- factor(df$true_fact_too_hard2, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$true_fact_30sec <- factor(df$true_fact_30sec, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))

df$false_fact_too_easy <- factor(df$false_fact_too_easy, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$false_fact_too_hard <- factor(df$false_fact_too_hard, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$false_fact_too_hard2 <- factor(df$false_fact_too_hard2, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$false_fact_30sec_unfair <- factor(df$false_fact_30sec_unfair, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))
df$false_fact_30sec_fairplay <- factor(df$false_fact_30sec_fairplay, levels = c(
  "Definitely False","Probably False","Probably True", "Definitely True"))

# true facts

df$true_fact_too_easy_num <- NA
df$true_fact_too_easy_num[df$true_fact_too_easy == "Definitely False"] <- 1
df$true_fact_too_easy_num[df$true_fact_too_easy == "Probably False"] <- 2
df$true_fact_too_easy_num[df$true_fact_too_easy == "Probably True"] <- 3
df$true_fact_too_easy_num[df$true_fact_too_easy == "Definitely True"] <- 4

df$true_fact_too_hard_num <- NA
df$true_fact_too_hard_num[df$true_fact_too_hard == "Definitely False"] <- 1
df$true_fact_too_hard_num[df$true_fact_too_hard == "Probably False"] <- 2
df$true_fact_too_hard_num[df$true_fact_too_hard == "Probably True"] <- 3
df$true_fact_too_hard_num[df$true_fact_too_hard == "Definitely True"] <- 4

df$true_fact_too_hard2_num <- NA
df$true_fact_too_hard2_num[df$true_fact_too_hard2 == "Definitely False"] <- 1
df$true_fact_too_hard2_num[df$true_fact_too_hard2 == "Probably False"] <- 2
df$true_fact_too_hard2_num[df$true_fact_too_hard2 == "Probably True"] <- 3
df$true_fact_too_hard2_num[df$true_fact_too_hard2 == "Definitely True"] <- 4

df$true_fact_30sec_num <- NA
df$true_fact_30sec_num[df$true_fact_30sec == "Definitely False"] <- 1
df$true_fact_30sec_num[df$true_fact_30sec == "Probably False"] <- 2
df$true_fact_30sec_num[df$true_fact_30sec == "Probably True"] <- 3
df$true_fact_30sec_num[df$true_fact_30sec == "Definitely True"] <- 4


df$false_fact_too_easy_num <- NA
df$false_fact_too_easy_num[df$false_fact_too_easy == "Definitely False"] <- 1
df$false_fact_too_easy_num[df$false_fact_too_easy == "Probably False"] <- 2
df$false_fact_too_easy_num[df$false_fact_too_easy == "Probably True"] <- 3
df$false_fact_too_easy_num[df$false_fact_too_easy == "Definitely True"] <- 4

df$false_fact_too_hard_num <- NA
df$false_fact_too_hard_num[df$false_fact_too_hard == "Definitely False"] <- 1
df$false_fact_too_hard_num[df$false_fact_too_hard == "Probably False"] <- 2
df$false_fact_too_hard_num[df$false_fact_too_hard == "Probably True"] <- 3
df$false_fact_too_hard_num[df$false_fact_too_hard == "Definitely True"] <- 4

df$false_fact_too_hard2_num <- NA
df$false_fact_too_hard2_num[df$false_fact_too_hard2 == "Definitely False"] <- 1
df$false_fact_too_hard2_num[df$false_fact_too_hard2 == "Probably False"] <- 2
df$false_fact_too_hard2_num[df$false_fact_too_hard2 == "Probably True"] <- 3
df$false_fact_too_hard2_num[df$false_fact_too_hard2 == "Definitely True"] <- 4

df$false_fact_30sec_fairplay_num <- NA
df$false_fact_30sec_fairplay_num[df$false_fact_30sec_fairplay == "Definitely False"] <- 1
df$false_fact_30sec_fairplay_num[df$false_fact_30sec_fairplay == "Probably False"] <- 2
df$false_fact_30sec_fairplay_num[df$false_fact_30sec_fairplay == "Probably True"] <- 3
df$false_fact_30sec_fairplay_num[df$false_fact_30sec_fairplay == "Definitely True"] <- 4

df$false_fact_30sec_unfair_num <- NA
df$false_fact_30sec_unfair_num[df$false_fact_30sec_unfair == "Definitely False"] <- 1
df$false_fact_30sec_unfair_num[df$false_fact_30sec_unfair == "Probably False"] <- 2
df$false_fact_30sec_unfair_num[df$false_fact_30sec_unfair == "Probably True"] <- 3
df$false_fact_30sec_unfair_num[df$false_fact_30sec_unfair == "Definitely True"] <- 4


#########################################################
# IVs -- Create numeric variables
#########################################################

### expectations ####
df$expectationsLastQ <- as.factor(as.character(df$expectationsLastQ))
levels(df$expectationsLastQ)

df$expectationsLastQ_f[df$expectationsLastQ == "Both teams' questions will be about the same difficulty."] <- 'Same Difficulty'
df$expectationsLastQ_f[df$expectationsLastQ == "Don't know. / Impossible to say."] <- 'DK'
df$expectationsLastQ_f[df$expectationsLastQ == "One team will get an easier question than the other team. Whether that is Team A or Team B is a 50-50 chance."] <- '50-50'
df$expectationsLastQ_f[df$expectationsLastQ == "Team A will get an easier question than Team B."] <- 'Team A Easier'
df$expectationsLastQ_f[df$expectationsLastQ == "Team B will get an easier question than Team A.."] <- 'Team B Easier'
df$expectationsLastQ_f <- as.factor(df$expectationsLastQ_f)
levels(df$expectationsLastQ_f)

df$expectationsLastQ_TeamA <- ifelse(df$expectationsLastQ_f == 'Team A Easier', 1, 0)

### identification #### 
df$identification
df$identification_num[df$identification == "Graph 1"] <- 1
df$identification_num[df$identification == "Graph 2"] <- 2
df$identification_num[df$identification == "Graph 3"] <- 3
df$identification_num[df$identification == "Graph 4"] <- 4
df$identification_num[df$identification == "Graph 5"] <- 5
df$identification_num[df$identification == "Graph 6"] <- 6
df$identification_num[df$identification == "Graph 7"] <- 7
df$identification_num[df$identification == "Don't Know"] <- NA

# df$identification <- NULL
tail(df$identification_num)

### whoGotHigherPayoff ### 
df$whoGotHigherPayoff_num
df$whoGotHigherPayoff_num[df$whoGotHigherPayoff == "Do not remember."] <- 'DK'
df$whoGotHigherPayoff_num[df$whoGotHigherPayoff == "On average, both teams got around the same payoff."] <- 'Same'
df$whoGotHigherPayoff_num[df$whoGotHigherPayoff == "On average, Team A got a higher payoff than Team B."] <- 'A'
df$whoGotHigherPayoff_num[df$whoGotHigherPayoff == "On average, Team B got a higher payoff than Team B."] <- 'B'

df$whoGotHigherPayoff <- df$whoGotHigherPayoff_num
df$whoGotHigherPayoff <- as.factor(as.character(df$whoGotHigherPayoff))
df$whoGotHigherPayoff_num <- NULL

### difference in payoffs

df$difPayoffs <- df$team_a_avg_payoff - df$team_b_avg_payoff

### byChanceTeamAEasierQs ### 

# == luckOfTheDraw
# 0 = Definitely by chance
# 100 = No way this happened by chance

df$byChanceTeamAEasierQs <- as.numeric(df$byChanceTeamAEasierQs)
# higher numbers == NOT by chance

df$luckOfTheDraw <- ifelse(df$byChanceTeamAEasierQs < 50, 1, 0)

### legitTeamAhigherPayoffs ### 

df$legitTeamAhigherPayoffs_no <- ifelse(df$legitTeamAhigherPayoffs == 'No', 1, 0)

### emotions ####

df$happy_num <- NA
df$happy_num[df$happy == "Not At All"] <- 1
df$happy_num[df$happy == "Slightly"] <- 2
df$happy_num[df$happy == "Somewhat"] <- 3
df$happy_num[df$happy == "Moderately"] <- 4
df$happy_num[df$happy == "Extremely"] <- 5
df$happy_num[df$happy == "Don't Know"] <- NA

df$angry_num <- NA
df$angry_num[df$angry == "Not At All"] <- 1
df$angry_num[df$angry == "Slightly"] <- 2
df$angry_num[df$angry == "Somewhat"] <- 3
df$angry_num[df$angry == "Moderately"] <- 4
df$angry_num[df$angry == "Extremely"] <- 5
df$angry_num[df$angry == "Don't Know"] <- NA

df$proud_num <- NA
df$proud_num[df$proud == "Not At All"] <- 1
df$proud_num[df$proud == "Slightly"] <- 2
df$proud_num[df$proud == "Somewhat"] <- 3
df$proud_num[df$proud == "Moderately"] <- 4
df$proud_num[df$proud == "Extremely"] <- 5
df$proud_num[df$proud == "Don't Know"] <- NA

df$resentful_num <- NA
df$resentful_num[df$resentful == "Not At All"] <- 1
df$resentful_num[df$resentful == "Slightly"] <- 2
df$resentful_num[df$resentful == "Somewhat"] <- 3
df$resentful_num[df$resentful == "Moderately"] <- 4
df$resentful_num[df$resentful == "Extremely"] <- 5
df$resentful_num[df$resentful == "Don't Know"] <- NA

### fair ####
df$fair <- factor(df$fair, levels = c(
  "Very Unlikely", "Somewhat Unlikely", "Somewhat Likely", "Very Likely"))

### feedbackMakesDifference ####
df$feedbackMakesDifference <- factor(df$feedbackMakesDifference, 
                                     levels = c(
                                       "Very Unlikely", 
                                       "Somewhat Unlikely", 
                                       "Somewhat Likely", 
                                       "Very Likely"))

df$feedbackMakesDifference_num <- NA
df$feedbackMakesDifference_num[df$feedbackMakesDifference == "Very Unlikely"] <- 1
df$feedbackMakesDifference_num[df$feedbackMakesDifference == "Somewhat Unlikely"] <- 2
df$feedbackMakesDifference_num[df$feedbackMakesDifference == "Somewhat Likely"] <- 3
df$feedbackMakesDifference_num[df$feedbackMakesDifference == "Very Likely"] <- 4

# levels(df$rigged) # Created this after 98 observations 

df$rigged <- factor(df$rigged, 
                    levels = c(
                      "Very Unlikely", 
                      "Unlikely", 
                      "Likely",
                      "Very Likely"))

df$rigged_num <- NA
df$rigged_num[df$rigged == "Very Unlikely"] <- 1
df$rigged_num[df$rigged == "Unlikely"] <- 2
df$rigged_num[df$rigged == "Likely"] <- 3
df$rigged_num[df$rigged == "Very Likely"] <- 4

df$rigged_d <- ifelse(df$rigged == "Likely" | 
                        df$rigged == "Very Likely", 1, 0)

#### TIPI (BIG5) ###################################################

# extraversion = Extraverted, Enthusiastic.
levels(df$extraversion)
df$extraversion_num <- NA
df$extraversion_num[df$extraversion == "Disagree Strongly"] <- 1
df$extraversion_num[df$extraversion == "Disagree Moderately"] <- 2
df$extraversion_num[df$extraversion == "Disagree A Little"] <- 3
df$extraversion_num[df$extraversion == "Neither Agree Nor Disagree"] <- 4
df$extraversion_num[df$extraversion == "Agree A Little"] <- 5
df$extraversion_num[df$extraversion == "Agree Moderately"] <- 6
df$extraversion_num[df$extraversion == "Agree Strongly"] <- 7

# extraversionR = Reserved, Quiet.
levels(df$extraversionR)
df$extraversionR_num <- NA
df$extraversionR_num[df$extraversionR == "Disagree Strongly"] <- 7
df$extraversionR_num[df$extraversionR == "Disagree Moderately"] <- 6
df$extraversionR_num[df$extraversionR == "Disagree A Little"] <- 5
df$extraversionR_num[df$extraversionR == "Neither Agree Nor Disagree"] <- 4
df$extraversionR_num[df$extraversionR == "Agree A Little"] <- 3
df$extraversionR_num[df$extraversionR == "Agree Moderately"] <- 2
df$extraversionR_num[df$extraversionR == "Agree Strongly"] <- 1

df$extraversion_avg <- (df$extraversion_num + 
                          df$extraversionR_num) / 2

# agreeableness = Sympathetic, Warm
levels(df$agreeableness)
df$agreeableness_num <- NA
df$agreeableness_num[df$agreeableness == "Disagree Strongly"] <- 1
df$agreeableness_num[df$agreeableness == "Disagree Moderately"] <- 2
df$agreeableness_num[df$agreeableness == "Disagree A Little"] <- 3
df$agreeableness_num[df$agreeableness == "Neither Agree Nor Disagree"] <- 4
df$agreeableness_num[df$agreeableness == "Agree A Little"] <- 5
df$agreeableness_num[df$agreeableness == "Agree Moderately"] <- 6
df$agreeableness_num[df$agreeableness == "Agree Strongly"] <- 7

# agreeablenessR = Reserved, Quiet.
levels(df$agreeablenessR)
df$agreeablenessR_num <- NA
df$agreeablenessR_num[df$agreeablenessR == "Disagree Strongly"] <- 7
df$agreeablenessR_num[df$agreeablenessR == "Disagree Moderately"] <- 6
df$agreeablenessR_num[df$agreeablenessR == "Disagree A Little"] <- 5
df$agreeablenessR_num[df$agreeablenessR == "Neither Agree Nor Disagree"] <- 4
df$agreeablenessR_num[df$agreeablenessR == "Agree A Little"] <- 3
df$agreeablenessR_num[df$agreeablenessR == "Agree Moderately"] <- 2
df$agreeablenessR_num[df$agreeablenessR == "Agree Strongly"] <- 1

df$agreeableness_avg <- (df$agreeableness_num + 
                           df$agreeablenessR_num) / 2

# conscientiousness = Dependable, Self-Disciplined.
levels(df$conscientiousness)
df$conscientiousness_num <- NA
df$conscientiousness_num[df$conscientiousness == "Disagree Strongly"] <- 1
df$conscientiousness_num[df$conscientiousness == "Disagree Moderately"] <- 2
df$conscientiousness_num[df$conscientiousness == "Disagree A Little"] <- 3
df$conscientiousness_num[df$conscientiousness == "Neither Agree Nor Disagree"] <- 4
df$conscientiousness_num[df$conscientiousness == "Agree A Little"] <- 5
df$conscientiousness_num[df$conscientiousness == "Agree Moderately"] <- 6
df$conscientiousness_num[df$conscientiousness == "Agree Strongly"] <- 7

# conscientiousnessR = Disorganized, Careless.
levels(df$conscientiousnessR)
df$conscientiousnessR_num <- NA
df$conscientiousnessR_num[df$conscientiousnessR == "Disagree Strongly"] <- 7
df$conscientiousnessR_num[df$conscientiousnessR == "Disagree Moderately"] <- 6
df$conscientiousnessR_num[df$conscientiousnessR == "Disagree A Little"] <- 5
df$conscientiousnessR_num[df$conscientiousnessR == "Neither Agree Nor Disagree"] <- 4
df$conscientiousnessR_num[df$conscientiousnessR == "Agree A Little"] <- 3
df$conscientiousnessR_num[df$conscientiousnessR == "Agree Moderately"] <- 2
df$conscientiousnessR_num[df$conscientiousnessR == "Agree Strongly"] <- 1

df$conscientiousness_avg <- (df$conscientiousness_num + 
                               df$conscientiousnessR_num) / 2

# neuroticism = Anxious, Easily upset.
levels(df$neuroticism)
df$neuroticism_num <- NA
df$neuroticism_num[df$neuroticism == "Disagree Strongly"] <- 1
df$neuroticism_num[df$neuroticism == "Disagree Moderately"] <- 2
df$neuroticism_num[df$neuroticism == "Disagree A Little"] <- 3
df$neuroticism_num[df$neuroticism == "Neither Agree Nor Disagree"] <- 4
df$neuroticism_num[df$neuroticism == "Agree A Little"] <- 5
df$neuroticism_num[df$neuroticism == "Agree Moderately"] <- 6
df$neuroticism_num[df$neuroticism == "Agree Strongly"] <- 7

# neuroticismR = Calm, Emotionally stable.
levels(df$neuroticismR)
df$neuroticismR_num <- NA
df$neuroticismR_num[df$neuroticismR == "Disagree Strongly"] <- 7
df$neuroticismR_num[df$neuroticismR == "Disagree Moderately"] <- 6
df$neuroticismR_num[df$neuroticismR == "Disagree A Little"] <- 5
df$neuroticismR_num[df$neuroticismR == "Neither Agree Nor Disagree"] <- 4
df$neuroticismR_num[df$neuroticismR == "Agree A Little"] <- 3
df$neuroticismR_num[df$neuroticismR == "Agree Moderately"] <- 2
df$neuroticismR_num[df$neuroticismR == "Agree Strongly"] <- 1

df$neuroticism_avg <- (df$neuroticism_num + 
                         df$neuroticismR_num) / 2

# openness = Open to new experiences, Complex.
levels(df$openness)
df$openness_num <- NA
df$openness_num[df$openness == "Disagree Strongly"] <- 1
df$openness_num[df$openness == "Disagree Moderately"] <- 2
df$openness_num[df$openness == "Disagree A Little"] <- 3
df$openness_num[df$openness == "Neither Agree Nor Disagree"] <- 4
df$openness_num[df$openness == "Agree A Little"] <- 5
df$openness_num[df$openness == "Agree Moderately"] <- 6
df$openness_num[df$openness == "Agree Strongly"] <- 7

# opennessR = Conventional, Uncreative.
levels(df$opennessR)
df$opennessR_num <- NA
df$opennessR_num[df$opennessR == "Disagree Strongly"] <- 7
df$opennessR_num[df$opennessR == "Disagree Moderately"] <- 6
df$opennessR_num[df$opennessR == "Disagree A Little"] <- 5
df$opennessR_num[df$opennessR == "Neither Agree Nor Disagree"] <- 4
df$opennessR_num[df$opennessR == "Agree A Little"] <- 3
df$opennessR_num[df$opennessR == "Agree Moderately"] <- 2
df$opennessR_num[df$opennessR == "Agree Strongly"] <- 1

df$openness_avg <- (df$openness_num + 
                      df$opennessR_num) / 2

# NOT RECORDED
# july30_long$group.n_team_a_consented 
# july30_long$group.n_team_b_consented
# july30_long$group.team_a_is_correct_total
# july30_long$group.team_a_add_on
# july30_long$group.team_b_add_on


### gender ###

# Have a look at the 3 people whose gender is
# 'Other/ Prefer Not To Say'

# temp <- df[ which(df$gender=='Other/ Prefer Not To Say'), ]
# Okay on the other variables
# temp$nationality
# temp$participant.payoff

# Create female dummy
df$female <- ifelse(df$gender == 'Female', 1, 0)
# df$female <- ifelse(df$gender == 'Female', 'female', 
#              ifelse(df$gender == 'Male', 'male', NA))
# df$female <- factor(df$female, 
#                     levels = c("male", "female"))

df$teamB <- ifelse(df$treatment=="B", 1, 0)
df$teamA <- ifelse(df$treatment=="A", 1, 0)
df$falseUnfair <- ifelse(df$falsefeedback == "unfair", 1, 0)
df$falseFairplay <- ifelse(df$falsefeedback == "fairplay", 1, 0)


### payoffs ### 

# Look at people who failed the manipulation check

# Look at people who thought the unfair feedback was fair
# 0 = unfair
# 100 = fair
df$unfair_manipulation_check <- as.numeric(df$unfair_manipulation_check)

# nrow(subset(df, df$unfair_manipulation_check > 50)) # 54 =( 
# nrow(subset(df, df$unfair_manipulation_check == 50)) # 17
# nrow(subset(df, df$unfair_manipulation_check < 50)) # 206 =) 

# Look at people who thought the fairplay feedback was unfair
# 0 = unfair
# 100 = fair
df$fairplay_manipulation_check <- as.numeric(df$fairplay_manipulation_check)

# nrow(subset(df, df$fairplay_manipulation_check > 50)) # 201 =) 
# nrow(subset(df, df$fairplay_manipulation_check == 50)) # 29
# nrow(subset(df, df$fairplay_manipulation_check < 50)) # 47 =(  


df$failMP_unfair <- ifelse(df$unfair_manipulation_check > 50, 1, 0)
# nrow(subset(df, df$failMP_unfair==1)) # 54


df$failMP_fairplay <- ifelse(df$fairplay_manipulation_check < 50, 1, 0)
# nrow(subset(df, df$failMP_fairplay==1)) # 47
# 
# 
# # People who got both wrong
# nrow(subset(df, df$failMP_unfair==1 & df$failMP_fairplay==1)) # 12

# People who got either wrong -- 
# Create a variable for people who failed the manipulation check
df$failMP <- ifelse(df$fairplay_manipulation_check < 50 | 
                      df$unfair_manipulation_check > 50, 1, 0)

# # See who failed the MP -- 1/3 of the sample
# nrow(subset(df, df$failMP==1)) # 89
# nrow(subset(df, df$failMP==0)) # 188
# 
# temp <- subset(df, df$failMP_unfair==1)

# See if they are clustered in any session where payoff differences
# were low 

# df$angry_num
# temp <- df[ which(df$failMP == 1) , 
#             c("treatment", 
#               "session", 
#               "share_hardQs", 
#               "difPayoffs", 
#               "angry",
#               "expectationsLastQ",
#               "fairplay_manipulation_check", 
#               "unfair_manipulation_check",
#               "accuracyRating_fairplay",
#               "accuracyRating_unfair",
#               "participant.payoff") ]

# # people who failed had the same payoff 
# mean(temp$participant.payoff) 
# mean(df$participant.payoff) 

# Create false expectations variable

df$expectationsLastQ_TeamA <- ifelse(df$expectationsLastQ == 
                                 'Team A will get an easier question than Team B.', 1, 0)
# df$falseExpectations <- ifelse(df$expectationsLastQ != 
#                                  'Team A Easier', 1, 0)

# Create age variable
df$age <- 2018 - df$born

# create student dummy
df$undergrad <- ifelse(df$positionAtEssex=='Undergraduate Student',
                       1, 0) 
# income
df$income[df$income == 14] <- NA # Do Not Know / Prefer Not To Say

# 1, 'No Income'],
# 2, 'Up to GBP 216/ Month Or GBP 2.600 / Year'
# 3, 'GBP 217 / Month Or GBP 2.600/ year Or More'
# 4, 'GBP 433 / Month Or GBP 5.200 / year Or More'
# 5, 'GBP 867 / Month Or GBP 10.400 / year Or More'
# 6, 'GBP 1.300 / Month Or GBP 15.600 / year Or More'
# 7, 'GBP 1.733 / Month Or GBP 20.800 / year Or More'
# 8, 'GBP 2.167 / Month Or GBP 26.000 / year Or More'
# 9, 'GBP 2.600 / Month Or GBP 31.200 / year Or More'
# 10, 'GBP 3.033 / Month Or GBP 36.400 / year Or More'
# 11, 'GBP 3.467 / Month Or GBP 41.600 / year Or More'
# 12, 'GBP 3.900 / Month Or GBP 46.800 / year Or More'
# 13, 'GBP 4.333 / Month Or GBP 52.000 / year Or More'
# 14, 'Do Not Know / Prefer Not To Say'

df$triviaPerson <- ifelse(df$likeTrivia == 'Like' | 
                          df$likeTrivia == 'Like A Lot ', 1, 0)



#########################################################´
# Revert to long
#########################################################

df$fair <- NULL # seems to be all NAs

df_wide <- df

# Rename variables that differ by unfair/fairplay 
# 6 characters . FP/UF

names(df_wide)[names(df_wide)=="agreeFeedback_fairplay"] <- "agreed.FP"
names(df_wide)[names(df_wide)=="agreeFeedback_unfair"] <- "agreed.UF"

names(df_wide)[names(df_wide)=="agreeFeedback_fairplay_num"] <- "agreeN.FP"
names(df_wide)[names(df_wide)=="agreeFeedback_unfair_num"] <- "agreeN.UF"

names(df_wide)[names(df_wide)=="educationRating_fairplay"] <- "educat.FP"
names(df_wide)[names(df_wide)=="educationRating_unfair"] <- "educat.UF"

names(df_wide)[names(df_wide)=="accuracyRating_fairplay"] <- "accura.FP"
names(df_wide)[names(df_wide)=="accuracyRating_unfair"] <- "accura.UF"

names(df_wide)[names(df_wide)=="representationRating_fairplay"] <- "repres.FP"
names(df_wide)[names(df_wide)=="representationRating_unfair"] <- "repres.UF"

names(df_wide)[names(df_wide)=="false_fact_30sec_fairplay"] <- "ff30se.FP"
names(df_wide)[names(df_wide)=="false_fact_30sec_unfair"] <- "ff30se.UF"

names(df_wide)[names(df_wide)=="false_fact_30sec_fairplay_num"] <- "ff30sN.FP"
names(df_wide)[names(df_wide)=="false_fact_30sec_unfair_num"] <- "ff30sN.UF"


# names(df_wide)

# # Turn all factors into characters
# df_wide <- data.frame(lapply(df_wide, as.character), 
#                       stringsAsFactors=FALSE)
# http://www.talkstats.com/threads/convert-all-factor-columns-to-character.39065/
# This worked... 

factors <- sapply(df_wide, is.factor)
df_wide[factors] <- lapply(df_wide[factors], as.character)

# str(df_wide)
# names(df_wide)

# Turn into long
df_long <-
  df_wide %>%
  gather(key, value, 
         agreed.FP, 
         agreed.UF,
         agreeN.FP,
         agreeN.UF,
         educat.FP, 
         educat.UF,
         accura.FP, 
         accura.UF,
         repres.FP, 
         repres.UF, 
         ff30se.FP, 
         ff30se.UF, 
         ff30sN.FP, 
         ff30sN.UF
         ) %>%
  extract(key, c("question", "feedback"), "(......)\\.(..)") %>%
  spread(question, value)

# Should be double the number of variables -- 418

# Check if it worked -- yes
# df_long$key
# df_long$value
# df_long$accura
# df_long$agreed
# df_long$agreeN

# Rename variables

names(df_long)[names(df_long)=="agreed"] <- "agreeFeedback"
names(df_long)[names(df_long)=="agreeN"] <- "agreeFeedback_num"
names(df_long)[names(df_long)=="educat"] <- "educationRating"
names(df_long)[names(df_long)=="accura"] <- "accuracyRating"
names(df_long)[names(df_long)=="repres"] <- "representationRating"
names(df_long)[names(df_long)=="ff30se"] <- "grossExag30sec"
names(df_long)[names(df_long)=="ff30sN"] <- "grossExag30sec_num"

# Now turn all character variables into factors
characters <- sapply(df_long, is.character)
df_long[characters] <- lapply(df_long[characters], as.factor)

# Turn numbers into numeric
df_long$agreed_num <- as.numeric(as.character(df_long$agreed_num))
df_long$educationRating <- as.numeric(as.character(df_long$educationRating))
df_long$accuracyRating <- as.numeric(as.character(df_long$accuracyRating))
df_long$representationRating <- as.numeric(as.character(df_long$representationRating))
df_long$grossExag30sec_num <- as.numeric(as.character(df_long$grossExag30sec_num))
# add this?
df_long$agreeFeedback_num <- as.numeric(as.character(df_long$agreeFeedback_num))

# Check other vars
# str(df_long)

# Change values for feedback
levels(df_long$feedback)[levels(df_long$feedback)=="UF"] <- "unfair"
levels(df_long$feedback)[levels(df_long$feedback)=="FP"] <- "fairplay"

# order
df_long <- df_long[order(df_long$participant.time_started, 
                         df_long$participant.code), ]

# temp <- 
# df_long[ , c("id", 
#              "treatment",
#              "participant.payoff",
#              "feedback", 
#              "falsefeedback", 
#              "agreeFeedback",
#              "agreeFeedback_num", 
#              "educationRating",
#              "accuracyRating",
#              "representationRating",
#              "grossExag30sec",
#              "grossExag30sec_num")  ]
# 
# temp2 <- 
#   df[ , c("id", 
#           "treatment",
#           "agreeFeedback_fairplay",
#           "agreeFeedback_unfair",
#           "accuracyRating_fairplay",
#           "accuracyRating_unfair",
#           "representationRating_fairplay",
#           "representationRating_unfair",
#           "false_fact_30sec_fairplay",
#           "false_fact_30sec_unfair")  ]

# works!
# 
# #########################################################
# # Subsetting ratings of the factually inaccurate feedback 
# # among Team B only
# 
# df_long_false_B <-
#   subset(df_long, 
#          treatment == "B" & feedback == falsefeedback) 
# 
# df_long_false_B <- 
#   df_long_false_B[ , c("id", 
#                        "treatment",
#                        "feedback",
#                        "falsefeedback",
#                        "agreeFeedback",
#                        "agreeFeedback_num",
#                        "educationRating",
#                        "accuracyRating",
#                        "representationRating",
#                        "grossExag30sec",
#                        "grossExag30sec_num")  ]
# #########################################################



# Save both datasets

save(df, file="df")
# load("df")

save(df_long, file="df_long")
# load("df_long")


### END DATA MANAGEMENT #################################

df$false_fact_too_hard2_num
df_long$false_fact_too_hard2_num # NOT OK all doubles always the first value


### DANISH DESIGN DATA ##################################

# Create minimalist dataset

df_mini <- df[ , c('id', 
                   'treatment', 
                   'falsefeedback', 
                   'firstfeedback',
                   
                   'agreeFeedback_fairplay',
                   'agreeFeedback_fairplay_num',
                   'agreeFeedback_unfair',
                   'agreeFeedback_unfair_num',
                   'accuracyRating_fairplay',
                   'accuracyRating_unfair',
                   
                   'representationRating_fairplay',
                   'representationRating_unfair',
                   
                   'true_fact_too_easy',
                   'true_fact_too_easy_num',
                   'true_fact_too_hard',
                   'true_fact_too_hard_num',
                   'true_fact_too_hard2',
                   'true_fact_too_hard2_num',
                   'true_fact_30sec',
                   'true_fact_30sec_num',
                   
                   'false_fact_too_easy',
                   'false_fact_too_easy_num',
                   'false_fact_too_hard',
                   'false_fact_too_hard_num',
                   'false_fact_too_hard2',
                   'false_fact_too_hard2_num',
                   'false_fact_30sec_unfair',
                   'false_fact_30sec_unfair_num',
                   'false_fact_30sec_fairplay',
                   'false_fact_30sec_fairplay_num')]

# Sort
df_mini <- df_mini[order(df_mini$treatment, 
                         df_mini$falsefeedback,
                         df_mini$firstfeedback
), ] 