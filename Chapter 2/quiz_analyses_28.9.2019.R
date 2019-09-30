#########################################################
# ESSEXLab Experiments 2018 (Round 2)
# 28 September 2019
# Part 3 -- Analyses
#########################################################

rm(list = ls())
setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/2.SeedcornProject/Data/Round2/") 
library(foreign) # for read.csv
# library(plyr)  # for plyr::rename AND revalue
library(plyr); 
library(dplyr)
library(tidyr)
library(car) # for recode
library(stargazer) 
library(ggplot2)
library(ggthemes)

library(jtools)
library(margins)
library(interplot)

# devtools::install_github("sammo3182/interplot")
# install.packages("sjPlot", dependencies=TRUE)


load("df")
load("df_long")


#########################################################
# Analyses
#########################################################

# First, look at means

df %>% 
  group_by(treatment, falseUnfair) %>%
  summarise(
    meanEdu_unfair = mean(educationRating_unfair, na.rm=T),
    meanRep_unfair = mean(representationRating_unfair, na.rm=T),
    meanAgreed_unfair = mean(agreeFeedback_unfair_num, na.rm=T),
    meanAccurate_unfair = mean(accuracyRating_unfair, na.rm=T))

df %>% 
  group_by(treatment, falseUnfair) %>%
  summarise(
    meanEdu_fairplay = mean(educationRating_fairplay, na.rm=T),
    meanRep_fairplay = mean(representationRating_fairplay, na.rm=T),
    meanAgreed_fairplay = mean(agreeFeedback_fairplay_num, na.rm=T),
    meanAccurate_fairplay = mean(accuracyRating_fairplay, na.rm=T))



#########################################################
# T-Tests
#########################################################

# Hypothesis 1 considers a scenario in which false claims circulate 
# on both sides. 

# Idea: Low-status groups overlook false facts by politicians 
# promising to raise their status but would notice (and punish) 
# the same false facts if these false facts were to come  
# from a politician siding not with them, but with the  
# high-status group. 

# H1a: Team B is more likely to overlook false facts in the 
# 'unfair' feedback than in the 'fair play' feedback. 

# // Those on the disadvantaged team who see a false ’unfair’  
# feedback are more likely to overlook false claims in that (false) ’unfair’ 
# than their peers who see a false ’fair play’ feedback rate that (false) 
# are to overlook the same false claims in the ’fair play’ feedback 


#########################################################
# Team B - false facts on both sides
#########################################################

### General agreement #### 
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 3.108434 
# mean of y 4.639344 
# t = -7.4053, df = 142, p-value = 1.061e-11

### Factually accurate #### 
t.test(subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 51.62651 
# mean of y 65.81967 
# t = -3.451, df = 137.41, p-value = 0.0007424

### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 42.24096
# mean of y 65.80328
# t = -5.0036, df = 134.88, p-value = 1.723e-06

### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 2.279412
# mean of y 2.319149
# t = -0.27274, df = 106.59, p-value = 0.7856


#########################################################
# Team A - false facts on both sides
#########################################################

# H1b: Team A is more likely to overlook false facts in the 
# 'fair play' feedback than in the 'unfair' feedback. 

# // Those on the advantaged team who see a false ’fair play’ feedback 
# rate this (false) ’fair play’ feedback as more ’factually accurate’ 
# than their peers who see a false ’unfair’ feedback rate that (false) 
# ’unfair’ feedback. 

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="unfair"))

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))

### Good Rep #### =)
t.test(subset(df$representationRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 63.89855 
# mean of y 46.67188 
# t = 3.7186, df = 111.54, p-value = 0.0003152

### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="unfair"))



#########################################################
# NEW Hypothesis 1 -- People overlook false claims in their own in-group
#########################################################

# Hypothesis 1 considers the hypothetical scenario in which a person who 
# sides with your team either makes false claims or doesn't: It is
# hypothesized that factual accuracy has no effect on perceived 
# suitability as a team representative.  

# H1a: Among the disadvantaged team, average ratings of the 'unfair' 
# feedback do not differ depending on whether it included a false 
# claim.

# // Among team B, average ratings of the 'unfair' feedback  
# do not differ depending on whether or not it included a false claim.

# // On average, those on Team B who rate an 'unfair' feedback 
# that contains false facts rate this feedback the same way 
# as their peers who rate an 'unfair' feedback that does 
# not contain any false facts

# Expectations... 
# I think people are just as likely to AGREE with a person 
# who lies a bit as they are to agree with a person who is honest. 
# BUT... that I do think they would NOTICE the false claims and 
# I don't think they would BELIEVE them. 

# H1a: Among the disadvantaged team, there is no significant difference 
# between representation ratings of  a false 'unfair' feedback and a  
# correct 'unfair' feedback. 

# H1b: Among the disadvantaged team, there is no significant difference 
# between agreement with a false 'unfair' feedback and a correct 'unfair'
# feedback. 

# H1c: Among the disadvantaged team, there is no significant difference 
# between accuracy ratings of  a false 'unfair' feedback and a correct 
# 'unfair'feedback. 

# H1d: Among the advantaged team, there is no significant difference 
# between representation ratings of a false 'fair play' feedback and a  
# correct 'fair play'feedback. 

# H1e: Among the advantaged team, there is no significant difference 
# between agreement with a false 'fair play' feedback and a correct 
# 'fair play' feedback. 

# H1f: Among the disadvantaged team, there is no significant difference 
# between accuracy ratings of  a false 'fair play' feedback and a correct 
# 'fair play' feedback. 

#########################################################
# Team B - Rating the (correct & incorrect) 'unfair' feedback
#########################################################

### Generally agreed ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x: 4.639344
# mean of y: 4.734940
# t = -0.53783, df = 130.18, p-value = 0.5916

mean(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
mean(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="fairplay"), na.rm = T)


### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x: 65.80328
# mean of y: 68.25301
# t = -0.91031, df = 126.22, p-value = 0.3644

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x: 65.80328
# mean of y: 68.25301
# t = -0.53421, df = 131.07, p-value = 0.5941


#########################################################
# Team A - Rating the (correct & incorrect) 'fair play' feedback
#########################################################

t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x: 4.144928
# mean of y: 4.250000
# t = -0.47782, df = 126.56, p-value = 0.6336

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x: 60.14493
# mean of y: 61.00000
# t = -0.1954, df = 129.91, p-value = 0.8454

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x: 45.39130
# mean of y: 46.67188
# t = -0.2589, df = 123.4, p-value = 0.7961




#########################################################
# NEW Hypothesis 2 -- People DO notice false claims in the out-group
#########################################################

# HIER WEITER -- USE THIS!!!

# Hypothesis 2 considers the hypothetical scenario in which a person who 
# sides with the other team either makes false claims or does 
# not. In this case, it is hypothesized that in-group members will notice 
# (and punish) the false claims in the other camp: 

# H2a: Among the disadvantaged team, the author of the false 'fair 
# play' feedback is seen as an (even) worse team representative than the 
# author of the correct 'fair play' feedback. 

# H2b: Among the disadvantaged team, average agreement with the false 
# 'fair play' feedback is (even) lower than average agreement with the  
# correct 'fair play' feedback. 

# H2c: Among the disadvantaged team, the author of the false 'fair 
# play' feedback is seen as (even) less accurate than the 
# author of the false 'fair play' feedback. 

# H2d: Among the advantaged team, the author of the false 'unfair'
# feedback is seen as an (even) worse team representative than the 
# author of the correct 'unfair' feedback. 

# H2e: Among the advantaged team, average agreement with the false 
# 'unfair' feedback is (even) lower than average agreement with the  
# correct 'unfair' feedback. 

# H2f: Among the advantaged team, the author of the false 'unfair' 
# feedback is seen as (even) less accurate than the author of the 
# false 'unfair' feedback. 

#########################################################
# Team B - Rating the (correct & incorrect) 'fair play' feedback
#########################################################

### Generally agreed ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x: 3.108434 # 3='Slightly Disagree'
# mean of y: 3.655738 # 4='Slightly Agree'
# t = -2.3175, df = 132.16, p-value = 0.02201

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x: 51.62651
# mean of y: 59.39344
# t = -1.8198, df = 133.44, p-value = 0.07103

### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x: 42.24096
# mean of y: 48.00000
# t = -1.2288, df = 135.4, p-value = 0.2213

#########################################################
# Team A - Rating the (correct & incorrect) 'unfair' feedback
#########################################################

### Generally agreed ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x: 4.03125 # 3='Slightly Agree'
# mean of y: 4.00000 # 4='Slightly Agree'
# t = 0.14634, df = 129.61, p-value = 0.8839

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x: 61.00000
# mean of y: 60.14493
# t = 0.1954, df = 129.91, p-value = 0.8454

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x: 46.67188
# mean of y: 45.39130
# t = 0.2589, df = 123.4, p-value = 0.7961


#########################################################
# NEW Hypothesis 3
#########################################################

#########################################################
# H3a: On average, Team B members who are exposed to factually 
# incorrect claims in the 'unfair' feedback are more supportive 
# of the (factually inaccurate) 'unfair' feedback as compared with 
# the (factually accurate) 'fair play' feedback. 
#########################################################

# H3a: Members of the disadvantaged team who see a false 'unfair' 
# feedback and a correct 'fair play' feedback rate the author of the 
# (false) 'unfair' feedback as a better team representative than the 
# author of the (correct) 'fair play' feedback. 

# H3b: Members of the disadvantaged team who see a false 'unfair' 
# feedback and a correct 'fair play' feedback are more agreed with the 
# (false) 'unfair' feedback than the (correct) 'fair play' feedback. 

# H3c: Members of the disadvantaged team who see a false 'unfair' 
# feedback and a correct 'fair play' feedback rate the (false) 'unfair' 
# feedback as more accurate than the (correct) 'fair play' feedback. 


# H3d: Members of the advantaged team who see a false 'fair play' 
# feedback and a correct 'unfair' feedback rate the author of the 
# (false) 'fair play' feedback as a better team representative than the 
# author of the (correct) 'fair play' feedback. 

# H3e: Members of the advantaged team who see a false 'fair play' 
# feedback and a correct 'unfair' feedback are more agreed with the 
# (false) 'fair play' feedback than the (correct) 'unfair' feedback. 

# H3f: Members of the advantaged team who see a false 'fair play' 
# feedback and a correct 'unfair' feedback rate the (false) 'fair play' 
# feedback as more accurate than the (correct) 'unfair' feedback. 


#########################################################
# Team B 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 4.639344 # 5='Agree'
# mean of y 3.655738 # 4='Slightly Agree'
# t = 4.4373, df = 112.11, p-value = 2.14e-05

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 59.39344
# mean of y 65.81967
# t = -1.4881, df = 119.42, p-value = 0.1394

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 65.80328 
# mean of y 48.00000
# t = 3.6677, df = 119.99, p-value = 0.0003662 # EDITED!!

### Belief in false facts ####
# Looking at one DV only, i.e. no t-test
mean(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)



#########################################################
# Team A 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 4.144928
# mean of y 4.000000
# t = 0.7081, df = 135.96, p-value = 0.4801

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 59.31884
# mean of y 60.14493
# t = -0.20006, df = 135.46, p-value = 0.8417

### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 63.89855
# mean of y 45.39130
# t = 4.5804, df = 131.54, p-value = 1.064e-05

### Belief in false facts ####
# Looking at one DV only, i.e. no t-test
mean(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), na.rm = T)
# mean=2.053571
# sd=0.7958904



#########################################################
# H4: Belief in false claims
# H5a: Team B overlooks false claims in the 
# 'unfair' feedback more than they overlook false 
# claims in the 'fair play' feedback
#########################################################

#########################################################
# Team B 
#########################################################

# Here, it really only makes sense to use the accuracy
# ratings: Does Team B think the false unfair feedback 
# is truer than the false fair play feedback?

### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 4.639344v # 5='Agree'
# mean of y 3.108434 # 4='Slightly Agree'
# t = 7.4053, df = 142, p-value = 1.061e-11

### Factually accurate #### -- USE THIS ONLY
t.test(subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 65.81967
# mean of y 51.62651
# t = 3.451, df = 137.41, p-value = 0.0007424


### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"),
       subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 65.80328 
# mean of y 42.24096
# t = 5.0036, df = 134.88, p-value = 1.723e-06

### Belief in false facts ####

# Looking at one DV only, i.e. no t-test
mean(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)

mean(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"), na.rm = T)


#########################################################
# Team A 
#########################################################

# Here, it really only makes sense to use the accuracy
# ratings: Does Team A think the false fairplay feedback 
# is truer than the false unfair feedback?

### Factually accurate #### -- USE THIS ONLY
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"),
       subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 61.00000
# mean of y 61.00000
# t = 0.39549, df = 127.95, p-value = 0.6931



#########################################################
# H: Belief in false claims
# Disadvantaged team members who see the false claim as 
# part of the 'unfair' feedback are more likely to believe in it than 
# their peers who see it as part of the 'fair play' feedback. 
#########################################################

t.test(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 2.319149
# mean of y 2.279412
# t = 0.27274, df = 106.59, p-value = 0.7856


#########################################################
# Advantaged team members who see the false claim as 
# part of the 'fair play' feedback are more likely to believe in it than 
# their peers who see it as part of the 'unfair' feedback. 
#########################################################

t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 2.053571
# mean of y 2.089286
# t = -0.24144, df = 109.87, p-value = 0.8097


# Pooled -- not any better 

df$MRnudged <- ifelse(df$treatment=="B" & df$falsefeedback=="unfair" |
                        df$treatment=="A" & df$falsefeedback=="fairplay", 
                      1, 0)

t.test(df$false_fact_too_hard2_num ~ 
         df$MRnudged)


table(df$false_fact_too_hard2, df$treatment)
round(prop.table(table(df$treatment, df$false_fact_too_hard2),1),2)


#########################################################
# H5a: False facts in the 'unfair' treatment are more likely to 
# be tolerated by Team B than by Team A. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"))

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))

### Belief in false facts ####
sum(is.na(df$false_fact_too_hard2_num)) # 50 missings - ARGH
sum(complete.cases(df$false_fact_too_hard2_num)) # 159 

t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"))


#########################################################
# H5b: False facts in the 'fairplay' treatment are more 
# likely to be tolerated by Team A than by Team B. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="fairplay"))

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="fairplay"))

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="fairplay"))

### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"))

#########################################################
# # Bonferroni Adjustment
#########################################################

# TO DO -- Look into this! 

# pairwise.t.test(df$false_fact_too_hard2_num, df$military_conditions, p.adj="bonferroni")

df$groups <- 99
df$groups[df$treatment == "B" & df$falsefeedback=="unfair"] <- 1
df$groups[df$treatment == "A" & df$falsefeedback=="unfair"] <- 2
df$groups[df$treatment == "B" & df$falsefeedback=="fairplay"] <- 3
df$groups[df$treatment == "A" & df$falsefeedback=="fairplay"] <- 4


# Creating groups for Bonferroni-adjusted t-tests
# PROBLEM: Different DVs!! 
tB_fUF_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="B" & df$falsefeedback=="unfair")
tB_fUF_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="B" & df$falsefeedback=="unfair")

tB_fFP_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="B" & df$falsefeedback=="fairplay")
tB_fFP_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="B" & df$falsefeedback=="fairplay")

tA_fUF_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="A" & df$falsefeedback=="unfair")
tA_fUF_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="A" & df$falsefeedback=="unfair")

tA_fFP_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="A" & df$falsefeedback=="fairplay")
tA_fFP_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="A" & df$falsefeedback=="fairplay")


df$groups[df$treatment == "B" & df$falsefeedback=="unfair"] <- 1
df$groups[df$treatment == "A" & df$falsefeedback=="unfair"] <- 2
df$groups[df$treatment == "B" & df$falsefeedback=="fairplay"] <- 3
df$groups[df$treatment == "A" & df$falsefeedback=="fairplay"] <- 4



#########################################################
# Create dummie variables
#########################################################

### generally agreed dummy #### 

# hist(df$agreeFeedback_fairplay_num)
# hist(df$agreeFeedback_unfair_num)

df$genAgreeFairplay <- ifelse(df$agreeFeedback_fairplay == 'Strongly agree' | 
                                df$agreeFeedback_fairplay == 'Agree' | 
                                df$agreeFeedback_fairplay == 'Slightly agree', 
                              1, 0)
table(df$genAgreeFairplay, df$treatment)

df$genAgreeUnfair <- ifelse(df$agreeFeedback_unfair == 'Strongly agree' | 
                              df$agreeFeedback_unfair == 'Agree' | 
                              df$agreeFeedback_unfair == 'Slightly agree', 
                            1, 0)
table(df$genAgreeUnfair, df$treatment)

### accuracy dummies #### 

df$accurateFairplay <- ifelse(df$accuracyRating_fairplay > 50, 1, 0)
table(df$accurateFairplay, df$treatment)

df$accurateUnfair <- ifelse(df$accuracyRating_unfair > 50, 1, 0)
table(df$accurateUnfair, df$treatment)


### goodRep dummies ####

# hist(df$representationRating_fairplay)
# hist(df$representationRating_unfair)

df$goodRepFairplay <- ifelse(df$representationRating_fairplay > 50, 1, 0)
table(df$goodRepFairplay, df$treatment)

df$goodRepUnfair <- ifelse(df$representationRating_unfair > 50, 1, 0)
table(df$goodRepUnfair, df$treatment)

### gullible dummies #### 

df$dontExist <- ifelse(df$false_fact_too_hard2 == "Definitely False" | 
                         df$false_fact_too_hard2 == "Probably False", 1, 0)




#########################################################
# Correlation matrices
#########################################################

library("Hmisc")

df_cor <- df[ , c("teamB", # dummy 
                  # "falseUnfair", # dummy 
                  # "difPayoffs", 
                  # "participant.payoff", # obviously correlated with teamB
                  # "share_hardQs", # obviously correlated with teamB/payoff
                  # "byChanceTeamAEasierQs", # "luckOfTheDraw", -- OK with teamB
                  # "legitTeamAhigherPayoffs_no" # -- OK with teamB
                  "feedbackMakesDifference_num"  # p=0.08 with teamB
                  # "triviaPerson", 
                  # "female",
                  # "income", 
                  # "angry_num"#, # correlated with payoff
                  # "failMP"
                  )]
str(df_cor)

# baseR -- no p-values
correlation_table <- round(
  cor(df_cor, use = "complete.obs")
  , 2) 

stargazer(correlation_table, 
          title = "Correlation Matrix",
          type="text",
          notes = "This is a two by two Matrix") 


# Hmisc -- Correlation matrix with significance levels (p-value)
library("Hmisc")

correlation_table <- rcorr(as.matrix(df_cor))
correlation_table

# https://rstudio-pubs-static.s3.amazonaws.com/240657_5157ff98e8204c358b2118fa69162e18.html
install.packages('tidyverse',
                 dependencies=TRUE, 
                 repos='http://cran.rstudio.com/')

# install.packages('jtools',
#                  dependencies=TRUE, repos='http://cran.rstudio.com/')


# Check correlations 

# Team B & ... 

# ... identifying with their team -- -0.28
round(cor(df$teamB, df$identification_num, use = "complete.obs"), 2) 

# ... anger -- 0.25
round(cor(df$teamB, df$angry_num, use = "complete.obs"), 2) 

# ... luck of the draw -- # 0.18
round(cor(df$teamB, df$byChanceTeamAEasierQs, use = "complete.obs"), 2) 

# ... thinking the feedback will be heard # -0.1 p=0.0843
round(cor(df$teamB, df$feedbackMakesDifference_num, use = "complete.obs"), 2)

# ... thinking the game is legit
round(cor(df$teamB, df$legitTeamAhigherPayoffs_no, use = "complete.obs"), 2) 

# ... failing the manipulation check
round(cor(df$teamB, df$failMP, use = "complete.obs"), 2) 

# income & age -- 0.49
cor(df$income, df$age, use = "complete.obs")

# luck of the draw & feeling the game is rigged -- # 0.24 (dugh)
round(cor(df$byChanceTeamAEasierQs, df$expectationsLastQ_TeamA, use = "complete.obs"), 2) 

# -0.09
round(cor(df$feedbackMakesDifference_num, df$legitTeamAhigherPayoffs_no, use = "complete.obs"), 2) 

# identification, legit
round(cor(df$feedbackMakesDifference_num, df$identification_num, use = "complete.obs"), 2) 

# identification, legit
round(cor(df$legitTeamAhigherPayoffs_no, df$identification_num, use = "complete.obs"), 2) 


#########################################################
# 1) OLS -- Does factual accuracy affect perceived 
# suitability as a team rep / perceived accuracy? 
#########################################################

# First, I am looking at how both feedback givers are rated:
# Does making a false claim disqualify people from being seen
# as accurate / good reps? 
# --> DV = agreement / accuracy / good rep for BOTH candidates

# cf with t-test: lying and not lying unfair // fair play

# df$byChanceTeamAEasierQs # == luckOfTheDraw
# 0 = Definitely by chance
# 100 = No way this happened by chance

#########################################################
# OLS1 -- unfair -- goodRep
#########################################################

# Stuff i added after 99 obs
df$identification_num # Graph 1 = outside the team. Graph 7 = totally within the team
df$angry_num

# Did they notice it was unfair
df$expectationsLastQ_TeamA
df$whoGotHigherPayoff

# Legit? 
df$legitTeamAhigherPayoffs

# Change?
df$feedbackMakesDifference


# Trials

ols1_unfair_goodRep <-lm( 
    representationRating_unfair ~ # myGuyGoodRep
    teamB + # treatment + 
    # angry_num +
    # identification_num + 
    falseUnfair + # falsefeedback +
    difPayoffs +
    expectationsLastQ_TeamA + # correlated with byChanceTeamAEasierQs
    # byChanceTeamAEasierQs + # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    # feedbackMakesDifference_num +
    legitTeamAhigherPayoffs_no + # ok
    # triviaPerson + 
    failMP + 
    female +
    income + # correlated with age
    # teamB * identification_num + 
    teamB * falseUnfair, 
    # teamB * feedbackMakesDifference_num, 
    # teamB * legitTeamAhigherPayoffs_no, 
    # teamB * byChanceTeamAEasierQs, 
    # teamB * falseUnfair * identification_num + 
    # teamB * falseUnfair * falseUnfair +
    # teamB * falseUnfair * feedbackMakesDifference_num,
    # teamB * falseUnfair * legitTeamAhigherPayoffs_no, 
    # teamB * falseUnfair * feedbackMakesDifference_num, 
    # teamB * falseUnfair * identification_num, 
    data = df)
summary(ols1_unfair_goodRep)


stargazer(ols1_unfair_goodRep,
          type="text",
          omit.stat=c("LL","ser","f"),
          no.space=TRUE)

# Check for multicollinearity
car::vif(ols1_unfair_goodRep)


ols1_unfair_goodRep <-lm( 
  representationRating_unfair ~
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    # feedbackMakesDifference_num + # deleted
    legitTeamAhigherPayoffs_no +  # added
    # triviaPerson + 
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_unfair_goodRep)

my_reg = 
  function(df, dv){
    lm( 
      dv ~
      teamB + 
      falseUnfair + 
      difPayoffs +
      byChanceTeamAEasierQs + 
      legitTeamAhigherPayoffs_no +  
      failMP + 
      female +
      income + 
      teamB*falseUnfair,
    data = df)
  }

ols1_unfair_agreed <- my_reg(df, df$agreeFeedback_unfair_num) # fill in
summary(ols1_unfair_agreed)
car::vif(ols1_unfair_agreed)

ols1_unfair_accurate <- my_reg(df, df$accuracyRating_unfair) # fill in
summary(ols1_unfair_accurate)
car::vif(ols1_unfair_accurate)

ols1_unfair_goodRep <- my_reg(df, df$representationRating_unfair) # fill in
summary(ols1_unfair_goodRep)
car::vif(ols1_unfair_goodRep)


ols1_fairplay_agreed <- my_reg(df, df$agreeFeedback_fairplay_num) # fill in
summary(ols1_fairplay_agreed)
car::vif(ols1_fairplay_agreed)

ols1_fairplay_accurate <- my_reg(df, df$accuracyRating_fairplay) # fill in
summary(ols1_fairplay_accurate)
car::vif(ols1_fairplay_accurate)

ols1_fairplay_goodRep <- my_reg(df, df$representationRating_fairplay) # fill in
summary(ols1_fairplay_goodRep)
car::vif(ols1_fairplay_goodRep)


# Export 4 models to Latex
stargazer(ols1_unfair_agreed,
          ols1_unfair_accurate, 
          ols1_unfair_goodRep,
          ols1_fairplay_agreed,
          ols1_fairplay_accurate,
          ols1_fairplay_goodRep,
          type="text", # for latex
          title="OLS Results: 'This person is a good representative of my team. // The points this person makes are factually accurate.'", 
          align=TRUE,  
          # dep.var.labels=c("'unfair': agreed", 
          #                  "'unfair': accurate", 
          #                  "'unfair': good rep", 
          #                  "'fair play': agreed",
          #                  "'fair play': accurate",
          #                  "'fair play': good rep"), 
          covariate.labels=c(
            "Disadvantaged", # team
            "False unfair", # False claim in unfair feedback
            "Difference in payoffs", # 2 teams' 
            "Luck of the draw", 
            "Payoffs not legitimate", # Belief that payoffs will be topped up
            "Failed manipulation check", 
            "Female",
            "Income",
            "Disadvantaged * False unfair"), 
          omit.stat=c("LL","ser","f"), 
          font.size = "small",
          column.sep.width = "-5pt", 
          no.space=TRUE)

# Problem: Stargazer doesn't name the 6 models (only 'dv')


#########################################################
# Function for often-used graphs
#########################################################

my_bar_graph = 
  function(df, group, fill, fill_title="") # 3 things to fill in
    {
  df = df[!is.na(df[[fill]]) & !is.na(df[[group]]),] # remove missings
  ggplot(data=df, 
         aes_string(group, 
                    fill=fill)) +
    geom_bar(position="fill") + 
    coord_flip() +
    scale_y_continuous(labels = percent) + 
    ylab("") + 
    xlab("") +
    guides(fill=guide_legend(title=fill_title))
  }
# my_sideplot(df, "age", "region", "Regions") # fill in

# http://reganmian.net/blog/2014/10/14/starting-data-analysiswrangling-with-r-things-i-wish-id-been-told/ 
# http://www.martinschweinberger.de/blog/writing-functions-in-r-a-practical-example-creating-a-customized-output-table-for-a-simple-linear-regression/
#########################################################

# stargazer(ols1_unfair_goodRep, 
#           type="text",
#           omit.stat=c("LL","ser","f"), 
#           no.space=TRUE)


#########################################################
# OLS1 -- unfair -- agreed
#########################################################

ols1_unfair_agreed <-lm( 
  agreeFeedback_unfair_num ~ 
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_unfair_agreed)

#########################################################
# OLS1 -- unfair -- accurate
#########################################################

ols1_unfair_accurate <-lm( 
  accuracyRating_unfair ~
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_unfair_accurate)

#########################################################
# OLS1 -- unfair -- goodRep
#########################################################

ols1_unfair_goodRep <-lm( 
  representationRating_unfair ~
    teamB + 
    falseUnfair +
    difPayoffs +
    byChanceTeamAEasierQs +
    legitTeamAhigherPayoffs_no +
    failMP +
    female +
    income +
    teamB*falseUnfair,
  data = df)
summary(ols1_unfair_goodRep)


#########################################################
# OLS1 -- fair play -- educated
#########################################################

ols1_fairplay_educated <-lm( 
  educationRating_fairplay ~ 
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_fairplay_educated)


#########################################################
# OLS1 -- fair play -- agreed
#########################################################

ols1_fairplay_agreed <-lm( 
  agreeFeedback_fairplay_num ~ 
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_fairplay_agreed)


#########################################################
# OLS1 -- fairplay -- accurate
#########################################################

ols1_fairplay_accurate <-lm( 
  accuracyRating_fairplay ~ 
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_fairplay_accurate)

#########################################################
# OLS1 -- fairplay -- goodRep
#########################################################

ols1_fairplay_goodRep <-lm( 
  representationRating_fairplay ~ 
    teamB + 
    falseUnfair + 
    difPayoffs +
    byChanceTeamAEasierQs + 
    legitTeamAhigherPayoffs_no +  
    failMP + 
    female +
    income + 
    teamB*falseUnfair,
  data = df)
summary(ols1_fairplay_goodRep)


#########################################################
# Marginal effects & interaction terms
#########################################################

#########################################################
# Marginal effects -- using jtools
#########################################################

# NEW -- using jtools
# Following https://cran.r-project.org/web/packages/jtools/readme/README.html
# and https://cran.r-project.org/web/packages/jtools/jtools.pdf
library(jtools)

coef_names <- c(  "Constant" = "(Intercept)",
                  "Disadvantaged" = "teamB",
                  "Contained false claims" = "falseUnfair",
                  "Difference in payoffs" = "difPayoffs",
                  "Belief in luck of the draw" = "byChanceTeamAEasierQs",
                  "Different payoffs not OK" = "legitTeamAhigherPayoffs_no",
                  "Failed manipulation check" = "failMP",
                  "Female" = "female",
                  "Income" = "income",
                  "Disadvantaged * Contained false claims" = "teamB:falseUnfair")           

coef_names <- coef_names[2:10] # omitting constant

library("jtools")
# Got rid of error using this from https://stackoverflow.com/questions/20155581/persistent-invalid-graphics-state-error-when-using-ggplot2
dev.off()
# plot_summs(ols1_unfair_goodRep)

plot_summs_unfair <- 
plot_summs(ols1_unfair_accurate,
           ols1_unfair_goodRep,
           coefs = coef_names, 
           model.names = c("DV = Factually accurate?", 
                           "DV = Good representative?"),
           # plot.distributions = TRUE, 
           scale = TRUE, 
           robust = TRUE
           )

print(plot_summs_unfair)
ggsave("images/plot_summs_unfair.png")


plot_summs_fairplay <- 
  plot_summs(ols1_fairplay_accurate,
             ols1_fairplay_goodRep,
             coefs = coef_names, 
             model.names = c("DV = Factually accurate?", 
                             "DV = Good representative?"),
             # plot.distributions = TRUE, 
             scale = TRUE, 
             robust = TRUE
  )

print(plot_summs_fairplay)
ggsave("images/plot_summs_fairplay.png")


plot_summs(ols1_unfair_agreed,
           coefs = coef_names, 
           model.names = c("DV = Agree?"),
           plot.distributions = TRUE, 
           scale = TRUE, 
           robust = TRUE
)




#########################################################
# Marginal effects -- using margins
#########################################################

# https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html#interactions_in_ols
# Look at interaction term
library("margins")

#  average marginal effects (AMEs)
(ME_unfair_goodRep <- margins(ols1_unfair_goodRep))
summary(ME_unfair_goodRep)
plot(ME_unfair_goodRep)
plot(ME_unfair_goodRep,
     main="DV: Agreement that the 'unfair' person is a 'good representative of my team.'",
     # sub="DV: Agreement that the 'unfair' person is a 'good representative of my team.'",
     labels=c(
       "Luck of \nthe draw", # of draw
       "Difference \nin payoffs", # different payoffs 2 teams' (average payoffs A-B)
       "Failed \nman. check", 
       "Contained \nfalse claim", # False claim in unfair feedback
       "Female", 
       "Income",
       "Different \npayoffs not ok", 
       "Team B" 
     )# ,
     # cex.main=0.9, 
     # cex.lab=0.9 
     # cex.axis=0.7
     )


#########################################################
# Marginal effects -- unfair -- accurate
#########################################################

(ME_unfair_accurate <- margins(ols1_unfair_accurate))
summary(ME_unfair_accurate)
plot(ME_unfair_accurate)
plot(ME_unfair_accurate,
     main="DV: Agreement that the points the 'unfair' person makes are 'factually accurate'",
     # sub="DV: Agreement that the 'unfair' person is a 'good representative of my team.'",
     labels=c(
       "Luck", # of draw
       "!= payoffs", # different payoffs 2 teams'
       "Failed MC", 
       "False", # False claim in unfair feedback
       "Female", 
       "Income",
       "Not legit", 
       "Disadvantaged" # 
     )# ,
     # cex.main=0.9, 
     # cex.lab=0.9 
     # cex.axis=0.7
)

# not working
ggsave("images/ME_unfair_accurate.png", 
       dpi = 600, width = 9, height = 7)

#########################################################
# Marginal effects -- unfair -- agreed
#########################################################

(ME_unfair_agreed <- margins(ols1_unfair_agreed))
summary(ME_unfair_agreed)
plot(ME_unfair_agreed)
plot(ME_unfair_agreed,
     main="DV: General agreement with the 'unfair' person",
     # sub="DV: Agreement that the 'unfair' person is a 'good representative of my team.'",
     labels=c(
       "Luck", # of draw
       "!= payoffs", # different payoffs 2 teams'
       "Failed MC", 
       "False", # False claim in unfair feedback
       "Female", 
       "Income",
       "Not legit", 
       "Disadvantaged" # 
     ) #,
     # cex.main=0.9, 
     # cex.lab=0.9 
     # cex.axis=0.7
)

# not working
ggsave("images/ME_unfair_agreed.png", 
       dpi = 600, width = 9, height = 7)


# By default, margins() will supply the average marginal 
# effects of the constituent variables in the model. ...
# Because there is a significant interaction, we can see 
# this by examining margins at different levels of the 
# constituent variables. 

margins(ols1_unfair_goodRep, 
        at = list(teamB = range(df$teamB)))

## For Team A: 9.816. If inaccurate --> better team rep? 
## For Team B: -5.859. If inaccurate --> slightly worse but n.s.

# Plot the interaction term
# https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html 

# To plot conditional coefficients, a user needs to provide 
# only three basic pieces of information: the object of a 
# regression result (m), the variable whose coefficient is 
# to be plotted (var1), and the variable on which the 
# coefficient is conditional (var2). 

# The plot clearly shows that as we move from Team A (0) to 
# Team B (1) (along the x axis), the magnitude of the coefficient
# of the falsehood on representation ratings decreases 
# (along the y axis). 

library(ggthemes)


#########################################################
# Interaction plots -- unfair
#########################################################

ols1_unfair_agreed_interaction <- 
  interplot(m = ols1_unfair_agreed,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on general agreement",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'unfair' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_unfair_agreed_interaction)
ggsave("images/ols1_unfair_agreed_interaction.png")


ols1_unfair_accurate_interaction <- 
  interplot(m = ols1_unfair_accurate,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on perceived accuracy",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'unfair' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_unfair_accurate_interaction)
ggsave("images/ols1_unfair_accurate_interaction.png")


ols1_unfair_goodRep_interaction <- 
  interplot(m = ols1_unfair_goodRep,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on perceived suitability as a team representative",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'unfair' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_unfair_goodRep_interaction)
ggsave("images/ols1_unfair_goodRep_interaction.png")

# margins(ols1_unfair_goodRep, 
#         at = list(teamB = range(df$teamB)))
# 
# Team A 8.461
# Team B -5.160

#########################################################
# Interaction plots -- fairplay
#########################################################

ols1_fairplay_agreed_interaction <- 
  interplot(m = ols1_fairplay_agreed,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on general agreement",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'fairplay' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_fairplay_agreed_interaction)
ggsave("images/ols1_fairplay_agreed_interaction.png")


ols1_fairplay_accurate_interaction <- 
  interplot(m = ols1_fairplay_accurate,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on perceived accuracy",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'fairplay' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_fairplay_accurate_interaction)
ggsave("images/ols1_fairplay_accurate_interaction.png")


ols1_fairplay_goodRep_interaction <- 
  interplot(m = ols1_fairplay_goodRep,
            var1 = "falseUnfair", # 
            var2 = "teamB") +
  xlab("Team B") + 
  ylab("Estimated coefficient") + 
  ggtitle("Estimated coefficient of factual inaccuracy on perceived suitability as a team representative",
          subtitle ="Advantaged (=0) and disadvantaged team (=1) rating the author of the 'fairplay' feedback") +
  theme_hc() +
  geom_hline(yintercept = 0, linetype = "dashed")

print(ols1_fairplay_goodRep_interaction)
ggsave("images/ols1_fairplay_goodRep_interaction.png")


#########################################################
# Export 6 models to Latex
#########################################################

library(stargazer)

stargazer(ols1_unfair_agreed,
          ols1_unfair_accurate, 
          ols1_unfair_goodRep,
          ols1_fairplay_agreed,
          ols1_fairplay_accurate,
          ols1_fairplay_goodRep,
          # type="text", # for latex
          title="OLS Results: 'This person is a good representative of my team. // The points this person makes are factually accurate.'", 
          label="table: OLS Regression Results",
          align=TRUE, 
          dep.var.labels=c("'unfair': agreed", 
                           "'unfair': accurate", 
                           "'unfair': good rep", 
                           "'fair play': agreed",
                           "'fair play': accurate",
                           "'fair play': good rep"), 
          covariate.labels=c(
            "Disadvantaged", # team
            "False unfair", # False claim in unfair feedback
            "Difference in payoffs", # 2 teams' 
            "Luck of the draw", 
            "Payoffs not legitimate", # Belief that payoffs will be topped up
            "Failed attention check", 
            "Female",
            "Income",
            "Disadvantaged * False unfair"), 
          omit.stat=c("LL","ser","f"), 
          font.size = "small",
          column.sep.width = "-5pt", 
          no.space=TRUE)



# Fazit: 
# False claims = insignificant = have no effect 
# on how people rate each feedback giver as a 
# team rep

#########################################################
# 2 Zoom in on group 4
#########################################################

# Things to include in the group 1/4 models: 
# Theory: 
# if you identify with your team : better ratings, more blind to false claims 
# falseUnfair * identification_num
# if you believe you have a chance of moving up: ...
# falseUnfair * feedbackMakesDifference_num
# if you believe this is illegitimate: ...
# falseUnfair * legitTeamAhigherPayoffs_no

#########################################################
# Team B -- false facts in 'unfair'
#########################################################

ols_group4_goodRep_smallN <-lm( 
  representationRating_unfair ~ 
  # false_fact_too_hard2_num ~ 
    difPayoffs + # share_hardQs + 
    identification_num + # Missings
    legitTeamAhigherPayoffs_no + # perceived legitimacy
    # feedbackMakesDifference_num +
    expectationsLastQ_TeamA + # know they were disadvantaged
    byChanceTeamAEasierQs + # sign! # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    triviaPerson + 
    failMP + 
    female +
    angry_num + # Missings
    extraversion_avg +
    openness_avg +
    agreeableness_avg +
    conscientiousness_avg +
    neuroticism_avg,
    # angry_num * neuroticism_avg , 
  data = subset(df, falsefeedback == "unfair" &
                  treatment=="B"))
summary(ols_group4_goodRep_smallN)

temp$legitTeamAhigherPayoffs_no # 3 missings
temp$angry_num # lots of missings


# How did people rate the false unfair feedback?

ols_B_goodRep <-lm(
  representationRating_unfair ~ 
    # false_fact_too_hard2_num ~ 
    teamB + 
    difPayoffs + # share_hardQs + 
    identification_num + # Missings
    legitTeamAhigherPayoffs_no + # perceived legitimacy
    # feedbackMakesDifference_num +
    expectationsLastQ_TeamA + # know they were disadvantaged
    # byChanceTeamAEasierQs + # sign! # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    triviaPerson + 
    failMP + 
    female +
    angry_num + # Missings
    extraversion_avg +
    openness_avg +
    agreeableness_avg +
    conscientiousness_avg +
    neuroticism_avg + 
    teamB*identification_num +
    teamB*legitTeamAhigherPayoffs_no + 
    teamB*feedbackMakesDifference_num + 
    # teamB*byChanceTeamAEasierQs +
    teamB*expectationsLastQ_TeamA + 
    teamB*legitTeamAhigherPayoffs_no, 
  # angry_num * neuroticism_avg , 
  data = subset(df, falsefeedback == "unfair"))
summary(ols_B_goodRep)


  falseUnfair*angry_num +
  falseUnfair*falseUnfair + # ARGH N.S.!!
  falseUnfair*byChanceTeamAEasierQs +
  falseUnfair*legitTeamAhigherPayoffs_no +
  falseUnfair*neuroticism_avg

summary(ols2_false_unfair_goodRep)
car::vif(ols2_false_unfair_goodRep)

df$believeLie <- ifelse(df$false_fact_too_hard2=="Probably True" |
                          df$false_fact_too_hard2=="Definitely True",
                        1, 0)
table(df$believeLie)


ols_group4_believeLies_smallN <- 
  glm(believeLie ~ 
                # teamB + 
                # falseUnfair + 
                difPayoffs + 
                identification_num + # missings! # identify with team
                byChanceTeamAEasierQs + # know they are disadvantaged
                legitTeamAhigherPayoffs_no + # thinks its not legit
                feedbackMakesDifference_num + # think they wont get more
                angry_num + # are angry
                extraversion_avg +
                openness_avg +
                agreeableness_avg +
                conscientiousness_avg +
                neuroticism_avg +
                triviaPerson + 
                failMP + 
                female, 
                # income +
                # falseUnfair*angry_num +
                # falseUnfair*falseUnfair + # ARGH N.S.!!
                # falseUnfair*byChanceTeamAEasierQs +
                # falseUnfair*legitTeamAhigherPayoffs_no +
                # falseUnfair*neuroticism_avg
              data = subset(df, falsefeedback == "unfair" &
                              treatment=="B"), 
              family = "binomial")
summary(ols_group4_believeLies)






ols1_unfair_goodRep <-lm(
  representationRating_unfair ~
    falseUnfair + 
    angry_num +
    difPayoffs +
    # share_hardQs + 
    expectationsLastQ_TeamA + # correlated with byChanceTeamAEasierQs
    # byChanceTeamAEasierQs + # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    identification_num + 
    # feedbackMakesDifference_num +
    legitTeamAhigherPayoffs_no + # ok
    triviaPerson + 
    failMP + 
    female +
    income + # correlated with age
    # falseUnfair * share_hardQs + 
    # falseUnfair * angry_num +
    falseUnfair * identification_num + 
    # falseUnfair * feedbackMakesDifference_num +
    falseUnfair * legitTeamAhigherPayoffs_no, 
  data = subset(df, treatment=="B"))

summary(ols1_unfair_goodRep)
car::vif(ols1_unfair_goodRep)

# teamB * byChanceTeamAEasierQs, 
# teamB * falseUnfair * identification_num + 
# teamB * falseUnfair * falseUnfair +
# teamB * falseUnfair * feedbackMakesDifference_num,
# teamB * falseUnfair * legitTeamAhigherPayoffs_no, 
# teamB * falseUnfair * feedbackMakesDifference_num, 
# teamB * falseUnfair * identification_num, 


# temp <- df[ , c(
#   "representationRating_unfair", "representationRating_fairplay", 
#   "accuracyRating_unfair", "accuracyRating_fairplay") ]

# https://www.r-statistics.com/tag/stargazer/



#########################################################
# Team B
#########################################################

# --> 3 columns only: agree with my guy / my guy is right 
# my guy is a good rep 
# need: Dummy (same team), 
# leave out because the 2 groups are too different. 

ols2_teamB <- lm(false_fact_too_hard2_num ~ 
                falseUnfair + 
                difPayoffs +
                # identification_num + # identify with team
                byChanceTeamAEasierQs + # know they are disadvantaged
                legitTeamAhigherPayoffs_no + # thinks its not legit
                feedbackMakesDifference_num + # think they wont get more
                angry_num + # are angry
                neuroticism_avg +
                triviaPerson + 
                failMP + 
                female +
                income +
                falseUnfair*angry_num +
                # falseUnfair*identification_num + 
                falseUnfair*byChanceTeamAEasierQs +
                falseUnfair*legitTeamAhigherPayoffs_no +
                falseUnfair*neuroticism_avg,
              data = subset(df, treatment=="B"))
summary(ols2_teamB)

# horrible r squared!

# extraversion_avg +
# openness_avg +
# agreeableness_avg +
# conscientiousness_avg +

#########################################################
# 2) Logistic regression -- What makes you believe in a 
# person who lies?
# He is on my team. 
#########################################################

df$believeLie <- ifelse(df$false_fact_too_hard2=="Probably True" |
                        df$false_fact_too_hard2=="Definitely True",
                        1, 0)
table(df$believeLie)

# Who believed in the false claim? Team B? Team B only if 
# it was the unfair person saying it? Does it matter who said it? 
# Hope teamB * falseUnfair are significant!

logit1 <- glm(believeLie ~ 
                # teamB + 
                falseUnfair + 
                difPayoffs +
                identification_num + # identify with team
                byChanceTeamAEasierQs + # know they are disadvantaged
                legitTeamAhigherPayoffs_no + # thinks its not legit
                feedbackMakesDifference_num + # think they wont get more
                angry_num + # are angry
                # extraversion_avg +
                # openness_avg +
                # agreeableness_avg +
                # conscientiousness_avg +
                neuroticism_avg +
                triviaPerson + 
                failMP + 
                female +
                income +
                falseUnfair*angry_num +
                falseUnfair*falseUnfair + # ARGH N.S.!!
                falseUnfair*byChanceTeamAEasierQs +
                falseUnfair*legitTeamAhigherPayoffs_no +
                falseUnfair*neuroticism_avg,
              data = subset(df, treatment=="B"), 
              family = "binomial")
summary(logit1)


# Within the disadvantaged who were nudged to be motivated 
# reasoners (i.e. false unfair feedback): Who believed in the 
# false claim?
logit1 <- glm(believeLie ~ 
                # teamB + 
                # falseUnfair + 
                difPayoffs +
                identification_num + # identify with team
                byChanceTeamAEasierQs + # know they are disadvantaged
                legitTeamAhigherPayoffs_no + # thinks its not legit
                feedbackMakesDifference_num + # think they wont get more
                angry_num + # are angry
                # extraversion_avg +
                # openness_avg +
                agreeableness_avg +
                conscientiousness_avg +
                neuroticism_avg +
                triviaPerson + 
                failMP + 
                female +
                income, 
                # teamB*falseUnfair, 
              data = subset(df, falsefeedback=="unfair" &
                              treatment=="B"), 
              family = "binomial")
summary(logit1)

# Within the advantaged who were nudged to be motivated 
# reasoners (i.e. false fairplay feedback): Who believed in the 
# false claim? 
logit2 <- glm(believeLie ~ 
                # teamB + 
                # falseUnfair + 
                difPayoffs +
                identification_num + # identify with team
                byChanceTeamAEasierQs + # know they are disadvantaged
                legitTeamAhigherPayoffs_no + # thinks its not legit
                feedbackMakesDifference_num + # think they wont get more
                angry_num + # are angry
                neuroticism_avg +
                triviaPerson + 
                failMP + 
                female +
                income +
                ex
              # teamB*falseUnfair, 
              data = subset(df, falsefeedback=="fairplay" &
                              treatment=="A"), 
              family = "binomial")
summary(logit2)





# Look at how they rate the LYING fair play / unfair feedback  
# smaller n. 
# subset: falsefeedback == unfair: look at how they rated
# the unfair person

# for falsefeedback == fairplay: look at how they rated the 
# fair play person. 

# 2 models, DV = agree / accurate / good rep


#########################################################
# OLS2 -- false unfair -- goodRep
#########################################################

ols2_false_unfair_agreed <-lm( 
  agreeFeedback_unfair_num ~ 
    teamB + 
    difPayoffs +
    byChanceTeamAEasierQs + # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    feedbackMakesDifference_num +
    triviaPerson + 
    failMP + 
    female +
    income, # + # correlated with age
  # teamB * falseUnfair,
  data = subset(df, falsefeedback == "unfair"))
summary(ols2_false_unfair_agreed)

ols2_false_unfair_accurate <-lm( 
  accuracyRating_unfair ~ 
    teamB + 
    difPayoffs +
    byChanceTeamAEasierQs + # luckOfTheDraw + # == dummy version of byChanceTeamAEasierQs 
    feedbackMakesDifference_num +
    triviaPerson + 
    failMP + 
    female +
    income,
  data = subset(df, falsefeedback == "unfair"))
summary(ols2_false_unfair_accurate)


#########################################################
# 1) Logistic regression -- What makes you believe in false
# claims? 
#########################################################

# DV = belief in false claim false_fact_too_hard2_num 



#########################################################
# OLS -- What makes people like a FALSE feedback? 
#########################################################

# What makes people overlook false facts? 
# What predicts how people rate a FALSE fairplay/unfair feedback?
# Look at how ALL rate the false unfair feedback 
# (i.e. those on team B who got a false unfair and those on
# team A who got a false unfair)
# Look at how those on team B who got a false unfair feedback 
# rated that false unfair feedback. 

# Hypotheses... things that should make you like the false 
# unfair person

# 1. Being on team B: +
df$teamB

# 2. Accuracy: n.s. (LEAVE OUT HERE; all get the false one)
# Social Identity Theory: 

# 3. Stability of inter-group differences: Team B players who 
# believe they have a fair shot at getting a top-up. // 
# Team A players who believe their higher status is threatened. 
df$feedbackMakesDifference_num # * team

# 3.Legitimacy: Team B players who believe their lower status 
# to be illegitimate // Team A players who believe their higher 
# status to be legit
df$legitTeamAhigherPayoffs_no
# opposite: df$byChanceTeamAEasierQs
# df$fair

# 4. Identity strength 
df$identification_num

# 5. Emotions -- Anger? Resentment? 
df$happy_num
df$angry_num
df$proud_num
df$resentful_num

# 6. Perceived disadvantage 
# The greater the perception that one's in-group is 
# disadvantaged relative to a relevant out-group the greater 
# the tolerance of false facts. 
# Feeling unfairly treated? Luck of the draw 
df$expectationsLastQ_TeamA # know that Team A will get an easier Q
df$byChanceTeamAEasierQs # opposite 

# 5. Actual disadvantage 
# Don't think this will have an effect 
df$difPayoffs 
df$share_hardQs 

# Manipulation check
# df$whoGotHigherPayoff -- could exclude cases where both teams
# got the same payoff
# df$failMP

# Belief in false claim 
# df$rigged

# Controls
df$female
df$age
df$income

# df$british # factor



df$feedbackMakesDifference_num

# what makes people overlook false facts in
# the unfair feedback? 

ols2_false_unfair_goodRep <-lm( 
  # representationRating_unfair ~ 
  accuracyRating_unfair ~ 
    teamB + 
    # share_hardQs +
    difPayoffs + 
    angry_num + # teamB
    # participant.payoff + # teamB
    feedbackMakesDifference_num + # stability of intergroup differences --> the higher the more likely to overlook ff
    legitTeamAhigherPayoffs_no + # legitimacy --> if no more likely to overlook ff
    # identification_num + # --> the higher the more likely
    # happy_num + 
    # angry_num + # teamB
    # proud_num + 
    # resentful_num + 
    # expectationsLastQ_TeamA + # or byChanceTeamAEasierQs 
    # openness_avg +
    # neuroticism_avg +
    # conscientiousness_avg +
    # agreeableness_avg +
    # extraversion_avg + 
    # byChanceTeamAEasierQs + 
    # triviaPerson + 
    failMP + 
    female +
    income,# + 
    # teamB*difPayoffs, 
    # teamB*angry_num, 
  data = subset(df, falsefeedback == 'unfair'))
summary(ols2_false_unfair_goodRep)

df$triviaPerson

df$wantFeedbackOn0 # too easy
df$wantFeedbackOn1 # too hard
df$wantFeedbackOn2 # time limit 

# How Team B rates the unfair guy 





# belief in false claim
df$fair
df$rigged_num

### Good Rep #### 
ols1_GoodRep_Unfair <-lm( 
  representationRating_unfair ~ 
    # treatment +
    falsefeedback + 
    # treatment*falsefeedback + 
    # share_hardQs + # probably correlated with payoff
    # treatment*share_hardQs +
    participant.payoff +
    # treatment*participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num, # + 
    # failMP + 
    # falseExpectations + 
    # byChanceTeamAEasierQs, 
  data = subset(df, treatment == 'B'))
summary(ols1_GoodRep_Unfair)

stargazer(ols1_GoodRep_Unfair, 
          type="text",
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE)

### Good Rep #### 
ols2_GoodRep_Unfair <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_GoodRep_Unfair)

### Accurate #### 
ols1_Accurate_Unfair <-lm( 
  accuracyRating_unfair ~ 
    # representationRating_unfair ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols1_Accurate_Unfair)

### Accurate #### 
ols2_Accurate_Unfair <-lm( 
  accuracyRating_unfair ~ 
    # representationRating_unfair ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    # female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_Accurate_Unfair)

# Export to Latex
stargazer(ols1_GoodRep_Unfair, ols2_GoodRep_Unfair,
          ols1_Accurate_Unfair, ols2_Accurate_Unfair,
          # type="text",
          title="OLS Regression Results (DVs -- Rating the 'Unfair' Feedback)", 
          align=TRUE, 
          dep.var.labels=c("Good Representative", "Factually Accurate"), 
          covariate.labels=c(
            "Team B",
            "Contained False Claims",
            "Share of Difficult Qs",
            "Payoff",
            "Female",
            "Age",
            "Income",
            "Like Pub Quizzes",
            "Angry",
            "Failed Manipulation Test",
            "False Expectations",
            "A had the luck of the draw"
          ), 
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE)

# https://www.r-statistics.com/tag/stargazer/







#########################################################
# OLS
#########################################################

#########################################################
# Looking at Group 1 only -- all n.s.
#########################################################

### Accurate #### 
ols_Group1_Accurate <-lm( 
  accuracyRating_unfair ~ 
  # representationRating_unfair ~ 
    # treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = subset(df, treatment == "B" & falsefeedback == "unfair"))
summary(ols_Group1_Accurate)

### GoodRep #### 
ols_Group1_GoodRep <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    # treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = subset(df, treatment == "B" & falsefeedback == "unfair"))
summary(ols_Group1_GoodRep)

# Belief in false claim

ols_Group1_noExist <-lm( 
  # accuracyRating_unfair ~ 
  # representationRating_unfair ~ 
  false_fact_too_hard2_num ~ 
    # treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = subset(df, treatment == "B" & falsefeedback == "unfair"))
summary(ols_Group1_noExist)



#########################################################
# Looking at Belief in the False Claim
#########################################################

ols1_noExist <-lm( 
  # accuracyRating_unfair ~ 
  # representationRating_unfair ~ 
  false_fact_too_hard2_num  ~
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols1_noExist)

### Good Rep #### 
ols2_GoodRep_Unfair <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_GoodRep_Unfair)


#########################################################
# Looking at Unfair Feedback
#########################################################

### Good Rep #### 
ols1_GoodRep_Unfair <-lm( 
                  # accuracyRating_unfair ~ 
                   representationRating_unfair ~ 
                   treatment +
                   falsefeedback + 
                   share_hardQs + 
                   participant.payoff +
                   female +
                   age +
                   income + 
                   triviaPerson +
                   angry_num + 
                   failMP + 
                   falseExpectations + 
                   byChanceTeamAEasierQs, 
                 data = df)
summary(ols1_GoodRep_Unfair)

### Good Rep #### 
ols2_GoodRep_Unfair <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_GoodRep_Unfair)

### Accurate #### 
ols1_Accurate_Unfair <-lm( 
  accuracyRating_unfair ~ 
  # representationRating_unfair ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols1_Accurate_Unfair)

### Accurate #### 
ols2_Accurate_Unfair <-lm( 
    accuracyRating_unfair ~ 
  # representationRating_unfair ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    # female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_Accurate_Unfair)

# Export to Latex
stargazer(ols1_GoodRep_Unfair, ols2_GoodRep_Unfair,
          ols1_Accurate_Unfair, ols2_Accurate_Unfair,
          # type="text",
          title="OLS Regression Results (DVs -- Rating the 'Unfair' Feedback)", 
          align=TRUE, 
          dep.var.labels=c("Good Representative", "Factually Accurate"), 
          covariate.labels=c(
                             "Team B",
                             "Contained False Claims",
                             "Share of Difficult Qs",
                             "Payoff",
                             "Female",
                             "Age",
                             "Income",
                             "Like Pub Quizzes",
                             "Angry",
                             "Failed Manipulation Test",
                             "False Expectations",
                             "A had the luck of the draw"
                             ), 
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE)

# https://www.r-statistics.com/tag/stargazer/
  
#########################################################
# Looking at Fair Play Feedback
#########################################################

### Good Rep #### 
ols1_GoodRep_FairPlay <-lm( 
  representationRating_fairplay ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols1_GoodRep_FairPlay)

### Good Rep #### 
ols2_GoodRep_FairPlay <-lm( 
  representationRating_fairplay ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_GoodRep_FairPlay)

### Accurate #### 
ols1_Accurate_FairPlay <-lm( 
  accuracyRating_fairplay ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols1_Accurate_FairPlay)

### Accurate #### 
ols2_Accurate_FairPlay <-lm( 
  accuracyRating_fairplay ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    # female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_Accurate_FairPlay)

# Export to Latex
stargazer(ols1_GoodRep_FairPlay, ols2_GoodRep_FairPlay,
          ols1_Accurate_FairPlay, ols2_Accurate_FairPlay,
          # type="text",
          title="OLS Regression Results (DVs -- Rating the 'Fair Play' Feedback)", 
          align=TRUE, 
          dep.var.labels=c("Good Representative", "Factually Accurate"), 
          covariate.labels=c(
                             "Team B",
                             "Did not contain False Claims",
                             "Share of Difficult Qs",
                             "Payoff",
                             "Female",
                             "Age",
                             "Income",
                             "Like Pub Quizzes",
                             "Angry",
                             "Failed Manipulation Test",
                             "False Expectations",
                             "A had the luck of the draw"
          ), 
          omit.stat=c("LL","ser","f"), 
          no.space=TRUE)


# RIGGED

# Failing the manipulation test predicts thinking
# this was rigged

### Good Rep #### 
ols_rigged <-lm( 
  rigged_num ~ 
    treatment +
    falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols_rigged)

# CUTS
# undergrad + 
# atEssex + 
# difPayoffs +
# participant.payoff * treatment,
# feedbackMakesDifference_d, 

# Look at 
df$education


## WHAT MAKES TEAM B VOTE FOR A LYING UNFAIR PERSON?
# LOOK AT PERSONALITY!! 


### Good Rep #### 
ols_GoodRep_Lying_Unfair <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    # treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    age +
    income + 
    triviaPerson +
    angry_num + 
    failMP + 
    falseExpectations + 
    legitTeamAhigherPayoffs +
    # conscientiousness +
    byChanceTeamAEasierQs, 
  data = subset(df, treatment == 'B' & falsefeedback == 'unfair'))

summary(ols_GoodRep_Lying_Unfair)



### Good Rep #### 
ols2_GoodRep_Unfair <-lm( 
  # accuracyRating_unfair ~ 
  representationRating_unfair ~ 
    treatment +
    # falsefeedback + 
    share_hardQs + 
    participant.payoff +
    female +
    # age +
    # income + 
    # triviaPerson +
    angry_num + 
    failMP + 
    # falseExpectations + 
    byChanceTeamAEasierQs, 
  data = df)
summary(ols2_GoodRep_Unfair)



# TO DO
# ... when I have a higher n, add
# df$angry_num
# df$legitTeamAhigherPayoffs
# Try a scale as a DV

# LOGIT
df$goodRep_unfair <- ifelse(df$representationRating_unfair > 50,
                            1, 0)
df$goodRep_fairplay <- ifelse(df$representationRating_fairplay > 50,
                            1, 0)

df$feedbackMakesDifference_d <- ifelse(df$feedbackMakesDifference == "Somewhat Likely" | 
                                         df$feedbackMakesDifference == "Very Likely", 1, 0)



logRGoodRepB <-glm(goodRep_unfair ~ # representationRating_unfair ~ 
                   falsefeedback + 
                   share_hardQs + 
                   gender +
                   born, 
                   family = "binomial", 
                 data = subset(df, treatment=='B'))
summary(logRGoodRepB)






# Team A who got false facts in their own feedback


#########################################################
# STATS for paper
#########################################################

names(df)
# see graphs
df$expectationsLastQ
df$wantFeedbackOn0 # too easy
df$wantFeedbackOn1 # too hard
df$wantFeedbackOn2 # 30sec

#########################################################
# H1: False facts in the 'unfair' treatment are more likely to 
# be tolerated by Team B than by Team A. 

# H1 (new): On average, tolerance of false facts in the 'unfair' 
# treatment is higher among Team B than it is among Team A. 
#########################################################


### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 4.021739
# mean of y 4.622222
# t = -2.3983, df = 85.376, p-value = 0.01865
test_gen_agreed <- t.test(agreeFeedback_unfair_num ~ treatment, 
                          data=subset(df, falsefeedback == 'unfair'))
test_gen_agreed

levels(df$agreeFeedback_unfair)

### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 46.52174
# mean of y 64.42222
# t = -2.8356, df = 86.246, p-value = 0.005698
test_goodRep <- t.test(representationRating_unfair ~ treatment, 
                       data=subset(df, falsefeedback == 'unfair'))
test_goodRep

### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 62.00000
# mean of y 63.82222
# t = -0.349, df = 88.999, p-value = 0.7279
test_accurate <- t.test(accuracyRating_unfair ~ treatment, 
                        data=subset(df, falsefeedback == 'unfair'))
test_accurate

### Belief in false facts ####
sum(is.na(df$false_fact_too_hard2_num)) # 50 missings - ARGH
sum(complete.cases(df$false_fact_too_hard2_num)) # 159 

t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="unfair"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 2.026316 -- closest to 2='Probably False'
# mean of y 2.258065 -- closest to 2='Probably False'
# t = -1.2807, df = 66.974, p-value = 0.2047
test_dontExist <- t.test(false_fact_too_hard2_num ~ treatment, 
                         data=subset(df, falsefeedback == 'unfair'))
test_dontExist

### Belief in gross exaggeration ####
test_30SecNoTime <- t.test(false_fact_30sec_unfair_num ~ treatment, 
                         data=subset(df, falsefeedback == 'unfair'))
test_30SecNoTime
# Not enough n!!

bonferroni <- p.adjust(0.05, method = "bonferroni", n=6)
bonferroni


#########################################################
# HIGH STATUS 
# H1b: False facts in the 'fairplay' treatment are more 
# likely to be tolerated by Team A than by Team B. 

# H1b (new): On average, tolerance of false facts in the 'fairplay' 
# treatment is higher among Team A than it is among Team B. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 4.218182
# mean of y 3.000000
# t = 5.1039, df = 115.53, p-value = 1.321e-06

mean(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), na.rm = T)
mean(subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"), na.rm = T)


### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 65.43636
# mean of y 37.03175
# t = 6.231, df = 111.91, p-value = 8.38e-09

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 58.38182
# mean of y 49.15873
# t = 1.9677, df = 115.9, p-value = 0.05149

### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 2.071429 -- closest to 2='Probably False'
# mean of y 2.375000 -- closest to 2='Probably False'
# t = -1.7117, df = 86.507, p-value = 0.09054

test_dontExist <- t.test(false_fact_too_hard2_num ~ treatment, 
                         data=subset(df, falsefeedback == 'fairplay'))
test_dontExist
# t = -1.7117, df = 86.507, p-value = 0.09054
# mean in group A 2.071429
# mean in group B 2.375000

test_10secPlenty <- t.test(false_fact_30sec_fairplay_num ~ treatment, 
                         data=subset(df, falsefeedback == 'fairplay'))
test_10secPlenty




#########################################################
# H2: Team B members are more likely to tolerate false facts 
# in the'unfair' feedback than they are to tolerate false facts 
# in the 'fair play' feedback # YES!

#########################################################

## Difference Team B rating the lying unfair feedback and 
## Team B rating the lying fairplay feedback 

### General agreement ####
t.test(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 4.639344 # 5=Agree
# mean of y 3.108434 # 3=Slightly disagree
# t = 7.4053, df = 142, p-value = 1.061e-11


### Factually accurate ####
t.test(subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 65.81967 
# mean of y 51.62651 
# t = 3.451, df = 137.41, p-value = 0.0007424


### Good Rep ####
t.test(subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 65.80328 
# mean of y 42.24096 
# t = 5.0036, df = 134.88, p-value = 1.723e-06


### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="fairplay"))
# mean of x 2.319149
# mean of y 2.279412
# t = 0.27274, df = 106.59, p-value = 0.7856



#########################################################
# HIGH STATUS 
# H2b: Team A members are more likely to tolerate false facts 
# in the 'fair play' feedback than they are to tolerate false facts 
# in the 'unfair' feedback. 
# H2b (new): Among Team A, tolerance of factual inaccuracies in 
# the 'fair play' feedback is higher, on average, than tolerance of 
# factual inaccuracies in the 'unfair' feedback. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 4.144928 -- False fact in 'unfair': closest to 4 = 'Slightly Agree'
# mean of y 4.031250 -- False fact in 'fair play': closest to 4 = 'Slightly Agree'
# t = 0.53667, df = 129.11, p-value = 0.5924
levels(df$agreeFeedback_fairplay)

### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 63.89855 
# mean of y 46.67188 
# t = 3.7186, df = 111.54, p-value = 0.0003152

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 59.31884 
# mean of y 61.00000 
# t = -0.39549, df = 127.95, p-value = 0.6931

### Belief in false facts ####
t.test(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="unfair"))
# mean of x 2.053571
# mean of y 2.089286
# t = -0.24144, df = 109.87, p-value = 0.8097

# temp <- df[ , c("id",
#                 "session.code", 
#                 "treatment",
#                 "falsefeedback",
#                 "false_fact_30sec_fairplay",
#                 "false_fact_30sec_unfair")]


#########################################################
# H3: Team B members who are exposed to factually incorrect claims in the 
# 'unfair' feedback are more likely to support the (factually inaccurate) 
# 'unfair' feedback than they are to support the (factually accurate) 
# 'fair play' feedback. 

# H3 (new): On average, Team B members who are exposed to factually 
# incorrect claims in the 'unfair' feedback are more supportive 
# of the (factually inaccurate) 'unfair' feedback than
# the (factually accurate) 'fair play' feedback. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 3.577778
# mean of y 4.622222
# t = -4.0831, df = 82.82, p-value = 0.000102

mean(subset(df$agreeFeedback_fairplay_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
mean(subset(df$agreeFeedback_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)


### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$representationRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 43.68889
# mean of y 64.42222
# t = -3.6661, df = 87.992, p-value = 0.0004207

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="B" & df$falsefeedback=="unfair"), 
       subset(df$accuracyRating_unfair, df$treatment=="B" & df$falsefeedback=="unfair"))
# mean of x 54.60000
# mean of y 63.82222
# t = -1.7768, df = 87.999, p-value = 0.07906

### Belief in false facts ####
# Looking at one DV only, i.e. no t-test
mean(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
# mean=2.258065
# sd=0.6815542

mean(subset(df$false_fact_30sec_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
sd(subset(df$false_fact_30sec_unfair_num, df$treatment=="B" & df$falsefeedback=="unfair"), na.rm = T)
# 1.842105
# 1.167293





#########################################################
# HIGH STATUS: 
# H3b: Team A members who are exposed to factually incorrect 
# claims in the 'fair play' feedback (group 4) are more likely to  
# support the (factually inaccurate) 'fair play' feedback than they 
# are to support the (factually accurate) 'unfair' treatment.

# H3b (new): On average, Team A members who are exposed to factually 
# incorrect claims in the 'fair play' feedback are more supportive 
# of the (factually inaccurate) 'fair play' feedback than  
# the (factually accurate) 'unfair' feedback. 
#########################################################

### General agreement ####
t.test(subset(df$agreeFeedback_fairplay_num, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$agreeFeedback_unfair_num, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 4.218182
# mean of y 4.000000
# t = 0.96009, df = 107.8, p-value = 0.3392

### Factually accurate ####
t.test(subset(df$accuracyRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$accuracyRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 58.38182
# mean of y 60.18182
# t = -0.38954, df = 107.98, p-value = 0.6976

### Good Rep ####
t.test(subset(df$representationRating_fairplay, df$treatment=="A" & df$falsefeedback=="fairplay"), 
       subset(df$representationRating_unfair, df$treatment=="A" & df$falsefeedback=="fairplay"))
# mean of x 65.43636
# mean of y 46.14545
# t = 4.3895, df = 103.7, p-value = 2.742e-05

### Belief in false facts ####
# Looking at one DV only, i.e. no t-test
mean(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), na.rm = T)
sd(subset(df$false_fact_too_hard2_num, df$treatment=="A" & df$falsefeedback=="fairplay"), na.rm = T)
# mean=2.071429
# sd=0.8379085



#########################################################

# Difference lying unfair feedback and not lying unfair feedback

#########################################################

# HIER WEITER!!!

### General agreement ####

### Good Rep ####

### Factually accurate ####

### Belief in false facts ####

# Create a dummy
df$false_fact_too_hard2_d <- ifelse(
  df$false_fact_too_hard2_num > 2, 1, 0)

table(df$false_fact_too_hard2_d) # 43 Prob True, 9 Def true
prop.table(table(df$treatment, df$false_fact_too_hard2_d), 1) # 43 Prob True, 9 Def true

table(df$falsefeedback, df$false_fact_too_hard2_d, df$treatment)

12/19
19/29

# TO DO LIST AGREEMENT WITH FALSE FACTS!!! 

# TO DO ASSESS the last claim



table(df$treatment, df$falsefeedback, df$rigged)

table(df$treatment, df$rigged)

table(df$rigged)
sum(complete.cases(df$rigged))


# Bonferroni Adjustment
# pairwise.t.test(df$false_fact_too_hard2_num, df$military_conditions, p.adj="bonferroni")

df$groups <- 99
df$groups[df$treatment == "B" & df$falsefeedback=="unfair"] <- 1
df$groups[df$treatment == "A" & df$falsefeedback=="unfair"] <- 2
df$groups[df$treatment == "B" & df$falsefeedback=="fairplay"] <- 3
df$groups[df$treatment == "A" & df$falsefeedback=="fairplay"] <- 4


# Creating groups for Bernoulli-adjusted t-tests
# PROBLEM: Different DVs!! 
tB_fUF_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="B" & df$falsefeedback=="unfair")
tB_fUF_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="B" & df$falsefeedback=="unfair")

tB_fFP_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="B" & df$falsefeedback=="fairplay")
tB_fFP_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="B" & df$falsefeedback=="fairplay")

tA_fUF_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="A" & df$falsefeedback=="unfair")
tA_fUF_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="A" & df$falsefeedback=="unfair")

tA_fFP_rFP <- subset(df$representationRating_fairplay, 
                     df$treatment=="A" & df$falsefeedback=="fairplay")
tA_fFP_rUF <- subset(df$representationRating_unfair, 
                     df$treatment=="A" & df$falsefeedback=="fairplay")


df$groups[df$treatment == "B" & df$falsefeedback=="unfair"] <- 1
df$groups[df$treatment == "A" & df$falsefeedback=="unfair"] <- 2
df$groups[df$treatment == "B" & df$falsefeedback=="fairplay"] <- 3
df$groups[df$treatment == "A" & df$falsefeedback=="fairplay"] <- 4


#########################################################


# genAgree dummies

hist(df$agreeFeedback_fairplay_num)
hist(df$agreeFeedback_unfair_num)

df$genAgreeFairplay <- ifelse(df$agreeFeedback_fairplay == 'Strongly agree' | 
                                df$agreeFeedback_fairplay == 'Agree' | 
                                df$agreeFeedback_fairplay == 'Slightly agree', 
                              1, 0)
table(df$genAgreeFairplay, df$treatment)

df$genAgreeUnfair <- ifelse(df$agreeFeedback_unfair == 'Strongly agree' | 
                              df$agreeFeedback_unfair == 'Agree' | 
                              df$agreeFeedback_unfair == 'Slightly agree', 
                            1, 0)
table(df$genAgreeUnfair, df$treatment)

### accuracy dummies #### 

df$accurateFairplay <- ifelse(df$accuracyRating_fairplay > 50, 1, 0)
table(df$accurateFairplay, df$treatment)

df$accurateUnfair <- ifelse(df$accuracyRating_unfair > 50, 1, 0)
table(df$accurateUnfair, df$treatment)

### goodRep dummies ####

hist(df$representationRating_fairplay)
hist(df$representationRating_unfair)

df$goodRepFairplay <- ifelse(df$representationRating_fairplay > 50, 1, 0)
table(df$goodRepFairplay, df$treatment)

df$goodRepUnfair <- ifelse(df$representationRating_unfair > 50, 1, 0)
table(df$goodRepUnfair, df$treatment)

### gullible dummies #### 

df$dontExist <- ifelse(df$false_fact_too_hard2 == "Definitely False" | 
                         df$false_fact_too_hard2 == "Probably False", 1, 0)






# For power analysis -- expect a 10-point difference on soft DVs

mean(subset(df, treatment=="A")$representationRating_fairplay)
mean(subset(df, treatment=="B")$representationRating_fairplay)

mean(subset(df, treatment=="A")$representationRating_unfair)
mean(subset(df, treatment=="B")$representationRating_unfair)

mean(subset(df, treatment=="A")$accuracyRating_fairplay)
mean(subset(df, treatment=="B")$accuracyRating_fairplay)

mean(subset(df, treatment=="A")$accuracyRating_unfair)
mean(subset(df, treatment=="B")$accuracyRating_unfair)

# For power analysis -- expect a half-point difference on hard DVs
# not sure... 
mean(subset(df, treatment=="B" & falsefeedback=="unfair")$false_fact_too_hard2_num, na.rm = T)
mean(subset(df, treatment=="B" & falsefeedback=="fairplay")$false_fact_too_hard2_num, na.rm = T)






### OLD

# H1 (LOW STATUS)



# H2 
test_dontExist <- t.test(false_fact_too_hard2_num ~ falsefeedback, 
                         data=subset(df, treatment == 'B'))
test_dontExist
# t = 0.58929, df = 51.227, p-value = 0.5583
# mean in group fairplay 2.351351 
# mean in group unfair 2.238095

# H1b (HIGH STATUS)

test_gen_agreed2 <- t.test(agreeFeedback_fairplay_num ~ treatment, 
                           data=subset(df, falsefeedback == 'fairplay'))
test_gen_agreed2

# t = 5.0702, df = 91.778, p-value = 2.055e-06  # SIGNIFICANT! 
# mean in group A 4.357143 
# mean in group B 3.057692

test_goodRep2 <- t.test(representationRating_fairplay ~ treatment, 
                        data=subset(df, falsefeedback == 'fairplay'))
test_goodRep2
# t = 5.7169, df = 91.86, p-value = 1.337e-07
# mean in group A 65.97619 
# mean in group B 36.71154

test_accurate2 <- t.test(accuracyRating_fairplay ~ treatment, 
                         data=subset(df, falsefeedback == 'fairplay'))
test_accurate2
# t = 2.0806, df = 90.645, p-value = 0.04029
# mean in group A 60.90476
# mean in group B 50.25000

test_dontExist <- t.test(false_fact_too_hard2_num ~ treatment, 
                         data=subset(df, falsefeedback == 'fairplay'))
test_dontExist
# t = -1.0378, df = 59.958, p-value = 0.3035
# mean in group A 2.137931
# mean in group B 2.351351

test_10secPlenty <- t.test(false_fact_30sec_fairplay_num ~ treatment, 
                           data=subset(df, falsefeedback == 'fairplay'))
test_10secPlenty
# t = 2.6906, df = 44.905, p-value = 0.009981
# mean in group A 3.043478
# mean in group B 2.208333

# H2b

test_dontExist2 <- t.test(false_fact_too_hard2_num ~ falsefeedback, 
                          data=subset(df, treatment == 'A'))
test_dontExist2
# t = 0.78926, df = 57.758, p-value = 0.4332
# mean in group fairplay 2.137931
# mean in group unfair 1.967742

# a) accuracy_unfair
# Team B -- Mean ratings of the FALSE unfair feedback:
# mean=67.34, sd=24.06

# b) accuracy_fairplay
# Team B -- Mean ratings of the TRUE fair play feedback:
# mean=50.25, sd=25.48







#########################################################

# Cuts

#########################################################

# Seems that half the bullet points did not get recorded =// 

temp <- df[ , c("id", 
                "treatment", 
                "firstfeedback",
                "falsefeedback",
                "false_fact_too_hard2", 
                "false_fact_too_hard2_num",
                "false_fact_30sec_unfair",
                "false_fact_30sec_fairplay")]

# order
temp <- temp[order(df$treatment, 
                   df$firstfeedback,
                   df$falsefeedback), ]

temp <-
  df %>%
  group_by(treatment, falsefeedback) %>%
  summarise(
    mean_false_fact_too_hard2 = mean(false_fact_too_hard2_num, na.rm = T),
    mean_false_fact_30sec_fairplay = mean(false_fact_30sec_fairplay_num, na.rm = T),
    mean_false_fact_30sec_unfair = mean(false_fact_30sec_unfair_num, na.rm = T))

# Exact    

dontExist <-
  ggplot(df, 
         aes(x=treatment, 
             y=false_fact_too_hard2_num,
             fill=falsefeedback)) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge") + # stack them next to each other
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2) + 
  scale_fill_manual(name="False Facts in",
                    labels=c("Fair Play", "Unfair"),
                    values=c("#eeccaa", "#8fb8bd")) +
  labs(x="Team",
       y="") + 
  ggtitle("'Unfair' Feedback", # \n
          subtitle = "Some of these places don't even exist (1=Definitely False, 2=Probably False, 3=Probably True, 4=Definitely True)") +
  cleanup

print(dontExist)
ggsave("images/dontExist.png")

# Warning messages:
# 1: Removed 50 rows containing non-finite values (stat_summary). 
# 2: Removed 50 rows containing non-finite values (stat_summary).

# KOMISCH -- The unfair bars should be at least a little higher

df$false_fact_too_hard2

library("RColorBrewer")
# AMAZING COLOURS 
# http://colorbrewer2.org/#type=diverging&scheme=RdYlGn&n=3
#fc8d59 red
#91cf60 green







### TABLES -- OLD

# by group
gen_agreed <- aggregate(df[c("agreeFeedback_fairplay_num",
                             "agreeFeedback_unfair_num")], 
                        by = df[c("treatment", "falsefeedback")], 
                        FUN=mean,
                        na.rm=TRUE)
gen_agreed 

# by session
mean_agreed <- aggregate(df[c("agreeFeedback_fairplay_num",
                              "agreeFeedback_unfair_num")], 
                         by = df[c("session.code")], 
                         FUN=mean,
                         na.rm=TRUE)
mean_agreed

mean_agreed <-
  df %>%
  group_by(treatment, falsefeedback) %>%
  summarize(
    mean_agreeFeedback_fairplay = mean(agreeFeedback_fairplay_num, na.rm = TRUE),
    mean_agreeFeedback_unfair = mean(agreeFeedback_unfair_num, na.rm = TRUE))

mean_accuracy <-
  df %>%
  group_by(treatment, falsefeedback) %>%
  summarize(
    mean_agreeFeedback_fairplay = mean(accuracyRating_fairplay, na.rm = TRUE),
    mean_agreeFeedback_unfair = mean(accuracyRating_unfair, na.rm = TRUE))
mean_accuracy

mean_representation <-
  df %>%
  group_by(treatment, falsefeedback) %>%
  summarize(
    mean_representation_fairplay = mean(representationRating_fairplay, na.rm = TRUE),
    mean_representation_unfair = mean(representationRating_unfair, na.rm = TRUE))
mean_representation

### PLOTS -- OLD

ggplot(mean_agreed,
       aes(x=treatment,
           y=mean_agreeFeedback_unfair
       )) +
  geom_bar(stat="identity")


agreed_unfair <- 
  ggplot(mean_agreed,
         aes(
           x=treatment,
           y=mean_agreeFeedback_unfair,
           fill=falsefeedback
         )) +
  geom_bar(stat="identity", position=position_dodge())


ggplot(mean_agreed,
       aes(x=treatment,
           y=mean_agreeFeedback_unfair
       )) +
  geom_point()

agreed_unfair <- 
  ggplot(data=subset(df, falsefeedback %in% c("unfair")),
         aes(
           x=treatment,
           y=mean(agreeFeedback_unfair_num),
           fill=falsefeedback
         )) +
  geom_bar(stat="identity")

agreed_fairplay <- 
  ggplot(data=subset(df, falsefeedback %in% c("fairplay")),
         aes(
           x=treatment,
           y=mean(agreeFeedback_fairplay_num),
           fill=falsefeedback
         )) +
  geom_bar(stat="identity")

levels(df$falsefeedback)

ggplot(data=df,
       aes(
         x=treatment,
         y=agreeFeedback_unfair_num,
         fill=falsefeedback
       )) +
  geom_bar(stat="identity")


agreed_unfair <- 
  ggplot(data=df,
         aes(
           x=treatment,
           y=agreeFeedback_unfair_num,
           fill=falsefeedback
         )) + 
  stat_summary(
  )













