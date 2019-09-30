#########################################################
# British Academy Project
# -- Analyses --
# -- 24 September 2019 -- 
#########################################################

rm(list = ls()) 
setwd("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/3.BAgrantProject/Data/") 
library(foreign) # for read.csv 
# library(plyr) 
library(dplyr) 
library(tidyr) 
library(stargazer) 
library(ggplot2) 
library(scales) 
library(ggthemes) 
library(ggThemeAssist) 
library(colorspace) 
library(RColorBrewer) 
library(purrr)
library(haven) 

library("sjPlot")
library(effects)

load("df")
load("df_long")

# Same same
# noAsylum2 <- df[ which(df$corrected=="noAsylum"), ]


#########################################################
# Centering IVs -- see data doc
# Creating subsets -- see graphs doc
#########################################################

# Define reference categories 

df$PollAtt <- factor(df$PollAtt,
                     labels=c("Medium (3-7)", 
                              "Low (0-2)",
                              "High (8-10)")) 

df$immigEcon <- factor(df$immigEcon,
                     labels=c("Don't Know", 
                              "Strongly agree",
                              "Agree",
                              "Disagree",
                              "Strongly disagree")) 


# RUN UNTIL HERE


#########################################################
# H0 -- Belief in false facts (pre-correction)
#########################################################

# What predicts whether or not you hold any false beliefs? 

ols_belief_T1 = 
  function(dv, df){
    lm( 
      dv ~
        leave + 
        immigNumbers_num + # immigEcon
        immigNo123 +  
        PollAtt + 
        university + 
        gender + 
        age +
        populism_c +
        ethno_c +
        trueScore7_c, 
        # populism_c:gender,
      data=df) # 
  }

# All respondents
ols_noAsylum_T1 <- ols_belief_T1(df$belief_noAsylum_T1, df)
ols_costImmig_T1 <- ols_belief_T1(df$belief_costImmig_T1, df)
ols_whiteCrime_T1 <- ols_belief_T1(df$belief_whiteCrime_T1, df)
ols_lowPaid_T1 <- ols_belief_T1(df$belief_lowPaid_T1, df)

# Targeted respondents
# ols_noAsylum_T1 <- ols_belief_T1(anti$belief_noAsylum_T1, anti)
# ols_costImmig_T1 <- ols_belief_T1(anti$belief_costImmig_T1, anti)
# ols_whiteCrime_T1 <- ols_belief_T1(pro$belief_whiteCrime_T1, pro)
# ols_lowPaid_T1 <- ols_belief_T1(pro$belief_lowPaid_T1, pro)

# summary(ols_noAsylum_T1)
# summary(ols_costImmig_T1)
# summary(ols_whiteCrime_T1)
# summary(ols_lowPaid_T1)

stargazer(ols_noAsylum_T1,
          ols_costImmig_T1, 
          ols_whiteCrime_T1,
          ols_lowPaid_T1,
          title="Belief in false claims", 
          align=TRUE, 
          dep.var.labels=c(""),
          column.labels=c("asylum seekers", "cost/benefits", "crime", "low-paid wages"),
          # column.labels=c("no of asylum seekers", "cost/benefits", "crime London", "low-paid wages"),
          covariate.labels=c("voted to leave the EU", #"curb immigration",
                             "immigration opinions",
                             "immigration important issue",
                             "low political attention", 
                             "high political attention", 
                             "university",
                             "female",
                             "age",
                             "populist (centred)", 
                             "nationalist (centred)",
                             "other T/F ratings",
                             "Constant"),
          omit.stat=c("ser","f"), # omit residual standard error & f-statistic
          no.space=TRUE, 
          type="latex") # text


# Just for fun ... looking if the same things predict 
# belief in the distractor items

ols_plasticBags_T1 <- ols_belief_T1(df$plasticBags_num_T1, df)
summary(ols_plasticBags_T1)

ols_fracking_T1 <- ols_belief_T1(df$fracking_num_T1, df)
summary(ols_fracking_T1)


#########################################################
# H1 -- Effect of the Fact-Check on Belief in False Claims
#########################################################

# Initial belief (all)

mean(df$belief_o_T1) # 4.902589
mean(df$belief_o_T2) # 3.546322

df %>%
  filter(
    motivatedToReject == 1
  ) %>%
  group_by(
    gotWhichCorrection 
    # pop_quartile # the more populist the less convinced
  ) %>%
  summarise(
    T1 = mean(belief_o_i2_T1, na.rm = T), 
    T2 = mean(belief_o_i2_T2, na.rm = T), 
    diff = mean(diff_i2_o, na.rm=T)
  )



# Is the difference between pre- and 
# post-correction vercity scores significant?

#########################################################
# PAIRED T-TESTS (before & after)
#########################################################

# Control group

t.test(belief_o ~ time,
       data=subset(df_long, 
                   df_long$RandomGrp %in% c(1,2) & 
                     df_long$motivatedToReject==1), 
       paired = TRUE)

# t = 21.433, df = 604, p-value < 2.2e-16
# mean of the differences: 1.796694 
# 95 percent confidence interval: 1.632060 1.961328


# Anti-immigration

t.test(belief_o ~ time, 
       data = subset(df_long, 
                     df_long$RandomGrp %in% c(1,2) &
                       df_long$corrected %in% c("noAsylum", "costImmig") & 
                       df_long$motivatedToReject==1),
       paired = TRUE)
# t = 18.868, df = 393, p-value < 2.2e-16
# mean of the differences: 1.954315 
# 95 percent confidence interval: 1.750680 2.157949

# Pro-immigration

t.test(belief_o ~ time, 
       data = subset(df_long, 
                     df_long$RandomGrp %in% c(1,2) &
                       df_long$corrected %in% c("whiteCrime", "lowPaid") & 
                       df_long$motivatedToReject==1),
       paired = TRUE)
# t = 10.668, df = 210, p-value < 2.2e-16
# mean of the differences: 1.50237 
# 95 percent confidence interval: 1.224757 1.779983


#########################################################
# Mean veracity scores in T1 and T2 for each corrected statement
#########################################################

# NB: 2 variations of the same DV: One including 0, one
# not including zero

# The original = diff_o
# T1-T2
# No effect: true-true = 7-7 = 0
# Positive effect: true-false=7-1= 6
# Backfire effect: false-true=1-7=

# The one scaled to not include any zeros
# diff = belief_T1 - belief_T2 + 7
# 1-6=backfire effect
# 7 = no effect
# 8-13 = positive effect. 

# Mean veracity scores at T1 and T2 for each corrected 
# statement

df %>%
  filter(motivatedToReject == 1 &
           RandomGrp == 1 | RandomGrp == 2) %>%
  group_by(
    corrected
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T),
    sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    sd_T2 = sd(belief_o_T2, na.rm = T),
    mean = mean(diff_o, na.rm=T),
    mean_diff = mean(diff_o, na.rm=T),
    sd_diff=sd(diff_o, na.rm=T))

# Mean veracity scores at T1 and T2 for pooled pro- and 
# anti-immigration people

df %>%
  filter(motivatedToReject == 1 &
           RandomGrp %in% c(1,2)
  ) %>%
  group_by(
    gotWhichCorrection
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T),
    #sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    #sd_T2 = sd(belief_o_T2, na.rm = T),
    mean_diff = mean(diff_o, na.rm=T)
    #sd_diff = sd(diff_o, na.rm=T)
  )


#########################################################
# H1 -- OLS
#########################################################

# What predicts to what extent you adapt your factual beliefs
# after seeing statistics?

# Other econ questions that might predict belief in these
df$immigEcon
df$immigFeelLikeHome_c

ols_diff = 
  function(dv, df){
    lm( 
      dv ~
        leave + 
        immigNumbers_num + # immigEcon
        immigNo123 +  
        PollAtt + 
        university + 
        gender + 
        age +
        populism_c +
        ethno_c +
        trueScore7_c + 
        # populism_c:leave +
        populism_c:immigNumbers_num +
        populism_c:gender,
      data=df) # 
  }

# Just playing... 

ols_diff2 = 
  function(dv, df){
    lm( 
      dv ~
        #leave + 
        immigNumbers_num + # immigEcon
        immigNo123 +  
        #PollAtt + 
        #university + 
        gender +
        # age +
        # populism_c +
        #ethno_c +
        #trueScore7_c + 
        populism_c:immigNumbers_num,
        # populism_c:gender,
      data=df) # 
  }


#########################################################
# noAsylum
#########################################################

temp <- 
  df %>%
  filter(RandomGrp %in% c(1,2) &
           corrected == "noAsylum" &
           immigOpinions == "fewer") 

ctrl_anti_noAsylum <- ols_diff(temp$diff, temp)
summary(ctrl_anti_noAsylum)
car::vif(ctrl_anti_noAsylum)

ctrl_anti_noAsylum2 <- ols_diff2(temp$diff, temp)
summary(ctrl_anti_noAsylum2)

#########################################################
# costImmig
#########################################################

temp <- 
  df %>%
  filter(RandomGrp %in% c(1, 2) &
           corrected == "costImmig" &
           immigOpinions == "fewer")

ctrl_anti_costImmig <- ols_diff(temp$diff, temp)
summary(ctrl_anti_costImmig)
# car::vif(ctrl_anti_costImmig)

ctrl_anti_costImmig2 <- ols_diff2(temp$diff, temp)
summary(ctrl_anti_costImmig2)

#########################################################
# whiteCrime
#########################################################

temp <-
  subset(df, RandomGrp == 1 | RandomGrp == 2 & 
           corrected == "whiteCrime" &
           immigOpinions %in% c("no change", "more"))

ctrl_pro_whiteCrime <- ols_diff(temp$diff, temp)
summary(ctrl_pro_whiteCrime)
# car::vif(ctrl_pro_whiteCrime)

ctrl_pro_whiteCrime2 <- ols_diff2(temp$diff, temp)
summary(ctrl_pro_whiteCrime2)

#########################################################
# lowPaid
#########################################################

temp <-
  subset(df, RandomGrp == 1 | RandomGrp == 2 & 
           corrected == "lowPaid" &
           immigOpinions %in% c("no change", "more"))

ctrl_pro_lowPaid <- ols_diff(temp$diff, temp)

summary(ctrl_pro_lowPaid)
# car::vif(ctrl_pro_lowPaid)

ctrl_pro_lowPaid2 <- ols_diff2(temp$diff, temp)
summary(ctrl_pro_lowPaid2)


stargazer(ctrl_anti_noAsylum,
          ctrl_anti_costImmig,
          ctrl_pro_whiteCrime,
          ctrl_pro_lowPaid, 
          title="Reactions to expert statements (control group)", 
          # subtitle="Control Group Respondents who are motivated to reject the statistics",
          align=TRUE, 
          dep.var.labels=c("Difference pre and post-correction veracity scores"),
          column.labels=c("asylum seekers", "cost/benefits", "crime", "low-paid wages"),
          covariate.labels=c("voted to leave the EU", #"curb immigration",
                             "increase immigration",
                             "immigration important issue",
                             "low political attention", 
                             "high political attention", 
                             "university",
                             "female",
                             "age",
                             "populist (centred)", 
                             "nationalist (centred)",
                             "other T/F ratings",
                             "populist:increase immigration",
                             "populist:female",
                             "Constant"),
          omit.stat=c("ser","f"), # omit residual standard error & f-statistic
          no.space=TRUE, 
          type="latex") # text



#########################################################
# H2 -- Effect of the fact-free comment on belief in false claims
#########################################################

# Main DV

# NB: We have different forms of the same diff 
# variable in here: 

# -- The original: Scale from 0 (False) - 6 (True).
# df$belief_o_T1 <- df$belief_T1 - 1
hist(df$belief_o_T1)

# -- Stata turned that into a 1 (False) to 7 (True) scale.
hist(df$belief_T1)


# --> The original diff: 
# df$diff_o <- df$belief_o_T1 - df$belief_o_T2
hist(df$diff_o) 

# Ranges from -6 to +6. 
# Positive values indicate a positive effect; 
# 0 indicates no effect. 
# Negative values indicate a backfire effect. 
# I am using this for the histograms as it makes it
# easy to see where 

# --> The new diff:
# diff = belief_T1 - belief_T2 + 7
hist(df$diff)
# I am adding 7 so as to get positive scores
# --> The higher the number the greater the effect. 
# 1-6=backfire effect; 
# 7 = no effect, 
# 8-13 = positive effect. 

mean(df$diff) # 8.356267
mean(df$diff_o) # 1.356267







#########################################################
# H2 -- Does the FFC have a significant effect?
#########################################################

df %>%
  filter(motivatedToReject == 1 
  ) %>%
  group_by(
    gotWhichCorrection,
    FactFreeReply
  ) %>%
  summarise(
    T1 = mean(belief_o_T1, na.rm = T),
    T2 = mean(belief_o_T2, na.rm = T),
    diff = mean(diff_o, na.rm=T),
    t1_t2 = T1 - T2,
    trust=mean(trustAuthority_num, na.rm = T),
    acc=mean(accurateAuthority_num, na.rm = T) 
  )


# 1.95 - 1.22 # 0.73
# 1.50 - 1.16 # 0.34

# All respondents -- Using diff

t_test <- t.test(diff_o ~ FactFreeReply,
                 data=subset(df, motivatedToReject == 1))
t_test
# t = 6.4528, df = 873.02, p-value = 1.818e-10
# mean in group No 1.796694
# mean in group Yes 1.201412

# Mean change: 0.595282
# 1.796694 - 1.201412


#########################################################
# H2 -- Effect of the FFC on anti-immigration Rs
#########################################################

# Mean veracity score among those who got anti-immigration
# statements (and wanted to reduce immigration)
mean(anti_m2r$belief_o_T1) #  5.04

t_test <- t.test(diff_o ~ FactFreeReply,
                 data=subset(df, motivatedToReject == 1 &
                               gotConsCorrection==1))

t_test
# t = 6.4269, df = 566.15, p-value = 2.766e-10
# mean in group No 1.954315
# mean in group Yes 1.222494 
# 1.954315 - 1.222494 # 0.731821


#########################################################
# H2 -- Effect of the FFC on pro-immigration Rs
#########################################################

# Mean veracity score among those who got anti-immigration
# statements (and wanted to reduce immigration)
mean(pro_m2r$belief_o_T1) # 4.744552

t_test <- t.test(diff_o ~ FactFreeReply,
                 data=subset(df, motivatedToReject == 1 &
                               gotLibCorrection==1))

t_test
# t = 2.2037, df = 308.26, p-value = 0.02828
# mean in group No 1.50237
# mean in group Yes 1.15935 
# 1.50237 - 1.15935 # 0.34302


#########################################################
# -- MODERATOR: SOURCE 
#########################################################

df %>%
  filter(
    motivatedToReject == 1 
  ) %>%
  group_by(
    gotWhichCorrection,
    FactFreeSource
  ) %>%
  summarise(
    n=n(),
    mean_T1 = mean(belief_o_T1, na.rm = T),
    #sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    #sd_T2 = sd(belief_o_T2, na.rm = T),
    t1_t2 = mean_T1 - mean_T2,
    mean_diff = mean(diff_o, na.rm=T)
    #sd_diff=sd(diff_o, na.rm=T)
  )

# Anti-immigration people were more responsive to a professor
# than a blogger
1.3-1.14 # 0.16

# Pro-immigration people were more responsive to a blogger
# than a professor.
1.26-1.06 # 0.2

# big diff --> correction was effective at 
# reducing misperceptions

# small diff --> correction was less effective 
# / post-truth comment was effective


# ANTI-IMMIG 
t.test(subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeSource == "Blogger"), 
       subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeSource == "Professor"))

# t = -1.7718, df = 1224.3, p-value = 0.07668
# mean of x 1.136895 # Blogger
# mean of y 1.304140 # Professor

# PRO-IMMIG
t.test(subset(df$diff_o, df$gotWhichCorrection=="pro-immigration" & df$motivatedToReject==1 & df$FactFreeSource == "Blogger"), 
       subset(df$diff_o, df$gotWhichCorrection=="pro-immigration" & df$motivatedToReject==1 & df$FactFreeSource == "Professor"))

# t = 1.5161, df = 611.92, p-value = 0.13
# mean of x 1.257143
# mean of y 1.056667 
# 0.9771574 - 0.6112469 # 0.3659105


#########################################################
# -- MODERATOR: CONTENT 
#########################################################

df %>%
  filter(
    motivatedToReject == 1
  ) %>%
  group_by(
    gotWhichCorrection,
    FactFreeMessage
  ) %>%
  summarise(
    n=n(),
    mean = mean(diff_new, na.rm=T),
    sd=sd(diff_new, na.rm=T)
  )


# Check if any of these 3 re-corrections produce a 
# significantly lower diff than no re-correction: 

# Any difference between Biased stats & None?
t.test(subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "Biased stats"), 
       subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "None"))
# t = -6.2085, df = 745.99, p-value = 8.876e-10
# mean of x (Biased stats) 1.141809
# mean of y (None) 1.954315

# Any difference between Personal experience & None?
t.test(subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "Personal experience"), 
       subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "None"))
# t = -5.3099, df = 765.33, p-value = 1.439e-07
# mean of x (Personal experience) 1.248210
# mean of y (None) 1.954315

# Any difference between OK to disagree & None?
t.test(subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "OK to disagree"), 
       subset(df$diff_o, df$gotConsCorrection==1 & df$motivatedToReject==1 & df$FactFreeMessage == "None"))
# t = -5.1103, df = 750.57, p-value = 4.084e-07
# mean of x (OK to disagree) 1.278195
# mean of y (None) 1.954315



# NB: If we leave the not m2r out then biased stats is the 
# most successful one
t.test(subset(df$diff, df$FactFreeMessage == "Biased stats"), 
       subset(df$diff, df$FactFreeMessage %in% c("OK to disagree", "Personal experience")))



#########################################################
# -- EFFECTS FOR POPULIST MALES
#########################################################

# Did populist people in the control group change their 
# opinions at all? -- YES! 

df %>%
  filter(motivatedToReject == 1 &
           RandomGrp %in% c(1,2) &
           complete.cases(gender)
  ) %>%
  group_by(
    pop_quartile, 
    FactFreeReply # Reply
    # gender
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T),
    #sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    #sd_T2 = sd(belief_o_T2, na.rm = T),
    mean_diff = mean(diff_o, na.rm=T),
    sd_diff = sd(diff_o, na.rm=T)
  )


# Did populists react more strongly to a PT blogger than a prof?

df %>%
  filter(motivatedToReject == 1 &
           complete.cases(gender)
  ) %>%
  group_by(
    pop_quartile, 
    FactFreeSource # Reply
    # gender
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T),
    #sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    #sd_T2 = sd(belief_o_T2, na.rm = T),
    mean_diff = mean(diff_o, na.rm=T)
    #sd_diff = sd(diff_o, na.rm=T)
  )

# Are populist men the worst? -- YES! diff=1 point

df %>%
  filter(motivatedToReject == 1 &
           complete.cases(gender)
  ) %>%
  group_by(
    pop_quartile, 
    FactFreeReply, 
    gender
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T),
    #sd_T1 = sd(belief_o_T1, na.rm = T),
    mean_T2 = mean(belief_o_T2, na.rm = T),
    #sd_T2 = sd(belief_o_T2, na.rm = T),
    mean_diff = mean(diff_o, na.rm=T)
    #sd_diff = sd(diff_o, na.rm=T)
  )


#########################################################
# H2 -- Effect of the FFC on perceived accuracy
#########################################################

# Mean veracity score among those who got anti-immigration
# statements (and wanted to reduce immigration)
mean(df$accurateAuthority_num) #  5.04

t.test(accurateAuthority_num ~ FactFreeReply, data=df)

t.test(accurateAuthority_num ~ FactFreeReply, 
       data=subset(df, motivatedToReject == 1 &
                     gotConsCorrection==1))

t.test(accurateAuthority_num ~ FactFreeReply,
       data=subset(df, motivatedToReject == 1 &
                     gotLibCorrection==1))


#########################################################
# H2 -- Effect of the FFC on trust
#########################################################

# Mean veracity score among those who got anti-immigration
# statements (and wanted to reduce immigration)
mean(df$trustAuthority_num) #  5.04

t.test(trustAuthority_num ~ FactFreeReply, data=df)

t.test(trustAuthority_num ~ FactFreeReply, 
       data=subset(df, motivatedToReject == 1 &
                     gotConsCorrection==1))

t.test(trustAuthority_num ~ FactFreeReply,
       data=subset(df, motivatedToReject == 1 &
                     gotLibCorrection==1))




#########################################################
# H3a -- SPILLOVER EFFECTS I 
#########################################################

# Does the factual correction spill over to belief in 
# other, uncorrected false claims? 
# Looking at people who got the correction only (no further
# comments): Did they become more/less certain about their 
# other misperceptions?
# 2 mechanisms:
# i. if my thing isnt true then the other thing may not be true either... <- I become more suspicious
# ii. if my thing isnt true then the other thing must be true ... 
# ... I need to be right about SOMETHING

mean(ctrl_m2r$belief_i2_T1) # 4.61
mean(ctrl_m2r$belief_i2_T2) # 4.59

mean(ctrl$diff_i2_o) # 0.02 -- almost no change


# But... look at those who got 2 things wrong!!

df %>%
  filter(wrong_T1 == 1 & 
           wrong_i2_T1 == 1 &
           RandomGrp %in% c(1,2) &
           motivatedToReject == 1
  ) %>%
  group_by(
    gotWhichCorrection
    # pop_quartile # the more populist the less spill-over
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_i2_T1, na.rm = T), 
    #sd_T1 = sd(belief_o_i2_T1, na.rm = T), 
    mean_T2 = mean(belief_o_i2_T2, na.rm = T), 
    #sd_T2 = sd(belief_o_i2_T2, na.rm = T), 
    mean_diff = mean(diff_i2_o, na.rm=T),
    #sd_diff = sd(diff_i2_o, na.rm=T),
    n = n()
  )

#########################################################
# Paired t-test (before & after) ... POOLED 
#########################################################

# Running a paired t-test on a subset of people who 
# got both false facts wrong at T1

head(df_long[ , c("user_id", 
                  "belief_o", 
                  "belief_o_i2", 
                  "belief_i3", 
                  "belief_i4",
                  "time",
                  "wrong",
                  "wrong_i2")])

# I want to filter out people who get belief_o and 
# belief_o_i2 wrong at time T1. --> Creating a variable
# that counts the number of wrongs on those 2 variables.

df_long$wrong <- as.numeric(as.character(df_long$wrong))
df_long$wrong_i2 <- as.numeric(as.character(df_long$wrong_i2))

x <-
  df_long %>% 
  # filter(time == "T1") %>% 
  # --> this works but it removes T2s from the df
  # --> could do that & merge again with data... 
  # --> ... seems complicated. 
  group_by(time, user_id) %>%
  mutate(
    n_wrong = sum(wrong, wrong_i2)
  )

head(x[ , c("user_id", 
                  "belief_o", 
                  "belief_o_i2", 
                  "belief_i3", 
                  "belief_i4",
                  "time",
                  "wrong",
                  "wrong_i2",
                  "n_wrong"
            )])

# Nice but not helpful... (This counts the number
# of wrongs at time T1 and T2 -- I want a variable
# that = 2 (or something) when they get the 2 things
# wrong at T1)

# New approach: Create a dummy variable that is 1 
# if belief_o & belief_o_i2 > 3 at time T1. 

df_long$bothWrongT1 <- ifelse(df_long$time == "T1" &
                              df_long$belief_o > 3 & 
                              df_long$belief_o_i2 > 3, 1, 0)

head(df_long[ , c("user_id", 
            "belief_o", 
            "belief_o_i2", 
            "belief_i3", 
            "belief_i4",
            "time",
            "wrong",
            "wrong_i2",
            "bothWrongT1"
)])

# Nice -- now I want the same value at T2

df_long <-
  df_long %>% 
  #filter(time == "T1") %>%
  group_by(user_id) %>%
  mutate(
    bothWrongT1 = sum(bothWrongT1)
  )

head(df_long[ , c("user_id", 
                  "belief_o", 
                  "belief_o_i2", 
                  "belief_i3", 
                  "belief_i4",
                  "time",
                  "wrong",
                  "wrong_i2",
                  "bothWrongT1"
)])

# --> Perfetto! 
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   df_long$bothWrongT1 == 1 &
                   df_long$RandomGrp %in% c(1,2) & 
                   df_long$motivatedToReject==1), 
       paired = TRUE)

# t = 5.1282, df = 302, p-value = 5.237e-07
# mean of the differences 0.330033 
# 95 percent confidence interval:  0.2033889 0.4566771


#########################################################
# Paired t-test (broken up by pro and anti) -- USE THIS!
#########################################################

# Anti-immigration -- YES
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   df_long$bothWrongT1 == 1 &
                   df_long$RandomGrp %in% c(1,2) & 
                   df_long$corrected %in% c("noAsylum", "costImmig") &
                   df_long$motivatedToReject==1), 
       paired = TRUE)
# t = 3.7525, df = 206, p-value = 0.0002277
# mean of the differences 0.2801932
# 95 percent confidence interval: 0.1329798 0.4274067


# Pro-immigration... YES
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   df_long$bothWrongT1 == 1 &
                   df_long$RandomGrp %in% c(1,2) & 
                   df_long$gotLibCorrection == 1 &
                     #df_long$corrected %in% c("whiteCrime", "lowPaid") &
                   df_long$motivatedToReject==1), 
       paired = TRUE)
# t = 3.5359, df = 95, p-value = 0.0006303
# mean of the differences 0.4375
# 95 percent confidence interval: 0.1918597 0.6831403




#########################################################
# H3b -- SPILLOVER EFFECTS II 
#########################################################

# Does the FFC spill over to increase belief in unrelated 
# claims? -- Looking at the treated: Did they become more 
# suspicious about the other facts? 

df %>%
  filter(wrong_T1 == 1 & 
         wrong_i2_T1 == 1 &
         motivatedToReject == 1
  ) %>%
  group_by(
    gotWhichCorrection, 
    FactFreeReply 
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_i2_T1, na.rm = T), 
    #sd_T1 = sd(belief_o_i2_T1, na.rm = T), 
    mean_T2 = mean(belief_o_i2_T2, na.rm = T), 
    #sd_T2 = sd(belief_o_i2_T2, na.rm = T), 
    mean_diff = mean(diff_i2_o, na.rm=T),
    #sd_diff = sd(diff_i2_o, na.rm=T),
    n = n()
  )

# Side note -- If you look at a subset of ALL m2r 
# (i.e. not just those who got 2 things wrong): 
# --> Those who did NOT see the FFC changed beliefs on 
# the other item by 0.02 / 0.01 points 
# --> no spillover effects of the initial expert correction.
# Those who saw the FFC did change their beliefs, 
# but only by -0.117 / -0.193 points.
# --> Spillover effects, but in the wrong direction: The FFC 
# made them rate the other false fact as TRUER the 2nd time 
# around. 
# --> In the following analyses, I am excluding those who 
# got the other thing right to begin with. (We are not 
# interested in how fact-checks affect correct beliefs.)


# If you look at a subset of only those who believed in 
# 2 false facts -- USE THIS! 
# Clear spillover effects of the expert statement: 
# difference between T1 and T2 ratings is 0.33 points 
# (anti: 0.280 points, pro: 0.438 points). 
# The FFC un-did some of that effect: Those who saw the FFC 
# did still change their beliefs, but only by 0.229 points
# (anti: 0.206, pro: 0.285 points). 
# --> no evidence that the FFC made them rate the other 
# false fact as falser the 2nd time around: they actually 
# still rated it as truer the second time around -- which 
# speaks to a strong spillover effect of the expert correction!)
# --> so the FFC un-did 0.330-0.229 = 0.101 points of the 
# spillover effect of the first comment! 
# (anti: 0.280-0.206 = 0.07 points // 
# pro: 0.438-0.285 = 0.153 points) 

#########################################################
# Two t-tests... POOLED DATA 
#########################################################

# a) Did the treated change their opinions on item 2?
# (Same model as above, only changed control to treated)
t.test(belief_o_i2 ~ time,
       data=subset(df_long,
                   bothWrongT1 == 1 &
                   RandomGrp %in% c(3, 8) & 
                   motivatedToReject==1), 
       paired = TRUE) 

# t = 4.661, df = 274, p-value = 4.919e-06
# mean of the differences 0.3018182
# 95 percent confidence interval: 0.1743395 0.4292969

# YES -- Mean difference is smaller among those who 
# got the FFC than among those who didn't. 
# --> The FFC reduces some of the positive spillover effect 
# of the fact-check. 


# b) Is the difference between how the treated & the control
# reacted to the fact-check significant? --> No!!
t.test(diff_i2_o ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 &
                   wrong_i2_T1 == 1 & 
                   motivatedToReject == 1))
# t = 1.3656, df = 507.47, p-value = 0.1727
# mean in group No 0.3300330
# mean in group Yes 0.2291196


#########################################################
# Two t-tests... (broken up by pro- and anti)
#########################################################

# a) Did the treated change their opinions on item 2?

# Pooled -- YES
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   df_long$bothWrongT1 == 1 &
                     df_long$RandomGrp %in% c(3, 8) & 
                     df_long$motivatedToReject==1), 
       paired = TRUE)
# t = 4.661, df = 274, p-value = 4.919e-06
# mean of the differences 0.3018182
# 95 percent confidence interval:  0.1743395 0.4292969

# Anti-immigration -- YES
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   df_long$bothWrongT1 == 1 &
                     df_long$RandomGrp %in% c(3, 8) & 
                     df_long$corrected %in% c("noAsylum", "costImmig") &
                     df_long$motivatedToReject==1), 
       paired = TRUE)
# t = 4.1575, df = 192, p-value = 4.845e-05
# mean of the differences 0.3264249
# 95 percent confidence interval: 0.1715623 0.4812875

# Funny -- the difference should be 0.206 (see dplyr table above)

# Pro-immigration -- YES
t.test(belief_o_i2 ~ time,
       data=subset(df_long, 
                   bothWrongT1 == 1 &
                   RandomGrp %in% c(3, 8) & 
                   corrected %in% c("whiteCrime","lowPaid") &
                   motivatedToReject==1), 
       paired = TRUE)
# t = 2.1296, df = 81, p-value = 0.03624
# mean of the differences 0.2439024
# 95 percent confidence interval: 0.01602624 0.47177864

# Funny... the difference should be 0.285.


# b) Is the difference between how the treated & the control
# reacted to the fact-check significant? ... NOPE

# Pooled -- NO
t.test(diff_i2_o ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 &
                   wrong_i2_T1 == 1 & 
                   motivatedToReject == 1))
# t = 1.3656, df = 507.47, p-value = 0.1727
# mean in group No 0.3300330
# mean in group Yes 0.2291196

# Anti-immigration -- NOPE
t.test(diff_i2_o ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 &
                   wrong_i2_T1 == 1 & 
                   corrected %in% c("noAsylum","costImmig") &
                   motivatedToReject == 1))
# t = 0.85723, df = 352.27, p-value = 0.3919
# mean in group No 0.2801932
# mean in group Yes 0.2063492

# Looking at the VERY m2r -- still not
t.test(diff_i2_o ~ FactFreeReply,
       data=subset(df,
                   wrong_T1 == 1 &
                     wrong_i2_T1 == 1 & 
                     corrected %in% c("noAsylum","costImmig") &
                     motivatedToReject == 1 & 
                     immigEcon_fac %in% c("Strongly disagree", "Disagree") &
                     immigNo123 == 1
                   ))

# Immigration is good for the British economy
df$immigEcon_fac %in% c("Strongly disagree", "Disagree")
df$immigNo123 == 1

# Pro-immigration --- NOPE
t.test(diff_i2_o ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 &
                   wrong_i2_T1 == 1 & 
                   corrected %in% c("whiteCrime","lowPaid") &
                   motivatedToReject == 1))
# t = 1.0789, df = 155.83, p-value = 0.2823
# mean in group No 0.4375000
# mean in group Yes 0.2851562





#########################################################
# H2 -- OLS Models
#########################################################

#########################################################
# H2 -- Effect of FFC on diff
#########################################################

#########################################################
# LONG
#########################################################

# NB: Predictors of diff among Rs who were in favour 
# of immigration differed substantially from predictors 
# for those who were in favour of it. Therefore, I am  
# using different models for pro and anti-immigration Rs.

reg_long = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  
        immigFeelLikeHome_c + 
        populism_c +
        ethno_c + 
        PollAtt + # leave + # 
        university + # socialGrade_num +  
        gender + 
        age_c +
        FactFreeReply*leave + # immigNumbers_num + 
        FactFreeReply*immigNo123 +
        FactFreeReply*immigFeelLikeHome_c + 
        FactFreeReply*populism_c +
        FactFreeReply*ethno_c + 
        FactFreeReply*university +
        FactFreeReply*gender +
        FactFreeReply*age_c,      
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }

noAsylum_diff1 <- reg_long(noAsylum$diff_o, noAsylum)
noAsylum_trust1 <- reg_long(noAsylum$trustAuthority_num, noAsylum)
noAsylum_accurate1 <- reg_long(noAsylum$accurateAuthority_num, noAsylum)
noAsylum_factOpinion1 <- reg_long(noAsylum$factOpinion_num, noAsylum)
noAsylum_ok2disagree1 <- reg_long(noAsylum$factOpinion_num, noAsylum)

costImmig_diff1 <- reg_long(costImmig$diff_o, costImmig)
costImmig_trust1 <- reg_long(costImmig$trustAuthority_num, costImmig)
costImmig_accurate1 <- reg_long(costImmig$accurateAuthority_num, costImmig)
costImmig_factOpinion1 <- reg_long(costImmig$factOpinion_num, costImmig)
costImmig_ok2disagree1 <- reg_long(costImmig$factOpinion_num, costImmig)

anti_diff1 <- reg_long(anti_m2r$diff_o, anti_m2r)
anti_trust1 <- reg_long(anti_m2r$trustAuthority_num, anti_m2r)
anti_accurate1 <- reg_long(anti_m2r$accurateAuthority_num, anti_m2r)
anti_factOpinion1 <- reg_long(anti_m2r$factOpinion_num, anti_m2r)
anti_ok2disagree1 <- reg_long(anti_m2r$factOpinion_num, anti_m2r)


whiteCrime_diff1 <- reg_long(whiteCrime$diff_o, whiteCrime)
whiteCrime_trust1 <- reg_long(whiteCrime$trustAuthority_num, whiteCrime)
whiteCrime_accurate1 <- reg_long(whiteCrime$accurateAuthority_num, whiteCrime)
whiteCrime_factOpinion1 <- reg_long(whiteCrime$factOpinion_num, whiteCrime)
whiteCrime_ok2disagree1 <- reg_long(whiteCrime$factOpinion_num, whiteCrime)

lowPaid_diff1 <- reg_long(lowPaid$diff_o, lowPaid)
lowPaid_trust1 <- reg_long(lowPaid$trustAuthority_num, lowPaid)
lowPaid_accurate1 <- reg_long(lowPaid$accurateAuthority_num, lowPaid)
lowPaid_factOpinion1 <- reg_long(lowPaid$factOpinion_num, lowPaid)
lowPaid_ok2disagree1 <- reg_long(lowPaid$factOpinion_num, lowPaid)

pro_diff1 <- reg_long(pro_m2r$diff_o, pro_m2r)
pro_trust1 <- reg_long(pro_m2r$trustAuthority_num, pro_m2r)
pro_accurate1 <- reg_long(pro_m2r$accurateAuthority_num, pro_m2r)
pro_factOpinion1 <- reg_long(pro_m2r$factOpinion_num, pro_m2r)
pro_ok2disagree1 <- reg_long(pro_m2r$factOpinion_num, pro_m2r)

#########################################################
# Have a look at the long models
#########################################################

summary(whiteCrime_diff1)
car::vif(whiteCrime_diff1)

summary(lowPaid_diff1)
car::vif(lowPaid_diff1)

stargazer(noAsylum_diff1,
          costImmig_diff1,
          no.space=TRUE,
          type="text")


summary(whiteCrime_diff1)
car::vif(whiteCrime_diff1)

summary(lowPaid_diff1)
car::vif(lowPaid_diff1)

stargazer(whiteCrime_diff1,
          lowPaid_diff1, 
          no.space=TRUE, 
          type="text") 


#########################################################
# SHORTER -- ANTI -- DIFF
#########################################################

reg_medium_anti_diff = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  # könnte man drin lassen
        immigFeelLikeHome_c + 
        populism_c +
        # ethno_c + 
        # PollAtt + # leave + # 
        # university + # socialGrade_num +  
        gender + 
        age_c +
        # FactFreeReply*leave + # immigNumbers_num + 
        # FactFreeReply*immigNo123 +
        FactFreeReply*immigFeelLikeHome_c + 
        FactFreeReply*populism_c + # could go
        # FactFreeReply*ethno_c + 
        # FactFreeReply*university +
        FactFreeReply*gender +
        FactFreeReply*age_c, 
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }


noAsylum_diff2 <- reg_medium_anti_diff(noAsylum$diff_o, noAsylum)
noAsylum_trust2 <- reg_medium_anti_diff(noAsylum$trustAuthority_num, noAsylum)
noAsylum_accurate2 <- reg_medium_anti_diff(noAsylum$accurateAuthority_num, noAsylum)

costImmig_diff2 <- reg_medium_anti_diff(costImmig$diff_o, costImmig)
costImmig_trust2 <- reg_medium_anti_diff(costImmig$trustAuthority_num, costImmig)
costImmig_accurate2 <- reg_medium_anti_diff(costImmig$accurateAuthority_num, costImmig)

anti_diff2 <- reg_medium_anti_diff(anti_m2r$diff_o, anti_m2r)
anti_trust2 <- reg_medium_anti_diff(anti_m2r$trustAuthority_num, anti_m2r)
anti_accurate2 <- reg_medium_anti_diff(anti_m2r$accurateAuthority_num, anti_m2r)

summary(noAsylum_diff2)
# car::vif(noAsylum_diff2)
summary(costImmig_diff2)
# car::vif(costImmig_diff2)

summary(anti_diff2)
# car::vif(anti_diff2)

stargazer(noAsylum_diff2, 
          costImmig_diff2,
          no.space=TRUE, 
          type="text") # text

# summary(noAsylum_diff1)
# summary(costImmig_diff1)
# summary(anti_diff1)

# summary(noAsylum_diff2)
# summary(costImmig_diff2)
# summary(anti_diff2) # oh vey -- FFC n.s.

# Adapting the shorter model for pro-immig peops

#########################################################
# SHORTER -- PRO -- DIFF
#########################################################

reg_medium_pro_diff = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  ##
        immigFeelLikeHome_c + 
        populism_c +
        ethno_c + 
        # PollAtt + # leave + # 
        university + #university + # socialGrade_num +  
        # gender + 
        # age_c +
        # FactFreeReply*leave + # immigNumbers_num + 
        FactFreeReply*immigNo123 + ##
        FactFreeReply*immigFeelLikeHome_c + 
        FactFreeReply*populism_c +
        FactFreeReply*ethno_c +
        FactFreeReply*university, ##
        # FactFreeReply*gender,
        # FactFreeReply*age_c,
      data=df)
  }

whiteCrime_diff2 <- reg_medium_pro_diff(whiteCrime$diff_o, whiteCrime)
whiteCrime_trust2 <- reg_medium_pro_diff(whiteCrime$trustAuthority_num, whiteCrime)

lowPaid_diff2 <- reg_medium_pro_diff(lowPaid$diff_o, lowPaid)
lowPaid_trust2 <- reg_medium_pro_diff(lowPaid$trustAuthority_num, lowPaid)

pro_diff2 <- reg_medium_pro_diff(pro_m2r$diff_o, pro_m2r)
pro_trust2 <- reg_medium_pro_diff(pro_m2r$trustAuthority_num, pro_m2r)

stargazer(whiteCrime_diff2, 
          lowPaid_diff2,
          #pro_diff2,
          no.space=TRUE, 
          type="text") # text

summary(pro_diff2)

# ACHTUNG -- immigFeelLikeHome_c has a positive
# effect among the whiteCrime folks 
# --> the more you feel crowded out the more you 
# change your mind on the crimes Q.
# and a negative effect among the lowPaid folks

# --> the more you feel crowded out the LESS you 
# change your mind on the low-paid Q. 
# --> this doesn't make sense! 

reg_short_pro_diff =
  function(dv, df){
    lm(
      dv ~
        FactFreeReply +
        # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        # immigNo123 +  ##
        immigFeelLikeHome_c +
        populism_c +
        ethno_c +
        # PollAtt + # leave + #
        # university + #university + # socialGrade_num +
        # gender +
        # age_c +
        # FactFreeReply*leave + # immigNumbers_num +
        # FactFreeReply*immigNo123 + ##
        FactFreeReply*immigFeelLikeHome_c +
        FactFreeReply*populism_c,
      # FactFreeReply*ethno_c,
      # FactFreeReply*university + ##
      # FactFreeReply*gender +
      # FactFreeReply*age_c
      data=df)
  }

pro_diff3 <- reg_short_pro_diff(pro_m2r$diff_o, pro_m2r)
summary(pro_diff3) ## USE THIS!!


#########################################################
# Create stargazer function
#########################################################

my_star = 
  function(model1, 
           model2,
           model3,
           model4,
           model5,
           model6,
           title, 
           dep.var.labels, 
           column.labels,
           type){
    stargazer(model1, 
              model2, 
              model3,
              model4,
              model5,
              model6,
              title=title, 
              align=TRUE, 
              dep.var.labels=dep.var.labels, 
              column.labels=column.labels,
              covariate.labels=c("fact free comment",
                                 "voted to leave", #"curb immigration",
                                 "important issue",
                                 "feel like home",
                                 "populist (centred)",
                                 "nationalist (centred)",
                                 "low pol. attention",
                                 "high pol. attention",
                                 "university",
                                 "female",
                                 "age (centred)",
                                 "FFC:leave",
                                 "FFC:important issue",
                                 "FFC:feel like home",
                                 "FFC:populist",
                                 "FFC:nationalist",
                                 "FFC:university",
                                 "FFC:female",
                                 "FFC:age",
                                 "Constant"
              ),
              font.size = "small",
              omit.stat=c("ser","f"), # omit residual standard error & f-statistic
              no.space=TRUE, 
              # single.row = TRUE,
              column.sep.width = "-15pt",
              type=type
    )
  }

#########################################################
# Call stargazer function -- diff
#########################################################

my_star(model1=noAsylum_diff1, 
        model2=noAsylum_diff2, 
        model3=costImmig_diff1,
        model4=costImmig_diff2,
        model5=anti_diff1,
        model6=anti_diff2,
        title="Effect of FFC on responsiveness to expert information (anti-immigration claims)",
        dep.var.labels = "Difference pre and post-correction veracity scores",
        column.labels=c("asylum seekers", "", "cost/benefits", "", "pooled", ""),
        type="latex"  # type="text"
) 

my_star(model1=whiteCrime_diff1, 
        model2=whiteCrime_diff2, 
        model3=lowPaid_diff1,
        model4=lowPaid_diff2,
        model5=pro_diff2,
        model6=pro_diff3,
        title="Effect of FFC on responsiveness to expert information (pro-immigration claims)",
        dep.var.labels = "Difference pre and post-correction veracity scores",
        column.labels=c("white crime", "", "low-paid wages", "", "pooled", ""),
        type="latex"  # type="text"
)



#########################################################
# H2 -- Effect of FFC on accuracy & trust ratings
#########################################################

#########################################################
# SHORTER -- ANTI -- ACCURACY
#########################################################

reg_medium_anti_acc = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  # könnte man drin lassen
        immigFeelLikeHome_c + 
        populism_c +
        # ethno_c + 
        PollAtt + # leave + # 
        # university + # socialGrade_num +  
        gender + 
        age_c +
        # FactFreeReply*leave + # immigNumbers_num + 
        # FactFreeReply*immigNo123 +
        FactFreeReply*immigFeelLikeHome_c,
        # FactFreeReply*populism_c, # could go
        # FactFreeReply*ethno_c + 
        # FactFreeReply*university +
        # FactFreeReply*gender +
        # FactFreeReply*age_c, 
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }

noAsylum_accurate2 <- reg_medium_anti_acc(noAsylum$accurateAuthority_num, noAsylum)
noAsylum_trust2 <- reg_medium_anti_acc(noAsylum$trustAuthority_num, noAsylum)

costImmig_accurate2 <- reg_medium_anti_acc(costImmig$accurateAuthority_num, costImmig)
costImmig_trust2 <- reg_medium_anti_acc(costImmig$trustAuthority_num, costImmig)

anti_accurate2 <- reg_medium_anti_acc(anti_m2r$accurateAuthority_num, anti_m2r)
anti_trust2 <- reg_medium_anti_acc(anti_m2r$trustAuthority_num, anti_m2r)

summary(noAsylum_accurate2)
summary(costImmig_accurate2)
summary(anti_accurate2)

summary(noAsylum_trust1)
summary(costImmig_trust1)
summary(anti_trust1)

my_star(model1=noAsylum_accurate1, 
        model2=noAsylum_accurate2, 
        model3=costImmig_accurate1,
        model4=costImmig_accurate2,
        model5=anti_accurate2,
        title="Effect of FFC on accuracy ratings (anti-immigration claims)",
        dep.var.labels = "Perceived accuracy (higher values = more accurate)",
        column.labels=c("asylum seekers", "asylum seekers", "cost/benefits", "cost/benefits", "pooled"),
        type="latex" # type="text"
) 

my_star(model1=noAsylum_trust1, 
        model2=noAsylum_trust2, 
        model3=costImmig_trust1,
        model4=costImmig_trust2,
        model5=anti_trust2,
        title="Effect of FFC on accuracy ratings (anti-immigration claims)",
        dep.var.labels = "Trust (higher values = more trustworthy)",
        column.labels=c("asylum seekers", "asylum seekers", "cost/benefits", "cost/benefits", "pooled"),
        type="latex" # type="text"
) 


#########################################################
# SHORTER -- PRO -- ACCURACY
#########################################################

summary(whiteCrime_accurate1)
summary(lowPaid_accurate1)

summary(whiteCrime_trust1)
summary(lowPaid_trust1)

reg_medium_pro_acc = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        # immigNo123 +  # könnte man drin lassen
        # immigFeelLikeHome_c + 
        populism_c + 
        ethno_c +
        # PollAtt + # leave + # 
        # university + # socialGrade_num +  
        # gender + 
        # age_c, 
        # FactFreeReply*leave + # immigNumbers_num + 
        # FactFreeReply*immigNo123 +
        # FactFreeReply*immigFeelLikeHome_c,
        # FactFreeReply*populism_c + # could go
        FactFreeReply*ethno_c,
        # FactFreeReply*university +
        # FactFreeReply*gender +
        # FactFreeReply*age_c, 
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }

whiteCrime_accurate2 <- reg_medium_pro_acc(whiteCrime$accurateAuthority_num, whiteCrime)
whiteCrime_trust2 <- reg_medium_pro_acc(whiteCrime$trustAuthority_num, whiteCrime)

lowPaid_accurate2 <- reg_medium_anti_acc(lowPaid$accurateAuthority_num, lowPaid)
lowPaid_trust2 <- reg_medium_anti_acc(lowPaid$trustAuthority_num, lowPaid)

pro_accurate2 <- reg_medium_anti_acc(pro_m2r$accurateAuthority_num, pro_m2r)
pro_trust2 <- reg_medium_anti_acc(pro_m2r$trustAuthority_num, pro_m2r)

summary(whiteCrime_accurate2)
summary(whiteCrime_trust2)


my_star(model1=whiteCrime_accurate1, 
        model2=whiteCrime_accurate2, 
        model3=lowPaid_accurate1,
        model4=lowPaid_accurate2,
        model5=pro_accurate2,
        title="Effect of FFC on accuracy ratings (pro-immigration claims)",
        dep.var.labels = "Perceived accuracy (higher values = more accurate)",
        column.labels=c("white crime", "white crime", "low-paid wages", "low-paid wages", "pooled"),
        type="latex" # type="text"
)

my_star(model1=whiteCrime_trust1, 
        model2=whiteCrime_trust2, 
        model3=lowPaid_trust1,
        model4=lowPaid_trust2,
        model5=pro_trust2,
        title="Effect of FFC on trust (pro-immigration claims)",
        dep.var.labels = "Trust (higher values = more trustworthy)",
        column.labels=c("white crime", "white crime", "low-paid wages", "low-paid wages", "pooled"),
        type="latex" # type="text"
) 


#########################################################
# H2 -- Effect of FFC on fact/opinion ratings
#########################################################

#########################################################
# SHORTER -- ANTI -- FACT/OPINION
#########################################################

reg_medium_anti_opinion = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  # könnte man drin lassen
        immigFeelLikeHome_c, 
        # populism_c +
        # ethno_c + 
        # PollAtt + # leave + # 
        # university + # socialGrade_num +  
        # gender,
        # age_c,
        # FactFreeReply*leave + # immigNumbers_num + 
        # FactFreeReply*immigNo123 +
        #FactFreeReply*immigFeelLikeHome_c +
      # FactFreeReply*populism_c, # could go
      # FactFreeReply*ethno_c + 
      # FactFreeReply*university +
       #FactFreeReply*gender, 
      # FactFreeReply*age_c, 
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }

noAsylum_factOpinion2 <- reg_medium_anti_opinion(noAsylum$factOpinion_num, noAsylum)
costImmig_factOpinion2 <- reg_medium_anti_opinion(costImmig$factOpinion_num, costImmig)
anti_factOpinion2 <- reg_medium_anti_opinion(anti_m2r$factOpinion_num, anti_m2r)

summary(noAsylum_factOpinion2)
summary(costImmig_factOpinion2)
summary(anti_factOpinion2)

my_star(model1=noAsylum_factOpinion1, 
        model2=noAsylum_factOpinion2, 
        model3=costImmig_factOpinion1,
        model4=costImmig_factOpinion2,
        model5=anti_trust2,
        title="Effect of FFC on rating facts as opinions (anti-immigration claims)",
        dep.var.labels = "Scale from 0 (Purely a matter of fact) to 6 (purely a matter of opinion)",
        column.labels=c("asylum seekers", "asylum seekers", "cost/benefits", "cost/benefits", "pooled"),
        type="latex" # type="text"
) 


# Using the same for pro-immigration 
# (because in there nothing but feels like home
# is significant)

whiteCrime_factOpinion2 <- reg_medium_anti_opinion(whiteCrime$factOpinion_num, whiteCrime)
lowPaid_factOpinion2 <- reg_medium_anti_opinion(lowPaid$factOpinion_num, lowPaid)
pro_factOpinion2 <- reg_medium_anti_opinion(pro_m2r$factOpinion_num, pro_m2r)

summary(whiteCrime_factOpinion2)
summary(lowPaid_factOpinion2)
summary(pro_factOpinion2)

my_star(model1=whiteCrime_factOpinion1, 
        model2=whiteCrime_factOpinion2, 
        model3=lowPaid_factOpinion1,
        model4=lowPaid_factOpinion2,
        model5=pro_factOpinion2,
        title="Effect of FFC on rating facts as opinions (pro-immigration claims)",
        dep.var.labels = "Scale from 0 (Purely a matter of fact) to 6 (purely a matter of opinion)",
        column.labels=c("white crime", "white crime", "low-paid wages", "low-paid wages", "pooled"),
        type="latex" # type="text"
) 


#########################################################
# H2 -- Effect of FFC on ok2disagree ratings
#########################################################

reg_medium_anti_ok2disagree = 
  function(dv, df){
    lm( 
      dv ~
        FactFreeReply + 
        # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        # immigNo123 +  # könnte man drin lassen
        immigFeelLikeHome_c +
      # populism_c +
      # ethno_c + 
      # PollAtt + # leave + # 
       #university + # socialGrade_num +  
      # gender,
       age_c,
      # FactFreeReply*leave + # immigNumbers_num + 
      # FactFreeReply*immigNo123 +
      # FactFreeReply*immigFeelLikeHome_c, ## 
      # FactFreeReply*populism_c, # could go
      # FactFreeReply*ethno_c + 
       # FactFreeReply*university,
      #FactFreeReply*gender, 
       # FactFreeReply*age_c, 
      data=df) # subset(df, motivatedToReject == 1 & corrected == "noAsylum"))
  }

noAsylum_ok2disagree2 <- reg_medium_anti_ok2disagree(noAsylum$ok2disagree_num, noAsylum)
costImmig_ok2disagree2 <- reg_medium_anti_ok2disagree(costImmig$ok2disagree_num, costImmig)
anti_ok2disagree2 <- reg_medium_anti_ok2disagree(anti_m2r$ok2disagree_num, anti_m2r)

summary(noAsylum_ok2disagree2)
summary(costImmig_ok2disagree2)
summary(anti_ok2disagree2)

my_star(model1=noAsylum_ok2disagree1, 
        model2=noAsylum_ok2disagree2, 
        model3=costImmig_ok2disagree1,
        model4=costImmig_ok2disagree2,
        model5=anti_ok2disagree2,
        title="Effect of FFC on agreement that it is OK to disagree with the facts (anti-immigration claims)",
        dep.var.labels = "0='Strongly disagree', 2='Disagree', 3='Agree',4='Strongly Agree'",
        column.labels=c("asylum seekers", "asylum seekers", "cost/benefits", "cost/benefits", "pooled"),
        type="latex" # type="text"
) 

noAsylum_ok2disagree2 <- reg_medium_anti_ok2disagree(noAsylum$ok2disagree_num, noAsylum)
costImmig_ok2disagree2 <- reg_medium_anti_ok2disagree(costImmig$ok2disagree_num, costImmig)
anti_ok2disagree2 <- reg_medium_anti_ok2disagree(anti_m2r$ok2disagree_num, anti_m2r)

# FactFreeReplyYes:age_c
# FactFreeReplyYes:immigFeelLikeHome_c

whiteCrime_ok2disagree2 <- reg_medium_anti_ok2disagree(whiteCrime$ok2disagree_num, whiteCrime)
lowPaid_ok2disagree2 <- reg_medium_anti_ok2disagree(lowPaid$ok2disagree_num, lowPaid)
pro_ok2disagree2 <- reg_medium_anti_ok2disagree(pro_m2r$ok2disagree_num, pro_m2r)

summary(whiteCrime_ok2disagree2)
summary(lowPaid_ok2disagree2)
summary(pro_ok2disagree2)


my_star(model1=whiteCrime_ok2disagree1, 
        model2=whiteCrime_ok2disagree2, 
        model3=lowPaid_ok2disagree1,
        model4=lowPaid_ok2disagree2,
        model5=pro_ok2disagree2,
        title="Effect of FFC on agreement that it is OK to disagree with the facts (pro-immigration claims)",
        dep.var.labels = "0='Strongly disagree', 2='Disagree', 3='Agree',4='Strongly Agree'",
        column.labels=c("white crime", "white crime", "low-paid wages", "low-paid wages", "pooled"),
        type="latex" # type="text"
) 



#########################################################
# PLOT INTERACTION TERMS 
#########################################################

#########################################################
# INTERACTION TERMS -- ANTI-IMMIGRATION CLAIMS
#########################################################

# Re-creating anti_diff2

pooled_anti_diff = 
  lm( 
    diff_o ~
      FactFreeReply + 
      immigNo123 +  
      immigFeelLikeHome_c + 
      populism_c +
      gender + 
      age_c +
      FactFreeReply*immigFeelLikeHome_c + 
      FactFreeReply*populism_c + 
      FactFreeReply*gender + 
      # FactFreeReply*immigFeelLikeHome_c*gender +
      # FactFreeReply*populism_c*age_c + # p=0.06 -- very close !! FIRST THING post-dissertation
      # FactFreeReply*populism_c*gender, + # close !!
      FactFreeReply*age_c, ## 
    data=anti_m2r) 

summary(pooled_anti_diff)

pooled_anti_diff_test = 
  lm( 
    diff_o ~
      FactFreeSource + 
      immigNo123 +  
      immigFeelLikeHome_c + 
      populism_c +
      gender + 
      age_c +
      # FactFreeSource*gender*immigFeelLikeHome_c, 
      # FactFreeSource*populism_c + 
      FactFreeSource*gender*populism_c, 
      # FactFreeReply*immigFeelLikeHome_c*gender +
      # FactFreeReply*populism_c*age_c + # p=0.06 -- very close !! FIRST THING post-dissertation
      # FactFreeReply*populism_c*gender, + # close !!
      #FactFreeSource*age_c, ## 
    data=anti_m2r) 

summary(pooled_anti_diff_test)


#########################################################
# Categorical * Categorical (gender, leave)
#########################################################

library(effects)

effect_gender <- effect('FactFreeReply:gender', pooled_anti_diff)
effect_gender <- as.data.frame(effect_gender)
effect_gender

# effect_leave <- effect('FactFreeReply:leave', anti_diff)
# effect_leave <- as.data.frame(effect_leave)
# summary(effect_leave)

#########################################################
# FFC * Gender
#########################################################

inter_plot_gender <-
  ggplot(data=effect_gender, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=gender))+
  geom_line(size=2, 
            aes(
              linetype=gender
              # color=gender
              ))+
  geom_ribbon(aes(ymin=fit-se, 
                  ymax=fit+se#,
                  #fill=gender
                  ),
              alpha=.2)+
  theme(legend.position="top") +
  labs(title = "The effect of the FFC depending on gender", 
       # subtitle = "", 
       caption = "\nModel 5 (Anti-immigration statements)",
       y="diff",
       x="Fact-Free Comment") +
 theme_bw()
  # theme(text = element_text(size=12),
  #       legend.text = element_text(size=12),
  #       legend.direction = "horizontal",
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       legend.position="top"
  #       )

print(inter_plot_gender)
ggsave("R/graphs/inter_plot_gender.png")


#########################################################
# Categorical * continuous
#########################################################

#########################################################
# FFC * populism_c
#########################################################

# Make your new IQ variable that asks for quantiles  
pop_quintile <- quantile(df$populism_c, 
                        probs=c(0,
                                .25, 
                                .50,
                                .75,
                                1))
pop_quintile <- round(pop_quintile, 2)
pop_quintile

# Have a look at the scaled populism variable
hist(df$populism_c)
min(df$populism_c)
max(df$populism_c)


# Run your interaction
effect_pop <- effect('FactFreeReply:populism_c', 
                     pooled_anti_diff,
                     xlevels=list(populism_c = c(-2.69,
                                                 -0.44,
                                                 0.06,
                                                 0.56,
                                                 1.31),
                                  FactFreeReply = c("No",
                                                    "Yes")),
                         se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_pop <- as.data.frame(effect_pop)
effect_pop

FourGreys <- c("#969696", "#737373", "#525252", "#252525")
FiveGreys <- c("#bdbdbd", "#969696", "#737373", "#525252", "#252525")
# FiveGreys <- c("#252525", "#525252", "#737373", "#969696","#bdbdbd")


# Create factors of the different variables in your interaction: 
# ONLY RUN THIS ONCE!!!
effect_pop$populism_c <- 
  factor(effect_pop$populism_c,
         levels=c(-2.69, -0.44, 0.06, 0.56, 1.31),
         labels=c("0%", "25%", "50%", "75%",  "100%"))

# Plot
inter_plot_pop <-
  ggplot(data=effect_pop, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=populism_c))+
  geom_line(size=2, 
            aes(color=populism_c))+
  labs(title = "The effect of the FFC depending on populist predispositions", 
       # subtitle = "", 
       caption = "\nModel 5 (Anti-immigration statements)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FiveGreys)+ #custom color coding 
  theme_bw() #deleting the gray background 
  # theme(text = element_text(family="Arial", 
  #                           size=14, 
  #                           color="black")) #changing font!
  # theme(text = element_text(size=12),
  #       legend.text = element_text(size=12),
  #       legend.direction = "horizontal",
  #       panel.grid.major = element_blank(),
  #       panel.grid.minor = element_blank(),
  #       legend.position="top") 


print(inter_plot_pop)
ggsave("R/graphs/inter_plot_pop.png")


#########################################################
# FFC * feels like home
#########################################################

# https://ademos.people.uic.edu/Chapter13.html

head(df$immigFeelLikeHome)
head(df$immigFeelLikeHome_num)
head(df$immigFeelLikeHome_c)
table(df$immigFeelLikeHome_c)

# 1 = Strongly disagree = -1.4908569 --> most pro-immig
# 4 = Strongly agree = 1.5091431 --> most anti-immig

# Run interaction
temp <- effect('FactFreeReply:immigFeelLikeHome_c', 
               pooled_anti_diff,
               xlevels=list(immigFeelLikeHome_c = 
                              c(-1.4908569379706,
                                -0.490856937970599,
                                0.509143062029401,
                                1.5091430620294),
                            FactFreeReply = c("No",
                                              "Yes")),
               se=TRUE, 
               confidence.level=.95, 
               typical=mean)

#Put data into data frame
temp <- as.data.frame(temp)
temp

# Create factor variable
temp$home <- NA
temp$home[temp$immigFeelLikeHome_c < -1.40] <- "Strongly disagree"
temp$home[temp$immigFeelLikeHome_c > -1.40 & temp$immigFeelLikeHome_c < 0.5] <- "Disagree"
temp$home[temp$immigFeelLikeHome_c > 0.5 & temp$immigFeelLikeHome_c < 1.50] <- "Agree"
temp$home[temp$immigFeelLikeHome_c > 1.50] <- "Strongly agree"

temp$home <- factor(temp$home,
         levels=c("Strongly disagree", "Disagree", 
                  "Agree", "Strongly agree"))

temp


# Plot
inter_plot_home <-
  ggplot(data=temp, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=home))+
  geom_line(size=2, 
            aes(color=home))+
  labs(title = "The effect of the FFC depending on feeling at home", 
       subtitle = "'There are so many foreigners round here that it doesn't feel like home anymore.'", 
       caption = "\nModel 5 (Anti-immigration statements)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FourGreys) + # custom color coding 
 theme_bw()  #deleting the gray background 

print(inter_plot_home)
ggsave("R/graphs/inter_plot_home.png")




#########################################################
# INTERACTION TERMS -- PRO-IMMIGRATION CLAIMS
#########################################################

# Re-creating pro_diff3

pooled_pro_diff = 
    lm( 
      diff_o ~
        FactFreeReply + 
        # leave + # immigNumbers_num + # leave + # EUVote
        # immigNo123 +  ##
        immigFeelLikeHome_c + 
        populism_c + ##
        ethno_c + 
        # PollAtt + # leave + # 
        # university + #university + # socialGrade_num +  
        # gender + 
        # age_c +
        # FactFreeReply*immigNumbers_num + #leave + # immigNumbers_num + 
        # FactFreeReply*immigNo123 + ##
        FactFreeReply*immigFeelLikeHome_c + 
        FactFreeReply*populism_c, 
        # FactFreeReply*ethno_c,
        # FactFreeReply*university, ##
        # FactFreeReply*gender,
        # FactFreeReply*age_c,
      data=pro_m2r)

# pooled_pro_diff <- reg_short_pro_diff(pro_m2r$diff_o, pro_m2r)
summary(pooled_pro_diff) ## USE THIS!!

# NEW NEW NEW -- FIRST THING POST-DISSERTATION!!

pro_diff4 = 
  lm( 
    diff_o ~
      FactFreeReply + 
      # leave + # immigNumbers_num + # leave + # EUVote
      # immigNo123 +  ##
      immigFeelLikeHome_c + 
      populism_c + ##
      ethno_c + 
      # PollAtt + # leave + # 
      # university + #university + # socialGrade_num +  
      # gender + 
      # age_c +
      # FactFreeReply*immigNumbers_num + #leave + # immigNumbers_num + 
      # FactFreeReply*immigNo123 + ##
      FactFreeReply*immigFeelLikeHome_c + 
      # FactFreeReply*populism_c, 
      # FactFreeReply*ethno_c,
      # FactFreeReply*university, ##
      # FactFreeReply*gender,
      # FactFreeReply*age_c,
      # FactFreeReply*populism_c*age_c + 
      FactFreeReply*populism_c*gender, # close !!
    data=pro_m2r)

summary(pro_diff4) ## USE THIS!!

#########################################################
# FFC * populism_c
#########################################################

# Make your new IQ variable that asks for quantiles  
pop_quintile <- quantile(df$populism_c, 
                         probs=c(0,
                                 .25, 
                                 .50,
                                 .75,
                                 1))
pop_quintile <- round(pop_quintile, 2)
pop_quintile 

# Run your interaction
effect_pro_pop <- effect('FactFreeReply:populism_c', 
                         pooled_pro_diff,
                     xlevels=list(populism_c = c(-2.69,
                                                 -0.44,
                                                 0.06,
                                                 0.56,
                                                 1.31),
                                  FactFreeReply = c("No",
                                                    "Yes")),
                     se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_pro_pop <- as.data.frame(effect_pro_pop)
effect_pro_pop


# Create factors of the different variables in your interaction: 
# ONLY RUN THIS ONCE!!!
effect_pro_pop$populism_c <- 
  factor(effect_pro_pop$populism_c,
         levels=c(-2.69, -0.44, 0.06, 0.56, 1.31),
         labels=c("0%", "25%", "50%", "75%",  "100%"))

# Plot
inter_plot_pro_pop <-
  ggplot(data=effect_pro_pop, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=populism_c))+
  geom_line(size=2, 
            aes(color=populism_c))+
  labs(title = "The effect of the FFC depending on populist predispositions", 
       # subtitle = "", 
       caption = "\nModel 6 (Pro-immigration statements)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FiveGreys)+ #custom color coding 
  theme_bw() #deleting the gray background 
# theme(text = element_text(family="Arial", 
#                           size=14, 
#                           color="black")) #changing font!
# theme(text = element_text(size=12),
#       legend.text = element_text(size=12),
#       legend.direction = "horizontal",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position="top") 


print(inter_plot_pro_pop)
ggsave("R/graphs/inter_plot_pro_pop.png")


#########################################################
# INTERACTION TERMS -- INDIVIDUAL PRO-IMMIGRATION CLAIMS
#########################################################

# ACHTUNG: Because I get different results for 
# whiteCrime and lowPaid I will make 2 intearaction 
# plots

stargazer(whiteCrime_diff2, 
          lowPaid_diff2,
          #pro_diff2,
          no.space=TRUE, 
          type="text") # text

# Re-creating whiteCrime_diff2 

# whiteCrime_diff2 <- reg_medium_pro_diff(whiteCrime$diff_o, whiteCrime)
# summary(whiteCrime_diff2)

whiteCrime_diff2 = 
  lm( 
    diff_o ~
      FactFreeReply + 
      # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  ##
      immigFeelLikeHome_c + 
      populism_c +
      ethno_c + 
      # PollAtt + # leave + # 
      university + #university + # socialGrade_num +  
      # gender + 
      # age_c +
      # FactFreeReply*leave + # immigNumbers_num + 
      FactFreeReply*immigNo123 + ##
      FactFreeReply*immigFeelLikeHome_c + 
      FactFreeReply*populism_c +
      FactFreeReply*ethno_c +
      FactFreeReply*university, ##
    # FactFreeReply*gender,
    # FactFreeReply*age_c,
    data=whiteCrime)

summary(whiteCrime_diff2)

# Plot interaction for feels like home
# populist and nationalist

#########################################################
# FFC * populism_c
#########################################################

# Make your new IQ variable that asks for quantiles  
pop_quintile <- quantile(df$populism_c, 
                         probs=c(0,
                                 .25, 
                                 .50,
                                 .75,
                                 1))
pop_quintile <- round(pop_quintile, 2)
pop_quintile 

# Have a look at the scaled populism variable
hist(df$populism_c)
min(df$populism_c)
max(df$populism_c)


# Run your interaction
effect_pop <- effect('FactFreeReply:populism_c', 
                     whiteCrime_diff2,
                     xlevels=list(populism_c = c(-2.69,
                                                 -0.44,
                                                 0.06,
                                                 0.56,
                                                 1.31),
                                  FactFreeReply = c("No",
                                                    "Yes")),
                     se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_pop <- as.data.frame(effect_pop)
effect_pop

FourGreys <- c("#969696", "#737373", "#525252", "#252525")
FiveGreys <- c("#bdbdbd", "#969696", "#737373", "#525252", "#252525")
# FiveGreys <- c("#252525", "#525252", "#737373", "#969696","#bdbdbd")


# Create factors of the different variables in your interaction: 
# ONLY RUN THIS ONCE!!!
effect_pop$populism_c <- 
  factor(effect_pop$populism_c,
         levels=c(-2.69, -0.44, 0.06, 0.56, 1.31),
         labels=c("0%", "25%", "50%", "75%",  "100%"))

# Plot 
inter_plot_whiteCrime_pop <-
  ggplot(data=effect_pop, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=populism_c))+
  geom_line(size=2, 
            aes(color=populism_c))+
  labs(title = "The effect of the FFC depending on populist predispositions", 
       # subtitle = "", 
       caption = "\nModel 2 (white crime)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FiveGreys)+ #custom color coding 
  theme_bw() #deleting the gray background 
# theme(text = element_text(family="Arial", 
#                           size=14, 
#                           color="black")) #changing font!
# theme(text = element_text(size=12),
#       legend.text = element_text(size=12),
#       legend.direction = "horizontal",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position="top") 


print(inter_plot_whiteCrime_pop)
ggsave("R/graphs/inter_plot_whiteCrime_pop.png")


#########################################################
# FFC * ethno_c
#########################################################

# Make your new IQ variable that asks for quantiles  
ethno_quintile <- quantile(df$ethno_c, 
                         probs=c(0,
                                 .25, 
                                 .50,
                                 .75,
                                 1))
ethno_quantile <- round(ethno_quintile, 2)
ethno_quantile 

# Have a look at the scaled nationalism variable
hist(df$ethno_c)
min(df$ethno_c)
max(df$ethno_c)


# Run your interaction
effect_ethno <- effect('FactFreeReply:ethno_c', 
                     whiteCrime_diff2,
                     xlevels=list(ethno_c = c(-1.88,
                                                 -0.38,
                                                 0.12,
                                                 0.46,
                                                 2.12),
                                  FactFreeReply = c("No",
                                                    "Yes")),
                     se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_ethno <- as.data.frame(effect_ethno)
effect_ethno

FourGreys <- c("#969696", "#737373", "#525252", "#252525")
FiveGreys <- c("#bdbdbd", "#969696", "#737373", "#525252", "#252525")
# FiveGreys <- c("#252525", "#525252", "#737373", "#969696","#bdbdbd")


# Create factors of the different variables in your interaction: 
# ONLY RUN THIS ONCE!!!
effect_ethno$ethno_c <-
  factor(effect_ethno$ethno_c,
         levels=c(-1.88, -0.38, 0.12, 0.46, 2.12),
         labels=c("0%", "25%", "50%", "75%",  "100%"))

# Plot
inter_plot_whiteCrime_ethno <-
  ggplot(data=effect_ethno, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=ethno_c))+
  geom_line(size=2, 
            aes(color=ethno_c))+
  labs(title = "The effect of the FFC depending on nationalist predispositions", 
       # subtitle = "", 
       caption = "\nModel 2 (white crime)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FiveGreys)+ #custom color coding 
  theme_bw() #deleting the gray background 
# theme(text = element_text(family="Arial", 
#                           size=14, 
#                           color="black")) #changing font!
# theme(text = element_text(size=12),
#       legend.text = element_text(size=12),
#       legend.direction = "horizontal",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position="top") 


print(inter_plot_whiteCrime_ethno)
ggsave("R/graphs/inter_plot_whiteCrime_ethno.png")



#########################################################
# FFC * feels like home
#########################################################

# https://ademos.people.uic.edu/Chapter13.html

# 1 = Strongly disagree = -1.4908569 --> most pro-immig
# 4 = Strongly agree = 1.5091431 --> most anti-immig

# Run interaction
temp <- effect('FactFreeReply:immigFeelLikeHome_c', 
               whiteCrime_diff2,
               xlevels=list(immigFeelLikeHome_c = 
                              c(-1.4908569379706,
                                -0.490856937970599,
                                0.509143062029401,
                                1.5091430620294),
                            FactFreeReply = c("No",
                                              "Yes")),
               se=TRUE, 
               confidence.level=.95, 
               typical=mean)

#Put data into data frame
temp <- as.data.frame(temp)
temp

# Create factor variable
temp$home <- NA
temp$home[temp$immigFeelLikeHome_c < -1.40] <- "Strongly disagree"
temp$home[temp$immigFeelLikeHome_c > -1.40 & temp$immigFeelLikeHome_c < 0.5] <- "Disagree"
temp$home[temp$immigFeelLikeHome_c > 0.5 & temp$immigFeelLikeHome_c < 1.50] <- "Agree"
temp$home[temp$immigFeelLikeHome_c > 1.50] <- "Strongly agree"

temp$home <- factor(temp$home,
                    levels=c("Strongly disagree", "Disagree", 
                             "Agree", "Strongly agree"))

# Plot
inter_plot_whiteCrime_home <-
  ggplot(data=temp, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=home))+
  geom_line(size=2, 
            aes(color=home))+
  labs(title = "The effect of the FFC depending on feeling at home", 
       subtitle = "'There are so many foreigners round here that it doesn't feel like home anymore.'", 
       caption = "\nModel 2 (white crime)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FourGreys) + # custom color coding 
  theme_bw()  #deleting the gray background 

print(inter_plot_whiteCrime_home)
ggsave("R/graphs/inter_plot_whiteCrime_home.png")



#########################################################
# lowPaid
#########################################################

lowPaid_diff2 = 
  lm( 
    diff_o ~
      FactFreeReply + 
      # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  ##
      immigFeelLikeHome_c + 
      populism_c +
      ethno_c + 
      # PollAtt + # leave + # 
      university + #university + # socialGrade_num +  
      # gender + 
      # age_c +
      # FactFreeReply*leave + # immigNumbers_num + 
      FactFreeReply*immigNo123 + ##
      FactFreeReply*immigFeelLikeHome_c + 
      FactFreeReply*populism_c +
      FactFreeReply*ethno_c +
      FactFreeReply*university, ##
    # FactFreeReply*gender,
    # FactFreeReply*age_c,
    data=lowPaid)

summary(lowPaid_diff2)

#########################################################
# FFC * ethno_c
#########################################################

# Run your interaction
effect_ethno <- effect('FactFreeReply:ethno_c', 
                       lowPaid_diff2,
                       xlevels=list(ethno_c = c(-1.88,
                                                -0.38,
                                                0.12,
                                                0.46,
                                                2.12),
                                    FactFreeReply = c("No",
                                                      "Yes")),
                       se=TRUE, 
                       confidence.level=.95, 
                       typical=mean)

#Put data into data frame
effect_ethno <- as.data.frame(effect_ethno)
effect_ethno

# Create factors of the different variables in your interaction: 
# ONLY RUN THIS ONCE!!!
effect_ethno$ethno_c <-
  factor(effect_ethno$ethno_c,
         levels=c(-1.88, -0.38, 0.12, 0.46, 2.12),
         labels=c("0%", "25%", "50%", "75%",  "100%"))

# Plot
inter_plot_lowPaid_ethno <-
  ggplot(data=effect_ethno, 
         aes(x=FactFreeReply, 
             y=fit, 
             group=ethno_c))+
  geom_line(size=2, 
            aes(color=ethno_c))+
  labs(title = "The effect of the FFC depending on nationalist predispositions", 
       # subtitle = "", 
       caption = "\nModel 4 (low paid)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=FiveGreys)+ #custom color coding 
  theme_bw() #deleting the gray background 
# theme(text = element_text(family="Arial", 
#                           size=14, 
#                           color="black")) #changing font!
# theme(text = element_text(size=12),
#       legend.text = element_text(size=12),
#       legend.direction = "horizontal",
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       legend.position="top") 


print(inter_plot_lowPaid_ethno)
ggsave("R/graphs/inter_plot_lowPaid_ethno.png")


#########################################################
# H2 -- OLS -- TOP QUINTILE POPULISTS -- ANTI
#########################################################

# NOT VERY POPULIST
df %>%
  filter(pop_quintile == 1
  ) %>%
  group_by(
    # FactFreeReply
    FactFreeSource
    # gotWhichCorrection
    # gender
  ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T), 
    mean_T2 = mean(belief_o_T2, na.rm = T), 
    mean_diff = mean(diff_o, na.rm=T),
    n = n()
  )

# VERY POPULIST
df %>%
  filter(pop_quintile == 5
  ) %>%
  group_by(
    # FactFreeReply
    FactFreeSource
    # gotWhichCorrection
    # gender
    ) %>%
  summarise(
    mean_T1 = mean(belief_o_T1, na.rm = T), 
    mean_T2 = mean(belief_o_T2, na.rm = T), 
    mean_diff = mean(diff_o, na.rm=T),
    n = n()
  )

anti_diff_pop_1 =
    lm( 
      diff_o ~
        FactFreeReply + 
        leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
        immigNo123 +  
        immigFeelLikeHome_c + 
        ethno_c + 
        PollAtt + # leave + # 
        university + # socialGrade_num +  
        gender + 
        age_c +
        FactFreeReply*leave + # immigNumbers_num + 
        FactFreeReply*immigNo123 +
        FactFreeReply*immigFeelLikeHome_c + 
        FactFreeReply*ethno_c + 
        FactFreeReply*university +
        FactFreeReply*gender +
        FactFreeReply*age_c,      
      data=subset(df, 
                  gotWhichCorrection == "anti-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(anti_diff_pop_1)


anti_diff_pop_2 =
  lm( 
    diff_o ~
      FactFreeReply + 
      leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c + 
      # PollAtt + # leave + # 
      # university + # socialGrade_num +  
      gender + 
      # age_c +
      FactFreeReply*leave + # immigNumbers_num + 
      FactFreeReply*immigNo123 +
      # FactFreeReply*immigFeelLikeHome_c + 
      # FactFreeReply*ethno_c + 
      # FactFreeReply*university +
      FactFreeReply*gender, 
      # FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "anti-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(anti_diff_pop_2)


#########################################################
# H2 -- OLS -- TOP QUINTILE POPULISTS -- PRO
#########################################################

pro_diff_pop_1 =
  lm( 
    diff_o ~
      FactFreeReply + 
      leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c + 
      PollAtt + # leave + # 
      university + # socialGrade_num +  
      gender + 
      age_c +
      FactFreeReply*leave + # immigNumbers_num + 
      FactFreeReply*immigNo123 +
      FactFreeReply*immigFeelLikeHome_c + 
      FactFreeReply*ethno_c + 
      FactFreeReply*university +
      FactFreeReply*gender +
      FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "pro-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(pro_diff_pop_1)


pro_diff_pop_2 =
  lm( 
    diff_o ~
      FactFreeReply + 
      # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      # immigNo123 +  
      # immigFeelLikeHome_c + 
      # ethno_c + 
      # PollAtt + # leave + # 
      university + # socialGrade_num +  
      gender + 
      age_c +
      # FactFreeReply*leave + # immigNumbers_num + 
      # FactFreeReply*immigNo123 +
      # FactFreeReply*immigFeelLikeHome_c + 
      FactFreeReply*ethno_c + 
      FactFreeReply*university +
      FactFreeReply*gender +
      FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "pro-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(pro_diff_pop_2)


stargazer(anti_diff_pop_1,
          anti_diff_pop_2,
          pro_diff_pop_1, 
          pro_diff_pop_2,
          title="Effect of FFC on responsiveness to expert information (top quintile populists)", 
          align=TRUE, 
          dep.var.labels="Difference pre- and post-correction veracity scores", 
          column.labels=c("anti-immigration (pooled)", "", "pro-immigration (pooled)", ""),
          covariate.labels=c("fact free comment (FFC)",
                             "voted to leave",
                             "important issue",
                             "feel like home",
                             "nationalist (centred)",
                             "low pol. attention",
                             "high pol. attention",
                             "university",
                             "female",
                             "age (centred)",
                             "FFC:leave",
                             "FFC:important issue",
                             "FFC:feel like home",
                             "FFC:nationalist",
                             "FFC:university",
                             "FFC:female",
                             "FFC:age",
                             "Constant"
              ),
              font.size = "small",
              omit.stat=c("ser","f"), # omit residual standard error & f-statistic
              no.space=TRUE, 
              # single.row = TRUE,
              column.sep.width = "-15pt",
              type="latex")



#########################################################
# TOP OF THE POPS -- INCLUDING THE SOURCE
#########################################################

#########################################################
# TOP OF THE POPS -- ANTI
#########################################################

anti_diff_pop_3 =
  lm( 
    diff_o ~
      FactFreeSource + 
      leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c + 
      PollAtt + # leave + # 
      university + # socialGrade_num +  
      gender + 
      FactFreeSource*gender +
      age_c +
      FactFreeSource*leave + # immigNumbers_num + 
    ### FactFreeReply*immigNo123 +
      FactFreeSource*immigFeelLikeHome_c,
    # FactFreeReply*ethno_c + 
    # FactFreeReply*university +
    ### FactFreeReply*gender, 
    # FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "anti-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(anti_diff_pop_3)

anti_diff_pop_4 =
  lm( 
    diff_o ~
      FactFreeSource + 
      leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      # immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c + 
      # PollAtt + # leave + # 
      # university + # socialGrade_num +  
      gender + 
      FactFreeSource*gender +
    # age_c +
      FactFreeSource*leave, # immigNumbers_num + 
    ### FactFreeReply*immigNo123 +
      # FactFreeSource*immigFeelLikeHome_c,
    # FactFreeReply*ethno_c + 
    # FactFreeReply*university +
    ### FactFreeReply*gender, 
    # FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "anti-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(anti_diff_pop_4)


#########################################################
# TOP OF THE POPS -- ANTI -- FFC/GENDER
#########################################################

effect_pop <- effect('FactFreeSource:gender', 
                     anti_diff_pop_4,
                     xlevels=list(gender = c("Male", 
                                             "Female"),
                                  FactFreeSource = c("None",
                                                     "Professor",
                                                     "Blogger")),
                     se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_pop <- as.data.frame(effect_pop)
effect_pop

effect_pop$FactFreeSource <- 
  factor(effect_pop$FactFreeSource,
         levels = c("None", "Professor", "Blogger"))

FourGreys <- c("#969696", "#737373", "#525252", "#252525")
FiveGreys <- c("#bdbdbd", "#969696", "#737373", "#525252", "#252525")
# FiveGreys <- c("#252525", "#525252", "#737373", "#969696","#bdbdbd")

# Plot 
top_pop_anti_FFC_gender <-
  ggplot(data=effect_pop, 
         aes(x=FactFreeSource, 
             y=fit, 
             group=gender))+
  geom_line(size=2, 
            aes(color=gender))+
  labs(title = "The effect of the FFC depending on its source and peoples' gender", 
       # subtitle = "", 
       caption = "\nModel 2 (pooled anti-immigration)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=twoGreys)+ #custom color coding 
  theme_bw() 

print(top_pop_anti_FFC_gender)
ggsave("R/graphs/top_pop_anti_FFC_gender.png")

#########################################################
# TOP OF THE POPS -- ANTI -- FFC/LEAVE
#########################################################

effect_pop <- effect('FactFreeSource:leave', 
                     anti_diff_pop_4,
                     xlevels=list(leave = c("leave", 
                                             "remain / DK"),
                                  FactFreeSource = c("None",
                                                     "Professor",
                                                     "Blogger")),
                     se=TRUE, 
                     confidence.level=.95, 
                     typical=mean)

#Put data into data frame
effect_pop <- as.data.frame(effect_pop)
effect_pop

effect_pop$FactFreeSource <- 
  factor(effect_pop$FactFreeSource,
         levels = c("None", "Professor", "Blogger"))

# Plot 
top_pop_anti_FFC_leave <-
  ggplot(data=effect_pop, 
         aes(x=FactFreeSource, 
             y=fit, 
             group=leave))+
  geom_line(size=2, 
            aes(color=leave))+
  labs(title = "The effect of the FFC depending on its source and peoples' Brexit vote", 
       # subtitle = "", 
       caption = "\nModel 2 (pooled anti-immigration)",
       y="diff",
       x="Fact-Free Comment") +
  scale_color_manual(values=twoGreys)+ #custom color coding 
  theme_bw() 

print(top_pop_anti_FFC_leave)
ggsave("R/graphs/top_pop_anti_FFC_leave.png")


#########################################################
# TOP OF THE POPS -- PRO
#########################################################

pro_diff_pop_3 =
  lm( 
    diff_o ~
      FactFreeSource + 
      leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c + 
      PollAtt + # leave + # 
      university + # socialGrade_num +  
      gender + 
      FactFreeSource*gender +
      age_c +
      FactFreeSource*leave, # immigNumbers_num + 
    ### FactFreeReply*immigNo123 +
    # FactFreeSource*immigFeelLikeHome_c,
    # FactFreeReply*ethno_c + 
    # FactFreeReply*university +
    ### FactFreeReply*gender, 
    # FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "pro-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(pro_diff_pop_3)

pro_diff_pop_4 =
  lm( 
    diff_o ~
      FactFreeSource + 
      # leave + #immigNumbers_num +  # leave + # immigNumbers_num + # leave + # EUVote
      # immigNo123 +  
      immigFeelLikeHome_c + 
      ethno_c +
      # PollAtt + # leave + # 
      # university + # socialGrade_num +  
      # gender + 
      # FactFreeSource*gender +
      # age_c,
      # FactFreeSource*leave, # immigNumbers_num + 
    ### FactFreeReply*immigNo123 +
      FactFreeSource*immigFeelLikeHome_c, 
    # FactFreeReply*ethno_c + 
    # FactFreeReply*university +
    ### FactFreeReply*gender, 
    # FactFreeReply*age_c,      
    data=subset(df, 
                gotWhichCorrection == "pro-immigration" &
                  motivatedToReject == 1 & 
                  pop_quintile == 5))

summary(pro_diff_pop_4)

stargazer(anti_diff_pop_3,
          anti_diff_pop_4,
          pro_diff_pop_3,
          pro_diff_pop_4,
          title="Effect of FFC on responsiveness to expert information (top quintile populists)", 
          align=TRUE, 
          dep.var.labels="Difference pre- and post-correction veracity scores", 
          column.labels=c("anti-immigration (pooled)", "", "pro-immigration (pooled)", ""),
          covariate.labels=c("fact free comment (Professor)",
                             "fact free comment (Blogger)",
                             "voted to leave",
                             "important issue",
                             "feel like home",
                             "nationalist (centred)",
                             "pol. attention (low)",
                             "pol. attention (high)",
                             "university",
                             "female",
                             "age (centred)",
                             "FFC (Prof):female",
                             "FFC (Blogger):female",
                             "FFC (Prof):leave",
                             "FFC (Blogger):leave",
                             "FFC (Prof):feel like home",
                             "FFC (Blogger):feel like home",
                             "Constant"
          ),
          font.size = "small",
          omit.stat=c("ser","f"), # omit residual standard error & f-statistic
          no.space=TRUE, 
          # single.row = TRUE,
          column.sep.width = "-15pt",
          type="latex")


#########################################################
# TOP OF THE POPS -- PRO -- FFC/FEEL LIKE HOME
#########################################################

# HIER WEITER

#########################################################
# NEW NEW NEW
# Using this website: 
# https://stats.idre.ucla.edu/r/seminars/interactions-r/#s5
#########################################################

# 2 categorical -- Quick and dirty

# FFC * gender
effect_gender2 <- emmeans(con_diff3, ~ FactFreeReply*gender)
summary(effect_gender2)
emmip(effect_gender2, gender ~ FactFreeReply, CIs=TRUE)

# FFC * leave
effect_leave2 <- emmeans(con_diff3, ~  FactFreeReply*leave)
summary(effect_leave2)
emmip(effect_leave2, leave ~ FactFreeReply, CIs=TRUE)


# 2 categorical -- Using ggplot

#########################################################
# FFC * gender
#########################################################

effect_ffc_gender <- emmip(effect_gender2, gender ~ FactFreeReply, 
                           CIs=TRUE,
                           plotit=FALSE)
effect_ffc_gender

inter_plot_gender2 <-
  ggplot(data=effect_ffc_gender, 
         aes(x=FactFreeReply, 
             y=yvar, 
             group=gender))+
  geom_line(size=2, 
            aes(color=gender))+
  geom_ribbon(aes(ymin=LCL, 
                  ymax=UCL,
                  fill=gender),
              alpha=.2)+
  ylab("Difference pre- and post-correction belief")+
  xlab("Fact Free Comment")+
  ggtitle("Fact-Free Comment and Gender as predictors of diff")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")

print(inter_plot_gender2)
ggsave("R/graphs/inter_plot_gender2.png")


#########################################################
# FFC * leave
#########################################################

effect_ffc_leave2 <- emmip(effect_leave2, 
                          leave ~ FactFreeReply, 
                          CIs=TRUE,
                          plotit=FALSE)
effect_ffc_leave2

inter_plot_leave2 <-
  ggplot(data=effect_ffc_leave2, 
         aes(x=FactFreeReply, 
             y=yvar, 
             group=leave))+
  geom_line(size=2, 
            aes(color=leave))+
  geom_ribbon(aes(ymin=LCL, 
                  ymax=UCL,
                  fill=leave),
              alpha=.2)+
  ylab("Difference pre- and post-correction belief")+
  xlab("Fact Free Comment")+
  ggtitle("Fact-Free Comment and Brexit Vote as Predictors of diff")+
  theme_bw()+
  theme(text = element_text(size=12),
        legend.text = element_text(size=12),
        legend.direction = "horizontal",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position="top")

print(inter_plot_leave2)
ggsave("R/graphs/inter_plot_leave2.png")


#########################################################
# PLOT INTERACTION TERMS
#########################################################

# CONSERVATIVE MISPERCEPTIONS
# The older you get the more/less responsive
# If female then more/less responsive
# If populist then more/less responsive

# GREAT WEBSITES -- USE THESE!!
# NO 1) https://cran.r-project.org/web/packages/margins/vignettes/Introduction.html
# NO 2) https://stats.idre.ucla.edu/r/seminars/interactions-r/

con_diff3 <- reg_final(con$diff, con)
summary(con_diff2)

# Plot marginal effects
library("margins")
(m <- margins(con_diff2))
summary(m)
plot(m)

# Plot interactions
# Using interplot: https://cran.r-project.org/web/packages/interplot/vignettes/interplot-vignette.html
library("interplot")

# The more populist the bigger the effect of the FFC
interplot(m = con_diff2,
          var1 = "FactFreeReply",
          var2 = "populism_c" )

# The older you get the smaller the effect of the FFC
interplot(m = con_diff2,
          var1 = "FactFreeReply",
          var2 = "age" )

str(df$gender)

interplot(m = con_diff2,
          var1 = "FactFreeReply",
          var2 = "gender" )


# Using plot_model: https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
# install.packages('sjPlot',
#                  dependencies=TRUE, 
#                  repos='http://cran.rstudio.com/')

library(sjPlot)
library(sjmisc)
library(ggplot2)

plot_model(con_diff3, 
           type="int")


plot_model(con_diff3, 
           type="int",
           terms=c("populism_c", "FactFreeReply"))


# Following: https://ademos.people.uic.edu/Chapter13.html
library(effects)
# install.packages("effects")

# TO DO

# CONSERVATIVE MISPERCEPTIONS
# The older you get the more/less responsive
# If female then more/less responsive
# If populist then more/less responsive



# LIBERAL MISPERCEPTIONS
# No significant interactions
# pol attention & nationalism significant



#########################################################
# H4 -- Evidence of post-truth reasoning
#########################################################

# We expect the fact-free comment to make respondents more 
# likely to ... 
# H4a  designate the statements as matters of opinion rather than fact 
# H4b state that the statistics are wrong 
# H4c if conceding that the statistics are correct, insist that they still believe something different 
# H4d agree that it is "okay to disagree with the facts, if that’s what you believe"

#########################################################
# Fact v. opinion
#########################################################

# All -- NOPE 
t.test(factOpinion_num ~ FactFreeReply,
       data=subset(df, motivatedToReject == 1))
# t = -0.84118, df = 954.3, p-value = 0.4005
# mean in group No 3.818182
# mean in group Yes 3.891965

# ANTI -- NOPE
t.test(factOpinion_num ~ FactFreeReply,
       data=subset(df, 
                   motivatedToReject == 1 &
                   corrected %in% c("noAsylum","costImmig")))

# PRO -- NOPE
t.test(factOpinion_num ~ FactFreeReply,
       data=subset(df, 
                   motivatedToReject == 1 &
                     corrected %in% c("lowPaid", "whiteCrime")))

# --> Exposure to the FFC did NOT make people more likely
# to say it is ok to say they are opinions, not facts

# OLS...

ols_factOpinion = 
  lm(factOpinion_num ~
       # corrected + # gotConsCorrection + 
       motivatedToReject +
       immigNumbers_num + # leave # EUVote
       immigNo123 + # leftright7_dk_centre + 
       PollAtt + 
       socialGrade_num + # university +
       gender + 
       age + 
       region +
       populism_c +
       ethno_c,
     data=subset(df, RandomGrp == 1 | RandomGrp == 2))

summary(ols_factOpinion)

# Cuts
# motivatedToReject * gender +
# motivatedToReject * immigNumbers_num +
# motivatedToReject * populism_c +
# motivatedToReject * ethno_c


ols_factOpinion2 = 
  lm(factOpinion_num ~
       corrected + # gotConsCorrection + 
       motivatedToReject +
       immigNumbers_num + # leave # EUVote
       immigNo123 + # leftright7_dk_centre + 
       PollAtt + 
       socialGrade_num + # university +
       gender + 
       age + 
       region +
       populism_c +
       ethno_c 
     ,
     data=subset(df, RandomGrp == 1 | RandomGrp == 2))

summary(ols_factOpinion2)


#########################################################
# ok2disagree
#########################################################

df$ok2disagree
df$ok2disagree_num

# All -- NOPE 
t.test(ok2disagree_num ~ FactFreeReply,
       data=subset(df, motivatedToReject == 1))
# t = -1.3737, df = 983, p-value = 0.1698
# mean in group No 2.737190
# mean in group Yes 2.790988

# ANTI -- NOPE
t.test(ok2disagree_num ~ FactFreeReply,
       data=subset(df, 
                   motivatedToReject == 1 &
                     corrected %in% c("noAsylum","costImmig")))

# PRO -- NOPE
t.test(ok2disagree_num ~ FactFreeReply,
       data=subset(df, 
                   motivatedToReject == 1 &
                     corrected %in% c("lowPaid", "whiteCrime")))

# --> Exposure to the FFC did NOT make people more likely
# to say it is ok to disagree with the facts

# OLS
ols_ok2disagree = 
  lm(ok2disagree_num ~
       corrected + 
       motivatedToReject +
       leave + # immigNumbers_num + # leave # EUVote
       immigNo123 + # leftright7_dk_centre + 
       PollAtt + 
       socialGrade_num + # university +
       gender + 
       age + 
       region +
       populism_c +
       ethno_c + 
       motivatedToReject * corrected
     # motivatedToReject * leave 
     # motivatedToReject * gender 
     # motivatedToReject * immigNumbers_num 
     # motivatedToReject * populism_c +
     # motivatedToReject * ethno_c
     ,
     data=subset(df, RandomGrp == 1 | RandomGrp == 2))

summary(ols_ok2disagree)



#########################################################
# The statistics are probably right, but I believe something different
# I think that the statistics are wrong.
# The statistics made me change my mind.
#########################################################

df$believeSomethingDifferent <- ifelse(df$ifInconsistent == "The statistics are probably right, but I believe something d", 1, 0)
df$believeStatsWrong <- ifelse(df$ifInconsistent == "I think that the statistics are wrong.", 1, 0)
df$changedMyMind <- ifelse(df$ifInconsistent == "The statistics made me change my mind.", 1, 0)

t.test(believeSomethingDifferent ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 & 
                   consistent == "No" &
                   motivatedToReject == 1))
# t = 2.4096, df = 361.48, p-value = 0.01647
# mean in group No 0.3432203
# mean in group Yes 0.2597240

t.test(believeStatsWrong ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 & 
                   consistent == "No" &
                   motivatedToReject == 1))
# t = -5.6015, df = 401.4, p-value = 3.951e-08
# mean in group No 0.3389831 
# mean in group Yes 0.5382685

# Antis... significant!
t.test(believeStatsWrong ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 & 
                   consistent == "No" &
                   corrected %in% c("noAsylum", "costImmig") &
                   motivatedToReject == 1))
# t = -4.9489, df = 279.75, p-value = 1.291e-06
# mean in group No 0.3491124
# mean in group Yes  0.5572391

# Pros... significant!
t.test(believeStatsWrong ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 & 
                   consistent == "No" &
                   corrected %in% c("costImmig", "lowPaid") &
                   motivatedToReject == 1))
# t = -3.4557, df = 140.45, p-value = 0.0007264
# mean in group No 0.3888889
# mean in group Yes 0.5899705


t.test(changedMyMind ~ FactFreeReply,
       data=subset(df, 
                   wrong_T1 == 1 & 
                   consistent == "No" &
                   motivatedToReject == 1))
# t = 3.452, df = 344.59, p-value = 0.0006256
# mean in group No 0.3177966 
# mean in group Yes 0.2020075 


df$notice
df$consistent
table(df$ifInconsistent)
table(ctrl$ifInconsistent) # about 100 in each group

# --> 3 levels! Nominal logistic regression

# Or logistic regression, see who says the stats
# are right but I believe sth dif




#########################################################

# DVs
df$diff
df$accurateAuthority_num
df$trustAuthority_num
df$factOpinion_num

df$leftright7_dk_centre

my_reg = 
  function(dv){
    lm( 
      dv ~
        FactFreeReply + 
        #immigNumbers_num +
        #immigNo123 + 
        # leftright + # turn into num
        # PollAtt + 
        EUVote +
        # socialGrade + 
        university +
        # region +
        gender + 
        age +
        populism_c +
        ethno_c +
        # FactFreeReply * PollAtt +
        FactFreeReply * EUVote +
        FactFreeReply * immigNumbers_num + # Are cons more responsive?
        FactFreeReply * motivatedToReject + # Are those who want to return to their old beliefs more responsive?
        FactFreeReply * populism_c +
        FactFreeReply * ethno_c,
      data=df)
  }

ols1_diff = 
  lm( diff ~
        FactFreeReply + 
        #immigNumbers_num +
        #immigNo123 + 
        # leftright + # turn into num
        # PollAtt + 
        EUVote +
        # socialGrade + 
        university +
        # region +
        gender + 
        age +
        populism_c +
        ethno_c +
        # FactFreeReply * PollAtt +
        FactFreeReply * EUVote +
        FactFreeReply * immigNumbers_num + # Are cons more responsive?
        FactFreeReply * motivatedToReject + # Are those who want to return to their old beliefs more responsive?
        FactFreeReply * populism_c +
        FactFreeReply * ethno_c,
      data=df)
# data = subset(df, gotLibCorrection == 1))
# data = subset(df, gotConsCorrection == 1))

summary(ols1_diff)

my_reg = 
  function(dv){
    lm( 
      dv ~
        FactFreeReply + 
        #immigNumbers_num +
        #immigNo123 + 
        # leftright + # turn into num
        PollAtt + 
        EUVote +
        # socialGrade + 
        university +
        region +
        gender + 
        age +
        populism_c +
        ethno_c +
        FactFreeReply * PollAtt +
        FactFreeReply * EUVote +
        FactFreeReply * immigNumbers_num + # Are cons more responsive?
        FactFreeReply * motivatedToReject + # Are those who want to return to their old beliefs more responsive?
        FactFreeReply * populism_c +
        FactFreeReply * ethno_c,
      data = df)
  }




my_reg = 
  function(dv){
    lm( 
      dv ~
        FactFreeReply + 
        #immigNumbers_num +
        #immigNo123 + 
        # leftright + # turn into num
        PollAtt + 
        EUVote +
        # socialGrade + 
        university +
        region +
        gender + 
        age +
        populism_c +
        ethno_c +
        FactFreeReply * PollAtt +
        FactFreeReply * EUVote +
        FactFreeReply * immigNumbers_num + # Are cons more responsive?
        FactFreeReply * motivatedToReject + # Are those who want to return to their old beliefs more responsive?
        FactFreeReply * populism_c +
        FactFreeReply * ethno_c,
      data = df)
  }


df$factOpinion_num
df$ok2disagree

ols1_diff = 
  lm( factOpinion_num ~
        FactFreeReply + 
        #immigNumbers_num +
        #immigNo123 + 
        # leftright + # turn into num
        # PollAtt + 
        EUVote +
        # socialGrade + 
        university +
        # region +
        gender + 
        age +
        populism_c +
        ethno_c +
        # FactFreeReply * PollAtt +
        FactFreeReply * EUVote +
        FactFreeReply * immigNumbers_num + # Are cons more responsive?
        FactFreeReply * motivatedToReject + # Are those who want to return to their old beliefs more responsive?
        FactFreeReply * populism_c,
      # FactFreeReply * ethno_c
      data=df)
# data = subset(df, gotLibCorrection == 1))
# data = subset(df, gotConsCorrection == 1))

summary(ols1_diff)



#########################################################
# OLS
#########################################################

#########################################################
# H1 Effect of the correction -- Control Group
# -- Anti-immigration statements
#########################################################

# What predicts to what extent you adapt your factual beliefs
# after seeing statistics?

ols_diff_ctrl_anti_m2r_1 = 
  lm(diff ~
       corrected + 
       leave + #curbImmig + # leave + 
       immigNumbers_num + # leave + # EUVote
       # immigFeelLikeHome_c + 
       immigNo123 +  
       PollAtt + 
       university + # socialGrade_num +  
       gender + 
       age +
       populism_scale +
       ethno_scale +
       # trueScore8_c + # no effect - kick out! 
       populism_scale:leave +
       populism_scale:gender +
       populism_scale:immigNo123,
     data=ctrl_anti_m2r)

summary(ols_diff_ctrl_anti_m2r_1)
car::vif(ols_diff_ctrl_anti_m2r_1)


ols_diff_ctrl_anti_m2r_2 = 
  lm(diff ~
       corrected + 
       leave + #curbImmig + # leave + 
       # immigNumbers_num + # leave + # EUVote
       immigNo123 +  
       # PollAtt + 
       # university + # socialGrade_num +  
       gender + 
       age +
       populism_scale +
       # ethno_scale +
       populism_scale:leave +
       populism_scale:gender +
       populism_scale:immigNo123,
     data=subset(ctrl_anti_m2r))

summary(ols_diff_ctrl_anti_m2r_2)
car::vif(ols_diff_ctrl_anti_m2r_2)

stargazer(ols_diff_ctrl_anti_m2r_1,
          ols_diff_ctrl_anti_m2r_2, 
          title="Reactions to expert statement", 
          # title="Adapting false beliefs after exposure to expert statement", 
          subtitle="Control Group Respondents who are sceptical of immigration and who saw Anti-Immigration Statements",
          align=TRUE, 
          dep.var.labels="Difference pre and post-correction veracity scores", 
          covariate.labels=c("corrected: cost/benefit", 
                             "voted to remain", #"curb immigration",
                             "increase immigration",
                             "immigration important issue",
                             "low political attention", 
                             "high political attention", 
                             "university",
                             "female",
                             "age",
                             "populist", 
                             "nationalist",
                             "populist:remain",
                             "populist:female",
                             "populist:important issue",
                             "Constant"),
          omit.stat=c("ser","f"), # omit residual standard error & f-statistic
          no.space=TRUE, 
          type="text") # text


#########################################################
# -- Control Group -- Pro-immigration statements
#########################################################

ols_diff_ctrl_cons_1 = 
  lm(diff ~
       corrected + 
       leave + #curbImmig + # leave + 
       immigNumbers_num + # leave + # EUVote
       immigNo123 +  
       PollAtt + 
       university + # socialGrade_num +  
       gender + 
       age +
       populism_scale +
       ethno_scale +
       populism_scale:leave +
       populism_scale:gender +
       populism_scale:immigNo123,
     data=subset(con, RandomGrp == 1 | RandomGrp == 2 & 
                   motivatedToReject == 1))

summary(ols_diff_ctrl_cons_1)
car::vif(ols_diff_ctrl_cons_1)


ols_diff_ctrl_cons_2 = 
  lm(diff ~
       corrected + 
       leave + #curbImmig + # leave + 
       # immigNumbers_num + # leave + # EUVote
       immigNo123 +  
       # PollAtt + 
       # university + # socialGrade_num +  
       gender + 
       age +
       populism_scale +
       # ethno_scale +
       populism_scale:leave +
       populism_scale:gender +
       populism_scale:immigNo123,
     data=subset(con, RandomGrp == 1 | RandomGrp == 2 & 
                   motivatedToReject == 1))

summary(ols_diff_ctrl_cons_2)
car::vif(ols_diff_ctrl_cons_2)




# Exploratory analyses...

#########################################################
# The effect of a fact-free correction massively depends 
# on M2R
#########################################################

# --> For the m2r, FFC has a significant effect on diff
ols_diff = 
  lm(diff ~ # trustAuthority_num ~ # accurateAuthority_num  ~ # diff
       FactFreeReply,
     data=subset(df, motivatedToReject == 1))
summary(ols_diff)

# --> For the not m2r, FFC does not have a significant 
# effect on diff
ols_diff = 
  lm(diff ~ # trustAuthority_num ~ # accurateAuthority_num  ~ # diff
       FactFreeReply,
     data=subset(df, motivatedToReject == 0))
summary(ols_diff)

# Next, excluding those not motivated to reject...


#########################################################
# Establishing that content and source of FFC doesn't matter
#########################################################

ols_diff = 
  lm(diff ~
       #FactFreeReply,
       # FactFreeSource2,
       FactFreeMessage2,
     data=subset(df, motivatedToReject == 1))
summary(ols_diff)

#########################################################
# Establishing that there is a difference in the 4 correction groups
#########################################################

ols_diff = 
  lm(diff ~
       FactFreeReply +
       corrected,
     data=subset(df, motivatedToReject == 1))
summary(ols_diff)

# Cannot pool the conservatives

ols_diff = 
  lm(diff ~
       FactFreeReply +
       corrected,
     data=subset(df, motivatedToReject == 1 & 
                   gotConsCorrection == 1))
summary(ols_diff)

# Can pool the liberals

ols_diff = 
  lm(diff ~
       FactFreeReply +
       corrected,
     data=subset(df, motivatedToReject == 1 & 
                   gotLibCorrection == 1))
summary(ols_diff)

#########################################################
# THE END
#########################################################

