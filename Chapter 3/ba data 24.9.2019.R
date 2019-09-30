#########################################################
# British Academy Project
# -- Data management --
# --24 September 2019 -- 
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

df <- read_dta("/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/3.BAgrantProject/Data/working data 20.9.2019.dta")

# Using haven to extract values and labels from Stata file
df <- as_factor(df, levels = "both")

# x <- as_factor(x, levels = "values")
# y <- as_factor(x, levels = "labels")

# Look at structure
# str(df)

# # Turn all character variables into factors
# characters <- sapply(df, is.character)
# df[characters] <- lapply(df[characters], as.factor)

# Change data types in bulk
# https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types

# Turn into factor
intoFactor <- c("user_id")

df[, intoFactor] <- lapply(df[,intoFactor], factor)

# Turn into numeric
intoNum <- c(
  "gotConsCorrection",
  "gotLibCorrection",
  "curbImmig",
  "increaseImmig",
  "motivatedToReject",
  
  "belief_noAsylum_T1",
  "belief_noAsylum_T2",
  "belief_costImmig_T1", 
  "belief_costImmig_T2",
  "belief_whiteCrime_T1",
  "belief_whiteCrime_T2",
  "belief_lowPaid_T1",
  "belief_lowPaid_T2",
  
  "belief_T1",
  "belief_T2",
  "belief_i2_T1",
  "belief_i2_T2",
  
  "diff",
  "diff_i2",
  
  "bothWrong_T1",
  "bothWrong_T2",
  
  "noAsylum_diff",
  "costImmig_diff",
  "whiteCrime_diff",
  "lowPaid_diff",
  
  "noAsylum_anyChange",
  "costImmig_anyChange",
  "whiteCrime_anyChange",
  "lowPaid_anyChange",
  
  "noAsylum_shift",
  "costImmig_shift",
  "whiteCrime_shift",
  "lowPaid_shift",
  
  "leave",
  "populism_scale",
  "ethno_scale",
  "trueScore7",
  "trueScore8"
)


# df$belief_noAsylum_T1 <- as.numeric(as.character(df$belief_noAsylum_T1))
# df$belief_noAsylum_T2 <- as.numeric(as.character(df$belief_noAsylum_T2))
# 
# df$belief_costImmig_T1 <- as.numeric(as.character(df$belief_costImmig_T1))
# df$belief_costImmig_T2 <- as.numeric(as.character(df$belief_costImmig_T2))
# 
# df$belief_whiteCrime_T1 <- as.numeric(as.character(df$belief_whiteCrime_T1))
# df$belief_whiteCrime_T2 <- as.numeric(as.character(df$belief_whiteCrime_T2))
# 
# df$belief_lowPaid_T1 <- as.numeric(as.character(df$belief_lowPaid_T1))
# df$belief_lowPaid_T2 <- as.numeric(as.character(df$belief_lowPaid_T2))
# 
# df$belief_T1 <- as.numeric(as.character(df$belief_T1))
# df$belief_T2 <- as.numeric(as.character(df$belief_T2))
# 
# df$belief_i2_T1 <- as.numeric(as.character(df$belief_i2_T1))
# df$belief_i2_T2 <- as.numeric(as.character(df$belief_i2_T2))
# 
# df$diff <- as.numeric(as.character(df$diff))
# df$diff_i2 <- as.numeric(as.character(df$diff_i2))
# 
# df$bothWrong_T1 <- as.numeric(as.character(df$bothWrong_T1))
# df$bothWrong_T2 <- as.numeric(as.character(df$bothWrong_T2))



# Turn into numeric
df[, intoNum] <- lapply(df[,intoNum], as.numeric)


# Turn all atomic variables into numeric
# atomic <- sapply(df, is.atomic)
# df[atomic] <- lapply(df[atomic], as.numeric)
# --> This turns ALMOST EVERYTHING into numeric!

#########################################################
# Data management
#########################################################

# Immigration opinions -- Change order of factor levels 

df$immigOpinions <- factor(df$immigOpinions, levels = 
                                 c("fewer", "no change", "more"))

df$curbImmig_fac <- NA
df$curbImmig_fac[df$curbImmig == 0] <- "more immigrants / same number"
df$curbImmig_fac[df$curbImmig == 1] <- "fewer immigrants"
df$curbImmig_fac <- as.factor(as.character(df$curbImmig_fac))

# Leave vote

# table(df$leave)
# We have 1356 leave voters (according to stata)
# so leave must be coded as 2

df$leave_f <- NA
df$leave_f[df$leave == 1] <- "remain / DK"
df$leave_f[df$leave == 2] <- "leave"
df$leave_f <- as.factor(as.character(df$leave_f))

df$leave <- df$leave_f
df$leave_f <- NULL

df$leave_f[df$leave == 1] <- "remain / DK"
df$leave_f[df$leave == 2] <- "leave"
df$leave_f <- as.factor(as.character(df$leave_f))


# Age -- create meaningful categories

df$ageCategory2 <- NA
df$ageCategory2[df$age < 20] <- "teens"
df$ageCategory2[df$age >= 20 & df$age < 30] <- "twens"
df$ageCategory2[df$age >= 30 & df$age < 40] <- "thirties"
df$ageCategory2[df$age >= 40 & df$age < 50] <- "fourties"
df$ageCategory2[df$age >= 50 & df$age < 60] <- "fifties"
df$ageCategory2[df$age >= 60 & df$age < 70] <- "sixties"
df$ageCategory2[df$age >= 70] <- "seventies+"

df$ageCategory2 <- as.factor(as.character(df$ageCategory2))

df$ageCategory2 <- factor(df$ageCategory2, levels = 
                             c("teens", 
                               "twens",
                               "thirties",
                               "fourties",
                               "fifties",
                               "sixties",
                               "seventies +"))


df$trustAuthority <- factor(df$trustAuthority, levels = 
                                 c("0 - Would not trust at all", 
                                   "1", "2", "3", "4", "5",
                                   "6 - Would trust a great deal"))

df$trustFactFreeProf <- factor(df$trustFactFreeProf, levels = 
                                c("0 - Would not trust at all", 
                                  "1", "2", "3", "4", "5",
                                  "6 - Would trust a great deal"))

df$trustBlogger <- factor(df$trustBlogger, levels = 
                                c("0 - Would not trust at all", 
                                  "1", "2", "3", "4", "5",
                                  "6 - Would trust a great deal"))

df$trust_source2 <- factor(df$trust_source2, levels = 
                            c("0 - Would not trust at all", 
                              "1", "2", "3", "4", "5",
                              "6 - Would trust a great deal"))

df$accurateAuthority <- factor(df$accurateAuthority, levels = 
                        c("Not at all accurate", "Not very accurate", 
                          "Fairly accurate", "Very accurate"))

df$accurateBlogger <- factor(df$accurateBlogger, levels = 
                               c("Not at all accurate", "Not very accurate", 
                                 "Fairly accurate", "Very accurate"))

df$accurateFactFreeProf <- factor(df$accurateFactFreeProf, levels = 
                               c("Not at all accurate", "Not very accurate", 
                                 "Fairly accurate", "Very accurate"))

df$accurate_source2 <- factor(df$accurate_source2, levels = 
                               c("Not at all accurate", "Not very accurate", 
                                 "Fairly accurate", "Very accurate"))


# Create numeric variables

df$trustAuthority_num <- NA
df$trustAuthority_num[df$trustAuthority == "0 - Would not trust at all"] <- 1
df$trustAuthority_num[df$trustAuthority == "1"] <- 2
df$trustAuthority_num[df$trustAuthority == "2"] <- 3
df$trustAuthority_num[df$trustAuthority == "3"] <- 4
df$trustAuthority_num[df$trustAuthority == "4"] <- 5
df$trustAuthority_num[df$trustAuthority == "5"] <- 6
df$trustAuthority_num[df$trustAuthority == "6 - Would trust a great deal"] <- 7

df$trustFactFreeProf_num <- NA
df$trustFactFreeProf_num[df$trustFactFreeProf == "0 - Would not trust at all"] <- 1
df$trustFactFreeProf_num[df$trustFactFreeProf == "1"] <- 2
df$trustFactFreeProf_num[df$trustFactFreeProf == "2"] <- 3
df$trustFactFreeProf_num[df$trustFactFreeProf == "3"] <- 4
df$trustFactFreeProf_num[df$trustFactFreeProf == "4"] <- 5
df$trustFactFreeProf_num[df$trustFactFreeProf == "5"] <- 6
df$trustFactFreeProf_num[df$trustFactFreeProf == "6 - Would trust a great deal"] <- 7

df$trustBlogger_num <- NA
df$trustBlogger_num[df$trustBlogger == "0 - Would not trust at all"] <- 1
df$trustBlogger_num[df$trustBlogger == "1"] <- 2
df$trustBlogger_num[df$trustBlogger == "2"] <- 3
df$trustBlogger_num[df$trustBlogger == "3"] <- 4
df$trustBlogger_num[df$trustBlogger == "4"] <- 5
df$trustBlogger_num[df$trustBlogger == "5"] <- 6
df$trustBlogger_num[df$trustBlogger == "6 - Would trust a great deal"] <- 7

df$trust_source2_num <- NA
df$trust_source2_num[df$trust_source2 == "0 - Would not trust at all"] <- 1
df$trust_source2_num[df$trust_source2 == "1"] <- 2
df$trust_source2_num[df$trust_source2 == "2"] <- 3
df$trust_source2_num[df$trust_source2 == "3"] <- 4
df$trust_source2_num[df$trust_source2 == "4"] <- 5
df$trust_source2_num[df$trust_source2 == "5"] <- 6
df$trust_source2_num[df$trust_source2 == "6 - Would trust a great deal"] <- 7

df$accurateAuthority_num[df$accurateAuthority == "Very accurate"] <- 4
df$accurateAuthority_num[df$accurateAuthority == "Fairly accurate"] <- 3
df$accurateAuthority_num[df$accurateAuthority == "Not very accurate"] <- 2
df$accurateAuthority_num[df$accurateAuthority == "Not at all accurate"] <- 1

df$factOpinion_num <- NA
df$factOpinion_num[df$factOpinion == "0 - Purely a matter of fact"] <- 1
df$factOpinion_num[df$factOpinion == "1"] <- 2
df$factOpinion_num[df$factOpinion == "2"] <- 3
df$factOpinion_num[df$factOpinion == "3"] <- 4
df$factOpinion_num[df$factOpinion == "4"] <- 5
df$factOpinion_num[df$factOpinion == "5"] <- 6
df$factOpinion_num[df$factOpinion == "6 - Purely a matter of opinion"] <- 7

df$factOpinion_i2_num <- NA
df$factOpinion_i2_num[df$factOpinion_i2 == "0 - Purely a matter of fact"] <- 1
df$factOpinion_i2_num[df$factOpinion_i2 == "1"] <- 2
df$factOpinion_i2_num[df$factOpinion_i2 == "2"] <- 3
df$factOpinion_i2_num[df$factOpinion_i2 == "3"] <- 4
df$factOpinion_i2_num[df$factOpinion_i2 == "4"] <- 5
df$factOpinion_i2_num[df$factOpinion_i2 == "5"] <- 6
df$factOpinion_i2_num[df$factOpinion_i2 == "6 - Purely a matter of opinion"] <- 7

# Create T1 and T2 variables for trust and accuracy
df$trust_T1 <- df$trustAuthority
df$trust_num_T1 <- df$trustAuthority_num
df$accurate_T1 <- df$accurateAuthority

df$trust_T2 <- df$trust_source2
df$trust_num_T2 <- df$trust_source2_num
df$accurate_T2 <- df$accurate_source2

# ACHTUNG -- FIND OUT WHAT THIS IS!!

# Create numeric variables -- DVs (belief in false claims)

df$noAsylum_num_T1 <- NA
df$noAsylum_num_T1[df$noAsylum_T1 == "0 - Definitely False"] <- 1
df$noAsylum_num_T1[df$noAsylum_T1 == "1"] <- 2
df$noAsylum_num_T1[df$noAsylum_T1 == "2"] <- 3
df$noAsylum_num_T1[df$noAsylum_T1 == "3"] <- 4
df$noAsylum_num_T1[df$noAsylum_T1 == "4"] <- 5
df$noAsylum_num_T1[df$noAsylum_T1 == "5"] <- 6
df$noAsylum_num_T1[df$noAsylum_T1 == "6 - Definitely True"] <- 7

df$costImmig_num_T1 <- NA
df$costImmig_num_T1[df$costImmig_T1 == "0 - Definitely False"] <- 1
df$costImmig_num_T1[df$costImmig_T1 == "1"] <- 2
df$costImmig_num_T1[df$costImmig_T1 == "2"] <- 3
df$costImmig_num_T1[df$costImmig_T1 == "3"] <- 4
df$costImmig_num_T1[df$costImmig_T1 == "4"] <- 5
df$costImmig_num_T1[df$costImmig_T1 == "5"] <- 6
df$costImmig_num_T1[df$costImmig_T1 == "6 - Definitely True"] <- 7

df$whiteCrime_num_T1 <- NA
df$whiteCrime_num_T1[df$whiteCrime_T1 == "0 - Definitely False"] <- 1
df$whiteCrime_num_T1[df$whiteCrime_T1 == "1"] <- 2
df$whiteCrime_num_T1[df$whiteCrime_T1 == "2"] <- 3
df$whiteCrime_num_T1[df$whiteCrime_T1 == "3"] <- 4
df$whiteCrime_num_T1[df$whiteCrime_T1 == "4"] <- 5
df$whiteCrime_num_T1[df$whiteCrime_T1 == "5"] <- 6
df$whiteCrime_num_T1[df$whiteCrime_T1 == "6 - Definitely True"] <- 7

df$lowPaid_num_T1 <- NA
df$lowPaid_num_T1[df$lowPaid_T1 == "0 - Definitely False"] <- 1
df$lowPaid_num_T1[df$lowPaid_T1 == "1"] <- 2
df$lowPaid_num_T1[df$lowPaid_T1 == "2"] <- 3
df$lowPaid_num_T1[df$lowPaid_T1 == "3"] <- 4
df$lowPaid_num_T1[df$lowPaid_T1 == "4"] <- 5
df$lowPaid_num_T1[df$lowPaid_T1 == "5"] <- 6
df$lowPaid_num_T1[df$lowPaid_T1 == "6 - Definitely True"] <- 7

df$no5_num_T1 <- NA
df$no5_num_T1[df$no5_T1 == "0 - Definitely False"] <- 1
df$no5_num_T1[df$no5_T1 == "1"] <- 2
df$no5_num_T1[df$no5_T1 == "2"] <- 3
df$no5_num_T1[df$no5_T1 == "3"] <- 4
df$no5_num_T1[df$no5_T1 == "4"] <- 5
df$no5_num_T1[df$no5_T1 == "5"] <- 6
df$no5_num_T1[df$no5_T1 == "6 - Definitely True"] <- 7

df$plasticBags_num_T1 <- NA
df$plasticBags_num_T1[df$plasticBags_T1 == "0 - Definitely False"] <- 1
df$plasticBags_num_T1[df$plasticBags_T1 == "1"] <- 2
df$plasticBags_num_T1[df$plasticBags_T1 == "2"] <- 3
df$plasticBags_num_T1[df$plasticBags_T1 == "3"] <- 4
df$plasticBags_num_T1[df$plasticBags_T1 == "4"] <- 5
df$plasticBags_num_T1[df$plasticBags_T1 == "5"] <- 6
df$plasticBags_num_T1[df$plasticBags_T1 == "6 - Definitely True"] <- 7

df$fracking_num_T1 <- NA
df$fracking_num_T1[df$fracking_T1 == "0 - Definitely False"] <- 1
df$fracking_num_T1[df$fracking_T1 == "1"] <- 2
df$fracking_num_T1[df$fracking_T1 == "2"] <- 3
df$fracking_num_T1[df$fracking_T1 == "3"] <- 4
df$fracking_num_T1[df$fracking_T1 == "4"] <- 5
df$fracking_num_T1[df$fracking_T1 == "5"] <- 6
df$fracking_num_T1[df$fracking_T1 == "6 - Definitely True"] <- 7

df$recession_num_T1 <- NA
df$recession_num_T1[df$recession_T1 == "0 - Definitely False"] <- 1
df$recession_num_T1[df$recession_T1 == "1"] <- 2
df$recession_num_T1[df$recession_T1 == "2"] <- 3
df$recession_num_T1[df$recession_T1 == "3"] <- 4
df$recession_num_T1[df$recession_T1 == "4"] <- 5
df$recession_num_T1[df$recession_T1 == "5"] <- 6
df$recession_num_T1[df$recession_T1 == "6 - Definitely True"] <- 7


df$belief_num_T1 <- as.numeric(as.character(df$belief_T1))
df$belief_num_T2 <- as.numeric(as.character(df$belief_T2))

df$noAsylum_num_T2 <- NA
df$noAsylum_num_T2[df$noAsylum_T2 == "0 - Definitely False"] <- 1
df$noAsylum_num_T2[df$noAsylum_T2 == "1"] <- 2
df$noAsylum_num_T2[df$noAsylum_T2 == "2"] <- 3
df$noAsylum_num_T2[df$noAsylum_T2 == "3"] <- 4
df$noAsylum_num_T2[df$noAsylum_T2 == "4"] <- 5
df$noAsylum_num_T2[df$noAsylum_T2 == "5"] <- 6
df$noAsylum_num_T2[df$noAsylum_T2 == "6 - Definitely True"] <- 7

df$costImmig_num_T2 <- NA
df$costImmig_num_T2[df$costImmig_T2 == "0 - Definitely False"] <- 1
df$costImmig_num_T2[df$costImmig_T2 == "1"] <- 2
df$costImmig_num_T2[df$costImmig_T2 == "2"] <- 3
df$costImmig_num_T2[df$costImmig_T2 == "3"] <- 4
df$costImmig_num_T2[df$costImmig_T2 == "4"] <- 5
df$costImmig_num_T2[df$costImmig_T2 == "5"] <- 6
df$costImmig_num_T2[df$costImmig_T2 == "6 - Definitely True"] <- 7

df$whiteCrime_num_T2 <- NA
df$whiteCrime_num_T2[df$whiteCrime_T2 == "0 - Definitely False"] <- 1
df$whiteCrime_num_T2[df$whiteCrime_T2 == "1"] <- 2
df$whiteCrime_num_T2[df$whiteCrime_T2 == "2"] <- 3
df$whiteCrime_num_T2[df$whiteCrime_T2 == "3"] <- 4
df$whiteCrime_num_T2[df$whiteCrime_T2 == "4"] <- 5
df$whiteCrime_num_T2[df$whiteCrime_T2 == "5"] <- 6
df$whiteCrime_num_T2[df$whiteCrime_T2 == "6 - Definitely True"] <- 7

df$lowPaid_num_T2 <- NA
df$lowPaid_num_T2[df$lowPaid_T2 == "0 - Definitely False"] <- 1
df$lowPaid_num_T2[df$lowPaid_T2 == "1"] <- 2
df$lowPaid_num_T2[df$lowPaid_T2 == "2"] <- 3
df$lowPaid_num_T2[df$lowPaid_T2 == "3"] <- 4
df$lowPaid_num_T2[df$lowPaid_T2 == "4"] <- 5
df$lowPaid_num_T2[df$lowPaid_T2 == "5"] <- 6
df$lowPaid_num_T2[df$lowPaid_T2 == "6 - Definitely True"] <- 7

# Original scale: 0 - 6. Stata turned that into a 
# 1 to 7 scale. Re-creating the original variables: 
df$belief_o_T1 <- df$belief_T1 - 1
df$belief_o_T2 <- df$belief_T2 - 1

df$belief_o_i2_T1 <- df$belief_i2_T1 - 1
df$belief_o_i2_T2 <- df$belief_i2_T2 - 1

df$belief_o_noAsylum_T1 <- df$belief_noAsylum_T1 -1
df$belief_o_costImmig_T1 <- df$belief_costImmig_T1 -1
df$belief_o_whiteCrime_T1 <- df$belief_whiteCrime_T1 -1
df$belief_o_lowPaid_T1 <- df$belief_lowPaid_T1 -1

df$belief_o_noAsylum_T2 <- df$belief_noAsylum_T2 -1
df$belief_o_costImmig_T2 <- df$belief_costImmig_T2 -1
df$belief_o_whiteCrime_T2 <- df$belief_whiteCrime_T2 -1
df$belief_o_lowPaid_T2 <- df$belief_lowPaid_T2 -1

# Creating the diff variable we had in the pre-analysis plan
df$diff_o <- df$belief_o_T1 - df$belief_o_T2
df$diff_i2_o <- df$belief_o_i2_T1 - df$belief_o_i2_T2

# Creating a new diff variable, staying on the 0-6 scale
# (for easier interpretation)
df$diff_new <- df$belief_o_T1/2 - df$belief_o_T2/2
df$diff_i2_new <- df$belief_o_i2_T1/2 - df$belief_o_i2_T2/2

# hist(df$diff_new)
# hist(df$diff_i2_new)

df$immigNumbers_num <- NA
df$immigNumbers_num[df$immigNumbers == "-3 = many fewer"] <- 1
df$immigNumbers_num[df$immigNumbers == "-2"] <- 2
df$immigNumbers_num[df$immigNumbers == "-1"] <- 3
df$immigNumbers_num[df$immigNumbers == "0 = no change"] <- 4
df$immigNumbers_num[df$immigNumbers == "+1"] <- 5
df$immigNumbers_num[df$immigNumbers == "+2"] <- 6
df$immigNumbers_num[df$immigNumbers == "+3 = many more"] <- 7

df$leftright7_dk_centre <- as.numeric(as.character(df$leftright7_dk_centre))
df$leftright7 <- as.numeric(as.character(df$leftright7))

# Did this in Stata, too.
df$socialGrade_num <- NA
df$socialGrade_num[df$socialGrade == "AB"] <- 4
df$socialGrade_num[df$socialGrade == "C1"] <- 3
df$socialGrade_num[df$socialGrade == "C2"] <- 2
df$socialGrade_num[df$socialGrade == "DE"] <- 1

# Create a pollitical attention (?) variable
# df$PollAtt_num <- NA
# df$PollAtt_num[df$PollAtt == "Low (0-2)"] <- 1
# df$PollAtt_num[df$PollAtt == "Medium (3-7)"] <- 2
# df$PollAtt_num[df$PollAtt == "High (8-10)"] <- 3

# df$correctedTxt <- as.character(df$correctedTxt)
df$w8 <- as.numeric(as.character(df$w8))
df$age <- as.numeric(df$age)

df$regions <- as.factor(as.character(df$regions))

# Set the reference category
df$region <- factor(df$region, 
                    levels = c("London",
                               "North",
                               "Midlands",
                               "South",
                               "Wales",
                               "Scotland"))

# Same same
# df$region <- relevel(df$region, ref="London")

#########################################################
# Create new variables 
#########################################################

# 

# df_long$wrong <- as.factor(df_long$wrong)
# 
# df_long$bothWrong <- NA
# df_long$bothWrong[df_long$time == "T1" & df_long$wrong == 1 &
#                   df_long$time == "T2" & df_long$wrong == 1] <- 1

# x <- 
# df_long[df_long$time == "T1" & df_long$wrong == 1 
#         , ]
# 
# head(x)

#########################################################
# Create variables for regression
#########################################################

#########################################################
# Quantiles and quartiles 
#########################################################

# To see where respondents rank on populism, nationalism,
# and how prone they are to saying things are true, generally,
# compared to others

df <- 
  df %>%
  mutate(ethno_decile = ntile(ethno_scale, 10),
         ethno_quartile = ntile(ethno_scale, 4),
         ethno_quintile = ntile(ethno_scale, 5),
         pop_decile = ntile(populism_scale, 10),
         pop_quartile = ntile(populism_scale, 4),
         pop_quintile = ntile(populism_scale, 5),
         trueScore7_quartile = ntile(trueScore7, 4),
         trueScore8_quartile = ntile(trueScore8, 4)
         )

# People should make decisions... agree --> pop
table(df$pop_peopleDecisions, df$pop_quintile)

# Compromise = BS
table(df$pop_compromise, df$pop_quintile)


df$pop_quartile_fac <- NA
df$pop_quartile_fac[df$pop_quartile == "1"] <- "very low (1)"
df$pop_quartile_fac[df$pop_quartile == "2"] <- "low (2)"
df$pop_quartile_fac[df$pop_quartile == "3"] <- "high (3)"
df$pop_quartile_fac[df$pop_quartile == "4"] <- "very high (4)"

df$pop_quartile_fac <- factor(df$pop_quartile_fac,
                              labels=c("very low (1)",
                                       "low (2)",
                                       "high (3)",
                                       "very high (4)")) 

df$ethno_quartile_fac <- NA
df$ethno_quartile_fac[df$ethno_quartile == "1"] <- "very low (1)"
df$ethno_quartile_fac[df$ethno_quartile == "2"] <- "low (2)"
df$ethno_quartile_fac[df$ethno_quartile == "3"] <- "high (3)"
df$ethno_quartile_fac[df$ethno_quartile == "4"] <- "very high (4)"

df$ethno_quartile_fac <- factor(df$ethno_quartile_fac,
                              labels=c("very low (1)",
                                       "low (2)",
                                       "high (3)",
                                       "very high (4)")) 

# see if it worked
# hist(df$ethno_quantile)
# hist(df$ethno_quartile)
# hist(df$trueScore7_quartile)
# hist(df$trueScore8_quartile)

df$immigFeelLikeHome_fac <- NA
df$immigFeelLikeHome_fac[df$immigFeelLikeHome %in% c("Strongly agree", "Agree")] <- "Agree"
df$immigFeelLikeHome_fac[df$immigFeelLikeHome %in% c("Disagree", "Strongly disagree")] <- "Disagree"
          
df$immigFeelLikeHome_num <- NA
df$immigFeelLikeHome_num[df$immigFeelLikeHome %in% c("Strongly agree")] <- 4
df$immigFeelLikeHome_num[df$immigFeelLikeHome %in% c("Agree")] <- 3
df$immigFeelLikeHome_num[df$immigFeelLikeHome %in% c("Disagree")] <- 2
df$immigFeelLikeHome_num[df$immigFeelLikeHome %in% c("Strongly disagree")] <- 1

# Immigration is good for the British economy.

df$immigEcon_fac <- NA
df$immigEcon_fac[df$immigEcon %in% c("Strongly agree", "Agree")] <- "Agree"
df$immigEcon_fac[df$immigEcon %in% c("Disagree", "Strongly disagree")] <- "Disagree"

df$immigEcon_num <- NA
df$immigEcon_num[df$immigEcon %in% c("Strongly agree")] <- 1
df$immigEcon_num[df$immigEcon %in% c("Agree")] <- 2
df$immigEcon_num[df$immigEcon %in% c("Don't Know ")] <- 3
df$immigEcon_num[df$immigEcon %in% c("Disagree")] <- 4
df$immigEcon_num[df$immigEcon %in% c("Strongly disagree")] <- 5

# table(df$immigEcon)
                                        
#########################################################
# Centred variables
#########################################################

df$populism_c <- scale(df$populism_scale,
                       center = TRUE,
                       scale = FALSE)[,]

df$ethno_c <- scale(df$ethno_scale,
                    center = TRUE,
                    scale = FALSE)[,]

df$trueScore7_c <- scale(df$trueScore7,
                         center = TRUE,
                         scale = FALSE)[,]

df$trueScore8_c <- scale(df$trueScore8,
                         center = TRUE,
                         scale = FALSE)[,]

df$immigFeelLikeHome_c <- scale(df$immigFeelLikeHome_num,
                         center = TRUE,
                         scale = FALSE)[,]

df$age_c <- scale(df$age,
                  center = TRUE,
                  scale = FALSE)[,]

# save(df, file="df")
# load(df)





#########################################################
# Wide to long
#########################################################

# Translating that to our dataset

# I want to gather all the T1 / T2 variables so that I have
# 2 rows per respondent, one for pre-correction ratings and
# one for post-correction ratings 
# I also want the trust and the accuracy variables in a long 
# format so I will treat those as T1 / T2, too. 

# Turn wide data into long 

# 20.9.2019 -- something went wrong here!!

names(df)


names(df_long)
df_long[, c("key", "value")]

df$belief_T1

x <- df[, c("user_id", "belief_T1", "belief_T1")]


df_long <-
  x %>% 
  gather(key, value, contains('_T', ignore.case = FALSE)) %>% 
  extract(key, c("question", "time"), "(.{2,})\\_(T.)") %>%
  spread(question, value)

# NB: gather all variables that contain the letters '_T'. 
# Split the key variable into 2 at the _ 
# Regular expressions: 
# .{5,} --> any 2 or more characters
# \\_  --> _
# T. --> for T1 or T2

# Can't bind data because some arguments have the same name

# Warning message:
# attributes are not identical across measure variables;
# they will be dropped 

# Lost some of the data types, so turn back into numeric

intoNum_long <- c(
  "noAsylum_num",
  "costImmig_num",
  "whiteCrime_num",
  "lowPaid_num",
  "trust_num",
  "belief",
  "belief_o",
  "belief_o_noAsylum",
  "belief_o_costImmig",
  "belief_o_whiteCrime",
  "belief_o_lowPaid",
  "belief_noAsylum",
  "belief_costImmig",
  "belief_whiteCrime",
  "belief_lowPaid",
  "belief_o_i2", 
  "belief_o_i2"
)


# Turn into numeric
df_long[ , intoNum_long] <- 
  lapply(df_long[ ,intoNum_long], as.numeric)


# Turn into factor
df_long$time <- as.factor(as.character(df_long$time))
df_long$wrong <- as.factor(as.character(df_long$wrong))

# str(df_long)

# Order columns
# df_long <- df_long[, c(1,137:157 ,2:136)]


# Order rows
df_long <- df_long[order(df_long$user_id),]

# END DATA MANAGEMENT

# TO DO -- FIGURE THIS OUT!!!

# save(df_long, file="df_long")


# # Same thing, more complicated
# # i. Run until select, check where the last _T variable is 
# # as of Sep 20th that's ro2 77...
# # ii. adapt gather (gather 2nd column : last _T column)
# # names(df_long1)
# 
# df_long1[ , c("key", "value")]
# 
# df_long1 <-
#   df %>%
#   select(user_id,
#          contains('_T', ignore.case = FALSE),
#          everything())  %>%
#   gather(key, value, 2:77) %>%
#   extract(key, c("question", "time"), "(.{2,})\\_(T.)") %>%
#   spread(question, value)

#########################################################
## How2 convert data from wide to long
#########################################################
# # 
# # General: http://www.cookbook-r.com/Manipulating_data/Converting_data_between_wide_and_long_format/
# # Gather multiple columns into 1: https://stackoverflow.com/questions/25925556/gather-multiple-sets-of-columns
# # Intro to stringr: https://r4ds.had.co.nz/strings.html
#
# temp_wide <- data.frame(
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
# library("dplyr")
# 
# temp_long1 <-
#   temp_wide %>%
#   gather(key, value, -id, -time)
# 
# temp_long2 <-
#   temp_wide %>%
#   gather(key, value, -id, -time) %>%
#   extract(key, c("question", "loop_number"), "(Q.\\..)\\.(.)")
# 
# temp_long3 <-
#   temp_wide %>%
#   gather(key, value, -id, -time) %>%
#   extract(key, c("question", "loop_number"), "(Q.\\..)\\.(.)") %>%
#   spread(question, value)
# 
# . matches any character
# \\. point (escape)

# Original variable Q3.2.1
# Transform into Q3.2 and add loop number 1

