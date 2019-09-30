#########################################################
# British Academy Project
# -- Graphs --
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


#########################################################
# R Colour Brewer
#########################################################

# Colour brewer
# pal <- choose_palette()
# http://colorbrewer2.org 

# Dark2_3 <- brewer.pal(3, "Dark2") 

# Same thing different order:
Dark2_3 <- c("#D95F02", "#7570B3", "#1B9E77")

# twoGreens <- brewer.pal(2, "Greens")
# twoGreens <- c("#99d8c9", "#2ca25f")
# twoGreens <- c("#238443", "#78c679") # one dark one light but different
# twoReds <- c("#cc4c02", "#fe9929") # one red one orange

twoGreens <- c("#74c476", "#bae4b3")
twoReds <- c("#fb6a4a", "#fcae91") 

fourDiverging <- c("#a6611a", "#dfc27d", "#80cdc1", "#018571")
twoBlues <- c("#a6bddb", "#2b8cbe")

lightBlue <- c("#a6bddb")
darkBlue <- c("#2b8cbe")

twoGreys <- c("#bdbdbd", "#636363")
threeGreys <- c("#f0f0f0", "#bdbdbd", "#636363")
fourGreys <- c("#f7f7f7", "#cccccc", "#969696", "#525252")

FourGreys <- c("#969696", "#737373", "#525252", "#252525")
FiveGreys <- c("#bdbdbd", "#969696", "#737373", "#525252", "#252525")
# FiveGreys <- c("#252525", "#525252", "#737373", "#969696","#bdbdbd")


noAsylum_lbl <- "'There has been a sharp rise in the number of people \napplying for asylum in the UK in the past 10 years.'"
costImmig_lbl <- "'Immigrants receive more in benefits and services than they pay in taxes.'"
whiteCrime_lbl <- "'The majority of crimes in London are committed by \nwhite people, not ethnic minorities.'"
lowPaid_lbl <- "'Immigration to the UK does not affect the wages of the low-paid.'"

accurate_lbl <- "1='Not at all accurate', 2='Not very accurate', 3='Fairly accurate', 4='Very accurate'"


#########################################################
# Create subsets
#########################################################

pro <- subset(df, immigOpinions == "more")
anti <- subset(df, immigOpinions == "fewer")
dk <- subset(df, immigOpinions == "no change")

anti_m2r <- subset(df, gotConsCorrection == 1  & motivatedToReject == 1)
pro_m2r <- subset(df, gotLibCorrection == 1 & motivatedToReject == 1)

ctrl <- subset(df, RandomGrp %in% c(1, 2))
ctrl_m2r <- subset(df, RandomGrp %in% c(1, 2) & motivatedToReject == 1)
ctrl_anti_m2r <- subset(df, gotConsCorrection == 1 & RandomGrp %in% c(1, 2) & motivatedToReject == 1)
ctrl_pro_m2r <- subset(df, gotLibCorrection == 1 & RandomGrp %in% c(1, 2) & motivatedToReject == 1)

trt <- subset(df, RandomGrp %in% c(3,8))
trt_m2r <- subset(df, RandomGrp %in% c(3, 8) & motivatedToReject == 1)
trt_anti_m2r <- subset(df, gotConsCorrection == 1 & RandomGrp %in% c(3, 8) & motivatedToReject == 1)
trt_pro_m2r <- subset(df, gotLibCorrection == 1 & RandomGrp %in% c(3, 8) & motivatedToReject == 1)

noAsylum <- subset(df, df$corrected == "noAsylum" & df$motivatedToReject == 1)
costImmig <- subset(df, df$corrected == "costImmig" & df$motivatedToReject == 1)
whiteCrime <- subset(df, df$corrected == "whiteCrime" & df$motivatedToReject == 1)
lowPaid <- subset(df, df$corrected == "lowPaid" & df$motivatedToReject == 1)


# con_m2r_biased <- subset(df, immigOpinions == "fewer" & motivatedToReject == 1 & FactFreeMessage=="Biased stats")
# con_m2r_experience <- subset(df, immigOpinions == "fewer" & motivatedToReject == 1 & FactFreeMessage=="Personal experience")
# con_m2r_ok2disagree  <- subset(df, immigOpinions == "fewer" & motivatedToReject == 1 & FactFreeMessage=="OK to disagree")
# 
# lib_m2r_biased <- subset(df, immigOpinions != "fewer" & motivatedToReject == 1 & FactFreeMessage=="Biased stats")
# lib_m2r_experience <- subset(df, immigOpinions != "fewer" & motivatedToReject == 1 & FactFreeMessage=="Personal experience")
# lib_m2r_ok2disagree <- subset(df, immigOpinions != "fewer" & motivatedToReject == 1 & FactFreeMessage=="OK to disagree")


#########################################################
# Sample characteristics
#########################################################

# Gender
prop.table(table(df$gender)) # 54% female

# Family
prop.table(table(df$marital))

# Age
min(df$age)
max(df$age)
mean(df$age) # 44.57 years
sd(df$age) # 17.45

# Region
prop.table(table(df$region))
prop.table(table(df$regions)) 

# Party
prop.table(table(df$supportParty))
prop.table(table(df$party)) 
prop.table(table(df$vote2017))  
# 26% Conservative, 30% Labour, 5% Liberal Democratic, 6% UKIP,
# 3% Green

# Education
prop.table(table(df$university)) # 51% university-educated

table(df$RandomGrp)
table(df$corrected)
table(df$immigOpinions, df$corrected)
round(prop.table(table(df$immigOpinions, df$corrected), 2), 2)
table(df$motivatedToReject)

# % of immigration-sceptics in each statement 
# 0.51 + 0.32 # 83
round(prop.table(table(con$corrected)), 2)

# % of pro-immigration people in each statement
# 0.27 + 0.31 # 0.58
round(prop.table(table(pro$corrected)), 2)

# % of dunnos in each statement
round(prop.table(table(dk$corrected)), 2)

#########################################################
# How many people got each statement? (by immigOpinions)
#########################################################

# dev.off()

corrected <- 
  ggplot(df,
         aes(corrected,
             fill=immigOpinions)) +
  geom_bar(position = "dodge") +
  facet_grid(. ~ immigOpinions) +
  scale_fill_manual(name="Immigration opinions",
                    values=Dark2_3) +
  labs(
    title = "Corrected statement by immigration opinions",
    # subtitle = a,
    x="",
    y="") + 
  theme(axis.text.x = element_text(angle = 90)) 
# theme_bw() # Hebt Farben auf

print(corrected)
ggsave("R/graphs/corrected.png")


#########################################################
# Immigration opinions
#########################################################

hist(df$immigNumbers_num)
table(df$immigNumbers)
prop.table(table(df$immigNumbers))
# This sample is VERY MUCH in favour of increasing immigration! 
# Only n=609 say fewer immigrants, n=1,388 say more

# prop.table(table(df$immigNo1))
# prop.table(table(df$immigNo123))

# Which issues do people list as most important? 
table(df$mostImportantIssue)

# Among those who thought immigration was the most 
# important issue, how many are anti-immig?
table(df$mostImportantIssue, df$immigOpinions)
table(df$immigOpinions, df$mostImportantIssue)


#########################################################
# Testing belief in false facts  
#########################################################

# How many people got NOTHING wrong, i.e. rated all false statements
# as 'Definitely false' ? --> 0 

temp <- 
  df %>%
  filter(
    noAsylum_num_T1 == 1 &
      costImmig_num_T1 == 1 &
      whiteCrime_num_T1 == 1 &
      lowPaid_num_T1 == 1
  ) %>%
  summarise(
    n=n()
  )

# How many people got their statement wrong (i.e. rated the 
# statement they got the correction for as TRUE (5, 6, or 7)

prop.table(table(df$wrong_T1))

# --> 87% got a correction for something they rated somewhere
# on the 'true' side of the scale.  

prop.table(table(df$belief_T1))

# --> 12% got a correction for something they didn't know.
# Less than 1% got a correction for something they rated 
# as 2 or 3, i.e. somewhere on the 'true' side of the scale.  

table(df$corrected)
round(prop.table(table(df$corrected)), 2)
# 0.40% noAsylum 0.24 costImmig 0.16 whiteCrime  0.20 lowPaid

table(df$motivatedToReject)
prop.table(table(df$motivatedToReject))

# motivated to reject?
round(prop.table(table(df$corrected, df$immigOpinions)), 2)


#########################################################
# H0 -- Belief in false facts (pre-correction)
#########################################################

# by immigration opinions

my_hist = function(x, y) {
  ggplot(df, 
         aes(x=.data[[x]], 
             fill=.data[[y]]
         )) +
    geom_bar(stat="count",
             position="dodge") +
    facet_grid(immigOpinions ~ .) +
    scale_fill_manual(name="Immigration opinions",
                      values=Dark2_3) +
    labs(
      # title = "'There has been a sharp rise in the number of people applying for \nasylum in the UK in the past ten years.'",
      subtitle = "All respondents",
      x="",
      y="") + 
    theme_bw()
}

#########################################################
# 1. There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years. 
#########################################################

hist_noAsylum_T1 <- my_hist("noAsylum_T1", "immigOpinions") + 
  ggtitle("'There has been a sharp rise in the number of people applying for \nasylum in the UK in the past ten years.'")
print(hist_noAsylum_T1)
ggsave("R/graphs/hist_noAsylum_T1.png")

#########################################################
# 2. Immigrants receive more in benefits and services than they pay in taxes.
#########################################################

hist_costImmig_T1 <- my_hist("costImmig_T1", "immigOpinions") + 
  ggtitle("'Immigrants receive more in benefits and services than they pay in taxes.'")
print(hist_costImmig_T1)
ggsave("R/graphs/hist_costImmig_T1.png")


#########################################################
# 3. The majority of crimes in London are committed by white people, not ethnic minorities. 
#########################################################

hist_whiteCrime_T1 <- my_hist("whiteCrime_T1", "immigOpinions") + 
  ggtitle("'The majority of crimes in London are committed by white people, not ethnic minorities.'")

print(hist_whiteCrime_T1)
ggsave("R/graphs/hist_whiteCrime_T1.png")


#########################################################
# 4. Immigration to the UK does not affect the wages of the low-paid.  
#########################################################

hist_lowPaid_T1 <- my_hist("whiteCrime_T1", "immigOpinions") + 
  ggtitle("'Immigration to the UK does not affect the wages of the low-paid.'")

print(hist_lowPaid_T1)
ggsave("R/graphs/hist_lowPaid_T1.png")


#########################################################
# Belief in distractor items by immigration opinions
#########################################################

# This was supposed to be appealing to liberals... 

hist_fracking_T1 <- my_hist("fracking_T1", "immigOpinions") + 
  ggtitle("'Fracking causes earthquakes.'")
print(hist_fracking_T1)
ggsave("R/graphs/hist_fracking_T1.png")

# This was supposed to be appealing to conservatives... 

hist_recession_T1 <- my_hist("recession_T1", "immigOpinions") + 
  ggtitle("'The Leave vote in June 2016 did not result in an instant UK recession.'")
print(hist_recession_T1)
ggsave("R/graphs/hist_recession_T1.png")

# Britain is the fifth largest economy in the world. 

hist_no5_T1 <- my_hist("no5_T1", "immigOpinions") + 
  ggtitle("'Britain is the fifth largest economy in the world.'")
print(hist_no5_T1)
ggsave("R/graphs/hist_no5_T1.png")

# England’s plastic bag usage dropped 85% since the 5p charge was introduced.

hist_plasticBags_T1 <- my_hist("plasticBags_T1", "immigOpinions") + 
  ggtitle("'England’s plastic bag usage dropped 85% since the 5p charge was introduced.'")
print(hist_plasticBags_T1)
ggsave("R/graphs/hist_plasticBags_T1.png")



#########################################################
# H1 -- Effect of the correction on belief in false claims
#########################################################

# Effect of the correction on veracity scores (pooled data)

# Before and after: convert to long
# https://stackoverflow.com/questions/50648388/histogram-with-two-variables-in-ggplot

# Looking at people who got the respective correction

#########################################################
# 1. There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years. 
#########################################################

# Histogram (numeric x)

hist_noAsylum_T1_T2 <- 
  df %>% 
  filter(corrected=="noAsylum") %>% 
  select(noAsylum_num_T1, noAsylum_num_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=Value,fill=Type)) + 
  geom_histogram(position="dodge", binwidth=.5) +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoGreens) +
  labs(
    title = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past ten years'",
    subtitle = "All respondents who saw the statistics",
    x="",
    y="") +
  scale_x_continuous(breaks=seq(1,7,1)) + # if numeric
  theme_bw() 

# Problem here: numeric  x; cannot rename the x-axis ticks
# --> turn x into factor:

# WORKING GRAPH
hist_noAsylum_ctrl_T1_T2.png <-
  df %>% 
  filter(corrected=="noAsylum" & 
           RandomGrp %in% c(1,2) &
           complete.cases(belief_noAsylum_T2) &
           immigOpinions == "fewer" 
  ) %>% 
  select(noAsylum_num_T1, noAsylum_num_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "'There has been a sharp rise in the number of people \napplying for asylum in the UK in the past ten years.'",
    subtitle = "Immigration-sceptics who saw the statistics",
    x="",
    y="") +
  # scale_x_continuous(breaks=seq(1,7,1))  + # if numeric
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) 

print(hist_noAsylum_ctrl_T1_T2.png)
ggsave("R/graphs/hist_noAsylum_ctrl_T1_T2.png")


# Now make a function
# PROBLEM: df$x2 is not working

my_hist_ctrl_T1_T2 = function(x, y, z) {
  df %>% 
    filter(corrected == x & 
             RandomGrp == 1 | df$RandomGrp == 2 &
             complete.cases(z) & # NOT WORKING! 
             immigOpinions == "fewer" 
    ) %>% 
    select(y, z) %>% 
    gather(key=Type, value=Value) %>% 
    ggplot(aes(x=factor(Value),fill=factor(Type))) +
    geom_bar(position = "dodge",
             color="black") +
    scale_fill_manual(name="Time",
                      labels=c("Before", "After"),
                      values=twoBlues) +
    labs(
      title = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past ten years.'",
      subtitle = "Immigration-sceptics who saw the statistics",
      x="",
      y="") +
    # scale_x_continuous(breaks=seq(1,7,1))  + # if numeric
    theme_bw() +
    scale_x_discrete(labels=c("1" = "0 Definitely false",
                              "2" = "1",
                              "3" = "2",
                              "4" = "3",
                              "5" = "4",
                              "6" = "5",
                              "7" = "6 Definitely true"))
}

# PROBLEM: Can't get rid of ugly NAs
# --> doing this manually, for the time being
p <- my_hist_ctrl_T1_T2("noAsylum",
                        "noAsylum_num_T1",
                        "noAsylum_num_T2")
p


#########################################################
# 2. Immigrants receive more in benefits and services than they pay in taxes.
#########################################################

hist_costImmig_ctrl_T1_T2.png <-
  df %>% 
  filter(corrected=="costImmig" & 
           RandomGrp == 1 | df$RandomGrp == 2 &
           complete.cases(df$belief_costImmig_T2) &
           immigOpinions == "fewer") %>% 
  select(costImmig_num_T1, costImmig_num_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "'Immigrants receive more in benefits and services than they pay in taxes.'",
    subtitle = "Immigration-sceptics who saw the statistics",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) 

print(hist_costImmig_ctrl_T1_T2.png)
ggsave("R/graphs/hist_costImmig_ctrl_T1_T2.png")


#########################################################
# 3. The majority of crimes in London are committed by white people, not ethnic minorities. 
#########################################################

hist_whiteCrime_ctrl_T1_T2 <-
  df %>% 
  filter(corrected=="whiteCrime" & 
           RandomGrp == 1 | df$RandomGrp == 2 &
           complete.cases(df$belief_whiteCrime_T2) &
           immigOpinions == "more"
  ) %>% 
  select(whiteCrime_num_T1, whiteCrime_num_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "'The majority of crimes in London are committed by white people, \nnot ethnic minorities.'",
    subtitle = "Pro-immigration respondents who saw the statistics",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) 

print(hist_whiteCrime_ctrl_T1_T2)
ggsave("R/graphs/hist_whiteCrime_ctrl_T1_T2.png")


#########################################################
# 4. Immigration to the UK does not affect the wages of the low-paid.  
#########################################################

hist_lowPaid_ctrl_T1_T2 <-
  df %>% 
  filter(corrected=="lowPaid" & 
           RandomGrp == 1 | df$RandomGrp == 2 &
           complete.cases(df$belief_lowPaid_T2) &
           immigOpinions == "fewer"
  ) %>% 
  select(belief_lowPaid_T1, belief_lowPaid_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge", 
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "'Immigration to the UK does not affect the wages of the low-paid.'",
    subtitle = "Pro-immigration respondents who saw the statistics",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) 

print(hist_lowPaid_ctrl_T1_T2)
ggsave("R/graphs/hist_lowPaid_ctrl_T1_T2.png")


#########################################################
# H1 -- Histogram -- pooled data (all statements)
#########################################################

# Effect of the correction on veracity scores (pooled data)

hist_all_ctrl_T1_T2 <-
  df %>% 
  filter(RandomGrp == 1 | df$RandomGrp == 2 & 
           motivatedToReject == 1) %>% 
  select(belief_T1, belief_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "Effect of the Correction on Belief in False Facts (Control Group)",
    subtitle = "Pooled Data (Control group, 2 * anti immmigration statements, 2 * pro, motivated to reject statistics)",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) +
  theme_light() + 
  theme(legend.position = "top")

print(hist_all_ctrl_T1_T2)
ggsave("R/graphs/hist_all_ctrl_T1_T2.png")


# ... Spillover I

hist_all_ctrl_i2_T1_T2 <-
  df %>% 
  filter(
    wrong_T1 == 1 & 
    wrong_i2_T1 == 1 &
    RandomGrp %in% c(1,2) &
    motivatedToReject == 1) %>% 
  select(belief_i2_T1, belief_i2_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "Effect of the Correction on Belief in Un-Fact-Checked False Fact (Control Group)",
    subtitle = "Pooled Data (Respondents who rated both false facts as true and were motivated to reject the statistics)",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) +
  theme_light() + 
  theme(legend.position = "top")

print(hist_all_ctrl_i2_T1_T2)
ggsave("R/graphs/hist_all_ctrl_i2_T1_T2.png")


# Spillover II

hist_all_trt_i2_T1_T2 <-
  df %>% 
  filter(
    wrong_T1 == 1 & 
      wrong_i2_T1 == 1 &
      RandomGrp %in% c(3, 8) &
      motivatedToReject == 1) %>% 
  select(belief_i2_T1, belief_i2_T2) %>% 
  gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(Value),fill=factor(Type))) +
  geom_bar(position = "dodge",
           color="black") +
  scale_fill_manual(name="Time",
                    labels=c("Before", "After"),
                    values=twoBlues) +
  labs(
    title = "Effect of the Correction on Belief in Un-Fact-Checked False Fact (Treatment Groups)",
    subtitle = "Pooled Data (Respondents who rated both false facts as true and were motivated to reject the statistics)",
    x="",
    y="") +
  theme_bw() +
  scale_x_discrete(labels=c("1" = "0 Definitely false",
                            "2" = "1",
                            "3" = "2",
                            "4" = "3",
                            "5" = "4",
                            "6" = "5",
                            "7" = "6 Definitely true")) +
  theme_light() + 
  theme(legend.position = "top")

print(hist_all_trt_i2_T1_T2)
ggsave("R/graphs/hist_all_trt_i2_T1_T2.png")


#########################################################
# H1 -- Effect of the correction on MEAN Belief in False Claims
#########################################################

# Control group

df$corrected <- factor(df$corrected, 
                       levels = c("noAsylum", "costImmig",
                                  "whiteCrime", "lowPaid"),
                  labels = c("number of asylum seekers", 
                             "cost/benefits", 
                             "white crime",
                             "low-paid wages"))

statement_labels <- c(noAsylum_lbl, costImmig_lbl,
                      whiteCrime_lbl, lowPaid_lbl)


# Effect of the correction on mean veracity scores

effect_ctrl_m2r <- 
ggplot(subset(df_long, 
              RandomGrp == 1 | RandomGrp == 2 & 
                motivatedToReject == 1),
       aes( time, # .data[[y]],  #FactFreeReply, 
            belief_o, # .data[[z]],  # noAsylum_num
            fill = time # fill=time # w
       )) +
  facet_grid(. ~ corrected,
             # labeller = labeller(corrected = c("df", "adfa", "afds", "fdsafd"))) +
             labeller = label_both) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(0,6)) +
  scale_y_continuous(breaks=seq(0,6,1)) +
  geom_abline(slope=0, intercept=3,  col = "black", lty=2) +
  scale_fill_manual(name = "time", 
                    # labels = c("before", 
                    #            "after"),
                    values=twoBlues) +
  labs(title="Effect of expert statement on belief in false claims",
       subtitle = "Average veracity scores (1='Definitely false' 7='Definitely true') \nControl Group who saw statistics that challenged their opinions about immigration.",
       x="", 
       y="") +
  theme_light() + 
  theme(legend.position = "top")

print(effect_ctrl_m2r)
ggsave("R/graphs/effect_ctrl_m2r.png")



#########################################################
# H2 -- Effect of the fact-free comment on belief in false claims
#########################################################

# Looking at the effect of the correction on MEAN
# i. belief in false claims, ii. trust, iii.accuracy

# ACHTUNG!! Within-subject error bars
# http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/ 

# ggplot function to plot means by whether or not they got a 
# fact-free follow-up. (I am using this function and then 
# adding subtitles for each statement)

# template: lying_not_lying_agreed.png in quiz_graphs
# automated exploratory plots: https://aosmith.rbind.io/2018/08/20/automating-exploratory-plots/

#########################################################
# Control -- diff by m2r
#########################################################

diff_by_m2r <- 
  ggplot(subset(df, RandomGrp == 1 | RandomGrp == 2), 
         aes(diff_o,
             fill=motivatedToReject_fac #corrected
         )) +
  geom_bar(position = "dodge",
           colour="black") +
  facet_grid(motivatedToReject_fac ~ .) + #corrected
  scale_fill_manual(name="Motivation to reject statistics",
                    values=redGreyGreen) + # threeGreys # Dark2_3
  # scale_fill_manual(name="Immigration opinions",
  #                   values=Dark2_3) +
  labs(
    title = "Effect of expert statement on belief in false claims",
    subtitle = "Pre-post-correction belief (+ positive effect; 0 no effect; - backfire effect). \nControl group.",
    x="",
    y="") +
  theme(strip.text.y = element_text(angle=0)) + 
  theme_bw() # Hebt Farben auf

print(diff_by_m2r)
ggsave("R/graphs/diff_by_m2r.png")


#########################################################
# Control -- diff by immigOpinions
#########################################################

diff_by_immigOpinions <- 
  ggplot(subset(df, 
                motivatedToReject == 1 & 
                  RandomGrp == 1 | RandomGrp == 2),
         aes(diff_o, # diff_o 
             fill=immigOpinions)) +
  geom_bar(position = "dodge",
           colour="black") +
  facet_grid(immigOpinions ~ .) + # FactFreeReply
  scale_fill_manual(name="Immigration opinions",
                    values=redGreyGreen) + # threeGreys # Dark2_3
  labs(
    title = "Effect of expert statement on belief in false claims",
    subtitle = "Pre-post-correction belief (+ positive effect; 0 no effect; - backfire effect). \nControl Group who saw statistics that challenged their opinions about immigration.",
    x="",
    y="") + 
  theme_bw() # Hebt Farben auf

print(diff_by_immigOpinions)
ggsave("R/graphs/diff_by_immigOpinions.png")


#########################################################
# Control -- diff by gender
#########################################################

diff_by_gender <- 
  ggplot(subset(df, RandomGrp == 1 | RandomGrp == 2 &
                  complete.cases(df$gender)), 
         # gender %in% c("Male", "Female") ), 
         aes(diff_o
         )) +
  geom_bar(position = "dodge",
           colour="black") +
  facet_grid(gender ~ .) + #corrected
  scale_fill_manual(name="Gender",
                    values=threeGreys) + # Dark2_3
  # scale_fill_manual(name="Immigration opinions",
  #                   values=Dark2_3) +
  labs(
    title = "Effect of expert statement on belief in false claims",
    subtitle = "Pre-post-correction belief (+ positive effect; 0 no effect; - backfire effect). \nControl group.",
    x="",
    y="") +
  theme(strip.text.y = element_text(angle=0)) + 
  theme_bw() # Hebt Farben auf

print(diff_by_gender)
ggsave("R/graphs/diff_by_gender.png")


#########################################################
# Control -- diff by populism
#########################################################

diff_by_pop <- 
  ggplot(subset(df, # motivatedToReject == 1 &
                  RandomGrp == 1 | RandomGrp == 2),
         aes(diff_o,
             fill=pop_quartile_fac)) +
  geom_bar(position = "dodge",
           colour="black") +
  facet_grid(pop_quartile_fac ~ immigOpinions) +
  scale_fill_manual(name="populism quartile",
                    values=fourGreys) +
  labs(
    title = "Effect of expert statement on belief in false claims",
    subtitle = "Pre-post-correction belief (+ positive effect; 0 no effect; - backfire effect). \nControl Group respondents who saw statistics that challenged their opinions about immigration.",
    x="",
    y="") +
  theme(strip.text.y = element_text(angle=0) ) + 
  theme_bw() # Hebt Farben auf

print(diff_by_pop)
ggsave("R/graphs/diff_by_pop.png")

# --> The higher you score on populism the less you 
# change your factual beliefs after reading an 
# expert opinion. (Duh)

#########################################################
# Control -- diff by nationalism
#########################################################

diff_by_ethno <- 
  ggplot(subset(df, motivatedToReject == 1 &
                  RandomGrp == 1 | RandomGrp == 2),
         aes(diff_o,
             fill=ethno_quartile_fac)) +
  geom_bar(position = "dodge",
           colour="black") +
  facet_grid(ethno_quartile_fac ~ immigOpinions) +
  scale_fill_manual(name="nationalism quartile",
                    values=fourGreys) +
  labs(
    title = "Effect of expert statement on belief in false claims",
    subtitle = "Pre-post-correction belief (+ positive effect; 0 no effect; - backfire effect). \nControl Group respondents who saw statistics that challenged their opinions about immigration.",
    x="",
    y="") +
  theme(strip.text.y = element_text(angle=0))
# theme_bw() # Hebt Farben auf

print(diff_by_ethno)
ggsave("R/graphs/diff_by_ethno.png")

# --> People high in nationalism & populism are less likely
# to change their factual opinions

# TO DO: Are they also more likely to discredit the source? 
# i.e. say they are not trustworthy / accurate? 


#########################################################
# H2 -- Four statements -- Full results
#########################################################

# TO DO: LEAVE OUT FOR DISSERTATION (TOO CLUTTERED)

my_plot_mean = function(x, y, z) {
  ggplot(x, 
         aes( .data[[y]],  #FactFreeMessage, 
              .data[[z]],  # noAsylum_num
              fill = time # fill=time # w
         )) +
    facet_grid(curbImmig_fac ~ FactFreeSource) +
    stat_summary(fun.y = mean,
                 geom = "bar",
                 position="dodge",
                 color="black") +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 position = position_dodge(width = .9),
                 width = .2
    ) +
    coord_cartesian(ylim=c(1,6)) +
    geom_abline(slope=0, intercept=3,  col = "black", lty=2) +
    scale_fill_manual(name = "time", 
                      labels = c("before", 
                                 "after"),
                      values=twoBlues) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(title="Effect of fact-free comments on belief in false claims",
         subtitle = "(1='Definitely false' 7='Definitely true')",
         x="", 
         y="")
}

#########################################################
# noAsylum
#########################################################

m2r_noAsylum <- my_plot_mean(subset(df_long, immigNumbers_num < 4),
                             "FactFreeMessage", "belief_noAsylum") +
  labs(title=noAsylum_lbl)
print(m2r_noAsylum)
ggsave("R/graphs/m2r_noAsylum.png")

# For anti-immig people, biased stats has the highest effect

all_noAsylum <- my_plot_mean(df_long,
                             "FactFreeMessage", 
                             "belief_noAsylum") +
  labs(title=noAsylum_lbl)
print(all_noAsylum)
ggsave("R/graphs/all_noAsylum.png")


#########################################################
# costImmig
#########################################################

m2r_costImmig <- my_plot_mean(subset(df_long, immigNumbers_num < 4),
                              "FactFreeMessage", "belief_costImmig") +
  labs(title=costImmig_lbl)
print(m2r_costImmig)
ggsave("R/graphs/effect of correction/motivatedToReject/m2r_costImmig.png")

all_costImmig <- my_plot_mean(df_long,
                              "FactFreeMessage", "belief_costImmig") +
  labs(title=costImmig_lbl)

print(all_costImmig)
ggsave("R/graphs/all_costImmig.png")


#########################################################
# whiteCrime
#########################################################

m2r_whiteCrime <- my_plot_mean(subset(df_long, 
                                      immigNumbers_num > 4),
                               "FactFreeMessage", 
                               "belief_whiteCrime") +
  labs(title=whiteCrime_lbl)
print(m2r_whiteCrime)
ggsave("R/graphs/m2r_whiteCrime.png")

# --> large CIs

all_whiteCrime <- my_plot_mean(df_long,
                               "FactFreeMessage", "belief_whiteCrime") +
  labs(title=whiteCrime_lbl)
print(all_whiteCrime)
ggsave("R/graphs/all_whiteCrime.png")


#########################################################
# lowPaid
#########################################################

m2r_lowPaid <- my_plot_mean(subset(df_long, 
                                   immigNumbers_num > 4),
                            "FactFreeMessage", 
                            "belief_lowPaid") +
  labs(title=lowPaid_lbl)
print(m2r_lowPaid)
ggsave("R/graphs/m2r_lowPaid.png")

# --> Large CIs. Re-correction had no effect. 

all_lowPaid <- my_plot_mean(df_long,
                            "FactFreeMessage", 
                            "belief_lowPaid") +
  labs(title=lowPaid_lbl)
print(all_lowPaid)
ggsave("R/graphs/all_lowPaid.png")





# HIER WEITER

#########################################################
# Effect of the correction on belief in false claims
# -- Pooling source and/or message
#########################################################

my_plot_mean = function(x, y, z) {
  ggplot(x, # df_long, 
         aes(.data[[y]], #=x, #FactFreeReply, 
             .data[[z]], #=y # noAsylum_num #,
             fill = time # fill=time
         )) +
    facet_grid(. ~ curbImmig_fac) +
    stat_summary(fun.y = mean,
                 geom = "bar",
                 position="dodge",
                 color="black") +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 position = position_dodge(width = .9),
                 width = .2
    ) +
    coord_cartesian(ylim=c(0,6)) +
    scale_y_continuous(breaks=seq(0,6,1)) + # tick every point
    geom_abline(slope=0, intercept=3,  col = "black", lty=2) +
    scale_fill_manual(name = "time", 
                      labels = c("before", 
                                 "after"),
                      values=twoBlues) +
    labs(title="Effect of fact-free comments on belief in false claims",
         subtitle = "(0='Definitely false' 6='Definitely true')",
         x="\nDid respondent receive a fact-free comment after they saw the statistics?", 
         y="") +
    theme_bw()
}


#########################################################
# 4 STATEMENTS -- Yes/No FFC
#########################################################

#########################################################
# noAylum 
#########################################################

# byReply
p1 <- 
  my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
               "FactFreeReply", "belief_o_noAsylum") +     
  labs(title = noAsylum_lbl) 

print(p1)
ggsave("R/graphs/noAsylum_T1_T2_byReply.png")

# byMessage
p1a <- my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
                    "FactFreeMessage", 
                    "belief_o_noAsylum") +     
  labs(title = noAsylum_lbl,
       x="") #+
#theme(axis.text.x = element_text(angle = 90))
print(p1a)
ggsave("R/graphs/noAsylum_T1_T2_byMessage.png")

# bySource
p1b <- my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
                    "FactFreeSource", "belief_o_noAsylum") +     
  labs(title = noAsylum_lbl,
       x="") # Who dunnit
print(p1b)
ggsave("R/graphs/noAsylum_T1_T2_bySource.png")


#########################################################
# costImmig
#########################################################

# byReply
p2 <- 
  my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
               "FactFreeReply", "belief_o_costImmig") +     
  labs(title = costImmig_lbl)
print(p2)
ggsave("R/graphs/costImmig_T1_T2_byReply.png")

# byMessage
p2a <- my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
                    "FactFreeMessage", "belief_o_costImmig") +     
  labs(title = costImmig_lbl,
       x="")
print(p2a)
ggsave("R/graphs/costImmig_T1_T2_byMessage.png")

# bySource
p2b <- my_plot_mean(subset(df_long, df_long$immigOpinions == "fewer"), 
                    "FactFreeSource", "belief_o_costImmig") +     
  labs(title = costImmig_lbl,
       x="")
print(p2b)
ggsave("R/graphs/costImmig_T1_T2_bySource.png")


#########################################################
# whiteCrime
#########################################################

# byComment
p3 <- 
  my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                        df_long$immigOpinions == "no change"), 
               "FactFreeReply", "belief_o_whiteCrime")  +     
  labs(title = whiteCrime_lbl)
print(p3)
ggsave("R/graphs/whiteCrime_T1_T2_byReply.png")

# byMessage
p3a <- my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                             df_long$immigOpinions == "no change"), 
                    "FactFreeMessage", "belief_o_whiteCrime")  +     
  labs(title = whiteCrime_lbl,
       x="")
print(p3a)
ggsave("R/graphs/whiteCrime_T1_T2_byMessage.png")

# bySource
p3b <- my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                             df_long$immigOpinions == "no change"), 
                    "FactFreeSource", "belief_o_whiteCrime")  +     
  labs(title = whiteCrime_lbl,
       x="")
print(p3b)
ggsave("R/graphs/whiteCrime_T1_T2_bySource.png")


#########################################################
# lowPaid
#########################################################

# byReply
p4 <- my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                            df_long$immigOpinions == "no change"), 
                   "FactFreeReply", "belief_o_lowPaid") +     
  labs(title = lowPaid_lbl)
print(p4)
ggsave("R/graphs/lowPaid_T1_T2_byReply.png")

# byMessage
p4a <- my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                             df_long$immigOpinions == "no change"), 
                    "FactFreeMessage", "belief_o_lowPaid") +     
  labs(title = lowPaid_lbl,
       x="")
print(p4a)
ggsave("R/graphs/lowPaid_T1_T2_byMessage.png")

# bySource
p4b <- my_plot_mean(subset(df_long, df_long$immigOpinions == "more" | 
                             df_long$immigOpinions == "no change"), 
                    "FactFreeSource", "belief_o_lowPaid") +     
  labs(title = lowPaid_lbl,
       x="")
print(p4b)
ggsave("R/graphs/lowPaid_T1_T2_bySource.png")




#########################################################
# H2 -- Effect of the FFC on belief in false claims 
# (pooled data) -- USE THIS
#########################################################

# byReply
p0 <- 
  my_plot_mean(
    subset(df_long, motivatedToReject == 1),
    "FactFreeReply", 
    "belief_o") 

print(p0)
ggsave("R/graphs/all_T1_T2_byReply.png")

# Just one graph
p0 <- 
ggplot(df_long, 
       aes(FactFreeReply, 
           belief,
           fill = time
       )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(0,6)) +
  scale_y_continuous(breaks=seq(0,6,1)) + # tick every point
  geom_abline(slope=0, intercept=3,  col = "black", lty=2) +
  scale_fill_manual(name = "time", 
                    labels = c("before", 
                               "after"),
                    values=twoBlues) +
  labs(title="Effect of fact-free comments on belief in false claims",
       subtitle = "(0='Definitely false' 6='Definitely true')",
       x="\nDid respondent receive a fact-free comment after they saw the statistics?", 
       y="") +
  theme_bw()


# byMessage
p0a <- 
  my_plot_mean(
    subset(df_long, motivatedToReject == 1),
    "FactFreeMessage", 
    "belief_o") +
  facet_grid(. ~ motivatedToReject)

print(p0a)
ggsave("R/graphs/all_T1_T2_byMessage.png")

# bySource
p0b <- 
  my_plot_mean(
    subset(df_long, motivatedToReject == 1),
    "FactFreeSource", 
    "belief_o") +
  facet_grid(. ~ motivatedToReject)

print(p0b) 
ggsave("R/graphs/all_T1_T2_bySource.png") 






#########################################################
# ALL ANTI-IMMIGRATION CLAIMS
#########################################################

# byReply
p0 <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions == "fewer" & 
                      df_long$corrected %in% c("noAsylum", "costImmig")
  ),
  "FactFreeReply", "belief_o") +     
  labs(title = "Anti-immigration false facts") 

print(p0)
ggsave("R/graphs/anti_T1_T2_byReply.png")

# byMessage
p0a <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions == "fewer" & 
                      df_long$corrected %in% c("noAsylum", "costImmig")
                      ), 
               "FactFreeMessage", 
               "belief_o") +     
  labs(title = "Anti-immigration false facts",
       x="") 

print(p0a)
ggsave("R/graphs/anti_T1_T2_byMessage.png")

# bySource
p0b <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions == "fewer" & 
                      df_long$corrected %in% c("noAsylum", "costImmig")), 
               "FactFreeSource", 
               "belief_o") + 
  labs(title = "Anti-immigration false facts", 
       x="")  

print(p0b) 
ggsave("R/graphs/anti_T1_T2_bySource.png") 



#########################################################
# ALL PRO-IMMIGRATION CLAIMS
#########################################################

# by Reply
p00 <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions %in% c("no change", "more") & 
                      df_long$corrected %in% c("whiteCrime", "lowPaid")),
               "FactFreeReply", "belief_o") +     
  labs(title = "Pro-immigration false facts") 

print(p00)
ggsave("R/graphs/pro_T1_T2_byReply.png")

# byMessage 
p00a <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions %in% c("no change", "more") & 
                        df_long$corrected %in% c("whiteCrime", "lowPaid")), 
               "FactFreeMessage", 
               "belief_o") +     
  labs(title = "Pro-immigration false facts",
       x="") 

print(p00a)
ggsave("R/graphs/pro_T1_T2_byMessage.png")

# bySource 
p00b <- 
  my_plot_mean(subset(df_long, 
                      df_long$immigOpinions %in% c("no change", "more") & 
                        df_long$corrected %in% c("whiteCrime", "lowPaid")), 
               "FactFreeSource", 
               "belief_o") + 
  labs(title = "Pro-immigration false facts", 
       x="") 

print(p00b)
ggsave("R/graphs/pro_T1_T2_bySource.png")







#########################################################
# 5.2	Effect of post-truth comments on trust & accuracy
#########################################################

# Baseline -- Trust & Accuracy in control group 

df %>%
  filter(motivatedToReject == 1 &
           RandomGrp == 1 | RandomGrp == 2) %>%
  group_by(corrected) %>%
  summarise(
    mean_trust = mean(trustAuthority_num, na.rm=T),
    sd_trust = sd(trustAuthority_num, na.rm=T),
    mean_acc = mean(accurateAuthority_num, na.rm=T),
    sd_acc = sd(accurateAuthority_num, na.rm=T) 
  )

# Trust & Accuracy in treatment groups

df %>%
  filter(motivatedToReject == 1 # &
           #corrected %in% c("noAsylum", "costImmig")
         ) %>%
  group_by(corrected, FactFreeReply) %>%
  summarise(
    mean_trust = mean(trustAuthority_num, na.rm=T),
    sd_trust = sd(trustAuthority_num, na.rm=T),
    mean_acc = mean(accurateAuthority_num, na.rm=T),
    sd_acc = sd(accurateAuthority_num, na.rm=T) 
  )


#########################################################
# Effect of the correction on accuracy & trustworthinesss
#########################################################

mean_by_corrected = function(x, y, z) {
  ggplot(x, 
       aes( .data[[y]], # FactFreeReply,  
            .data[[z]] # trustAuthority_num
            # fill = time # fill=time # w
       )) +
  facet_grid(curbImmig_fac ~ corrected) + 
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black",
               fill=darkBlue # fill="gainsboro" # = SAME GREY AS FACTOR LEVEL BACKGROUND IN FACET_GRID
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  # coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(name = "time", 
                    labels = c("before", 
                               "after"),
                    values=twoBlues) +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Effect of fact-free comments on trust in expert",
       subtitle = "(1='Would not trust at all' 7='Would trust a great deal')",
       x="\n Fact Free Reply", 
       y="") +
  theme_bw()
}


#########################################################
# Effect of the PT commment on PT thinking
#########################################################

all_factOpinion_byReply <- mean_by_corrected(df, 
                                             "FactFreeReply", 
                                             "factOpinion_num")


all_ok2disagree_byReply <- mean_by_corrected(df, 
                                             "FactFreeReply", 
                                             "ok2disagree_num")

df$ok2disagree_num


#########################################################
# i. trust and iii. accuracy -- POOLED (pro and anti-immig)
#########################################################

#########################################################
# ALL STATEMENTS
#########################################################

all_trust_byReply <- mean_by_corrected(df, 
                                        "FactFreeReply", 
                                        "trustAuthority_num")

all_trust_bySource <- mean_by_corrected(df, 
                                       "FactFreeSource", 
                                       "trustAuthority_num")

all_trust_byMessage <- mean_by_corrected(df, 
                                        "FactFreeMessage", 
                                        "trustAuthority_num")



all_accuracy_byReply <- mean_by_corrected(df, 
                                       "FactFreeReply", 
                                       "accurateAuthority_num")

all_accuracy_bySource <- mean_by_corrected(df, 
                                        "FactFreeSource", 
                                        "accurateAuthority_num")

all_accuracy_byMessage <- mean_by_corrected(df, 
                                         "FactFreeMessage", 
                                         "accurateAuthority_num")


# The below is too much... 

anti_trust_byReply <- mean_by_corrected(subset(df, #
                              df$corrected %in% c("noAsylum", "costImmig") &
                                df$immigOpinions == "fewer"),
                       "FactFreeReply",
                       "trustAuthority_num")

print(anti_trust_byReply)
ggsave("R/graphs/anti_trust_byReply.png")

anti_trust_byMessage <- mean_by_corrected(subset(df, #
                                       df$corrected %in% c("noAsylum", "costImmig") &
                                         df$immigOpinions == "fewer"),
                                "FactFreeMessage",
                                "trustAuthority_num")

print(anti_trust_byMessage)
ggsave("R/graphs/anti_trust_byMessage.png")

anti_trust_bySource <- mean_by_corrected(subset(df, #
                                                 df$corrected %in% c("noAsylum", "costImmig") &
                                                   df$immigOpinions == "fewer"),
                                          "FactFreeSource",
                                          "trustAuthority_num")

print(anti_trust_bySource)
ggsave("R/graphs/anti_trust_bySource.png")


anti_acc_byReply <- 
  mean_by_corrected(subset(df, 
                           df$corrected %in% c("noAsylum", "costImmig") & 
                             df$immigOpinions == "fewer"),
                    "FactFreeReply", 
                    "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="\n Fact Free Reply", 
       y="")
  
print(anti_acc_byReply)
ggsave("R/graphs/anti_acc_byReply.png")


anti_acc_byMessage <- 
  mean_by_corrected(
    subset(df,  df$corrected %in% c("noAsylum", "costImmig") &
             df$immigOpinions == "fewer"),
    "FactFreeMessage", 
    "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="", 
       y="")

print(anti_acc_byMessage)
ggsave("R/graphs/anti_acc_byMessage.png")

anti_acc_bySource <- mean_by_corrected(subset(df, 
                                               df$corrected %in% c("noAsylum", "costImmig") & 
                                                 df$immigOpinions == "fewer"),
                                        "FactFreeSource", 
                                        "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="", 
       y="")

print(anti_acc_bySource)
ggsave("R/graphs/anti_acc_bySource.png")


#########################################################
# Pro-immigration false claims
#########################################################

pro_trust_byReply <- mean_by_corrected(subset(df,
                                  df$corrected %in% c("whiteCrime", "lowPaid") & 
                                  df$immigOpinions %in% c("no change", "more")),
                            "FactFreeReply", "trustAuthority_num")

print(pro_trust_byReply)
ggsave("R/graphs/pro_trust_byReply.png")

pro_trust_byMessage <- mean_by_corrected(subset(df,
                                              df$corrected %in% c("whiteCrime", "lowPaid") & 
                                                df$immigOpinions %in% c("no change", "more")),
                                       "FactFreeMessage", "trustAuthority_num")

print(pro_trust_byMessage)
ggsave("R/graphs/pro_trust_byMessage.png")

pro_trust_bySource <- mean_by_corrected(subset(df,
                                                df$corrected %in% c("whiteCrime", "lowPaid") & 
                                                  df$immigOpinions %in% c("no change", "more")),
                                         "FactFreeSource", "trustAuthority_num")

print(pro_trust_bySource)
ggsave("R/graphs/pro_trust_bySource.png")



pro_acc_byReply <- mean_by_corrected(subset(df, 
                                df$corrected %in% c("whiteCrime", "lowPaid") & 
                                df$immigOpinions %in% c("no change", "more")),
                          "FactFreeReply", 
                          "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="\n Fact Free Reply", 
       y="")

print(pro_acc_byReply)
ggsave("R/graphs/pro_acc_byReply.png")


pro_acc_byMessage <- mean_by_corrected(subset(df, 
                                            df$corrected %in% c("whiteCrime", "lowPaid") & 
                                              df$immigOpinions %in% c("no change", "more")),
                                     "FactFreeMessage", 
                                     "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="\n Fact Free Reply", 
       y="")

print(pro_acc_byMessage)
ggsave("R/graphs/pro_acc_byMessage.png")

pro_acc_bySource <- mean_by_corrected(subset(df, 
                                              df$corrected %in% c("whiteCrime", "lowPaid") & 
                                                df$immigOpinions %in% c("no change", "more")),
                                       "FactFreeSource", 
                                       "accurateAuthority_num") +
  geom_abline(slope=0, intercept=2.5,  col = "black", lty=2) +
  labs(title="Effect of fact-free comments on perceived accuracy",
       subtitle = "(1='Not at all accurate', 2='Not very accurate' 3='Fairly accurate', 4='Very accurate')",
       x="\n Fact Free Reply", 
       y="")

print(pro_acc_bySource)
ggsave("R/graphs/pro_acc_bySource.png")



#########################################################
# ii. trust -- FULL RESULTS
#########################################################

# my_plot_mean3 = function(x, z) {
#   ggplot(x, 
#          aes( FactFreeMessage, # .data[[y]], # FactFreeMessage,    
#               .data[[z]] #,
#               # fill = time 
#          )) +
#     facet_grid(curbImmig_fac ~ FactFreeSource) +
#     stat_summary(fun.y = mean,
#                  geom = "bar",
#                  position="dodge",
#                  color="black") +
#     stat_summary(fun.data = mean_cl_normal,
#                  geom = "errorbar",
#                  position = position_dodge(width = .9),
#                  width = .2
#     ) +
#     coord_cartesian(ylim=c(1,7)) +
#     geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
#     scale_fill_manual(name = "Rating", 
#                       labels = c("Expert", 
#                                  "Commentator"),
#                       values=twoBlues) +
#     labs(title="Effect of fact-free comments on trust in expert and commentator",
#          subtitle = "(1='Would not trust at all', 7='Would trust a great deal')",
#          x="\nMessage", 
#          y="") +
#     theme_bw() + 
#     theme(axis.text.x = element_text(angle = 90),
#           strip.text.x = element_text(size=10, face="bold"), 
#           strip.text.y = element_text(size=10, face="bold"), # angle=0
#           strip.background = element_rect(colour="black", fill="WhiteSmoke"
#           )) 
# }
# 
# # trust
# 
# noAsylum_trust2.png <- my_plot_mean3(subset(df_long, 
#                                             df_long$corrected=="noAsylum" &
#                                               df_long$immigNumbers_num < 4),
#                                      "trust_num") + 
#   labs(subtitle = noAsylum_lbl)  
# # labs(subtitle = "(1='Would not trust at all', 7='Would trust a great deal')")
# 
# print(noAsylum_trust2.png)
# ggsave("R/graphs/noAsylum_trust2.png")
# 
# # ...
# # --> Not that important

mean_by_Msg_Source = function(x, z) {  
  ggplot(x, 
         aes(FactFreeMessage, 
             .data[[z]], # y=trustAuthority_num,
             fill=FactFreeSource
         )) +
    facet_grid(curbImmig_fac ~ FactFreeSource) +
    stat_summary(fun.y = mean,
                 geom = "bar",
                 position="dodge",
                 colour="black") +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 position = position_dodge(width = .9),
                 width = .2
    ) +
    # coord_cartesian(ylim=c(1,7)) +
    # geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
    scale_fill_manual(values=c("grey99", "#002147", "grey66")) +
    labs(title="Effect of fact-free comment on trust in expert opinion",
         x="\nStated reason to 'take those statistics with a pinch of salt'", y="",
         subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
    ) +
    theme(axis.text.x = element_text(angle = 90),
          strip.text.x = element_text(size=10, face="bold"), 
          strip.text.y = element_text(size=10, face="bold"), # angle=0
          strip.background = element_rect(colour="black", fill="WhiteSmoke"
          )) 
  #theme_bw()
}

# USE THIS!

#########################################################
# noAylum
#########################################################

m2r_noAsylum_trust <- 
  mean_by_Msg_Source(subset(df, 
                            df$corrected=="noAsylum" &
                              df$immigOpinions == "fewer"),
                     "trustAuthority_num") + 
  labs(subtitle = noAsylum_lbl)  

print(m2r_noAsylum_trust)
ggsave("R/graphs/m2r_noAsylum_trust.png")


m2r_noAsylum_acc <- 
  mean_by_Msg_Source(subset(df,
                            df$corrected=="noAsylum" &
                              df$immigOpinions == "fewer"),
                     "accurateAuthority_num") + 
  labs(subtitle = noAsylum_lbl)  

print(m2r_noAsylum_acc)
ggsave("R/graphs/m2r_noAsylum_acc.png")


#########################################################
# costImmig
#########################################################

m2r_costImmig_trust <- 
  mean_by_Msg_Source(subset(df, 
                            df$corrected=="costImmig" &
                              df$immigOpinions == "fewer"),
                     "trustAuthority_num") + 
  labs(subtitle = costImmig_lbl)  

print(m2r_costImmig_trust)
ggsave("R/graphs/m2r_costImmig_trust.png")


m2r_costImmig_acc <- 
  mean_by_Msg_Source(subset(df,
                            df$corrected=="costImmig" &
                              df$immigOpinions == "fewer"),
                     "accurateAuthority_num") + 
  labs(subtitle = costImmig_lbl)  

print(m2r_costImmig_acc)
ggsave("R/graphs/m2r_costImmig_acc.png")

#########################################################
# whiteCrime
#########################################################

m2r_whiteCrime_trust <- 
  mean_by_Msg_Source(subset(df, 
                            df$corrected=="whiteCrime" &
                            df$immigOpinions %in% c("no change", "more")),
                     "trustAuthority_num") + 
  labs(subtitle = whiteCrime_lbl)  

print(m2r_whiteCrime_trust)
ggsave("R/graphs/m2r_whiteCrime_trust.png")


m2r_whiteCrime_acc <- 
  mean_by_Msg_Source(subset(df,
                            df$corrected=="whiteCrime" &
                              df$immigOpinions %in% c("no change", "more")),
                     "accurateAuthority_num") + 
  labs(subtitle = whiteCrime_lbl)  

print(m2r_whiteCrime_acc)
ggsave("R/graphs/m2r_whiteCrime_acc.png")

#########################################################
# lowPaid
#########################################################

m2r_lowPaid_trust <- 
  mean_by_Msg_Source(subset(df, 
                            df$corrected=="lowPaid" &
                              df$immigOpinions %in% c("no change", "more")),
                     "trustAuthority_num") + 
  labs(subtitle = lowPaid_lbl)  

print(m2r_lowPaid_trust)
ggsave("R/graphs/m2r_lowPaid_trust.png")


m2r_lowPaid_acc <- 
  mean_by_Msg_Source(subset(df,
                            df$corrected=="lowPaid" &
                              df$immigOpinions %in% c("no change", "more")),
                     "accurateAuthority_num") + 
  labs(subtitle = lowPaid_lbl)  

print(m2r_lowPaid_acc)
ggsave("R/graphs/m2r_lowPaid_acc.png")















## THE END -- I THINK


# There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.
# Immigrants receive more in benefits and services than they pay in taxes.
# The majority of crimes in London are committed by white people, not ethnic minorities. 
# Immigration to the UK does not affect the wages of the low-paid.  

# ACHTUNG DIE ZWEIT-COMMENTS SIND MIT DRIN!!

#########################################################
# ii. Trustworthiness
#########################################################

my_plot_mean2 = function(x) {
  ggplot(df, 
         aes(FactFreeMessage,
             .data[[x]]
         )) +
    facet_grid(curbImmig_fac ~ FactFreeSource) +
    stat_summary(fun.y = mean,
                 geom = "bar",
                 position="dodge",
                 colour="black" 
                 # fill=c("indianred3", "indianred2", "seagreen3", "seagreen2" )
                 # fill=c("indianred3", "indianred3", "seagreen3", "seagreen3" )
    ) +
    stat_summary(fun.data = mean_cl_normal,
                 geom = "errorbar",
                 position = position_dodge(width = .9),
                 width = .2
    ) +
    coord_cartesian(ylim=c(1,7)) +
    geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
    scale_fill_manual(values=twoBlues) +
    labs(title="Effect of fact-free comments on trust in expert opinion",
         x="", 
         y="",
         subtitle = "(1='Would not trust at all', 7='Would trust a great deal')"
    ) +
    theme_bw()
}

# byReply
p1 <- my_plot_mean2("trustAuthority_num") +     
  labs(subtitle = noAsylum_lbl)
print(p1)
ggsave("R/graphs/effect of correction/all/noAsylum_trust_T1_T2_byReply.png")


ggplot(df, 
       aes(x=FactFreeMessage, 
           y=trustAuthority_num
       )) +
  facet_grid(curbImmig_fac ~ FactFreeSource) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               colour="black" 
               # fill="indianred3", "seagreen3"
               # fill=c("indianred3", "indianred2", "seagreen3", "seagreen2" )
               # fill=c("indianred3", "indianred3", "seagreen3", "seagreen3" )
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="", 
       y="",
       subtitle = noAsylum_lbl
  ) +
  theme_bw()


# OLD

noAsylum_T1_T2_byReply_trust <-
  ggplot(df, 
         aes(x=FactFreeReply, 
             y=trustAuthority_num
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               colour="black",
               # fill=c("indianred3", "indianred2", "seagreen3", "seagreen2" )
               fill=c("indianred3", "indianred3", "seagreen3", "seagreen3" )
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nDid respondent receive a fact-free comment after they saw the statistics?", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()

twoGreens <- c("#238443", "#78c679")
twoReds <- c("#fe9929", "#fe9929")

print(noAsylum_T1_T2_byReply_trust)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byReply_trust.png")


# byMessage
noAsylum_T1_T2_byMessage_trust <-
  ggplot(df, 
         aes(x=FactFreeMessage, 
             y=trustAuthority_num
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               colour="black",
               fill= c(rep("indianred3", 4), rep("seagreen3", 4) )
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nReason the commentator gives to 'take those statistics with a pinch of salt'", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()

print(noAsylum_T1_T2_byMessage_trust)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byMessage_trust.png")

df$FactFreeMessage_sur3 <- ifelse(df$FactFreeMessage != "None",
                                  df$FactFreeMessage, NA)

# bySource and byMessage
# byMessage
noAsylum_T1_T2_byMessage_trust <-
  ggplot(df, 
         aes(x=FactFreeMessage, 
             y=trustAuthority_num
         )) +
  facet_grid(curbImmig_fac ~ FactFreeSource) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge" #,
               # colour="black",
               # fill= c(rep("indianred3", 4), rep("seagreen3", 4))
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nReason the commentator gives to 'take those statistics with a pinch of salt'", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()

print(noAsylum_T1_T2_byMessage_trust)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byMessage_trust.png")


OxfordBlue <- c("#002147")


# 2nd attempt

ggplot(df, 
       aes(x=FactFreeMessage, 
           y=trustAuthority_num,
           fill=FactFreeSource
       )) +
  facet_grid(curbImmig_fac ~ .) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               colour="black" #,
               # fill="steelblue"
               # fill= c(rep("blue", 2), rep("indianred3", 6))
               #         rep("blue", 2), rep("seagreen3", 6))
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  # coord_cartesian(ylim=c(1,7)) +
  # geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=c("grey99", "#002147", "grey66")) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nReason the commentator gives to 'take those statistics with a pinch of salt'", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()



# byMessage
noAsylum_T1_T2_byMessage_trust <-
  ggplot(df, 
         aes(x=FactFreeSource, 
             y=trustAuthority_num
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               colour="black",
               fill= c(rep("indianred3", 3), rep("seagreen3", 3) )
  ) +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nDid respondent receive a fact-free comment after they saw the statistics?", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()


print(noAsylum_T1_T2_byMessage_trust)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byMessage_trust.png")




# byReply
noAsylum_T1_T2_byReply_trust <-
  ggplot(df_long, 
         aes(x=FactFreeReply, 
             y=trust_num,
             fill=time
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(name = "trust", 
                    labels = c("authority", 
                               "non-authority"),
                    values=twoBlues) +
  labs(title="Effect of fact-free comments on trust in expert and non-expert opinion",
       x="\nDid respondent receive a fact-free comment after they saw the statistics?", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Would not trust at all', 7='Would trust a great deal')"
  ) +
  theme_bw()

print(noAsylum_T1_T2_byReply_trust)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byReply_trust")

df$trust_T1

# byMessage
noAsylum_T1_T2_byMessage <-
  ggplot(df_long, 
         aes(x=FactFreeMessage, 
             y=noAsylum_num,
             fill=time
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(name = "time", 
                    labels = c("before", 
                               "after"),
                    values=twoBlues) +
  labs(title="Effect of fact-free comments on belief in false claims",
       x="", y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Definitely false' 7='Definitely true')"
  ) +
  theme_bw()

print(noAsylum_T1_T2_byMessage)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_byMessage.png")

# bySource
noAsylum_T1_T2_bySource <-
  ggplot(df_long, 
         aes(x=FactFreeSource, 
             y=noAsylum_num,
             fill=time
         )) +
  facet_grid(. ~ curbImmig_fac) +
  stat_summary(fun.y = mean,
               geom = "bar",
               position="dodge",
               color="black") +
  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               position = position_dodge(width = .9),
               width = .2
  ) +
  coord_cartesian(ylim=c(1,7)) +
  geom_abline(slope=0, intercept=4,  col = "black", lty=2) +
  scale_fill_manual(name = "time", 
                    labels = c("before", 
                               "after"),
                    values=twoBlues) +
  labs(title="Effect of fact-free comments on belief in false claims",
       x="\nSource of the fact-free comment", 
       y="",
       subtitle = "'There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years.' \n(1='Definitely false' 7='Definitely true')"
  ) +
  theme_bw()

print(noAsylum_T1_T2_bySource)
ggsave("R/graphs/effect of correction/all/noAsylum_T1_T2_bySource.png")





df_long$FactFreeReply

noAsylum_byReply
noAsylum_byReply

noAsylum_T1_T2_bySource

levels(df_long$FactFreeMessage)

# theme_hc() 

#########################################################
# Function to calculate the mean and sd for each group
#########################################################
# http://www.sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization 
# data : a data frame
# varname : column containing the variable to be summarized
# groupnames : vector of column names to be used as grouping variables

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))}
  data_sum <- ddply(data, 
                    groupnames, 
                    .fun=summary_func, 
                    varname)
  data_sum <- rename(data_sum, 
                     c("mean" = varname))
  return(data_sum)
}

library(plyr); library(dplyr)

df2 <- data_summary(df_long, 
                    varname="noAsylum_num", 
                    groupnames=c("FactFreeMessage", "time"))

p <- ggplot(df2, aes(x=FactFreeMessage, 
                     y=noAsylum_num, 
                     fill=time)) + 
  geom_bar(stat="identity", 
           color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=noAsylum_num-sd, 
                    ymax=noAsylum_num+sd), 
                width=.2,
                position=position_dodge(.9)) 




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


geom_histogram(position="dodge", binwidth=.5) +
  
  noAsylum_T1_T2_byMessage <- 
  ggplot(df_long,
         aes(x=FactFreeMessage,
             y=noAsylum) +
           geom_bar(stat="identity", color="black", 
                    position=position_dodge()) +
           stat_summary(fun.y = mean,
                        geom = "bar",
                        position="dodge"))

p<- ggplot(df2, aes(x=dose, y=len, fill=supp)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=len-sd, ymax=len+sd), width=.2,
                position=position_dodge(.9)) 




#########################################################
# 1. There has been a sharp rise in the number of people applying for asylum in the UK in the past 10 years. 
#########################################################

#########################################################
# 2. Immigrants receive more in benefits and services than they pay in taxes.
#########################################################

#########################################################
# 3. The majority of crimes in London are committed by white people, not ethnic minorities. 
#########################################################

#########################################################
# 4. Immigration to the UK does not affect the wages of the low-paid.  
#########################################################


#########################################################
# H4 -- Evidence of post-truth reasoning
#########################################################

#########################################################
# Matter of opinion? 
#########################################################

# Control group respondents
round(prop.table(table(ctrl$factOpinion_fac)), 2)
# 37% matter of fact
# 27% undecided
# 35% matter of opinion

# Control group respondents who received stats that 
# disconfirmed their beliefs
round(prop.table(table(ctrl_m2r$factOpinion_fac)), 2)
# 39% matter of fact
# 27% undecided
# 34% matter of opinion 

# This is funny -- people who have a m2r are LESS likely
# to think the statement is a matter of opinion.

# Treatment group respondents 
round(prop.table(table(trt$factOpinion_fac)), 2) 
# 38% matter of fact
# 29% undecided
# 33% matter of opinion -- again, very similar!
# --> FFC does not seem to increase post-truth thinking!
# (Ceiling effects?)

# By statements
round(prop.table(table(noAsylum$factOpinion_fac)), 2) # 35% opinion
round(prop.table(table(costImmig$factOpinion_fac)), 2) # 37% opinion
round(prop.table(table(whiteCrime$factOpinion_fac)), 2) # 30% opinion
round(prop.table(table(lowPaid$factOpinion_fac)), 2) # 34% opinion

# Is there a difference between pro- and anti-immigration 
# people? 
# --> Slightly fewer pro-immig people in the 'opinion'
# category

df %>%
  filter(motivatedToReject==1 &
           RandomGrp %in% c(1,2)) %>%
  group_by(gotWhichCorrection, factOpinion_fac) %>%
  summarise(n = n()) %>%
  mutate(
    freq = n / sum(n))


# See if any of those small group differences are 
# significant -- YES 
m1 <- lm(factOpinion_num ~ 
           gotWhichCorrection + 
           motivatedToReject + 
           immigNumbers_num, 
         data=df)
summary(m1)


#########################################################
# Matter of opinion? -- Control group
#########################################################

ctrl_factOpinion_pooled_pc <- 
  df %>%
  filter(
    RandomGrp %in% c(1,2) &
      complete.cases(factOpinion)) %>%
  group_by(factOpinion) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  as.data.frame()

# WORKING GRAPH

ctrl_factOpinion_pooled_pc <-
  ctrl_factOpinion_pooled_pc %>% 
  ggplot(aes(x=factOpinion,
             y=freq)) +
  geom_bar(stat="identity",
           fill="steelblue") +
  geom_text(aes(label=paste0(round(freq*100, 0), "%")), 
            vjust=1.6, 
            color="white", 
            size=3.5) +
  coord_cartesian(ylim=c(0,0.3)) +
  labs(
    title = "'Matter of fact or matter of opinion?'",
    x="",
    y="") +
  scale_y_continuous(labels=scales::percent) +
  # theme_minimal() + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
  ) 

print(ctrl_factOpinion_pooled_pc)
ggsave("R/graphs/ctrl_factOpinion_pooled_pc.png")


# TURN INTO FUNCTION

my_hist_pc = function(df) {
  df %>% 
    ggplot(aes(x=factOpinion,
               y=freq)) +
    geom_bar(stat="identity",
             fill="steelblue") +
    geom_text(aes(label=paste0(round(freq*100, 0), "%")), 
              vjust=1.6, 
              color="white", 
              size=3.5) +
    coord_cartesian(ylim=c(0,0.3)) +
    labs(
      title = "'Matter of fact or matter of opinion?'",
      x="",
      y="") +
    scale_y_continuous(labels=scales::percent) +
    # theme_minimal() + 
    theme_classic() + 
    theme(axis.line = element_blank(),
          # axis.text = element_blank(),
          axis.text.y = element_blank(), 
          axis.ticks = element_blank(),
          plot.title = element_text(hjust = 0.5, 
                                    color = "#666666")
    ) 
}


#########################################################
# Matter of opinion? -- Treatment groups
#########################################################

trt_factOpinion_pooled_pc <- 
  df %>%
  filter(
    RandomGrp %in% c(3,8) &
      complete.cases(factOpinion)) %>%
  group_by(factOpinion) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  as.data.frame()

trt_factOpinion_pooled_pc <- 
  my_hist_pc(trt_factOpinion_pooled_pc) 

print(trt_factOpinion_pooled_pc)
ggsave("R/graphs/trt_factOpinion_pooled_pc.png")

# TO DO: Excluding those who would have said DK! 


#########################################################
# Matter of opinion? -- By corrected statement
#########################################################

temp <- 
  df %>%
  filter(
    RandomGrp %in% c(3,8) &
      complete.cases(factOpinion_num)) %>%
  group_by(corrected, factOpinion_num) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  as.data.frame()

my_hist_pc(temp) + 
  facet_grid(. ~ corrected) # Problem: y axis limits


factOpinion_byCorrected_pc <-
  temp %>% 
  ggplot(aes(x=factOpinion_num,
             y=freq)) +
  geom_bar(stat="identity",
           fill="steelblue") +
  # geom_text(aes(label=paste0(round(freq*100, 0), "%")), 
  #           vjust=1.6, 
  #           color="white", 
  #           size=3.5) +
  # coord_cartesian(ylim=c(0,0.3)) +
  labs(
    title = "'Matter of fact or matter of opinion?'\n",
    x="",
    y="") +
  scale_y_continuous(labels=scales::percent) +
  theme_bw() + 
  # theme_classic() + 
  theme(axis.line = element_blank(),
        # axis.text = element_blank(),
        # axis.text.y = element_blank(), 
        # axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
  ) + 
  facet_grid(. ~ corrected)

print(factOpinion_byCorrected_pc)
ggsave("R/graphs/factOpinion_byCorrected_pc.png")



# PLAIN JANE -- USING DF

simple_hist_pc = function(x, y, z) {
  ggplot(x, # subset(df, RandomGrp == 1 | df$RandomGrp == 2),
         aes( .data[[y]],
              count())) +
    geom_bar(aes(
      y = (..count..) / sum(..count..) #,
      # fill= "#1c9099"
    )) +
    # scale_fill_manual(values=c("red", "blue", "green",
    #                            "green", "green", 
    #                            "green", "green")) +
    scale_y_continuous(labels=scales::percent) +
    labs(
      title = z,
      subtitle = "Control group only",
      x="",
      y="") + 
    theme(legend.position = "none") +
    theme_light() 
}

# ctrl_factOpinion_pooled_pc <- simple_hist_pc(
#   subset(df, RandomGrp == 1 | df$RandomGrp == 2),
#   "factOpinion", "'Matter of fact or matter of opinion?'") 
# 
# print(ctrl_factOpinion_pooled_pc)
# ggsave("R/graphs/ctrl_factOpinion_pooled_pc.png")
# 
# 
# treated_factOpinion_pooled_pc <- simple_hist_pc(
#   subset(df, RandomGrp != 1 | df$RandomGrp != 2),
#   "factOpinion", "'Matter of fact or matter of opinion?'") +
#   labs(
#     subtitle = "Treatment groups only")
# 
# print(treated_factOpinion_pooled_pc) 
# ggsave("R/graphs/treated_factOpinion_pooled_pc.png")



# Populism scale (higher values=more populist)
df$populism_scale <- as.numeric(as.character(df$populism_scale))
hist(df$populism_scale)

# Nationlism scale (higher values=more nationlist)
df$ethno_scale
hist(df$ethno_scale)

# TO DO: See if people high/low in populism/nationalism behave any
# differently


#########################################################
# Consistent? 
#########################################################

prop.table(table(df$notice))
prop.table(table(subset(df, belief_num_T1 > 4)$notice))
prop.table(table(subset(df, belief_num_T1 < 4)$notice))
# 54 % of those who believed the false claim was correct 
# did not notice that the statistics they saw were not in line
# with what they believed. 

# TRY PIE
# How2 
# https://www.r-bloggers.com/how-to-make-a-pie-chart-in-r/
# https://stackoverflow.com/questions/47752037/pie-chart-with-ggplot2-with-specific-order-and-percentage-annotations

# 1) Create dataset -- % of people in each category

ctrl_consistent <- 
  df %>%
  filter(belief_T1 > 4 & 
           RandomGrp %in% c(1,2)) %>%
  group_by(consistent) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  as.data.frame()

# 2) Plot 

pie_ctrl_consistent <- 
  ggplot(ctrl_consistent,
         aes(x="", 
             y=freq,
             fill=consistent
         )) +
  geom_bar(width=1, 
           size=1,
           color="white",
           stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(freq, 0), "%")) , 
            position = position_stack(vjust = 0.5)) +
  # scale_fill_manual(values=c("#fc8d59", "#91cf60")) + # red green - ish
  # scale_fill_manual(values=c("#ffd700", "#254290")) + # yellow - blue
  scale_fill_manual(values=c("#ffa500", "#254290")) + # orange - blue
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       # title = "Control Group"
       title = "Would you say that the statistics here were consistent with what you believed?\n(They weren't.)"
  )+ 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666"))

print(pie_ctrl_consistent)
ggsave("R/graphs/pie_ctrl_consistent.png")



trt_consistent <- 
  df %>%
  filter(belief_T1 > 4 & 
           RandomGrp %in% c(3,8)) %>%
  group_by(consistent) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  as.data.frame()


pie_trt_consistent <- 
  ggplot(trt_consistent,
         aes(x="", 
             y=freq,
             fill=consistent
         )) +
  geom_bar(width=1, 
           size=1,
           color="white",
           stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(freq, 0), "%")) , 
            position = position_stack(vjust = 0.5)) +
  # scale_fill_manual(values=c("#fc8d59", "#91cf60")) + # red green - ish
  # scale_fill_manual(values=c("#ffd700", "#254290")) + # yellow - blue
  scale_fill_manual(values=c("#ffa500", "#254290")) + # orange - blue
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       # title = "Treatment Groups" # EPOP
       title = "Would you say that the statistics here were consistent with what you believed?\n(They weren't.)"
  )+ 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666"))

print(pie_trt_consistent)
ggsave("R/graphs/pie_trt_consistent.png")



#########################################################
# If not consistent... 
#########################################################

table(df$ifInconsistent)

#########################################################
# ... Control group
#########################################################

ctrl_ifInconsistent <- 
  df %>%
  filter(
    RandomGrp %in% c(1,2) &
      complete.cases(ifInconsistent)) %>%
  group_by(ifInconsistent) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  as.data.frame()

pie_ctrl_ifInconsistent <- 
  ggplot(ctrl_ifInconsistent,
         aes(x="", 
             y=freq,
             fill=ifInconsistent
         )) +
  geom_bar(width=1, 
           size=1,
           color="white",
           stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(freq, 0), "%")) , 
            position = position_stack(vjust = 0.5)) +
  # scale_fill_manual(values=c("#fc8d59", "#91cf60")) + # red green - ish
  # scale_fill_manual(values=c("#ffd700", "#254290")) + # yellow - blue
  scale_fill_manual(
    values=c("#ffa500", "#bcbcbc", "#254290"),
    name = "", 
    labels = c("The statistics are probably right \nbut I believe something different", 
               "The statistics are probably wrong", 
               "The statistics made me change my mind")) + # orange - blue
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       title = "If the statistics were 'not consistent' with what I believed"
  ) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
        # legend.position = "top"
  ) 

print(pie_ctrl_ifInconsistent)
ggsave("R/graphs/pie_ctrl_ifInconsistent.png")


#########################################################
# ... Treatment groups
#########################################################

trt_ifInconsistent <- 
  df %>%
  filter(
    RandomGrp %in% c(3,8) &
      complete.cases(ifInconsistent)) %>%
  group_by(ifInconsistent) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>%
  as.data.frame()


pie_trt_ifInconsistent <- 
  ggplot(trt_ifInconsistent,
         aes(x="", 
             y=freq,
             fill=ifInconsistent
         )) +
  geom_bar(width=1, 
           size=1,
           color="white",
           stat="identity") +
  coord_polar("y", start=0) +
  geom_text(aes(label = paste0(round(freq, 0), "%")) , 
            position = position_stack(vjust = 0.5)) +
  # scale_fill_manual(values=c("#fc8d59", "#91cf60")) + # red green - ish
  # scale_fill_manual(values=c("#ffd700", "#254290")) + # yellow - blue
  scale_fill_manual(
    values=c("#ffa500", "#bcbcbc", "#254290"),
    name = "", 
    labels = c("The statistics are probably right \nbut I believe something different", 
               "The statistics are probably wrong", 
               "The statistics made me change my mind")) + # orange - blue
  labs(x = NULL, 
       y = NULL, 
       fill = NULL, 
       # title = "Treatment Groups"
       title = "If the statistics were 'not consistent' with what I believed"
  ) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
        # legend.position = "top"
  ) 


print(pie_trt_ifInconsistent)
ggsave("R/graphs/pie_trt_ifInconsistent.png")



#########################################################
# OK to disagree
#########################################################

#########################################################
# OK to disagree // Treatment Groups
#########################################################

trt_ok2disagree <- 
  df %>%
  filter(
    RandomGrp %in% c(3,8) &
      complete.cases(ok2disagree)) %>%
  group_by(ok2disagree) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  as.data.frame()

trt_ok2disagree <-
  trt_ok2disagree %>% 
  ggplot(aes(x=ok2disagree,
             y=freq)) +
  geom_bar(stat="identity",
           fill="steelblue") +
  geom_text(aes(label=paste0(round(freq*100, 0), "%")), 
            vjust=1.6, 
            color="white", 
            size=3.5) +
  coord_cartesian(ylim=c(0,0.55)) +
  labs(
    title = "'It's OK to disagree with the facts if that's what you believe.'",
    subtitle = "",
    x="",
    y="") +
  scale_y_continuous(labels=scales::percent) +
  # theme_minimal() + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
  ) 

print(trt_ok2disagree)
ggsave("R/graphs/trt_ok2disagree.png")


#########################################################
# OK to disagree // Control 
#########################################################

ctrl_ok2disagree <- 
  df %>%
  filter(
    RandomGrp %in% c(1,2) &
      complete.cases(ok2disagree)) %>%
  group_by(ok2disagree) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  as.data.frame()

ctrl_ok2disagree <-
  ctrl_ok2disagree %>% 
  ggplot(aes(x=ok2disagree,
             y=freq)) +
  geom_bar(stat="identity",
           fill="steelblue") +
  geom_text(aes(label=paste0(round(freq*100, 0), "%")), 
            vjust=1.6, 
            color="white", 
            size=3.5) +
  coord_cartesian(ylim=c(0,0.55)) +
  labs(
    title = "'It's OK to disagree with the facts if that's what you believe.'",
    subtitle = "",
    x="",
    y="") +
  scale_y_continuous(labels=scales::percent) +
  # theme_minimal() + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        # axis.text = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, 
                                  color = "#666666")
  ) 

print(ctrl_ok2disagree)
ggsave("R/graphs/ctrl_ok2disagree.png")

table(df$motivatedToReject, df$corrected)


#########################################################
# OK to disagree // Treatment AND Control 
#########################################################

all_ok2disagree <-
  df %>% 
  filter(# RandomGrp == 1 | df$RandomGrp == 2 # &
    # immigOpinions == "fewer"
  ) %>%
  # select(noAsylum_num_T1, noAsylum_num_T2) %>% 
  # gather(key=Type, value=Value) %>% 
  ggplot(aes(x=factor(ok2disagree),
             fill=factor(FactFreeReply))) +
  geom_bar(position = "dodge") +
  scale_fill_manual(name="Fact Free Comment",
                    labels=c("No", "Yes"),
                    values=twoGreens) +
  labs(
    title = "'It's OK to disagree with the facts if that's what you believe.'",
    subtitle = "",
    x="",
    y="") +
  # scale_x_continuous(breaks=seq(1,7,1))  + # if numeric
  theme_bw() 

print(all_ok2disagree)
ggsave("R/graphs/all_ok2disagree.png")



#########################################################
# ifInconsistent
#########################################################

table(df$ifInconsistent)
round(prop.table(table(df$ifInconsistent)), 2)
# Among those who did not notice the stats were inconsistent with
# what they believed, 48% thought the statatistics were false;
# 28% believed that 'The statistics are probably right, but I believe something d
levels(df$ifInconsistent)

ctrl_ifInconsistent_pooled_pc <- simple_hist_pc(
  subset(df, complete.cases(as.factor(as.character(df$ifInconsistent))) & 
           RandomGrp == 1 | df$RandomGrp == 2),
  "ifInconsistent", 
  "'If statistics were not consistent with what you believed...'")

print(ctrl_ifInconsistent_pooled_pc)
ggsave("R/graphs/ctrl_ifInconsistent_pooled_pc.png")


treated_ifInconsistent <- my_plot_hist(subset(df, df$RandomGrp !=  1 & df$RandomGrp != 2),
                                       "ifInconsistent",
                                       "'If statistics were not consistent with what you believed...'", 
                                       "Respondents who saw a second opinion")

print(treated_ifInconsistent)
ggsave("R/graphs/treated_ifInconsistent.png")



#########################################################
# ok2disagree
#########################################################

round(prop.table(table(df$ok2disagree)), 2)
round(prop.table(table(ctrl$ok2disagree)), 2)

all_ok2disagree <- my_plot_hist(df, 
                                "ok2disagree",
                                "'It's OK to disagree with the facts if that's what you believe.", 
                                "All respondents")

print(all_ok2disagree)
ggsave("R/graphs/all_ok2disagree.png")


treated_ok2disagree <- my_plot_hist(subset(df, df$RandomGrp !=  1 & df$RandomGrp != 2), 
                                    "ok2disagree",
                                    "'It's OK to disagree with the facts if that's what you believe.'", 
                                    "Respondents who saw a second opinion")

print(treated_ok2disagree)
ggsave("R/graphs/treated_ok2disagree.png")


# Same same
# ctrl_ok2disagree_pooled_pc <-
#   df %>% 
#   filter(
#     RandomGrp == 1 | df$RandomGrp == 2) %>% 
#   ggplot(aes(x=factor(ok2disagree))) +
#   # facet_grid(. ~ corrected) +
#   geom_bar(aes(y = (..count..) / sum(..count..))) + # for %
#   scale_y_continuous(labels=scales::percent) + 
#   labs(
#     title = "'It’s OK to disagree with the facts if that’s what you believe.'",
#     subtitle = "Control group (Respondents who noticed that the statistics were not in line with what they believed)",
#     x="",
#     y="relative frequencies") +
#   theme_bw()


# # Plane Jane
# ctrl_ok2disagree_pooled_pc <- simple_hist_pc(
#   subset(df, complete.cases(df$ok2disagree) & 
#            RandomGrp == 1 | df$RandomGrp == 2),
#   "ok2disagree", 
#   "'It’s OK to disagree with the facts if that’s what you believe.'")
# 
# print(ctrl_ok2disagree_pooled_pc)
# ggsave("R/graphs/ctrl_ok2disagree_pooled_pc.png")


# Pie charts
# http://www.sthda.com/english/wiki/ggplot2-pie-chart-quick-start-guide-r-software-and-data-visualization
