************************************************
*** BA Project -- Post-truth Politics        ***
*** 20/09/2019                               ***
*** Data set: Deltapoll190627_Bartle.sav     ***
************************************************

cd "/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/3.BAgrantProject/Data"
use "/Users/cstedtnitz/Dropbox/1.PhD/1.Papers/3.BAgrantProject/Data/Original data Stata version.dta"
* use "working data 4.7.2019.dta", clear 

* Find out how to convert the raw dataset 
* Deltapoll190704_Bartle.sav to .dta. Then use the .dta dataset
* 
* <-- Right now this code is NOT REPRODUCIBLE! Using the 4.7. dataset as 
* a starting point, not the original one.w

set more off


************************************************
*** Data Management
************************************************

rename E1_1 issues_immig
rename E1_2 issues_econ
rename E1_3 issues_nhs
rename E1_4 issues_crime
rename E1_5 issues_eu
rename E1_6 issues_housing
rename E1_7 issues_edu

rename E2_1 ethno1
rename E2_2 populism6
rename E2_3 ethno2
rename E2_4 populism2
rename E2_5 immigFeelLikeHome
rename E2_7 ethno6

rename E3 leftright
rename E5 immigNumbers
rename E6 immigImportance

rename E8_1 ethno3
rename E8_2 ethno5
rename E8_3 populism4
rename E8_4 populism5
rename E8_5 immigEcon
rename E8_6 ethno4

* @R&J: In an earlier version of this dataset I had named the 
*'immigration is good for the economy" variable immig2 and the 
* 'There are so many foreigners round here that it doesn't feel like home 
* anymore' variable immig1. 
* NEW VAR NAMES: 
* I have now renamed the former immigEcon (to match its name in the BES) and 
* the latter 'immigFeelLikeHome'. 

rename E9_1 noAsylum_T1
rename E9_2 costImmig_T1
rename E9_3 whiteCrime_T1
rename E9_4 lowPaid_T1
rename E9_5 no5_T1
rename E9_6 recession_T1
rename E9_7 fracking_T1
rename E9_8 plasticBags_T1

rename E15_1 noAsylum_T2
rename E15_2 costImmig_T2
rename E16_1 whiteCrime_T2
rename E16_2 lowPaid_T2

rename E17_1 noAsylum_opinion
rename E17_2 costImmig_opinion
rename E18_1 whiteCrime_opinion
rename E18_2 lowPaid_opinion

rename E19 consistent
rename E20 ifInconsistent
rename E21 ok2disagree

* @Rob: I renamed accurateRetiredProf accurateFactFreeProf 
* and I rename trustRetiredProf trustFactFreeProf
* (because they are no longer retired!)

rename P11 accurateAuthority
rename P12 trustAuthority

rename P13 accurateBlogger
rename P14 accurateFactFreeProf

rename P15 trustBlogger
rename P16 trustFactFreeProf

rename QD7 marital
rename QD8_0 ChildrenNoAnswer
rename QD8_1 noChildren
rename QD8_2 under5
rename QD8_3 FiveTo10
rename QD8_4 ElevenTo15
rename QD8_5 sixteenTo18

rename QD9 householdIncome
rename E4a supportParty
rename E4b party
rename E4b_8_Other otherParty

rename E7 votedEURef
rename E7A EUVote
rename E7B hypEUVote

rename PolInt attention
rename FullAge age
rename Regions regions
rename Gender gender
rename Age ageCategory
rename Vote17 vote2017
rename SocGrade socialGrade
rename Region region
rename Quals university

rename HIGH_TEXT2 correctedTxt
rename HIGH_TEXT_ID2 corrected

rename user_id id

************************************************
*** Treatment groups
************************************************

recode RandomGrp (1/2=0) (3/8=1), gen(FactFreeReply)
label define FactFreeReply_lbl 0 "No" 1 "Yes"
label values FactFreeReply FactFreeReply_lbl
label variable FactFreeReply "Did they get fact-free reply?"

recode RandomGrp (1/2=0) (3/5=1) (6/8=2), gen(FactFreeSource)
label define FactFreeSource_lbl 0 "None" 1 "Professor" 2 "Blogger"
label values FactFreeSource FactFreeSource_lbl
label variable FactFreeSource "Who provided fact-free reply?"

recode RandomGrp (1/2=0) (3=1) (6=1) (4=2) (7=2) (5=3) (8=3), gen(FactFreeMessage)
label define FactFreeMessage_lbl 0 "None" 1 "Biased stats" 2 "Personal experience" 3 "OK to disagree"
label values FactFreeMessage FactFreeMessage_lbl
label variable FactFreeMessage "Content of fact-free reply"


************************************************
*** Create these same variables excluding the 'none' option
************************************************

recode FactFreeSource (0=.) (1=1) (2=2), gen(FactFreeSource2)
label define FactFreeSource2_lbl 1 "Professor" 2 "Blogger"
label values FactFreeSource2 FactFreeSource2_lbl

recode FactFreeMessage (0=.) (1=1) (2=2) (3=3), gen(FactFreeMessage2)
label define FactFreeMessage2_lbl  1 "Biased stats" 2 "Personal experience" 3 "OK to disagree"
label values FactFreeMessage2 FactFreeMessage2_lbl


************************************************
*** Source of the fact-free reply
************************************************

gen source2 = .
replace source2 = 1 if inrange(RandomGrp, 3, 5) 
replace source2 = 2 if inrange(RandomGrp, 6, 8) 
replace source2 = 3 if inrange(RandomGrp, 1, 2) 

label define source2_lbl 1 "Professor" 2 "Blogger" 3 "None"
label values source2 source2_lbl




************************************************
*** Corrected statement
************************************************

label define corrected_lbl 1 "noAsylum" 2 "costImmig" 3 "whiteCrime" 4 "lowPaid"
label values corrected corrected_lbl

* Check if respondents were correctly assigned to the 4 groups
* (Respondents receive the correction that corresponds to the statement 
* they have the highest rating on a scale from 0 (def false) to 6 (def true)
* Whenever they gave 2 statements the same rating they were randomly assigned 
* to see 1 of the 2 corresponding corrections.  

gen mostFalsest = .
replace mostFalsest = 1 if noAsylum_T1 > costImmig_T1 & noAsylum_T1 > whiteCrime_T1 & noAsylum_T1 > lowPaid_T1
replace mostFalsest = 2 if costImmig_T1 > noAsylum_T1 & costImmig_T1 > whiteCrime_T1 & costImmig_T1 > lowPaid_T1
replace mostFalsest = 3 if whiteCrime_T1 > costImmig_T1 & whiteCrime_T1 > noAsylum_T1 & whiteCrime_T1 > lowPaid_T1
replace mostFalsest = 4 if lowPaid_T1 > costImmig_T1 & lowPaid_T1 > noAsylum_T1 & lowPaid_T1 > whiteCrime_T1

label variable mostFalsest "Which misperception was given the highest rating (most certain to be true)?"
label define mostFalsest 1 "noAsylum" 2 "costImmig" 3 "whiteCrime" 4 "lowPaid"
label values mostFalsest mostFalsest

* tab mostFalsest corrected
* --> Works! 

drop mostFalsest


************************************************
*** Create new variables
************************************************

* HOW2: https://www.stata.com/support/faqs/data-management/creating-dummy-variables/ 

************************************************
*** Belief in CORRECTED false facts 
************************************************

gen belief_noAsylum_T1 = .
replace belief_noAsylum_T1 = noAsylum_T1 if corrected == 1
gen belief_noAsylum_T2 = .
replace belief_noAsylum_T2 = noAsylum_T2 if corrected == 1

gen belief_costImmig_T1 = .
replace belief_costImmig_T1 = costImmig_T1 if corrected == 2
gen belief_costImmig_T2 = .
replace belief_costImmig_T2 = costImmig_T2 if corrected == 2


gen belief_whiteCrime_T1 = .
replace belief_whiteCrime_T1 = whiteCrime_T1 if corrected == 3
gen belief_whiteCrime_T2 = .
replace belief_whiteCrime_T2 = whiteCrime_T2 if corrected == 3

gen belief_lowPaid_T1 = .
replace belief_lowPaid_T1 = lowPaid_T1 if corrected == 4
gen belief_lowPaid_T2 = .
replace belief_lowPaid_T2 = lowPaid_T2 if corrected == 4


label variable belief_noAsylum_T1 "Pre-correction veracity score (corrected item)"
label variable belief_noAsylum_T2 "Post-correction veracity score (corrected item)"

label variable belief_costImmig_T1 "Pre-correction veracity score (corrected item)"
label variable belief_costImmig_T2 "Post-correction veracity score (corrected item)"

label variable belief_whiteCrime_T1 "Pre-correction veracity score (corrected item)"
label variable belief_whiteCrime_T2 "Post-correction veracity score (corrected item)"

label variable belief_lowPaid_T1 "Pre-correction veracity score (corrected item)"
label variable belief_lowPaid_T2 "Post-correction veracity score (corrected item)"


* T1 refers to pre-treatment belief in the statement respondents gave 
* the highest rating, i.e. closest to "Definitely true". This is the 
* statement that will then be corrected. 

gen belief_T1 = .
replace belief_T1 = belief_noAsylum_T1 if corrected == 1
replace belief_T1 = belief_costImmig_T1 if corrected == 2
replace belief_T1 = belief_whiteCrime_T1 if corrected == 3
replace belief_T1 = belief_lowPaid_T1 if corrected == 4

label variable belief_T1 "Pre-correction veracity score (corrected item)"


* T2 refers to post-correction belief in the corrected statement. 

gen belief_T2 = .
replace belief_T2 = belief_noAsylum_T2 if corrected == 1
replace belief_T2 = belief_costImmig_T2 if corrected == 2
replace belief_T2 = belief_whiteCrime_T2 if corrected == 3
replace belief_T2 = belief_lowPaid_T2 if corrected == 4

label variable belief_T2  "Post-correction veracity score (corrected item)"



************************************************
*** Belief in the UN-corrected statement
************************************************

gen belief_i2_T1 = .
replace belief_i2_T1 = noAsylum_T1 if corrected == 2
replace belief_i2_T1 = costImmig_T1 if corrected == 1
replace belief_i2_T1 = whiteCrime_T1 if corrected == 4
replace belief_i2_T1 = lowPaid_T1 if corrected == 3

label variable belief_i2_T1 "Pre-correction veracity score (claim that is NOT to be corrected)"
label values belief_i2_T1 belief_lbl


gen belief_i2_T2 = .
replace belief_i2_T2 = noAsylum_T2 if corrected == 2
replace belief_i2_T2 = costImmig_T2 if corrected == 1
replace belief_i2_T2 = whiteCrime_T2 if corrected == 4
replace belief_i2_T2 = lowPaid_T2 if corrected == 3

label variable belief_i2_T2 "Post-correction veracity (claim that was NOT corrected)"
label values belief_i2_T2 belief_lbl


************************************************
*** Belief in the 2 statements on the other side of the immigration debate
************************************************

gen belief_i3_T1 = .
replace belief_i3_T1 = whiteCrime_T1 if corrected == 1
replace belief_i3_T1 = whiteCrime_T1 if corrected == 2

replace belief_i3_T1 = noAsylum_T1 if corrected == 3
replace belief_i3_T1 = noAsylum_T1 if corrected == 4

label variable belief_i3_T1 "Belief in item 3 (false fact on the other side: noAsylum or whiteCrime)"


gen belief_i4_T1 = .
replace belief_i4_T1 = lowPaid_T1 if corrected == 1
replace belief_i4_T1 = lowPaid_T1 if corrected == 2

replace belief_i4_T1 = costImmig_T1 if corrected == 3
replace belief_i4_T1 = costImmig_T1 if corrected == 4

label variable belief_i4_T1 "Belief in item 4 (false fact on the other side: costImmig or lowPaid)"


************************************************
*** factor
************************************************

gen belief_fac_T1 = . 
replace belief_fac_T1 = 1 if belief_T1 < 4 
replace belief_fac_T1 = 2 if belief_T1 == 4 
replace belief_fac_T1 = 3 if belief_T1 > 4 

label variable belief_fac_T1 "Pre-correction veracity ratings"

label define belief_fac_lbl 1 "false" 2 "DK" 3 "true" 
label values belief_fac_T1 belief_fac_lbl


gen belief_fac_T2 = . 
replace belief_fac_T2 = 1 if belief_T2 < 4 
replace belief_fac_T2 = 2 if belief_T2 == 4 
replace belief_fac_T2 = 3 if belief_T2 > 4 

label variable belief_fac_T2 "Post-correction veracity ratings"
label values belief_fac_T2 belief_fac_lbl


tab belief_fac_T1 belief_fac_T2, row
* --> Among those who thought their false fact was true, 55% maintained their
*  belief after they saw the statistics; 21% did not know, and 24% switched
* to 'false'


gen belief_i2_fac_T1 = . 
replace belief_i2_fac_T1 = 1 if belief_i2_T1 < 4 
replace belief_i2_fac_T1 = 2 if belief_i2_T1 == 4 
replace belief_i2_fac_T1 = 3 if belief_i2_T1 > 4 

label variable belief_i2_fac_T1 "Pre-correction veracity ratings (item 2)"
label values belief_i2_fac_T1 belief_fac_lbl


gen belief_i2_fac_T2 = . 
replace belief_i2_fac_T2 = 1 if belief_i2_T2 < 4 
replace belief_i2_fac_T2 = 2 if belief_i2_T2 == 4 
replace belief_i2_fac_T2 = 3 if belief_i2_T2 > 4 

label variable belief_i2_fac_T2 "Post-correction veracity ratings (item 2)"
label values belief_i2_fac_T2 belief_fac_lbl


tab belief_i2_fac_T1 belief_i2_fac_T2, row
* --> Among those who thought their item 2 was true, 80% maintained their
*  belief after they saw the statistics; 15% did not know, and 20% switched
* to 'false'



gen factOpinion_fac = . 
replace factOpinion_fac = 1 if factOpinion < 4 
replace factOpinion_fac = 2 if factOpinion == 4 
replace factOpinion_fac = 3 if factOpinion > 4 

label define factOpinion_fac_lbl 1 "fact" 2 "undecided" 3 "opinion" 
label values factOpinion_fac factOpinion_fac_lbl


************************************************
*** Dummies
************************************************

* Creating a dummy variable to indicate if people got their facts wrong 
* before they saw the statistics
* 1 = got them wrong; 0 = got them right or did not know 

************************************************
*** Pre-correction -- False beliefs? 
************************************************

gen wrong_T1 = . 
replace wrong_T1 = 1 if belief_T1 > 4 
replace wrong_T1 = 0 if belief_T1 <= 4 

gen wrong_noAsylum_T1 = .
replace wrong_noAsylum_T1 = 1 if noAsylum_T1 > 4 
replace wrong_noAsylum_T1 = 0 if noAsylum_T1 <= 4 

gen wrong_costImmig_T1 = .
replace wrong_costImmig_T1 = 1 if costImmig_T1 > 4 
replace wrong_costImmig_T1 = 0 if costImmig_T1 <= 4 

gen wrong_whiteCrime_T1 = .
replace wrong_whiteCrime_T1 = 1 if whiteCrime_T1 > 4 
replace wrong_whiteCrime_T1 = 0 if whiteCrime_T1 <= 4 

gen wrong_lowPaid_T1 = .
replace wrong_lowPaid_T1 = 1 if lowPaid_T1 > 4 
replace wrong_lowPaid_T1 = 0 if lowPaid_T1 <= 4 


* Distractor items 

gen wrong_no5 = .
replace wrong_no5 = 1 if lowPaid_T1 > 4 
replace wrong_no5 = 0 if lowPaid_T1 <= 4 

gen wrong_recession = .
replace wrong_recession = 1 if lowPaid_T1 > 4 
replace wrong_recession = 0 if lowPaid_T1 <= 4 

gen wrong_fracking = .
replace wrong_fracking = 1 if lowPaid_T1 > 4 
replace wrong_fracking = 0 if lowPaid_T1 <= 4 

gen wrong_plasticBags = .
replace wrong_plasticBags = 1 if lowPaid_T1 > 4 
replace wrong_plasticBags = 0 if lowPaid_T1 <= 4 


* label

label variable wrong_T1 "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_noAsylum "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_costImmig "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_whiteCrime "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_lowPaid "Pre-correction: Rated false claim as 'true' (5-7)"

label variable wrong_no5 "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_recession "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_fracking "Pre-correction: Rated false claim as 'true' (5-7)"
label variable wrong_plasticBags "Pre-correction: Rated false claim as 'true' (5-7)"


************************************************
*** Post-correction -- False beliefs? 
************************************************

gen wrong_T2 = . 
replace wrong_T2 = 1 if belief_T2 > 4 
replace wrong_T2 = 0 if belief_T2 <= 4 

gen wrong_noAsylum_T2 = .
replace wrong_noAsylum_T2 = 1 if noAsylum_T2 > 4 
replace wrong_noAsylum_T2 = 0 if noAsylum_T2 <= 4 

gen wrong_costImmig_T2 = .
replace wrong_costImmig_T2 = 1 if costImmig_T2 > 4 
replace wrong_costImmig_T2 = 0 if costImmig_T2 <= 4 

gen wrong_whiteCrime_T2 = .
replace wrong_whiteCrime_T2 = 1 if whiteCrime_T2 > 4 
replace wrong_whiteCrime_T2 = 0 if whiteCrime_T2 <= 4 

gen wrong_lowPaid_T2 = .
replace wrong_lowPaid_T2 = 1 if lowPaid_T2 > 4 
replace wrong_lowPaid_T2 = 0 if lowPaid_T2 <= 4 

* label

label variable wrong_T2 "Post-correction: Rated false claim as 'true' (5-7)"
label variable wrong_noAsylum_T2 "Post-correction: Rated false claim as 'true' (5-7)"
label variable wrong_costImmig_T2 "Post-correction: Rated false claim as 'true' (5-7)"
label variable wrong_whiteCrime_T2 "Post-correction: Rated false claim as 'true' (5-7)"
label variable wrong_lowPaid_T2 "Post-correction: Rated false claim as 'true' (5-7)"




************************************************
*** Un-corrected item 
************************************************

gen wrong_i2_T1 = . 
replace wrong_i2_T1 = 1 if belief_i2_T1 > 4 
replace wrong_i2_T1 = 0 if belief_i2_T1 <= 4 

gen wrong_i2_T2 = . 
replace wrong_i2_T2 = 1 if belief_i2_T2 > 4 
replace wrong_i2_T2 = 0 if belief_i2_T2 <= 4 

label variable wrong_i2_T1 "Pre-correction: Rated un-corrected false claim as true (5-7)"
label variable wrong_i2_T2 "Post-correction: Rated un-corrected false claim as true (5-7)"


gen bothWrong_T1 = . 
replace bothWrong_T1 = 1 if belief_T1 > 4 & belief_i2_T1 > 4 
replace bothWrong_T1 = 0 if belief_T1 <= 4 & belief_i2_T1 <= 4 

gen bothWrong_T2 = . 
replace bothWrong_T2 = 1 if belief_T2 > 4 & belief_i2_T2 > 4 
replace bothWrong_T2 = 0 if belief_T2 <= 4 & belief_i2_T2 <= 4 

label variable bothWrong_T1 "Pre-correction: Rated both false claim as true"
label variable bothWrong_T2 "Post-correction: Rated both false claim as true"



************************************************
*** partisanship
************************************************

* Create dummy for people who support a party
recode supportParty (1=1) (2=0), gen(partisan)
label define dummy_lbl 1 "Yes" 0 "No" 
label values supportParty dummy_lbl

* Create dummy for independents
recode supportParty (1=0) (2=1), gen(independent)
label values independent dummy_lbl

* Create dummies for other parties
recode party (1=1) (2/8=0), gen(partyid_con)
recode party (1=0) (2=1) (3/8=0), gen(partyid_lab)
recode party (1/2=0) (3=1) (4/8=0), gen(partyid_libdem)
recode party (1/3=0) (4=1) (5/8=0), gen(partyid_snp)
recode party (1/4=0)(5=1) (6/8=0), gen(partyid_ukip)
recode party (1/5=0) (6=1) (7/8=0), gen(partyid_green)
recode party (1/7=0) (8=1), gen(partyid_brexit)

replace partyid_con = 0 if (partyid_con >= .)
replace partyid_lab = 0 if (partyid_lab >= .)
replace partyid_libdem = 0 if (partyid_libdem >= .)
replace partyid_snp = 0 if (partyid_snp >= .)
replace partyid_ukip = 0 if (partyid_ukip >= .)
replace partyid_ukip = 0 if (partyid_ukip >= .)
replace partyid_green = 0 if (partyid_green >= .)
replace partyid_brexit = 0 if (partyid_brexit >= .)


************************************************
*** Brexit
************************************************

recode EUVote (1=1) (2/3=0), gen(leave)
recode EUVote (1=0) (2=1) (3=0), gen(remain)

* Add hypothetical votes
replace leave = 1 if (hypEUVote == 1)
replace remain = 1 if (hypEUVote == 2)


label define leave_lbl 0 "remain / DK" 1 "leave" 
label values leave leave_lbl

label define remain_lbl 0 "leave / DK" 1 "remain" 
label values remain remain_lbl

************************************************
*** Left/right scale
************************************************

* Problem with left-right scale: 20% DK. 
* First, create a 7-point scale in which DKs appear as missing values.

recode leftright (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=.), gen(leftright_dk_mis)

* Second, create a 7-point scale in which DKs appear as 'centre'. (CAN I DO THAT?)

recode leftright (1=1) (2=2) (3=3) (4=4) (5=5) (6=6) (7=7) (8=4), gen(leftright_dk_centre)

* Third, create a 3-point factor using the leftright scale

recode leftright (1/3=1) (4=2) (5/7=3) (8=2), gen(leftright3)

label define leftright3_lbl 1 "left" 2 "centre/DK" 3 "right"
label values leftright3 leftright3_lbl


************************************************
*** Liberal/Conservative scale
************************************************

* Use party support & the left-right scale to create a libCon dummy
* Conservative: Conservative, UKIP
* Liberal: Labour, Liberal Democrat, SNP, PC, Green

recode party (1=2) (2=1) (3=1) (4=1) (5=1) (6=2) (7=1) (8=.), gen(libCon)
label define libCon 1 "liberal" 2 "conservative"
label values libCon libCon

* -- Problem: Half of them do not support any party, i.e. lots of missings
* --> For those people I am using the left right variable instead: 

replace libCon = 1 if missing(libCon) & leftright < 4
replace libCon = 2 if missing(libCon) & leftright > 4



************************************************
*** Immigration attitudes
************************************************

* Dummies
gen curbImmig = .
replace curbImmig = 1 if immigNumbers > 4
replace curbImmig = 0 if immigNumbers <= 4

gen increaseImmig = .
replace increaseImmig = 1 if immigNumbers < 4
replace increaseImmig = 0 if immigNumbers >= 4

* Factor
recode immigNumbers (1/3=1) (4=2) (5/7=3), gen(immigOpinions)
label define immigOpinions_lbl 1 "more" 2 "no change" 3 "fewer"
label values immigOpinions immigOpinions_lbl


* Recoded variable in which higher values = more immigration

recode immigNumbers (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen(numbers)
label define numbers_lbl 1 "-3=many fewer" 4 "4=no change" 7 "7=many more"
label define numbers_lbl 1 "-3=many fewer" 2 "-2" 3 "-1" 4 "0=no change" 5 "+1" 6 "+2" 7 "+3=many more"
label values numbers numbers_lbl

label variable numbers "Immigration opinions"



************************************************
*** Ranking issues
************************************************

* Immigration = most important issue
generate immigNo1 = 0
replace immigNo1 = 1 if issues_immig == 1

label variable immigNo1 "Immigration most important issue"


* Immigration =  most/ 2nd most / 3rd most important issue
generate immigNo123 = .
replace immigNo123 = 1 if !missing(issues_immig)
replace immigNo123 = 0 if missing(issues_immig)

label variable immigNo1 "Immigration among 3 most important issues"

* Create a variable for the most important issue 

generate mostImportantIssue = .
replace mostImportantIssue = 1 if issues_immig == 1
replace mostImportantIssue = 2 if issues_econ == 1
replace mostImportantIssue = 3 if issues_nhs == 1
replace mostImportantIssue = 4 if issues_crime == 1
replace mostImportantIssue = 5 if issues_eu == 1
replace mostImportantIssue = 6 if issues_housing == 1
replace mostImportantIssue = 7 if issues_edu == 1

label variable mostImportantIssue "Most important issue facing Britain today"
label define issues_lbl 1 "Immigration" 2 "The economy" 3 "NHS/Health" 4 "Crime" 5 "EU/Brexit" 6 "Housing" 7 "Schools/Education"
label values mostImportantIssue issues_lbl


************************************************
*** Liberal / Conservative correction dummy 
************************************************

generate gotLibCorrection = 0
replace gotLibCorrection = 1 if !missing(lowPaid_T2)

generate gotConsCorrection = 0
replace gotConsCorrection = 1 if !missing(noAsylum_T2)

label variable gotLibCorrection "Corrected statement = whiteCrime/lowPaid"
label variable gotConsCorrection "Corrected statement = noAsylum/costImmig"

generate gotWhichCorrection = .
replace gotWhichCorrection = 1 if inrange(corrected, 1, 2)
replace gotWhichCorrection = 2 if inrange(corrected, 3, 4)

label variable gotWhichCorrection "Corrected statement"
label define gotWhichCorrection_lbl 1 "anti-immigration" 2 "pro-immigration" 
label values gotWhichCorrection gotWhichCorrection_lbl


************************************************
*** No motivation to disbelieve experts 
************************************************

* Creating a variable for people who got a correction that disconfirmed their  
* belief in one of our false facts BUT confirmed their opinions about immigration.

* NB: immigNumbers is coded so that higher values = LESS immigration! 
* i.e. immigNumbers > 4: fewer immigrants; immigNumbers < 4 : more


* I am creating 2 variables: 

* i. A dummy treating people who want the number of immigrants to stay the same 
* as pro-immigration: 

gen motivatedToReject = 1
replace motivatedToReject = 0 if immigNumbers >= 4 & gotLibCorrection == 1
replace motivatedToReject = 0 if immigNumbers < 4 & gotConsCorrection == 1

label variable motivatedToReject "Stats challenged immig opinions (no change = pro-immig)"


* ii. A three-level factor

gen motivatedToReject_fac = .

replace motivatedToReject_fac = 1 if immigNumbers > 4 & gotConsCorrection == 1
replace motivatedToReject_fac = 1 if immigNumbers < 4 & gotLibCorrection == 1

replace motivatedToReject_fac = 2 if immigNumbers == 4

replace motivatedToReject_fac = 3 if immigNumbers > 4 & gotLibCorrection == 1
replace motivatedToReject_fac = 3 if immigNumbers < 4 & gotConsCorrection == 1


label define m2r_label 1 "Stats challenged opinions" 2 "No opinions" 3 "Stats did not challenge opinions" 
label values motivatedToReject_fac m2r_label

label variable motivatedToReject "Did statistics challenge immigration opinions?"


************************************************
*** Predispositions -- Populist
************************************************

* Recoding the populism variables so that higher values = more populist
recode populism2 (1=4) (2=3) (5=2) (3=1) (4=0), gen(pop_peopleDecisions)
recode populism4 (1=4) (2=3) (5=2) (3=1) (4=0), gen(pop_repByCitizen)
recode populism5 (1=4) (2=3) (5=2) (3=1) (4=0), gen(pop_talkNoAction)
recode populism6 (1=4) (2=3) (5=2) (3=1) (4=0), gen(pop_compromise)

label define pop_label 0 "Strongly disagree" 1 "Disagree" 2 "Don't know" 3 "Agree" 4 "Strongly agree"

label values pop_peopleDecisions pop_label
label values pop_repByCitizen pop_label
label values pop_talkNoAction pop_label
label values pop_compromise pop_label

* Create a scale
gen populism_scale = (pop_peopleDecisions + pop_repByCitizen + pop_talkNoAction + pop_compromise) / 4
label variable populism_scale "Populism scale (higher values=more populist)"

* Check correlations
corr pop_peopleDecisions pop_repByCitizen pop_talkNoAction pop_compromise
alpha pop_peopleDecisions pop_repByCitizen pop_talkNoAction pop_compromise


************************************************
*** Predispositions -- Nationalist
************************************************

* Variables that are coded in the right direction
recode ethno1 (1=0) (2=1) (5=2) (3=3) (4=4), gen(ethno_lotsToLearn)
recode ethno3 (1=0) (2=1) (5=2) (3=3) (4=4), gen(ethno_ashamed2b)
recode ethno6 (1=0) (2=1) (5=2) (3=3) (4=4), gen(ethno_lessProud)

label define ethno_lbl 0 "Strongly agree" 1 "Agree" 2 "Don't know" 3 "Disagree" 4 "Strongly disagree"

label values ethno_lotsToLearn ethno_lbl
label values ethno_ashamed2b ethno_lbl
label values ethno_lessProud ethno_lbl

* Reverse-coded items
recode ethno2 (1=4) (2=3) (5=2) (3=1) (4=0), gen(ethno_ratherBritain)
recode ethno4 (1=4) (2=3) (5=2) (3=1) (4=0), gen(ethno_2Critical)
recode ethno5 (1=4) (2=3) (5=2) (3=1) (4=0), gen(ethno_moreLikeBritain)

label define ethno_label_rev 0 "Strongly Disagree" 1 "Disgree" 2 "Don't know" 3 "Agree" 4 "Strongly agree"

label values ethno_ratherBritain ethno_label_rev
label values ethno_2Critical ethno_label_rev
label values ethno_moreLikeBritain ethno_label_rev


* Create a scale
gen ethno_scale=(ethno_lotsToLearn + ethno_ashamed2b + ethno_lessProud + ethno_ratherBritain + ethno_2Critical + ethno_moreLikeBritain)/6
label variable ethno_scale "Nationlism scale (higher values=more nationlist)"

* TO DO: FIND OUT HOW TO DO PERCENTILES IN STATA!!
* https://www.stata.com/manuals13/dpctile.pdf
gen ethno_fac = . 
replace ethno_fac = 1 if ethno_scale < 0.4 
replace ethno_fac = 2 if ethno_scale >= 0.4 & ethno_scale < 0.5
replace ethno_fac = 3 if ethno_scale >= 0.5

label define ethno_fac_lbl 1 "low" 2 "medium" 3 "high" 
label values ethno_fac ethno_fac_lbl


************************************************
*** Socio-Demographics
************************************************

recode gender (1=1) (2=0), gen (male)
recode gender (1=0) (2=1), gen (female)

recode marital (0=0)(1=1) (3/7=0), gen(single)


************************************************
*** True-Scores
************************************************

/*
Creating a total veracity score for all factual statements. 
--> To control for general proclivity to rate statements as 'true'
*/

gen trueScore8 = . 
replace trueScore8 = noAsylum_T1 + costImmig_T1 + whiteCrime_T1 + lowPaid_T1 + no5_T1 + recession_T1 + fracking_T1 + plasticBags_T1 -8

gen trueScore7 = . 
replace trueScore7 = belief_i2_T1 + belief_i3 + belief_i4 + no5_T1 + recession_T1 + fracking_T1 + plasticBags_T1 -7

label variable trueScore8 "Additive score of all eight veracity scores"
label variable trueScore7 "Additive score of seven veracity scores (excluding corrected statement)"


************************************************
*** DVs
************************************************

************************************************
*** Difference between pre- and post-correction veracity scores 
************************************************

* I am adding 7 so as to get positive scores
* @Rob&John: This is different from the PAP (forgot about negative numbers in there)
* --> The higher the number the greater the effect. 
*
* PAP: Range from -6 and +6. Positive values indicate a positive effect; 
* 0 indicates no effect, and negative values indicate a backfire effect. 
* NEW: 1-6=backfire effect; 7 = no effect, 8-13 = positive effect. 

gen noAsylum_diff = . 
replace noAsylum_diff = noAsylum_T1 - noAsylum_T2 + 7

gen costImmig_diff = .
replace costImmig_diff = costImmig_T1 - costImmig_T2 + 7

gen whiteCrime_diff = .
replace whiteCrime_diff = whiteCrime_T1 - whiteCrime_T2 + 7

gen lowPaid_diff = .
replace lowPaid_diff = lowPaid_T1 - lowPaid_T2 + 7


* tab motivatedToReject noAsylum_diff, row chi2
* --> Unsurprisingly, people who got a correction to a false fact that 
* buttressed their opinions are more likely to adapt their T/F ratings after 
* reading the expert advice. 


gen diff = .
replace diff = belief_T1 - belief_T2 + 7
label variable diff "Difference between pre- and post-correction belief in corrected statement (T1-T2)"

gen diff_i2 = .
replace diff_i2 = belief_i2_T1 - belief_i2_T2 + 7
label variable diff_i2 "Difference between pre- and post-correction belief in un-corrected statement (T1-T2)"


* NB: Same as but easier than: 

* gen diff = .
* replace diff = noAsylum_diff if corrected == 1
* replace diff = costImmig_diff if corrected == 2
* replace diff = whiteCrime_diff if corrected == 3
* replace diff = lowPaid_diff if corrected == 4

* gen diff_i2 = .
* replace diff_i2 = noAsylum_diff if corrected == 2
* replace diff_i2 = costImmig_diff if corrected == 1
* replace diff_i2 = whiteCrime_diff if corrected == 4
* replace diff_i2 = lowPaid_diff if corrected == 3



************************************************
*** Any positive change in veracity scores 
************************************************

* Coding all who got a conservative misperception as 0. 
* Coding all who did got a conservative misperception AND who rated the 
* respective statement as falser the 2nd time around as 1. 

gen noAsylum_anyChange = .
replace noAsylum_anyChange = 0 if inrange(corrected, 1, 2)
replace noAsylum_anyChange = 1 if inrange(corrected, 1, 2) & noAsylum_T1 > noAsylum_T2

gen costImmig_anyChange = .
replace costImmig_anyChange = 0 if inrange(corrected, 1, 2)
replace costImmig_anyChange = 1 if inrange(corrected, 1, 2) & costImmig_T1 > costImmig_T2

gen whiteCrime_anyChange = .
replace whiteCrime_anyChange = 0 if inrange(corrected, 3, 4)
replace whiteCrime_anyChange = 1 if inrange(corrected, 3, 4) & whiteCrime_T1 > whiteCrime_T2

gen lowPaid_anyChange = .
replace lowPaid_anyChange = 0 if inrange(corrected, 3, 4)
replace lowPaid_anyChange = 1 if inrange(corrected, 3, 4) & lowPaid_T1 > lowPaid_T2


gen anyChange = .
replace anyChange = noAsylum_anyChange if corrected == 1
replace anyChange = costImmig_anyChange if corrected == 2
replace anyChange = whiteCrime_anyChange if corrected == 3
replace anyChange = lowPaid_anyChange if corrected == 4

label variable anyChange "Post-correction belief < Pre-correction belief in corrected claim"

gen anyChange_i2 = .
replace anyChange_i2 = noAsylum_anyChange if corrected == 2
replace anyChange_i2 = costImmig_anyChange if corrected == 1
replace anyChange_i2 = whiteCrime_anyChange if corrected == 4
replace anyChange_i2 = lowPaid_anyChange if corrected == 3

label variable anyChange_i2 "Post-correction belief < Pre-correction belief in un-corrected claim"


************************************************
*** Change from true to false
************************************************

gen noAsylum_shift = .
replace noAsylum_shift = 0 if inrange(corrected, 1, 2) 
replace noAsylum_shift = 1 if inrange(corrected, 1, 2) & inrange(noAsylum_T1, 5, 7) & inrange(noAsylum_T2, 1, 3)

gen costImmig_shift = .
replace costImmig_shift = 0 if inrange(corrected, 1, 2) 
replace costImmig_shift = 1 if inrange(corrected, 1, 2) & inrange(costImmig_T1, 5, 7) & inrange(costImmig_T2, 1, 3)

gen whiteCrime_shift = .
replace whiteCrime_shift = 0 if inrange(corrected, 3, 4) 
replace whiteCrime_shift = 1 if inrange(corrected, 3, 4) & inrange(whiteCrime_T1, 5, 7) & inrange(whiteCrime_T2, 1, 3)

gen lowPaid_shift = .
replace lowPaid_shift = 0 if inrange(corrected, 3, 4) 
replace lowPaid_shift = 1 if inrange(corrected, 3, 4) & inrange(lowPaid_T1, 5, 7) & inrange(lowPaid_T2, 1, 3)


gen shift = .
replace shift = noAsylum_shift if corrected == 1
replace shift = costImmig_shift if corrected == 2
replace shift = whiteCrime_shift if corrected == 3
replace shift = lowPaid_shift if corrected == 4

label variable shift "Pre-correction:'true'; post-correction: false"

tab shift
* --> We have convinced 605 people! 

gen shift_i2 = .
replace shift_i2 = noAsylum_shift if corrected == 2
replace shift_i2 = costImmig_shift if corrected == 1
replace shift_i2 = whiteCrime_shift if corrected == 4
replace shift_i2 = lowPaid_shift if corrected == 3

label variable shift_i2 "Un-corrected claim: Pre-correction:'true'; post-correction: false"

tab shift_i2
* --> 67 people shifted on the un-corrected claim


************************************************
*** Do people notice if the stats are inconsistent with what they believed?
************************************************

gen notice = .
replace notice = 1 if belief_T1 <= 4
replace notice = 2 if belief_T1 > 4 & consistent == 2
replace notice = 3 if belief_T1 > 4 & consistent == 1

label variable notice "Did R think that statistics were consistent with what they believed"

label define notice_lbl 1 "Stats WERE consistent" 2 "Thought stats were consistent" 3 "Thought stats were not consistent"
label values notice notice_lbl





* Create dummies

tabulate ifInconsistent, generate(dum)

rename dum1 statsRightBeliefDif
rename dum2 statsWrong
rename dum3 statsChangedMind

label variable statsRightBeliefDif "The statistics are probably right, but I believe something different."
label variable statsWrong "I think that the statistics are wrong."
label variable statsChangedMind "The statistics made me change my mind."


************************************************
*** How people rate source 2
************************************************

* The source of the post-truth comment depends on the group: Groups 3, 4, and
* 5 see a PT comment from a professor; groups 6, 7, and 8 see a PT comment 
* from a blogger.  

gen accurate_source2 = .
replace accurate_source2 = accurateFactFreeProf if inrange(RandomGrp, 3, 5) 
replace accurate_source2 = accurateBlogger if inrange(RandomGrp, 6, 8) 

label define accurate_source2 1 "Very accurate" 2 "Fairly accurate" 3 "Not very accurate" 4 "Not at all accurate" 98 "Can't remember"
label values accurate_source2 accurate_source2

label variable accurate_source2 "How accurate is the information source2 uses?"

gen trust_source2 = .
replace trust_source2 = trustFactFreeProf if inrange(RandomGrp, 3, 5) 
replace trust_source2 = trustBlogger if inrange(RandomGrp, 6, 8) 

label variable accurate_source2 "How much do you trust source2?"

label define trust_source2 1 "6 - Would trust a great deal" 2 "5" 3 "4" 4 "3" 5 "2" 6 "1" 7 " 0 - Would not trust at all" 98 "Can't remember"
label values trust_source2 trust_source2


************************************************
*** Fact/Opinion
************************************************

* Corrected statement

gen factOpinion = .
replace factOpinion = noAsylum_opinion if corrected == 1
replace factOpinion = costImmig_opinion if corrected == 2
replace factOpinion = whiteCrime_opinion if corrected == 3
replace factOpinion = lowPaid_opinion if corrected == 4

label variable factOpinion "Corrected statement: A matter of fact or a matter of opinion?"

label define factOpinion_lbl 1 "0 - Purely a matter of fact" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6 - Purely a matter of opinion"
label values factOpinion factOpinion_lbl

* Uncorrected statement

gen factOpinion_i2 = .
replace factOpinion_i2 = noAsylum_opinion if corrected == 2
replace factOpinion_i2 = costImmig_opinion if corrected == 1
replace factOpinion_i2 = whiteCrime_opinion if corrected == 4
replace factOpinion_i2 = lowPaid_opinion if corrected == 3

label variable factOpinion_i2 "Un-corrected statement: A matter of fact or a matter of opinion?"

label define factOpinion_i2_lbl 1 "0 - Purely a matter of fact" 2 "1" 3 "2" 4 "3" 5 "4" 6 "5" 7 "6 - Purely a matter of opinion"
label values factOpinion_i2 factOpinion_i2_lbl


* Social grades: https://en.wikipedia.org/wiki/NRS_social_grade
* Re-coding so that higher values = higher standing

gen socialGrade_num = .
replace socialGrade_num = 4 if socialGrade == 1
replace socialGrade_num = 3 if socialGrade == 2
replace socialGrade_num = 2 if socialGrade == 3
replace socialGrade_num = 1 if socialGrade == 4

label variable socialGrade_num "Social grade (higher values=higher grades)"

* @Rob&John: Grades D and E seem to be overrepresented (40% here, 23% in the general
* population according to wikipedia. --> Should we use weights? 
* (Could check this against the BES.)

* Also @ Rob&John: PollAtt is a 3-level variable that seems to have been 
* recoded from an 11-point variable. Any way we could get JT to get us 
* the 11-point variable so we can include the real deal in our regression models? 

* tab PollAtt

gen ok2disagree_num = .
replace ok2disagree_num = 4 if ok2disagree == 1
replace ok2disagree_num = 3 if ok2disagree == 2
replace ok2disagree_num = 2 if ok2disagree == 3
replace ok2disagree_num = 1 if ok2disagree == 4


save "working data 20.9.2019.dta", replace
outsheet using "working data 20.9.2019.csv", replace

*** END DATA MANAGEMENT ***







* Left out the following variables for a cleaner dataset
* (@Rob&John: Feel free to put these variables back into the code!)


************************************************
*** Belief in false facts -- reverse-coded
************************************************

************************************************
*** T1, reverse-coded
************************************************

* @John: These are the variables you called knowasylum_T1 etc. 
* In the original scale, higher values = greater belief in false facts
* Here: higher values = less belief in false facts

recode belief_T1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_T1)
recode noAsylum_T1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_noAsylum_T1)
recode costImmig_T1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_costImmig_T1)
recode whiteCrime_T1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_whiteCrime_T1)
recode lowPaid_T1 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_lowPaid_T1)

label variable know_T1 "Pre-correction veracity score, reverse coded (7=Definitely false)"
label variable know_noAsylum_T1 "Pre-correction veracity score, reverse coded (7=Definitely false)"
label variable know_costImmig_T1 "Pre-correction veracity score, reverse coded (7=Definitely false)"
label variable know_whiteCrime_T1 "Pre-correction veracity score, reverse coded (7=Definitely false)"
label variable know_lowPaid_T1 "Pre-correction veracity score, reverse coded (7=Definitely false)"

************************************************
*** T2, reverse-coded
************************************************

recode belief_T2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_T2)
recode noAsylum_T2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_noAsylum_T2)
recode costImmig_T2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_costImmig_T2)
recode whiteCrime_T2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_whiteCrime_T2)
recode lowPaid_T2 (1=7) (2=6) (3=5) (4=4) (5=3) (6=2) (7=1), gen (know_lowPaid_T2)

label variable know_T2 "Post-correction veracity score, reverse coded (7=Definitely false)"
label variable know_noAsylum_T2 "Post-correction veracity score, reverse coded (7=Definitely false)"
label variable know_costImmig_T2 "Post-correction veracity score, reverse coded (7=Definitely false)"
label variable know_whiteCrime_T2 "Post-correction veracity score, reverse coded (7=Definitely false)"
label variable know_lowPaid_T2 "Post-correction veracity score, reverse coded (7=Definitely false)"


************************************************
*** Pre-correction -- Correct beliefs? 
************************************************

* @JB: These are your correctAsylum_T1 etc. variables (I renamed them to match 
* my wrong_noAsylum_T1 etc. variables). 
* For simplicity I took this out of the main dataset because I have been 
* working with the wrong_ ... variables. If you do use these please 
* put them back in! a
* put them back in! 

gen correct_T1 = .
replace correct_T1 = 1 if belief_T1 < 4 
replace correct_T1 = 0 if belief_T1 >= 4 

gen correct_noAsylum_T1 = .
replace correct_noAsylum_T1 = 1 if noAsylum_T1 < 4 
replace correct_noAsylum_T1 = 0 if noAsylum_T1 >= 4 

gen correct_costImmig_T1 = .
replace correct_costImmig_T1 = 1 if costImmig_T1 < 4 
replace correct_costImmig_T1 = 0 if costImmig_T1 >= 4 

gen correct_whiteCrime_T1 = .
replace correct_whiteCrime_T1 = 1 if whiteCrime_T1 < 4 
replace correct_whiteCrime_T1 = 0 if whiteCrime_T1 >= 4 

gen correct_lowPaid_T1 = .
replace correct_lowPaid_T1 = 1 if lowPaid_T1 < 4 
replace correct_lowPaid_T1 = 0 if lowPaid_T1 >= 4 


* label
label variable correct_T1 "Pre-correction: Rated false claim as false (1-3)"
label variable correct_noAsylum_T1 "Pre-correction: Rated false claim as false (1-3)"
label variable correct_costImmig_T1 "Pre-correction: Rated false claim as false (1-3)"
label variable correct_whiteCrime_T1 "Pre-correction: Rated false claim as false (1-3)"
label variable correct_lowPaid_T1 "Pre-correction: Rated false claim as false (1-3)"



************************************************
*** Post-correction -- Correct beliefs? 
************************************************

gen correct_T2 = .
replace correct_T2 = 1 if belief_T2 < 4 
replace correct_T2 = 0 if belief_T2 >= 4 

gen correct_noAsylum_T2 = .
replace correct_noAsylum_T2 = 1 if noAsylum_T2 < 4 
replace correct_noAsylum_T2 = 0 if noAsylum_T2 >= 4 

gen correct_costImmig_T2 = .
replace correct_costImmig_T2 = 1 if costImmig_T2 < 4 
replace correct_costImmig_T2 = 0 if costImmig_T2 >= 4 

gen correct_whiteCrime_T2 = .
replace correct_whiteCrime_T2 = 1 if whiteCrime_T2 < 4 
replace correct_whiteCrime_T2 = 0 if whiteCrime_T2 >= 4 

gen correct_lowPaid_T2 = .
replace correct_lowPaid_T2 = 1 if lowPaid_T2 < 4 
replace correct_lowPaid_T2 = 0 if lowPaid_T2 >= 4 


* label
label variable correct_T2 "Post-correction: Rated false claim as false (1-3)"
label variable correct_noAsylum_T2 "Post-correction: Rated false claim as false (1-3)"
label variable correct_costImmig_T2 "Post-correction: Rated false claim as false (1-3)"
label variable correct_whiteCrime_T2 "Post-correction: Rated false claim as false (1-3)"
label variable correct_lowPaid_T2 "Post-correction: Rated false claim as false (1-3)"



************************************************
*** Un-corrected item 
************************************************


gen correct_i2_T1 = . 
replace correct_i2_T1 = 1 if belief_i2_T1 < 4 
replace correct_i2_T1 = 0 if belief_i2_T1 >= 4 

gen correct_i2_T2 = . 
replace correct_i2_T2 = 1 if belief_i2_T2 < 4 
replace correct_i2_T2 = 0 if belief_i2_T2 >= 4 

label variable correct_i2_T2 "Pre-correction: Rated un-corrected false claim as false (1-3)"
label variable correct_i2_T2 "Post-correction: Rated un-corrected false claim as false (1-3)"

