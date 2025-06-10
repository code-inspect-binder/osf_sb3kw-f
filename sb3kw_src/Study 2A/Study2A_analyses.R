#####
#Analyses of Study 2A
#
#On OSF the data have been de-identified before uploading. The steps taken to achieve that are also described in the code.
#For the data file with the full information please contact Jens Lange.
#
#First parts of the code refer to the full file. It's commented out.
#####
#####

setwd("X:/OneDrive/Envy/Entitlement, Status, and Envy/Schriftliches/Documentation for OSF/Study 2A")

library(rio)
library(psych)
library(PerformanceAnalytics)
library(lavaan)
library(afex)
library(heplots)

# data <- import("Study2A.sav", header = TRUE)
# 
# #####
# #Editing the data set
# #####
# 
# #delete useless variables
# 
# data <- subset(data, select = -c(V1,V2,V3,V4,V5,V6,V7,V8,V9,consent,consentyes,mturkid,pesinstr,
#                                  prestdommotinstr,envymanipfem1,envymanipfem2,envymanipfem2.0,
#                                  Q67,Q66,envymanipmale1,envymanipmale2,envymanipmale2.0,Q68,Q69,
#                                  envyscaleinstr,LocationLatitude,LocationLongitude,LocationAccuracy))
# 
# #exclude participants who did not finish the study (no actual participations)
# 
# data <- data[data$V10 == 1, ]
# 
# data <- subset(data, select = -c(V10))
# 
# #the first five rows were attempts by us to test the study
# 
# data <- data[data$MTurkCode != "81598597" & data$MTurkCode != "84637916" & data$MTurkCode != "93648639"
#              & data$MTurkCode != "77158876" & data$MTurkCode != "41357521", ]
# 
# ###recoding variables and create variables for further analysis
# 
# #variables related to the Manipulation
# data$Cond <- ifelse(data$entmanipinstr == 1, 1, 0)
# data$Cond[is.na(data$Cond)] <- 0
# data$Cond <- factor(data$Cond, levels = c(0,1), labels = c("Control","Entitlement"))
# data$reason1 <- ifelse(data$Cond == "Control", data$loent1, data$hient1)
# data$reason2 <- ifelse(data$Cond == "Control", data$loent2, data$hient2)
# data$reason3 <- ifelse(data$Cond == "Control", data$loent3, data$hient3)
# data <- subset(data, select = -c(entmanipinstr,entcontrolinstr,hient1,hient2,hient3,loent1,loent2,loent3))
# 
# #demographic variables
# data$gender <- factor(data$gender, levels = c(1,2), labels = c("female","male"))
# data$race <- factor(data$race, levels = c(1,2,3,4,5,6,7,8,9),
#                     labels = c("American Indian/Alaska Native","East Asian","South Asian","Native Hawaiian or other Pacific Islander",
#                                "Black or African American","White","More than one race: Black/White",
#                                "More than one race: Other","Other or unknown"))
# data$age <- as.numeric(as.character(data$age))
# 
# #variables related to gender-matched envy situation (manipulation check)
# data$mc <- ifelse(data$gender == "female", data$mcheckfemale, data$Q70)
# #categories were coded 4, 5, and 7; I recode it to 1, 2, and 3
# data$mc <- ifelse(data$mc == 4, 1, ifelse(data$mc == 5, 2, 3))
# data$mc <- factor(data$mc, levels = c(1,2,3), labels = c("authentic pride","hubristic pride","neither"))
# data <- subset(data, select = -c(mcheckfemale,Q70))
# 
# #Cond == Entitlement
# #reason1 - "Please write one reason you personally should demand the best in life."
# #reason2 - "Please write one reason you deserve more than others."
# #reason3 - "Please write one reason you should get your way in life."
# #Cond == Control
# #reason1 - "Please write one reason you personally should not demand the best in life."
# #reason2 - "Please write one reason you do not deserve more than others."
# #reason3 - "Please write one reason you should not expect to get your own way in life."
# #
# #pes1 - "I honestly feel I'm just more deserving than others."
# #pes2 - "Great things should come to me."
# #pes3 - "If I were on the Titanic, I would deserve to be on the first lifeboat!"
# #pes4 - "I demand the best because I'm worth it."
# #pes5 - "I do not necessarily deserve special treatment."
# #pes6 - "I deserve more things in my life."
# #pes7 - "People like me deserve an extra break now and then."
# #pes8 - "Things should go my way."
# #pes9 - "I feel entitled to more of everything."
# #Responses were given on a scale from 1 (strongly disagree), 2 (moderately disagree), 3 (slightly disagree),
# #4 (neither agree nor disagree), 5 (slightly agree), 6 (moderately agree), to 7 (strongly agree).
# #
# #dom1 - "I enjoy having control over others."
# #dom2 - "I often try to get my own way regardless of what others may want."
# #dom3 - "I am willing to use aggressive tactics to get my way."
# #dom4 - "I try to control others rather than permit them to control me."
# #dom5 - "I do not aim at having a forceful or dominant personality."
# #dom6 - "I want others to know it is better to let me have my way."
# #dom7 - "I do not enjoy having authority over other people."
# #dom8 - "I like when some people are afraid of me."
# #prest1 - "I try to get members of my group to respect and admire me."
# #prest2 - "It does not bother me if people do not want to be like me."
# #prest3 - "I enjoy it when others expect me to be successful."
# #prest4 - "It does not bother me if others do not value my opinion."
# #prest5 - "I try to be held in high esteem by those I know."
# #prest6 - "I like it when my unique talents and abilities are recognized by others."
# #prest7 - "I like it when I am considered an expert on some matters by others."
# #prest8 - "I enjoy it when others seek my advice on a variety of matters."
# #prest9 - "It would not bother me if others do not enjoy hanging out with me."
# #Responses were given on a scale from 1 (strongly disagree), 2 (moderately disagree), 3 (slightly disagree),
# #4 (neither agree nor disagree), 5 (slightly agree), 6 (moderately agree), to 7 (strongly agree).
# #
# #mc - "Please choose which of the following options best describes the way that Tom/Tina seems to be feeling.
# #mc == "authentic pride" - "accomplished, achieving, confident, fulfilled, productive, has self-worth, successful"
# #mc == "hubristic pride" - "arrogant, conceited, egotistical, pompous, smug, snobbish, stuck-up"
# #mc == "neither" - "neither of these is correct"
# #
# #benign1 - "I would feel deep longing for the other student's success."
# #benign2 - "I would want to work harder to also obtain the other student's success."
# #benign3 - "I would want to devise a plan to obtain the other student's success as well."
# #benign4 - "The other student would motivate me to become just like him/her."
# #malicious1 - "I would want to complain to someone else about the other student."
# #malicious2 - "I would feel hostile towards the other student."
# #malicious3 - "I would wish that the other student would lose their success."
# #malicious4 - "The situation would make me feel hatred."
# #pain1 - "The situation would make me feel bothered."
# #pain2 - "The situation would make me feel inadequate."
# #pain3 - "The situation would make me feel depressed."
# #Responses were given on a scale from 1 (strongly disagree), 2 (moderately disagree), 3 (slightly disagree),
# #4 (neither agree nor disagree), 5 (slightly agree), 6 (moderately agree), to 7 (strongly agree).
# 
# coding <- import("coding.sav", header = TRUE)
# data <- merge(data, coding, by = c("MTurkCode"), all.x = TRUE)
# 
# rm(coding)
# 
# ###From here on, you find the steps to de-identify the data to reach the version on OSF.
# 
# #MTurkCode could be traced back via MTurk Badge files. I deleted it.
# #I created a random number as participant ID.
# 
# data <- subset(data, select = -c(MTurkCode))
# 
# data$id <- sample(nrow(data), replace = FALSE)
# data <- data[order(data$id, decreasing = FALSE), ]
# 
# #Several ages occurred only once. I formed groups.
# 
# quantile(data$age, c(.33,.66), na.rm = TRUE)
# 
# data$age <- ifelse(data$age <= 28, 1, ifelse(data$age > 28 & data$age <= 37, 2, 3))
# 
# #age 1 'bottom third' 2 'middle third' 3 'upper third'
# 
# #Some ethnicities were rare. I formed groups.
# 
# data$race <- ifelse(data$race == "White", "White",
#                     ifelse(data$race == "East Asian", "East Asian",
#                            ifelse(data$race == "Black or African American", "Black or African American", "Other")))
# 
# #The reasons participants mentioned as part of the manipulation were often rather personal.
# #So I deleted these variables.
# 
# data <- subset(data, select = -c(reason1,reason2,reason3))
# 
# #Final OSF version reached. Code below saves file. This is the data set on OSF.
# 
# export(data, "Study2A.csv")
# 
# #The code below allows you to run all the analyses from the paper as long as the respective variables are unchanged.

data <- import("Study2A.csv", header = TRUE)

#####
#Manipulation check for entitlement manipulation
#####

#inter-rater agreement
cohen.kappa(data[ , c("coder1","coder2")])

#only if both coders indicated that a participant should be kept, s/he is retained
data$exclude <- ifelse(data$coder1 == 0 & data$coder2 == 0, 0, 1)
data$exclude <- factor(data$exclude, levels = c(0,1), labels = c("no","yes"))

#test whether Condition predicts exclusion probability
Cond_exclude <- table(data$Cond, data$exclude)
chisq.test(Cond_exclude)
rm(Cond_exclude)

#test whether Entitlement scores based on PES differ by exclusion
data$pes5r <- 8 - data$pes5 #recoding of item to then average all items of PES as measure of entitlement
data$pes <- rowMeans(data[c("pes1","pes2","pes3","pes4","pes5r","pes6","pes7","pes8","pes9")], na.rm = TRUE)
exclude_entitle <- aov_ez("id", "pes", data = data, between = c("exclude"))
nice(exclude_entitle, es = "pes")
rm(exclude_entitle)
data <- subset(data, select = -c(pes5r,pes))

#exclude participants who failed the manipulation check
data <- data[data$exclude == "no" & !is.na(data$exclude), ]

#####
#Demographics
#####

describe(data.frame(data$age))
summary(data.frame(data$gender))
summary(data.frame(data$Cond))

#####
#Entitlement
#####

data$pes5r <- 8 - data$pes5

psych::alpha(data.frame(data$pes1,data$pes2,data$pes3,data$pes4,data$pes5r,data$pes6,data$pes7,data$pes8,data$pes9))

data$pes <- rowMeans(data[c("pes1","pes2","pes3","pes4","pes5r","pes6","pes7","pes8","pes9")], na.rm = TRUE)

#####
#Status
#####

data$dom5r <- 8 - data$dom5
data$dom7r <- 8 - data$dom7
data$prest2r <- 8 - data$prest2
data$prest4r <- 8 - data$prest4
data$prest9r <- 8 - data$prest9

#prestige

psych::alpha(data.frame(data$prest1,data$prest2r,data$prest3,data$prest4r,data$prest5,data$prest6,
                        data$prest7,data$prest8,data$prest9r))

data$prestige <- rowMeans(data[c("prest1","prest2r","prest3","prest4r","prest5","prest6",
                                 "prest7","prest8","prest9r")], na.rm = TRUE)

#dominance

psych::alpha(data.frame(data$dom1,data$dom2,data$dom3,data$dom4,data$dom5r,data$dom6,data$dom7r,data$dom8))

data$dominance <- rowMeans(data[c("dom1","dom2","dom3","dom4","dom5r","dom6","dom7r","dom8")], na.rm = TRUE)

#####
#Envy
#####

#benign envy

psych::alpha(data.frame(data$benign1,data$benign2,data$benign3,data$benign4))

data$benign <- rowMeans(data[c("benign1","benign2","benign3","benign4")], na.rm = TRUE)

#malicious envy

psych::alpha(data.frame(data$malicious1,data$malicious2,data$malicious3,data$malicious4))

data$malicious <- rowMeans(data[c("malicious1","malicious2","malicious3","malicious4")], na.rm = TRUE)

#pain

psych::alpha(data.frame(data$pain1,data$pain2,data$pain3))

data$pain <- rowMeans(data[c("pain1","pain2","pain3")], na.rm = TRUE)

#####
#Analyses
#####

describe(data.frame(data$pes,data$prestige,data$dominance,data$benign,data$malicious,data$pain))

###correlations separately for each condition

data_con <- data[data$Cond == "Control", ]
data_ent <- data[data$Cond == "Entitlement", ]

chart.Correlation(data_con[ , c("pes","prestige","dominance","benign","malicious","pain")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)
chart.Correlation(data_ent[ , c("pes","prestige","dominance","benign","malicious","pain")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

rm(data_con,data_ent)

###Manipulation Check

describeBy(data$pes, data$Cond)
t.test(data$pes ~ data$Cond, var.equal = TRUE)
#effect size Hedges g
(mean(data$pes[data$Cond == "Control"]) - mean(data$pes[data$Cond == "Entitlement"])) / sqrt((sd(data$pes[data$Cond == "Control"])^2 + sd(data$pes[data$Cond == "Entitlement"])^2) / 2)

###Mean Comparisons

DVs <- cbind(data$prestige,data$dominance,data$benign,data$malicious)

manova_DVs <- manova(DVs ~ data$Cond)
summary(manova_DVs)
etasq(manova_DVs, partial = TRUE)

#prestige
describeBy(data$prestige, data$Cond)
nice(aov_ez("id", "prestige", data = data, between = c("Cond")), es = "pes")

#dominance
describeBy(data$dominance, data$Cond)
nice(aov_ez("id", "dominance", data = data, between = c("Cond")), es = "pes")

#benign
describeBy(data$benign, data$Cond)
nice(aov_ez("id", "benign", data = data, between = c("Cond")), es = "pes")

#malicious
describeBy(data$malicious, data$Cond)
nice(aov_ez("id", "malicious", data = data, between = c("Cond")), es = "pes")

#####
###Path Analysis

#recoding of Condition
data$Condr <- ifelse(data$Cond == "Control", -0.5, 0.5)

model_initial <-
'
#regressions with constraints

prestige ~ ep*Condr
dominance ~ ed*Condr

benign ~ 0*Condr + pb*prestige + 0*dominance
malicious ~ 0*Condr + 0*prestige + dm*dominance

#covariances

prestige ~~ pd*dominance
benign ~~ bm*malicious

#definitions

indirect_e_p_b := ep*pb
indirect_e_d_m := ed*dm

total_e_b := 0 + (ep*pb) + (ed*0)
total_e_m := 0 + (ed*dm) + (ep*0)
'

fit_model_initial <- sem(model_initial, data = data, likelihood = "wishart")

summary(fit_model_initial, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

parameterEstimates(fit_model_initial)

#bootstrapped bias-corrected confidence intervals for indirect effects
# fit_model_initial_se <- sem(model_initial, data = data, se = "boot", bootstrap = 5000)
# save(fit_model_initial_se, file = "bc_bootstrap_model_initial.RData")

load("bc_bootstrap_model_initial.RData") #loads the data from previous bootstrap
                                         #can be run again with code above; results will be slightly different
as.data.frame(parameterEstimates(fit_model_initial_se, boot.ci.type = "bca.simple"))[16:19, ]

#####
###Comparison with a model indcluding the direct effects of Cond on benign and malicious envy

model_direct <-
'
#regressions with constraints

prestige ~ ep*Condr
dominance ~ ed*Condr

benign ~ eb*Condr + pb*prestige + 0*dominance
malicious ~ em*Condr + 0*prestige + dm*dominance

#covariances

prestige ~~ pd*dominance
benign ~~ bm*malicious

#definitions

indirect_e_p_b := ep*pb
indirect_e_d_m := ed*dm

total_e_b := eb + (ep*pb) + (ed*0)
total_e_m := em + (ed*dm) + (ep*0)
'

fit_model_direct <- sem(model_direct, data = data, likelihood = "wishart")

anova(fit_model_initial,fit_model_direct)
