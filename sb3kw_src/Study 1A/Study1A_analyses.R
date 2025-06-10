#####
#Analyses of Study 1A
#
#On OSF the data have been de-identified before uploading. The steps taken to achieve that are also described in the code.
#For the data file with the full information please contact Jens Lange.
#
#First parts of the code refer to the full file. It's commented out.
#####
#####

setwd("X:/OneDrive/Envy/Entitlement, Status, and Envy/Schriftliches/Documentation for OSF/Study 1A")

library(rio)
library(psych)
library(PerformanceAnalytics)
library(lavaan)

# data <- import("Study1A.sav", header = TRUE)
# 
# #####
# #Editing the data set
# #####
# 
# #delete useless variables
# 
# data <- subset(data, select = -c(V1,V2,V3,V4,V5,V6,V7,V8,V9,consent,consentyes,pesinstr,prestdommotinstr,
#                                  envyscaleinstr,socialdesir_instr,
#                                  LocationLatitude,LocationLongitude,LocationAccuracy))
# 
# #exclude attempts to look at the study (no actual participants)
# 
# data <- data[data$V10 == "1", ]
# 
# data <- subset(data, select = -c(V10))
# 
# #the first three rows were attempts by us to test the study
# 
# data <- data[data$MTurkCode != "64145088" & data$MTurkCode != "46175244" & data$MTurkCode != "30986345", ]
# 
# #recoding variables
# 
# data$gender <- factor(data$gender, levels = c(1,2,3), labels = c("female","male","other or decline"))
# data$race <- factor(data$race, levels = c(1,2,3,4,5,6,7,8,9),
#                     labels = c("American Indian/Alaska Native","East Asian","South Asian","Native Hawaiian or other Pacific Islander",
#                                "Black or African American","White","More than one race: Black/White",
#                                "More than one race: Other","Other or unknown"))
# data$age <- as.numeric(as.character(data$age))
# 
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
# #benign1 - "When I envy others, I focus on how I can become equally successful in the future."
# #malicious1 - "I wish that superior people lose their advantage."
# #benign2 - "If I notice that another person is better than me, I try to improve myself."
# #benign3 - "Envying others motivates me to accomplish my goals."
# #malicious2 - "If other people have something that I want for myself, I wish to take it away from them."
# #malicious3 - "I feel ill will towards people I envy."
# #benign4 - "I strive to reach other people's superior achievements."
# #malicious4 - "Envious feelings cause me to dislike the other person."
# #benign5 - "If someone has superior qualities, achievements, or possessions, I try to attain them for myself."
# #malicious5 - "Seeing other people's achievements makes me resent them."
# #Responses were given on a scale from 1 (strongly disagree), 2 (moderately disagree), 3 (slightly disagree),
# #5 (slightly agree), 6 (moderately agree), to 7 (strongly agree).
# #
# #socialdesir1 - "It is sometimes hard for me to go on with my work if I am not encouraged."
# #socialdesir2 - "I sometimes feel resentful when I don't get my way."
# #socialdesir3 - "On a few occasions, I have given up doing something because I thought too little of my ability."
# #socialdesir4 - "There have been times when I felt like rebelling against people in authority even though I knew they were right."
# #socialdesir5 - "No matter who I'm talking to, I'm always a good listener."
# #socialdesir6 - "There have been occasions when I took advantage of someone."
# #socialdesir7 - "I'm always willing to admit it when I make a mistake."
# #socialdesir8 - "I sometimes try to get even rather than forgive and forget."
# #socialdesir9 - "I am always courteous, even to people who are disagreeable."
# #socialdesir10 - "I have never been irked when people expressed ideas very different from my own."
# #socialdesir11 - "There have been times when I was quite jealous of the good fortune of others."
# #socialdesir12 - "I am sometimes irritated by people who ask favors of me."
# #socialdesir13 - "I have never deliberately said something that hurt someone's feelings."
# #Responses were given on a scale from 1 (True) to 2 (False).
# 
# #Benign and malicious envy items were accidentally coded with skipping value 4 (see above).
# #Therefore, I recode all variables.
# 
# data$benign1 <- ifelse(data$benign1 > 3, data$benign1 - 1, data$benign1)
# data$benign2 <- ifelse(data$benign2 > 3, data$benign2 - 1, data$benign2)
# data$benign3 <- ifelse(data$benign3 > 3, data$benign3 - 1, data$benign3)
# data$benign4 <- ifelse(data$benign4 > 3, data$benign4 - 1, data$benign4)
# data$benign5 <- ifelse(data$benign5 > 3, data$benign5 - 1, data$benign5)
# data$malicious1 <- ifelse(data$malicious1 > 3, data$malicious1 - 1, data$malicious1)
# data$malicious2 <- ifelse(data$malicious2 > 3, data$malicious2 - 1, data$malicious2)
# data$malicious3 <- ifelse(data$malicious3 > 3, data$malicious3 - 1, data$malicious3)
# data$malicious4 <- ifelse(data$malicious4 > 3, data$malicious4 - 1, data$malicious4)
# data$malicious5 <- ifelse(data$malicious5 > 3, data$malicious5 - 1, data$malicious5)
# 
# ###From here on, you find the steps to de-identify the data to reach the version on OSF.
# 
# #mturkid is personal information. I deleted it.
# 
# data <- subset(data, select = -c(mturkid))
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
# data$age <- ifelse(data$age <= 27, 1, ifelse(data$age > 27 & data$age <= 36, 2, 3))
# 
# #age 1 'bottom third' 2 'middle third' 3 'upper third'
# 
# #Only two participants indicated "other" for gender.
# #I recode gender such that the "other" category is grouped to "male".
# 
# data$genderr <- ifelse(data$gender == "female", 1, 2)
# data$genderr <- factor(data$genderr, levels = c(1,2), labels = c("female","male"))
# data <- subset(data, select = -c(gender))
# 
# #Some ethnicities were rare. I formed groups.
# 
# data$race <- ifelse(data$race == "White", "White", ifelse(data$race == "East Asian", "East Asian", "Other"))
# 
# #Final OSF version reached. Code below saves file. This is the data set on OSF.
# 
# export(data, "Study1A.csv")
# 
# #The code below allows you to run all the analyses from the paper as long as the respective variables are unchanged.

data <- import("Study1A.csv", header = TRUE)

#####
#Demographics
#####

#editing variables

describe(data.frame(data$age))
summary(data.frame(data$gender))

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

psych::alpha(data.frame(data$benign1,data$benign2,data$benign3,data$benign4,data$benign5))

data$benign <- rowMeans(data[c("benign1","benign2","benign3","benign4","benign5")], na.rm = TRUE)

#malicious envy

psych::alpha(data.frame(data$malicious1,data$malicious2,data$malicious3,data$malicious4,data$malicious5))

data$malicious <- rowMeans(data[c("malicious1","malicious2","malicious3","malicious4","malicious5")], na.rm = TRUE)

#####
#Social Desirability
#####

data$socialdesir5r <- 3 - data$socialdesir5
data$socialdesir7r <- 3 - data$socialdesir7
data$socialdesir9r <- 3 - data$socialdesir9
data$socialdesir10r <- 3 - data$socialdesir10
data$socialdesir13r <- 3 - data$socialdesir13

psych::alpha(data.frame(data$socialdesir1,data$socialdesir2,data$socialdesir3,data$socialdesir4,
                        data$socialdesir5r,data$socialdesir6,data$socialdesir7r,data$socialdesir8,
                        data$socialdesir9r,data$socialdesir10r,data$socialdesir11,
                        data$socialdesir12,data$socialdesir13r))

data$s_d <- rowMeans(data[c("socialdesir1","socialdesir2","socialdesir3","socialdesir4",
                            "socialdesir5r","socialdesir6","socialdesir7r","socialdesir8",
                            "socialdesir9r","socialdesir10r","socialdesir11","socialdesir12",
                            "socialdesir13r")], na.rm = TRUE)

#####
#Analyses
#####

describe(data.frame(data$pes,data$prestige,data$dominance,data$benign,data$malicious,data$s_d))

chart.Correlation(data[ , c("pes","prestige","dominance","benign","malicious","s_d")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

#####
###Initial path model that tests the hypotheses and controls for social desirability.

model_initial_sd <-
'
#regressions with constraints

prestige ~ ep*pes + sp*s_d
dominance ~ ed*pes + sd*s_d

benign ~ 0*pes + pb*prestige + 0*dominance + sb*s_d
malicious ~ 0*pes + 0*prestige + dm*dominance + sm*s_d

#covariances

prestige ~~ pd*dominance
benign ~~ bm*malicious
pes ~~ es*s_d

#definitions

indirect_e_p_b := ep*pb
indirect_e_d_m := ed*dm

total_e_b := 0 + (ep*pb) + (ed*0)
total_e_m := 0 + (ed*dm) + (ep*0)
'

fit_model_initial_sd <- sem(model_initial_sd, data = data, likelihood = "wishart")

summary(fit_model_initial_sd, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

###We added the direct effects of entitlement on benign and malicious envy.

model_direct_sd <-
'
#regressions with constraints

prestige ~ ep*pes + sp*s_d
dominance ~ ed*pes + sd*s_d

benign ~ eb*pes + pb*prestige + 0*dominance + sb*s_d
malicious ~ em*pes + 0*prestige + dm*dominance + sm*s_d

#covariances

prestige ~~ pd*dominance
benign ~~ bm*malicious
pes ~~ es*s_d

#definitions

indirect_e_p_b := ep*pb
indirect_e_d_m := ed*dm

total_e_b := eb + (ep*pb) + (ed*0)
total_e_m := em + (ed*dm) + (ep*0)
'

fit_model_direct_sd <- sem(model_direct_sd, data = data, likelihood = "wishart")

anova(fit_model_initial_sd,fit_model_direct_sd)
summary(fit_model_direct_sd, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)

parameterEstimates(fit_model_direct_sd)

#bootstrapped bias-corrected confidence intervals for indirect effects
# fit_model_direct_sd_se <- sem(model_direct_sd, data = data, se = "boot", bootstrap = 5000)
# save(fit_model_direct_sd_se, file = "bc_bootstrap_model_direct_social_desirability.RData")

load("bc_bootstrap_model_direct_social_desirability.RData") #loads the data from previous bootstrap
                                                            #can be run again with code above; results will be slightly different
as.data.frame(parameterEstimates(fit_model_direct_sd_se, boot.ci.type = "bca.simple"))[22:25, ]

#####
###Further model tests without social desirability.

model_initial <-
'
#regressions with constraints

prestige ~ ep*pes
dominance ~ ed*pes

benign ~ 0*pes + pb*prestige + 0*dominance
malicious ~ 0*pes + 0*prestige + dm*dominance

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

###We added the direct effects of entitlement on benign and malicious envy.

model_direct <-
'
#regressions with constraints

prestige ~ ep*pes
dominance ~ ed*pes

benign ~ eb*pes + pb*prestige + 0*dominance
malicious ~ em*pes + 0*prestige + dm*dominance

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

anova(fit_model_initial, fit_model_direct)
summary(fit_model_direct, standardized = TRUE, fit.measures = TRUE, rsq = TRUE)
parameterEstimates(fit_model_direct)

#bootstrapped bias-corrected confidence intervals for indirect effects
# fit_model_direct_se <- sem(model_direct, data = data, se = "boot", bootstrap = 5000)
# save(fit_model_direct_se, file = "bc_bootstrap_model_direct.RData")

load("bc_bootstrap_model_direct.RData") #loads the data from previous bootstrap
                                        #can be run again with code above; results will be slightly different
as.data.frame(parameterEstimates(fit_model_direct_se, boot.ci.type = "bca.simple"))[16:19, ]
