#####
#Analyses of Study 3B
#
#On OSF the data have been de-identified before uploading. The steps taken to achieve that are also described in the code.
#For the data file with the full information please contact Jens Lange.
#
#First parts of the code refer to the full file. It's commented out.
#####
#####

setwd("X:/OneDrive/Envy/Entitlement, Status, and Envy/Schriftliches/Documentation for OSF/Studies 3A, 3B, & 3C")

library(rio)
library(psych)
library(PerformanceAnalytics)
library(dplyr)
library(tidyr)

# data <- import("Study3B.sav", header = TRUE)
# 
# #####
# #Editing the data set
# #####
# 
# #delete useless variables
# 
# data <- subset(data, select = -c(V2,V3,V4,V5,V6,V7,V8,V9,
#                                  intro_motivation, LocationLatitude,LocationLongitude,LocationAccuracy))
# 
# #exclude participants who did not finish the study (no actual participations)
# 
# data <- data[data$V10 == 1, ]
# 
# data <- subset(data, select = -c(V10))
# 
# #delete attempts from us to test the questionnaire following technical problems
# 
# data <- data[data$dyad != 999, ]
# 
# #During data collection, seven dyads encountered technical difficulties, preventing participation.
# #These are no real participations. I therefore deleted them,
# 
# data <- data[data$dyad != 46 & data$dyad != 66 & data$dyad != 80 & data$dyad != 96 & data$dyad != 97
#              & data$dyad != 138 & data$dyad != 203, ]
# 
# #In two dyads, participant had problems handling the tablet. I deleted them.
# 
# data <- data[data$dyad != 198 & data$dyad != 199, ]
# 
# #In three dyads, participants did not follow instructions.
# #In two dyads, they rated another person than the partner and in one dyad, one partner did not complete the PES.
# #I deleted them.
# 
# data <- data[data$dyad != 72 & data$dyad != 83 & data$dyad != 144, ]
# 
# #One dyad participated multiple times. One was deleted
# 
# data <- data[data$dyad != 202, ]
# 
# #One dyad number was assigned three times, but one data point was collected on an entirely different day.
# #I assume, this one is incorrect, so I deleted it.
# 
# data <- data[data$V1 != "R_zVHn6trwFheAJ33", ]
# 
# #Four dyad numbers were given two times and it was impossible to determine who rated who. I deleted all.
# 
# data <- data[data$dyad != 55 & data$dyad != 56 & data$dyad != 147 & data$dyad != 174, ]
# 
# #Four dyad numbers occurred only once. I deleted them.
# 
# data <- data[data$dyad != 143 & data$dyad != 145 & data$dyad != 176 & data$dyad != 183, ]
# 
# ###recoding variables
# 
# ##correcting some errors
# 
# #correcting age errors to the best of my knowledge
# data[data$age == ".24", "age"] <- "24"
# data[data$age == "t48", "age"] <- "48"
# data[data$age == "99", "age"] <- NA
# data[data$age == "999", "age"] <- NA
# data[data$age == "9999", "age"] <- NA
# 
# #One dyad number was given two times.
# #I matched them according to their dates and assigned one dyad = 300.
# 
# data[data$V1 == "R_2Xmwbo1fUiBqg8t", "dyad"] <- 300
# data[data$V1 == "R_3hg6TKUnwjvAeKJ", "dyad"] <- 300
# 
# #The following happened twice: One dyad number was assigned three times and another one time.
# #I could match them according to the time they participated.
# 
# data[data$V1 == "R_2uIJYO6joGhgaXC", "dyad"] <- 139
# data[data$V1 == "R_7ahWYZfngOrjIu5", "dyad"] <- 209
# 
# #For two dyads, partner values did not have one 1 and one 2. I recoded one of these values so that it is correct.
# data[data$V1 == "R_TaLZCaYBksf5dKN", "partner"] <- 2
# data[data$V1 == "R_3Gq5gDsYHtDybx8", "partner"] <- 2
# data[data$V1 == "R_25zGJ0Tc5yYKGW9", "partner"] <- 2
# data[data$V1 == "R_cM7bAYs5bVC007v", "partner"] <- 2
# 
# ##demographic variables
# data$sex <- factor(data$sex, levels = c(1,2), labels = c("male","female"))
# data$native_language <- factor(data$native_language, levels = c(1,2), labels = c("German","Other"))
# data$age <- as.numeric(as.character(data$age))
# 
# #pes1 - "Ehrlich gesagt habe ich das Gefühl, Anrecht auf mehr als andere zu haben."
# #pes2 - "Mir sollte Tolles widerfahren."
# #pes3 - "Wenn ich auf der Titanic wäre, würde ich es verdienen, auf dem ersten Rettungsboot zu sein."
# #pes4 - "Ich verlange das Beste, weil ich es wert bin."
# #pes5 - "Ich verdiene nicht unbedingt eine besondere Behandlung."
# #pes6 - "Ich verdiene mehr Dinge in meinem Leben."
# #pes7 - "Menschen wie ich verdienen es, ab und zu mehr Glück zu haben als andere."
# #pes8 - "Dinge sollten in meinem Interesse ablaufen."
# #pes9 - "Ich fühle mich berechtigt, mehr von allem zu erhalten."
# #Responses were given on a scale from 1 (do not agree at all) to 7 (agree completely).
# #
# #p1_self - "Ich werde von anderen in einigen Angelegenheiten als ExpertIn angesehen."
# #p2_self - "Meine einzigartigen Talente und Fähigkeiten werden von anderen anerkannt."
# #p3_self - "Andere fragen mich in einer Vielzahl von Angelegenheiten nach Rat."
# #p4_self - "Andere respektieren und bewundern mich."
# #d1_self - "Ich genieße es Kontrolle über andere zu haben."
# #d2_self - "Ich bin geneigt aggressive Vorgehensweisen zu nutzen, um meinen Willen zu bekommen."
# #d3_self - "Ich versuche immer meinen Willen zu bekommen, ohne Rücksicht darauf was andere wollen könnten."
# #d4_self - "Ich versuche eher andere zu kontrollieren, als ihnen zu erlauben mich zu kontrollieren."
# #
# #p1_other - "Mein/e Bekannte/r wird von anderen in einigen Angelegenheiten als ExpertIn angesehen."
# #p2_other - "Die einzigartigen Talente und Fähigkeiten meines/er Bekannten werden von anderen anerkannt."
# #p3_other - "Andere fragen meine/n Bekannte/n in einer Vielzahl von Angelegenheiten nach Rat."
# #p4_other - "Andere respektieren und bewundern meine/n Bekannte/n."
# #d1_other - "Mein/e Bekannte/r genießt es Kontrolle über andere zu haben."
# #d2_other - "Mein/e Bekannte/r ist geneigt aggressive Vorgehensweisen zu nutzen, um seinen/ihren Willen zu bekommen."
# #d3_other - "Mein/e Bekannte/r versucht immer seinen/ihren Willen zu bekommen, ohne Rücksicht darauf was andere wollen könnten."
# #d4_other - "Mein/e Bekannte/r versucht eher andere zu kontrollieren, als ihnen zu erlauben ihn/sie zu kontrollieren."
# #
# #Motivation items - "Wie sehr denken Sie, dass Ihr/e Bekannte/r sich WÜNSCHT, dass ..."
# #p1_other_mot - "er/sie von anderen in einigen Angelegenheiten als ExpertIn angesehen wird?"
# #p2_other_mot - "seine/ihre einzigartigen Talente und Fähigkeiten von anderen anerkannt werden?"
# #p3_other_mot - "Andere ihn/sie in einer Vielzahl von Angelegenheiten nach Rat fragen?"
# #p4_other_mot - "Andere ihn/sie respektieren und bewundern?"
# #d1_other_mot - "er/sie Kontrolle über andere hat?"
# #d2_other_mot - "er/sie aggressive Vorgehensweise nutzen kann, um seinen/ihren Willen zu bekommen?"
# #d3_other_mot - "er/sie immer seinen/ihren Willen bekommt, ohne Rücksicht darauf was andere wollen könnten?
# #d4_other_mot - "er/sie eher andere kontrollieren kann, als ihnen zu erlauben ihn/sie zu kontrollieren?"
# #Responsen were given on a scale from 1 (not at all), 4 (somewhat), to 7 (very)
# #
# #closeness - "Ich kenne die andere Person ..."
# #Responsen were given on a scale from 1 (not at all), 2 (fleeting), 3 (a little bit), 4 (pretty well),
# #to 5 (extremely well).
# 
# ###From here on, you find the steps to de-identify the data to reach the version on OSF.
# 
# #Several ages occurred only once. I formed groups.
# 
# quantile(data$age, c(.33,.66), na.rm = TRUE)
# 
# data$age <- ifelse(data$age <= 21, 1, ifelse(data$age > 21 & data$age <= 24, 2, 3))
# 
# #age 1 'bottom third' 2 'middle third' 3 'upper third'
# 
# #As dyads of acquainted participants took part in the study, one partner could identify the other with his/her responses.
# #Therefore, it is reasonable to report only mean values of the scales.
# #Thus, below you will find all steps towards this point (i.e., reliability analysis, mean calculation, descriptives).
# 
# #####
# #Entitlement
# #####
# 
# data$pes5r <- 8 - data$pes5
# 
# psych::alpha(data.frame(data$pes1,data$pes2,data$pes3,data$pes4,data$pes5r,data$pes6,data$pes7,data$pes8,data$pes9))
# 
# data$pes <- rowMeans(data[c("pes1","pes2","pes3","pes4","pes5r","pes6","pes7","pes8","pes9")], na.rm = TRUE)
# 
# #####
# #Status
# #####
# 
# ###Self-ratings
# 
# #self prestige
# 
# psych::alpha(data.frame(data$p1_self,data$p2_self,data$p3_self,data$p4_self))
# 
# data$p_self <- rowMeans(data[c("p1_self","p2_self","p3_self","p4_self")], na.rm = TRUE)
# 
# #self dominance
# 
# psych::alpha(data.frame(data$d1_self,data$d2_self,data$d3_self,data$d4_self))
# 
# data$d_self <- rowMeans(data[c("d1_self","d2_self","d3_self","d4_self")], na.rm = TRUE)
# 
# ###other-ratings
# 
# #other prestige
# 
# psych::alpha(data.frame(data$p1_other,data$p2_other,data$p3_other,data$p4_other))
# 
# data$p_other <- rowMeans(data[c("p1_other","p2_other","p3_other","p4_other")], na.rm = TRUE)
# 
# #other dominance
# 
# psych::alpha(data.frame(data$d1_other,data$d2_other,data$d3_other,data$d4_other))
# 
# data$d_other <- rowMeans(data[c("d1_other","d2_other","d3_other","d4_other")], na.rm = TRUE)
# 
# ###other-ratings motivation
# 
# #other prestige motivation
# 
# psych::alpha(data.frame(data$p1_other_mot,data$p2_other_mot,data$p3_other_mot,data$p4_other_mot))
# 
# data$p_other_mot <- rowMeans(data[c("p1_other_mot","p2_other_mot","p3_other_mot","p4_other_mot")], na.rm = TRUE)
# 
# #other dominance motivation
# 
# psych::alpha(data.frame(data$d1_other_mot,data$d2_other_mot,data$d3_other_mot,data$d4_other_mot))
# 
# data$d_other_mot <- rowMeans(data[c("d1_other_mot","d2_other_mot","d3_other_mot","d4_other_mot")], na.rm = TRUE)
# 
# ###selecting only the aggregated variables
# 
# data <- subset(data, select = c(dyad,partner,age,sex,native_language,closeness,pes:d_other_mot))
# 
# #Final OSF version reached. Code below saves file. This is the data set on OSF.
# 
# export(data, "Study3B.csv")
# 
# #The code below allows you to run all the analyses from the paper as long as the respective variables are unchanged.

data <- import("Study3B.csv", header = TRUE)

#####
#Demographics
#####

data$sex <- factor(data$sex, levels = c("male","female"), labels = c("male","female"))
data$native_language <- factor(data$native_language, levels = c("German","Other"), labels = c("German","Other"))

describe(data.frame(data$age))
describe(data$closeness)

#####
#Analyses
#####

describe(data.frame(data$pes,data$p_self,data$d_self,data$p_other,data$d_other))

#####
###creating the dyadic data set

#I do not take the motivation items into account

data_trans <- data
data_trans <- subset(data_trans, select = c(dyad,partner,age,sex,native_language,closeness,pes:d_other))
data_trans$sex <- as.numeric(data_trans$sex)
data_trans$native_language <- as.numeric(data_trans$native_language)

data_dyadic <- data_trans %>%
  mutate(partner = ifelse(partner == 1, "1", "2")) %>%
  gather(variable, value, age:d_other) %>%
  unite(var_partner, variable, partner) %>%
  spread(var_partner, value)

rm(data_trans)

###gender composition of dyads

data_dyadic$gender_comp <- ifelse(data_dyadic$sex_1 == data_dyadic$sex_2 & data_dyadic$sex_1 == 1, 1,
                                  ifelse(data_dyadic$sex_1 == data_dyadic$sex_2 & data_dyadic$sex_1 == 2, 2, 3))

data_dyadic$gender_comp <- factor(data_dyadic$gender_comp, levels = c(1,2,3), labels = c("same-gender male","same-gender female","mixed"))

summary(data.frame(data_dyadic$gender_comp))

###within-dyad correlations (in every other cell on the diagonal)

chart.Correlation(data_dyadic[, c("pes_1","pes_2","p_self_1","p_self_2","d_self_1","d_self_2",
                                  "p_other_1","p_other_2","d_other_1","d_other_2")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

###correlations separately for each partner

#partner 1
chart.Correlation(data_dyadic[ , c("pes_1","p_self_1","d_self_1","p_other_1","d_other_1")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

#partner 2
chart.Correlation(data_dyadic[ , c("pes_2","p_self_2","d_self_2","p_other_2","d_other_2")],
                  use = "pairwise.complete.obs", pch = 20, histogram = TRUE)

###convergence of self- and peer-ratings

#partner 1
cor.test(data_dyadic$p_self_1,data_dyadic$p_other_2)
cor.test(data_dyadic$d_self_1,data_dyadic$d_other_2)

#partner 2
cor.test(data_dyadic$p_self_2,data_dyadic$p_other_1)
cor.test(data_dyadic$d_self_2,data_dyadic$d_other_1)

export(data_dyadic, "Study3B_dyadic.csv")
